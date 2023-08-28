{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds, KindSignatures, AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}


module LS.XPile.LogicalEnglish.SimplifyL4 (simplifyL4hc, SimpL4(..), SimL4Error(..)) where

import Data.Text qualified as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B

import Control.Monad.Validate
  ( MonadValidate (..)
    , Validate
    , refute
    , dispute
    , runValidate
    )
import Control.Monad.Identity

import Data.Bifunctor       ( first )
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.HashMap.Strict qualified as Map
import Data.String (IsString)
import Data.List.NonEmpty qualified as NE
import Debug.Trace (trace)

import qualified AnyAll as AA
import LS.Types qualified as L4
import LS.Types (RelationalPredicate(..), RPRel(..), MTExpr(..), BoolStructR(..), BoolStructT)
import LS.Rule qualified as L4 (Rule(..))
import LS.XPile.LogicalEnglish.Types
import LS.XPile.LogicalEnglish.ValidateL4Input
      (L4Rules, ValidHornls, Unvalidated,
      loadRawL4AsUnvalid)


{-
TODOs: 
1. Think about checking for some of the `error ..`s upfront in the ValidateL4Input module
2. Check whether there are also regulative rules; throw error if so. This should prob be in the ValidateL4Input module
-}

-- | TODO: use more fine-grained types when time permits
newtype SimL4Error = MkErr { unpackErr :: T.Text }
  deriving newtype (Eq, Ord, IsString, Semigroup, Monoid)
  deriving stock (Show)

newtype SimpL4 a = SimpL4 { runSimpL4 :: Validate [SimL4Error] a }
    deriving newtype (Functor, Applicative, Monad, MonadValidate [SimL4Error])
{- ^ TODOs: 
1. Use the newtype...
2. Per monad-validate's docs, move away from native linked list when time permits --- use Dual [a] or Seq or even Hashset
3. When time permits, we probably want to switch to validateT add Reader in there for metadata like the location of the erroring rule
-}

-- TODO: Switch over to this, e.g. with coerce or with `over` from new-type generic when have time: simplifyL4rule :: L4Rules ValidHornls -> SimpleL4HC
{- | 
  Precondition: assume that the input L4 rules only have 1 HC in their Horn clauses. 
  TODO: This invariant will have to be established in the next iteration of work on this transpiler (mainly by desugaring the 'ditto'/decision table stuff accordingly first) 
-}
simplifyL4hc :: L4.Rule -> SimpL4 SimpleL4HC
simplifyL4hc l4hc = do
  let gvars  = gvarsFromL4Rule l4hc
      clause = Prelude.head $ L4.clauses l4hc
              -- ^ this use of head will be safe in the future iteration when we do validation and make sure that there will be exactly one HC in every L4 rule that this fn gets called on
  simpHead  <- simplifyHead clause.hHead

  case clause.hBody of
    Nothing   ->
      pure $ MkL4FactHc {fgiven = gvars, fhead = simpHead}
    Just rbod -> do
      simpBod <- simplifyHcBodyBsr rbod
      pure $ MkL4RuleHc {rgiven = gvars, rhead = simpHead, rbody = simpBod}
    -- ^ There are Facts / HCs with Nothing in the body in the encoding 

{-------------------------------------------------------------------------------
    Simplifying L4 HCs
-------------------------------------------------------------------------------}

simplifyHead :: L4.RelationalPredicate -> SimpL4 L4AtomicP
simplifyHead = \case
  RPMT exprs                      -> pure $ ABPatomic . mtes2cells $ exprs
  RPConstraint exprsl RPis exprsr -> pure $ simpheadRPC exprsl exprsr
                                    {- ^ 
                                      1. Match on RPis directly cos no other rel operator shld appear here in the head, given the encoding convention / invariants.

                                      2. Can't just lowercase IS and transform the mtexprs to (either Text or Integer) Cells 
                                        because it could be a IS-number, 
                                        and when making template vars later, we need to be able to disambiguate between something tt was an IS-kw and smtg tt was originally lowercase 'is'. 
                                        TODO: But do think more abt this when we implement the intermed stage
                                        TODO: Need to account for / stash info for IS NOT --- I think that would be handled in the intermed stage, but shld check again when we get there.

                                      We handle the case of RPis in a RPConstraint the same way in both the body and head. 
                                    -}
  RPConstraint {}                -> SimpL4 $ refute [MkErr "should not be seeing RPConstraints other than the RPis pattern in head"]
  RPBoolStructR {}               -> SimpL4 $ refute [MkErr "RPBoolStructR in head of HC not supported"]
  RPParamText _                  -> SimpL4 $ refute [MkErr "RPParamText in head of HC not supported"]
  RPnary {}                      -> SimpL4 $ refute [MkErr "RPnary in the head of HC not supported."]


{- ^
An example of an is-num pattern in a RPConstraint
[ HC
    { hHead = RPConstraint
        [ MTT "total savings" ] RPis
        [ MTI 100 ]
    , hBody = Just
        ( All Nothing
            [ Leaf
                ( RPConstraint
                    [ MTT "initial savings" ] RPis
                    [ MTF 22.5 ]
                )
-}
{- | 
Simplifies the RPConstraint in the head of a L4 HC (from an encoding that conforms to the L4->LE spec).

Given left and right exprs that flank an RPIs,
return a L4AtomicP where 
    <IS NUM>s have been marked accordingly in the numcell,
    and where the IS is otherwise made normal lowercase text.

Two cases of IS-ing to consider:
  1. It ends with an IS <NUM>
    in which case we should convert the NUM to text and warp it in a MkCellIsNum
  2. It does not
    in which case we should replace the IS with 'is' text
-}
simpheadRPC :: [MTExpr] -> [MTExpr] -> L4AtomicP
simpheadRPC exprsl exprsr =
  let lefts = mtes2cells exprsl
  in case exprsr of
    (MTI int : xs)   -> 
      ABPatomic $ lefts <> [MkCellIsNum (int2Text int)] <> mtes2cells xs
    (MTF float : xs) -> 
      ABPatomic $ lefts <> [MkCellIsNum (float2Text float)] <> mtes2cells xs
    _           ->
      ABPatomic (lefts <> [MkCellT "is"] <> mtes2cells exprsr)
  

{-------------------------------------------------------------------------------
    simplifying body of L4 HC
-------------------------------------------------------------------------------}

simplifyHcBodyBsr :: L4.BoolStructR -> SimpL4 (BoolPropn L4AtomicP)
simplifyHcBodyBsr = \case
  AA.Leaf rp      -> 
    simplifybodyRP rp
  AA.All _ propns -> do
    props <- mapM simplifyHcBodyBsr propns
    return $ And props
  AA.Any _ propns -> do
    props <- mapM simplifyHcBodyBsr propns
    return $ Or props
  AA.Not propn    -> do
    prop <- simplifyHcBodyBsr propn
    return $ Not prop
{- ^ where a 'L4 propn' = BoolStructR =  BoolStruct _lbl RelationalPredicate.
Note that a BoolStructR is NOT a 'RPBoolStructR' --- a RPBoolStructR is one of the data constructors for the RelationalPredicate sum type
-}

-- patterns for simplifybodyRP
pattern T1IsNotT2 :: L4.MTExpr -> L4.MTExpr -> L4.RelationalPredicate
pattern T1IsNotT2 t1 t2 <- RPBoolStructR [t1] RPis (AA.Not (AA.Leaf (RPMT [t2])))

pattern TermIsOpOfAtomicTerms :: RPRel -> MTExpr -> [RelationalPredicate] -> RelationalPredicate
pattern TermIsOpOfAtomicTerms op result args <- RPnary RPis (RPMT [result] : [RPnary op args])
  where TermIsOpOfAtomicTerms op result args = RPnary RPis (RPMT [result] : [RPnary op args])
  -- needed b/c GHC can't infer tt this is invertible if OverloadedLists extn is enabled

pattern TotalIsSumTerms :: MTExpr -> [RelationalPredicate] -> RelationalPredicate
pattern TotalIsProductTerms :: MTExpr -> [RelationalPredicate] -> RelationalPredicate
pattern TermIsMax :: MTExpr -> [RelationalPredicate] -> RelationalPredicate
pattern TermIsMin :: MTExpr -> [RelationalPredicate] -> RelationalPredicate

pattern TotalIsSumTerms total summandAtomRPs = TermIsOpOfAtomicTerms RPsum total summandAtomRPs
pattern TotalIsProductTerms total atomargs = TermIsOpOfAtomicTerms RPproduct total atomargs
pattern TermIsMax maxE atomargs = TermIsOpOfAtomicTerms RPmax maxE atomargs
pattern TermIsMin minE atomargs = TermIsOpOfAtomicTerms RPmin minE atomargs


pattern TermIsOpSuchThat :: RPRel -> MTExpr -> [MTExpr] -> RelationalPredicate
pattern TermIsOpSuchThat op term φx <- RPnary RPis
                                          (RPMT [term] :
                                              [RPnary op
                                                [RPMT (MTT "x": (MTT "where" : φx))]])
  where TermIsOpSuchThat op term φx = RPnary RPis (RPMT [term] : [RPnary op [RPMT (MTT "x": (MTT "where" : φx))]])
  -- needed b/c GHC can't infer tt this is invertible if OverloadedLists extn is enabled

pattern TermIsMaxXWhere :: MTExpr -> [MTExpr] -> RelationalPredicate
pattern TermIsMinXWhere :: MTExpr -> [MTExpr] -> RelationalPredicate
pattern TermIsSumXWhere :: MTExpr -> [MTExpr] -> RelationalPredicate

pattern TermIsMaxXWhere term φx = TermIsOpSuchThat RPmax term φx
pattern TermIsMinXWhere term φx = TermIsOpSuchThat RPmin term φx
pattern TermIsSumXWhere term φx = TermIsOpSuchThat RPsum term φx

{- ^ 
Examples of the L4 patterns
    TermIsMaxXWhere:
          ( RPnary RPis
              [ RPMT
                  [ MTT "savings" ]
              , RPnary RPmax
                  [ RPMT
                      [ MTT "x"
                      , MTT "where"
                      , MTT "x"
                      , MTT "is the thing u saved"
                      ]]])

t1 IS NOT t2:
```
      ( RPBoolStructR
          [ MTT "stumbling" ] RPis
          ( Not ( Leaf ( RPMT [ MTT "walking" ] ) ) )
```

t IS SUM t1 t2 ... tn:
            ( RPnary RPis
                [ RPMT
                    [ MTT "z" ]
                , RPnary RPsum
                    [ RPMT
                        [ MTT "initial savings" ]
                    , RPMT
                        [ MTT "inititial savings * percentage" ]
                    ]])

t IS MIN t1 t2 .. tn:
            ( RPnary RPis
                [ RPMT
                    [ MTT "amountsaved" ]
                , RPnary RPmin
                    [ RPMT
                        [ MTT "1.5 * initial savings" ]
                    , RPMT
                        [ MTI 1000 ]
                    ]])
-}

simplifybodyRP :: RelationalPredicate -> SimpL4 (BoolPropn L4AtomicP)
simplifybodyRP = \case
  RPMT exprs                         -> pure $ MkTrueAtomicBP (mtes2cells exprs)
                                     -- ^ this is the same for both the body and head
  RPConstraint exprsl rel exprsr     -> case rel of
                                          RPis  -> pure $ simpbodRPC @RPis exprsl exprsr
                                          RPor  -> pure $ simpbodRPC @RPor exprsl exprsr
                                          RPand -> pure $ simpbodRPC @RPand exprsl exprsr
                                          _     -> SimpL4 $ refute [MkErr "shouldn't be seeing other rel ops in rpconstraint in body"]
                                          {- ^ Special case to handle for RPConstraint in the body but not the head: non-propositional connectives / anaphora!
                                              EG: ( Leaf
                                                    ( RPConstraint
                                                        [ MTT "data breach" , MTT "came about from"] 
                                                        RPor
                                                        [ MTT "luck, fate", MTT "acts of god or any similar event"]
                                                    )
                                                  )                           -}

  -- max / min / sum x where φ(x)
  TermIsMaxXWhere term φx            -> pure $ MkIsOpSuchTtBP (mte2cell term) MaxXSuchThat (mtes2cells φx)
  TermIsMinXWhere term φx            -> pure $ MkIsOpSuchTtBP (mte2cell term) MinXSuchThat (mtes2cells φx)
  TermIsSumXWhere total φx           -> pure $ MkIsOpSuchTtBP (mte2cell total) SumEachXSuchThat (mtes2cells φx)

  -- max / min / sum of terms
  TermIsMax term maxargRPs           -> pure $ termIsNaryOpOf MaxOf term maxargRPs
  TermIsMin term minargRPs           -> pure $ termIsNaryOpOf MinOf term minargRPs
  TotalIsSumTerms total summandRPs   -> pure $ termIsNaryOpOf SumOf total summandRPs
  TotalIsProductTerms total argRPs   -> pure $ termIsNaryOpOf ProductOf total argRPs

  T1IsNotT2 t1 t2                    -> pure $ MkIsDiffFr (mte2cell t1) (mte2cell t2)

  RPnary{}                           -> SimpL4 $ refute [MkErr "The spec doesn't support other RPnary constructs in the body of a HC"]
  RPBoolStructR {}                   -> SimpL4 $ refute [MkErr "The spec does not support a RPRel other than RPis in a RPBoolStructR"]
  RPParamText _                      -> SimpL4 $ refute [MkErr "should not be seeing RPParamText in body"]


termIsNaryOpOf :: Foldable seq => OpOf -> MTExpr -> seq RelationalPredicate -> BoolPropn L4AtomicP
termIsNaryOpOf op mteTerm rpargs = MkIsOpOf term op argterms
  where term     = mte2cell mteTerm
        argterms = concatMap atomRPoperand2cell rpargs


atomRPoperand2cell :: RelationalPredicate -> [Cell]
atomRPoperand2cell = \case
  RPMT mtexprs    -> mtes2cells mtexprs
  RPParamText _pt -> error "not sure if we rly need this case (RPParamText in fn atomRPoperand2cell); erroring as a diagnostic tool"
                    -- mtes2cells (concatMap (NE.toList . fst) (NE.toList pt)) 
  _               -> error "input rp supposed to be atomic"


--------- simplifying RPConstraint in body of L4 HC ------------------------------------

-- https://www.tweag.io/blog/2022-11-15-unrolling-with-typeclasses/
class SimpBodyRPConstrntRPrel (rp :: RPRel) where
  simpbodRPC :: [MTExpr] -> [MTExpr] -> BoolPropn L4AtomicP

instance SimpBodyRPConstrntRPrel RPis where
  simpbodRPC exprsl exprsr = AtomicBP (simpheadRPC exprsl exprsr)

instance SimpBodyRPConstrntRPrel RPor where
  simpbodRPC exprsl exprsr = undefined
  -- TODO: implement this!

instance SimpBodyRPConstrntRPrel RPand where
  simpbodRPC exprsl exprsr = undefined
  -- TODO: implement this!
--------------------------------------------------------------------------------


{-------------------------------------------------------------------------------
    Misc
-------------------------------------------------------------------------------}

------------    Extracting vars from given   -----------------------------------

extractGiven :: L4.Rule -> [MTExpr]
  -- [(NE.NonEmpty MTExpr, Maybe TypeSig)]
extractGiven L4.Hornlike {given=Nothing}        = []
-- won't need to worry abt this when we add checking upfront
extractGiven L4.Hornlike {given=Just paramtext} = concatMap (NE.toList . fst) (NE.toList paramtext)
extractGiven _                                  = trace "not a Hornlike rule, not extracting given" mempty
-- also won't need to worry abt this when we add checking + filtering upfront


gvarsFromL4Rule :: L4.Rule -> GVarSet
gvarsFromL4Rule rule = let givenMTExprs = extractGiven rule
                       in HS.fromList $ map gmtexpr2gvar givenMTExprs
        where
          -- | Transforms a MTExpr tt appears in the GIVEN of a HC to a Gvar. This is importantly different from `mtexpr2text` in that it only converts the cases we use for LE and that we would encounter in the Givens on our LE conventions
          gmtexpr2gvar :: MTExpr -> GVar
          gmtexpr2gvar = \case
            MTT var -> MkGVar var
            _       -> error "non-text mtexpr variable names in the GIVEN are not allowed on our LE spec :)"

------------    MTExprs to [Cell]    ------------------------------------------

mte2cell :: L4.MTExpr -> Cell
mte2cell = \case
  MTT t -> MkCellT t
  MTI i -> MkCellT (int2Text i)
  MTF f -> MkCellT (float2Text f)
  _     -> error "Booleans in cells currently not supported"

-- | convenience function for when `map mte2cell` too wordy 
mtes2cells :: [L4.MTExpr] -> [Cell]
mtes2cells = map mte2cell

------ Other misc utils
{-| From https://github.com/haskell/text/issues/218 lol
Thanks to Jo Hsi for finding these!
-}
float2Text :: RealFloat a => a -> T.Text
float2Text = T.toStrict . B.toLazyText . B.realFloat

int2Text :: Integral a => a -> T.Text
int2Text = T.toStrict . B.toLazyText . B.decimal


--- misc notes
-- wrapper :: L4Rules ValidHornls -> [(NE.NonEmpty MTExpr, Maybe TypeSig)]
-- wrapper = concat . map extractGiven . coerce
{- a more ambitious version, for the future: 
data SimL4Error = Error {  errInfo :: SimL4ErrorInfo
                            --  in the future: errLoc :: ... 
                        }


data SimL4ErrorInfo = HeadErr !T.Text
                    | BodyErr !T.Text
pattern MkHeadErr :: T.Text -> SimL4Error
pattern MkBodyErr :: T.Text -> SimL4Error
pattern MkHeadErr errtxt = Error (HeadErr errtxt)
pattern MkBodyErr errtxt = Error (BodyErr errtxt)
-}