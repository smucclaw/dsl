{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds, KindSignatures, AllowAmbiguousTypes, ApplicativeDo #-}
{-# LANGUAGE TypeApplications, GADTs #-}
{-# LANGUAGE PatternSynonyms #-}


module LS.XPile.LogicalEnglish.SimplifyL4 (simplifyL4rule, SimpL4(..), SimL4Error(..)) where

import Data.Text qualified as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B

import Control.Monad.Validate
  ( MonadValidate (..)
    , Validate
    , refute
    )
import Optics 
import Data.Generics.Product.Types (types)
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.String (IsString)

import qualified AnyAll as AA
import LS.Types qualified as L4
import LS.Types (RelationalPredicate(..), RPRel(..), MTExpr(..))
import LS.Rule qualified as L4 (Rule(..))
import LS.XPile.LogicalEnglish.Types
-- import LS.XPile.LogicalEnglish.ValidateL4Input
--       (L4Rules, ValidHornls, Unvalidated,
--       loadRawL4AsUnvalid)


{-
TODOs: 
1. Think about checking for some of the `error ..`s upfront in the ValidateL4Input module
2. Check whether there are also regulative rules; throw error if so. This should prob be in the ValidateL4Input module
-}

-- | TODO: use more fine-grained types when time permits
newtype SimL4Error = MkErr { unpackErr :: T.Text }
  deriving newtype (Eq, Ord, IsString, Semigroup, Monoid, Hashable)
  deriving stock (Show)

newtype SimpL4 a = SimpL4 { runSimpL4 :: Validate (HS.HashSet SimL4Error) a }
    deriving newtype (Functor, Applicative, Monad, MonadValidate (HS.HashSet SimL4Error))
{- ^ TODOs: 
  * When time permits, we probably want to switch to validateT add Reader in there for metadata like the location of the erroring rule
-}

-- TODO: Switch over to this, e.g. with coerce or with `over` from new-type generic when have time: simplifyL4rule :: L4Rules ValidHornls -> [SimpL4 SimpleL4HC]
{- | 
  It's fine if  input L4 rules have more than 1 HC in their Horn clauses, as in the ditto syntax --- we get the givens and simplify each of the clauses with those givens   
  
  When writing L4, it’s important that
    * there not be empty cells in the head or body between contentful cells,
    * there not be `""` below the `DECIDE` --- if there are `""` below the `DECIDE`, then the stuff below will get parsed as distinct Hornlikes but without the givens, and the only way to then figure out what the original givens were will be very hacky / fragile 
-}
simplifyL4rule :: L4.Rule -> [SimpL4 SimpleL4HC]
simplifyL4rule l4rule =
  let
    gvars = gvarsFromL4Rule l4rule
    hcs = L4.clauses l4rule
  in map (simplifyL4hc gvars) hcs
  -- TODO: would probably be good to check upfront for whether there are L4 rules with no clauses and log a warning if such L4rules are found

{- | an L4 hc, in this context, is taken to be a L4.Rule with ___exactly one__ elt in its `clauses` field  
-}
simplifyL4hc :: GVarSet -> L4.HornClause2 -> SimpL4 SimpleL4HC
simplifyL4hc gvars l4hc = do
  simpHead  <- simplifyHead l4hc.hHead
  case l4hc.hBody of
    Nothing   ->
      pure $ MkL4FactHc {fgiven = gvars, fhead = simpHead}
      -- ^ There are Facts / HCs with Nothing in the body in the encoding 
    Just rbod -> do
      simpBod <- simplifyHcBodyBsr rbod
      pure $ MkL4RuleHc {rgiven = gvars, rhead = simpHead, rbody = simpBod}


{-------------------------------------------------------------------------------
    Simplifying L4 HCs
-------------------------------------------------------------------------------}

simplifyHead :: forall m. MonadValidate (HS.HashSet SimL4Error) m => L4.RelationalPredicate -> m L4AtomicP
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
  RPConstraint {}                -> refute [MkErr "should not be seeing RPConstraints other than the RPis pattern in head"]
  RPBoolStructR {}               -> refute [MkErr "RPBoolStructR in head of HC not supported"]
  RPParamText _                  -> refute [MkErr "RPParamText in head of HC not supported"]
  RPnary {}                      -> refute [MkErr "RPnary in the head of HC not supported."]



{- |  Simplifies the RPConstraint in the head of a L4 HC (from an encoding that conforms to the L4->LE spec).
Right now, the only RPConstraint tt can appear in head of L4 HC, according to spec, is RPis
-}
simpheadRPC :: [MTExpr] -> [MTExpr] -> L4AtomicP
simpheadRPC = simpRPCis

{- |
Given left and right exprs that flank an RPIs,
return a L4AtomicP where 
    <IS NUM>s have been marked accordingly in the numcell,
    and where the IS is otherwise made normal lowercase text.

Two cases of IS-ing to consider:
  1. It ends with an IS <NUM>
    in which case we should convert the NUM to text and warp it in a MkCellIsNum
  2. It does not
    in which case we should replace the IS with 'is' text


  An example of an is-num pattern in a RPConstraint:
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
simpRPCis :: [MTExpr] -> [MTExpr] -> L4AtomicP
simpRPCis exprsl exprsr =
  let lefts   = mtes2cells exprsl
      txtRPis = "is" :: T.Text
  in case exprsr of
    (MTI int : xs)   ->
      ABPatomic $ lefts <> [MkCellIsNum (int2Text int)] <> mtes2cells xs
    (MTF float : xs) ->
      ABPatomic $ lefts <> [MkCellIsNum (float2Text float)] <> mtes2cells xs
    _           ->
      ABPatomic (lefts <> [MkCellT txtRPis] <> mtes2cells exprsr)


{-------------------------------------------------------------------------------
    simplifying body of L4 HC
-------------------------------------------------------------------------------}

simplifyHcBodyBsr :: L4.BoolStructR -> SimpL4 (BoolPropn L4AtomicP)
simplifyHcBodyBsr = \case
  AA.Leaf rp      -> simplifybodyRP rp
  AA.All _ propns -> And <$> traverse simplifyHcBodyBsr propns
  AA.Any _ propns -> Or <$> traverse simplifyHcBodyBsr propns
  AA.Not propn    -> Not <$> simplifyHcBodyBsr propn
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

simplifybodyRP :: forall m. MonadValidate (HS.HashSet SimL4Error) m => 
                    RelationalPredicate -> m (BoolPropn L4AtomicP)
simplifybodyRP = \case
  RPMT exprs                         -> pure $ MkTrueAtomicBP (mtes2cells exprs)
                                        -- ^ this is the same for both the body and head
  RPConstraint exprsl rel exprsr     -> simpbodRPC exprsl exprsr rel

  -- max / min / sum x where φ(x)
  TermIsMaxXWhere term φx            -> pure $ MkIsOpSuchTtBP (mte2cell term) MaxXSuchThat (mtes2cells φx)
  TermIsMinXWhere term φx            -> pure $ MkIsOpSuchTtBP (mte2cell term) MinXSuchThat (mtes2cells φx)
  TermIsSumXWhere total φx           -> pure $ MkIsOpSuchTtBP (mte2cell total) SumEachXSuchThat (mtes2cells φx)

  -- max / min / sum of terms
  TermIsMax term maxargRPs           -> termIsNaryOpOf MaxOf term maxargRPs
  TermIsMin term minargRPs           -> termIsNaryOpOf MinOf term minargRPs
  TotalIsSumTerms total summandRPs   -> termIsNaryOpOf SumOf total summandRPs
  TotalIsProductTerms total argRPs   -> termIsNaryOpOf ProductOf total argRPs

  T1IsNotT2 t1 t2                    -> pure $ MkIsDiffFr (mte2cell t1) (mte2cell t2)

  RPnary{}                           -> refute [MkErr "The spec doesn't support other RPnary constructs in the body of a HC"]
  RPBoolStructR {}                   -> refute [MkErr "The spec does not support a RPRel other than RPis in a RPBoolStructR"]
  RPParamText _                      -> refute [MkErr "should not be seeing RPParamText in body"]


termIsNaryOpOf :: 
  (Foldable seq, Traversable seq, MonadValidate (HS.HashSet SimL4Error) m) => 
    OpOf -> MTExpr -> seq RelationalPredicate -> m (BoolPropn L4AtomicP)
termIsNaryOpOf op mteTerm rpargs = MkIsOpOf term op <$> argterms
  where term     = mte2cell mteTerm
        argterms = concat <$> traverse atomRPoperand2cell rpargs

atomRPoperand2cell :: forall m. MonadValidate (HS.HashSet SimL4Error) m =>
                          RelationalPredicate -> m [Cell]
atomRPoperand2cell = \case
  RPMT mtexprs    -> pure $ mtes2cells mtexprs
  RPParamText _pt -> refute ["not sure if we rly need this case (RPParamText in fn atomRPoperand2cell); erroring as a diagnostic tool"]
                    -- mtes2cells (concatMap (NE.toList . fst) (NE.toList pt)) 
  _               -> refute ["input rp supposed to be atomic"]


--------- simplifying RPConstraint in body of L4 HC ------------------------------------

simpbodRPC :: forall m. MonadValidate (HS.HashSet SimL4Error) m => 
                [MTExpr] -> [MTExpr] -> RPRel -> m (BoolPropn L4AtomicP)
simpbodRPC exprsl exprsr = \case
  RPis  -> pure $ AtomicBP (simpheadRPC exprsl exprsr)

  RPor  -> pure $ simBodRPCboolop RpcRPor exprsl exprsr
  RPand -> pure $ simBodRPCboolop RpcRPand exprsl exprsr

  RPlt  -> pure $ simBodRPCarithcomp RpcRPlt exprsl exprsr 
  RPlte -> pure $ simBodRPCarithcomp RpcRPlte exprsl exprsr 
  RPgt  -> pure $ simBodRPCarithcomp RpcRPgt exprsl exprsr 
  RPgte -> pure $ simBodRPCarithcomp RpcRPgte exprsl exprsr 
  
  _     -> refute [MkErr "shouldn't be seeing other rel ops in rpconstraint in body"]

{- |
  -- TODO: Check if this is still required in light of recent discussion
  Special case to handle for RPConstraint in the body but not the head: non-propositional connectives / anaphora!
      EG: ( Leaf
            ( RPConstraint
                [ MTT "data breach" , MTT "came about from"] 
                RPor
                [ MTT "luck, fate", MTT "acts of god or any similar event"]
            )
          )                           -}
simBodRPCboolop :: RpcRPrel RPnonPropAnaph -> [MTExpr] -> [MTExpr] -> BoolPropn L4AtomicP
simBodRPCboolop anaOp exprsl exprsr = 
  let 
      withLefts :: MTExpr -> BoolPropn L4AtomicP
      withLefts exprr = MkTrueAtomicBP (mtes2cells exprsl <> [mte2cell exprr])
  in case anaOp of 
    RpcRPor  -> Or (map withLefts exprsr)
    RpcRPand -> And (map withLefts exprsr) 
  
simBodRPCarithcomp :: RpcRPrel RParithComp -> [MTExpr] -> [MTExpr] -> BoolPropn L4AtomicP
simBodRPCarithcomp comp exprsl exprsr = 
  MkTrueAtomicBP (mtes2cells exprsl <> [MkCellT (comp2txt comp)] <> mtes2cells exprsr)
  where 
    comp2txt :: RpcRPrel RParithComp -> T.Text
    comp2txt = \case
      RpcRPlt  -> "<"
      RpcRPlte -> "<=" 
      RpcRPgt  -> ">"   
      RpcRPgte -> ">="  


{-------------------------------------------------------------------------------
    Misc
-------------------------------------------------------------------------------}

------------    Extracting vars from given   -----------------------------------

{- | Preconditions / invariants:
      * The input L4 rule is a Hornlike (TODO: And actually this fn is an eg of where it *would* be helpful to use the phantom type technique to tag that this is a Hornlike in the type)
      * Each gvar in the GIVEN declaration should occupy only one cell in the spreadsheet,
        so that the head of each NonEmpty MTExpr in the TypedMulti tuple would correspond to the gvar for that spreadsheet row in the declaration

An example of GIVENs in the AST, as of Sep 8 2023:
    given = Just (
            ( MTT "sightg" :| []
            , Just
                ( SimpleType TOne "Sighting" )
            ) :|
            [
                ( MTT "fun activity" :| []
                , Just
                    ( SimpleType TOne "Fun Activity" )
                )
            ,
                ( MTT "perzon" :| []
                , Just
                    ( SimpleType TOne "Person" )
                )
            ])
-}
getGivens :: L4.Rule -> [MTExpr]
getGivens l4rule = l4rule.given ^.. types @MTExpr

gvarsFromL4Rule :: L4.Rule -> GVarSet
gvarsFromL4Rule rule = 
  let givenMTExprs = getGivens rule
  in HS.fromList $ map gmtexpr2gvar givenMTExprs
    where
      -- | Transforms a MTExpr tt appears in the GIVEN of a HC to a Gvar. 
      gmtexpr2gvar :: MTExpr -> GVar
      gmtexpr2gvar = textifyMTE MkGVar
      -- TODO: Check upfront for wehther there are non-text mtexpr variable names in the GIVENs; raise a `dispute` if so and print warning as comment in resulting .le

------------    MTExprs to [Cell]    ------------------------------------------

textifyMTE :: (T.Text -> t) -> MTExpr -> t
textifyMTE constrtr = \case
  MTT t -> constrtr t
  MTI i -> constrtr (int2Text i)
  MTF f -> constrtr (float2Text f)
  MTB b -> constrtr (T.pack (show b))
            -- TODO: Prob shld check upfront for whether there are any MTB MTExprs in cells; raise a `dispute` if so and print warning as comment in resulting .le

mte2cell :: L4.MTExpr -> Cell
mte2cell = textifyMTE MkCellT

-- | convenience function for when `map mte2cell` too wordy 
mtes2cells :: [L4.MTExpr] -> [Cell]
mtes2cells = fmap mte2cell

------ Other misc utils
{-| From https://github.com/haskell/text/issues/218
Thanks to Jo Hsi for finding these!
-}
float2Text :: RealFloat a => a -> T.Text
float2Text = T.toStrict . B.toLazyText . B.realFloat

int2Text :: Integral a => a -> T.Text
int2Text = T.toStrict . B.toLazyText . B.decimal


--- misc notes
-- wrapper :: L4Rules ValidHornls -> [(NE.NonEmpty MTExpr, Maybe TypeSig)]
-- wrapper = concat . map getGivens . coerce
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