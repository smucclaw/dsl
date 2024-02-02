{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds, KindSignatures, AllowAmbiguousTypes, ApplicativeDo #-}
{-# LANGUAGE TypeApplications, GADTs #-}
{-# LANGUAGE PatternSynonyms #-}


module LS.XPile.LogicalEnglish.SimplifyL4
  ( simplifyL4rule,
    SimpL4 (..),
    SimL4Error (..),
  )
where

import Data.Text qualified as T
import LS.Utils.TextUtils (int2Text, float2Text)

import Control.Monad.Validate
  ( MonadValidate (..)
    , Validate
    , refute
  )
-- import Optics
-- import Data.Generics.Product.Types (types)
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.String (IsString)
import Data.String.Interpolate (i)
import Data.Traversable (for)

import AnyAll qualified as AA
import LS.Types qualified as L4
import LS.Types (RelationalPredicate(..), RPRel(..), MTExpr(..))
import LS.Rule qualified as L4 (Rule(..), extractMTExprs)
import LS.XPile.LogicalEnglish.Types
  ( BoolPropn(..)
    -- L4-related types
    , RpcRPrel(..)

    , RParithComp

    , GVar(..)
    , GVarSet
    , Cell(..)

    , SimpleL4HC(MkL4FactHc, fgiven, fhead,
                 MkL4RuleHc, rgiven, rhead, rbody)

    , OpOf(..)
    , OpSuchTt(..)
    , AtomicBPropn(..)
    , L4AtomicP
    , pattern MkTrueAtomicBP
    , pattern MkIsOpSuchTtBP
    , pattern MkIsOpOf
    , pattern MkIsDiffFr
    , pattern MkIsIn
  )
import LS.XPile.LogicalEnglish.ReplaceTxt (replaceTxt)
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
We no longer want to generate annotations 
from things involving an IS cell --- IS should be treated specially, as a built-in predicate
-}
simpRPCis :: [MTExpr] -> [MTExpr] -> L4AtomicP
simpRPCis exprsl exprsr =
  let (lefts, rights) = (mtes2cells exprsl, mtes2cells exprsr)
  in ABPBaseIs lefts rights

{-------------------------------------------------------------------------------
    simplifying body of L4 HC
-------------------------------------------------------------------------------}

simplifyHcBodyBsr :: L4.BoolStructR -> SimpL4 (BoolPropn L4AtomicP)
simplifyHcBodyBsr = \case
  AA.Leaf rp      -> simplifybodyRP rp
  AA.All _ propns -> go And $ for propns
  AA.Any _ propns -> go Or $ for propns
  AA.Not propn    -> go Not ($ propn)
  where
    go ctor propns = ctor <$> propns simplifyHcBodyBsr
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

pattern T1IsInT2 :: MTExpr -> MTExpr -> RelationalPredicate
pattern T1IsInT2 t1 t2 <- RPnary RPis [ RPMT [t1]
                                      , RPnary RPelem
                                            [ RPMT
                                              [t2] ]]
  where T1IsInT2 t1 t2 = RPnary RPis [ RPMT [t1], RPnary RPelem [ RPMT [t2] ]]

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
pattern TermIsNumberOfXWhere :: MTExpr -> MTExpr -> [MTExpr] -> [MTExpr]

pattern TermIsMaxXWhere term φx = TermIsOpSuchThat RPmax term φx
pattern TermIsMinXWhere term φx = TermIsOpSuchThat RPmin term φx
pattern TermIsSumXWhere term φx = TermIsOpSuchThat RPsum term φx
pattern TermIsNumberOfXWhere term x φx <- (term :
                                            (MTT "is the number of" : (x :
                                            (MTT "where" : φx))))

{- ^ 
Examples of the L4 patterns

```
        [ Leaf
            ( RPMT
                [ MTT "n"
                , MTT "is the number of"
                , MTT "friend"
                , MTT "where"
                , MTT "person"
                , MTT "met"
                , MTT "friend"
                , MTT "on"
                , MTT "date"
                ]
            )
```

TermIsMaxXWhere:
```
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
```

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

t1 IS IN t2:
        ( RPnary RPis
            [ RPMT
                [ MTT "thing" ]
            , RPnary RPelem
                [ RPMT
                    [ MTT "set of things" ]
                ] ] )
-}
simplifybodyRP :: forall m. MonadValidate (HS.HashSet SimL4Error) m =>
                    RelationalPredicate -> m (BoolPropn L4AtomicP)
simplifybodyRP = \case

  -- is number of x where φ(x)
  RPMT (TermIsNumberOfXWhere term x φx) -> pure $ MkIsOpSuchTtBP (mte2cell term) (NumOfSuchThat $ mte2cell x) (mtes2cells φx)
  RPMT otherExprs                       -> pure $ MkTrueAtomicBP (mtes2cells otherExprs)

  RPConstraint exprsl rel exprsr        -> simpbodRPC exprsl exprsr rel

  -- max / min / sum x where φ(x), corresponding to RPnary RPis ...
  TermIsMaxXWhere term φx            -> pure $ MkIsOpSuchTtBP (mte2cell term) MaxXSuchThat (mtes2cells φx)
  TermIsMinXWhere term φx            -> pure $ MkIsOpSuchTtBP (mte2cell term) MinXSuchThat (mtes2cells φx)
  TermIsSumXWhere total φx           -> pure $ MkIsOpSuchTtBP (mte2cell total) SumEachXSuchThat (mtes2cells φx)

  -- max / min / sum of terms; another RPnary RPis ...
  TermIsMax term maxargRPs           -> termIsNaryOpOf MaxOf term maxargRPs
  TermIsMin term minargRPs           -> termIsNaryOpOf MinOf term minargRPs
  TotalIsSumTerms total summandRPs   -> termIsNaryOpOf SumOf total summandRPs
  TotalIsProductTerms total argRPs   -> termIsNaryOpOf ProductOf total argRPs

  -- t1 is not t2 / t1 is in t2
  T1IsNotT2 t1 t2                    -> pure $ MkIsDiffFr (mte2cell t1) (mte2cell t2)
  T1IsInT2  t1 t2                    -> pure $ MkIsIn     (mte2cell t1) (mte2cell t2)

  -- others not supported
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
                    -- mtes2cells (foldMap (NE.toList . fst) (NE.toList pt)) 
  _               -> refute ["input rp supposed to be atomic"]


--------- simplifying RPConstraint in body of L4 HC ------------------------------------

simpbodRPC :: forall m. MonadValidate (HS.HashSet SimL4Error) m =>
                [MTExpr] -> [MTExpr] -> RPRel -> m (BoolPropn L4AtomicP)
simpbodRPC exprsl exprsr = \case
  RPis  -> pure $ AtomicBP $ simpheadRPC exprsl exprsr

  RPlt  -> comp2boolPropn RpcRPlt
  RPlte -> comp2boolPropn RpcRPlte
  RPgt  -> comp2boolPropn RpcRPgt
  RPgte -> comp2boolPropn RpcRPgte
  RPeq  -> comp2boolPropn RpcRPeq

  RPor  -> refute [MkErr "|| no longer supported -- use ditto and OR instead"]
  RPand -> refute [MkErr "&& no longer supported -- use ditto and AND instead"]
  -- TODO: test this

  _     -> refute [MkErr "shouldn't be seeing other rel ops in rpconstraint in body"]
  where
    comp2boolPropn :: RpcRPrel RParithComp -> m (BoolPropn L4AtomicP)
    comp2boolPropn comp = pure $ simBodRPCarithcomp comp exprsl exprsr

simBodRPCarithcomp :: RpcRPrel RParithComp -> [MTExpr] -> [MTExpr] -> BoolPropn L4AtomicP
simBodRPCarithcomp comp exprsl exprsr =
  MkTrueAtomicBP $ mtes2cells exprsl <> [comp2cell comp] <> mtes2cells exprsr
  where
    comp2cell :: RpcRPrel RParithComp -> Cell
    comp2cell comp = MkCellT (comp2txt comp)
    comp2txt :: RpcRPrel RParithComp -> T.Text
    comp2txt = \case
      RpcRPlt  -> "<"
      RpcRPlte -> "<="
      RpcRPgt  -> ">"
      RpcRPgte -> ">="
      RpcRPeq  -> "="

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
getGivens l4rule = L4.extractMTExprs l4rule.given

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
textifyMTE constrtr =
  constrtr . \case
    MTT t -> replaceTxt t
    MTI i -> int2Text i
    MTF f -> leFloat2Text f
    MTB b -> T.toLower [i|#{b}|]

mte2cell :: L4.MTExpr -> Cell
mte2cell = \case
  mte@(MTT _) -> textify mte
  mte@(MTB _) -> textify mte

  mte@(MTI _) -> numify mte
  mte@(MTF _) -> numify mte
  where
    textify = textifyMTE MkCellT
    numify  = textifyMTE MkCellNum
  -- could use prisms and guards instead to get less 'repetition of code', but that would also increase the risk of incomplete case matching in the future

-- | convenience function for when `map mte2cell` too wordy 
mtes2cells :: [L4.MTExpr] -> [Cell]
mtes2cells = fmap mte2cell

------ Other misc utils

leFloat2Text :: RealFloat a => a -> T.Text
leFloat2Text f =
  case (isInfinite f, f > 0) of
    (True, True) -> "inf"
    (True, False) -> "-inf"
    _ -> float2Text f

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
