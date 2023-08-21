{-# OPTIONS_GHC -W #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds, KindSignatures, AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}


module LS.XPile.LogicalEnglish.SimplifyL4 where
-- TODO: Make export list

import Data.Text qualified as T
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

import LS.XPile.LogicalEnglish.UtilsLEReplDev -- for prototyping

{-
TODO: All the `error ..`s should be checked for upfront in the ValidateL4Input module
-}


-- TODO: Switch over to this, e.g. with coerce or with `over` from new-type generic when have time: simplifyL4rule :: L4Rules ValidHornls -> SimpleL4HC
{- | 
  Precondition: assume that the input L4 rules only have 1 HC in their Horn clauses. 
  TODO: This invariant will have to be established in the next iteration of work on this transpiler (mainly by desugaring the 'ditto'/decision table stuff accordingly first) 
-}
simplifyL4ruleish :: L4.Rule -> SimpleL4HC
simplifyL4ruleish l4r = 
  let gvars  = gvarsFromL4Rule l4r
      clause = Prelude.head $ L4.clauses l4r
      simpHead  = simplifyHead clause.hHead
               -- this use of head will be safe in the future iteration when we do validation and make sure that there will be exactly one HC in every L4 rule that this fn gets called on
  in case clause.hBody of 
    Nothing   -> MkL4FactHc {fgiven = gvars, fhead = simpHead}
    Just rbod -> MkL4RuleHc {rgiven = gvars, rhead = simpHead, rbody = simplifyHcBodyBsr rbod}
    -- ^ There are Facts / HCs with Nothing in the body in the encoding 

{-------------------------------------------------------------------------------
    Simplifying L4 HCs
-------------------------------------------------------------------------------}

simplifyHead :: L4.RelationalPredicate -> L4AtomicBP
simplifyHead = \case
  RPMT exprs                      -> ABPatomic $ mtes2cells exprs
  RPConstraint exprsl RPis exprsr -> simpheadRPC exprsl exprsr
                                    {- ^ 
                                      1. Match on RPis directly cos no other rel operator shld appear here in the head, given the encoding convention / invariants.

                                      2. Can't just lowercase IS and transform the mtexprs to (either Text or Integer) Cells 
                                        because it could be a IS-number, 
                                        and when making template vars later, we need to be able to disambiguate between something tt was an IS-kw and smtg tt was originally lowercase 'is'. 
                                        TODO: But do think more abt this when we implement the intermed stage
                                        TODO: Need to account for / stash info for IS NOT --- I think that would be handled in the intermed stage, but shld check again when we get there.

                                      We handle the case of RPis in a RPConstraint the same way in both the body and head. 
                                    -}
  RPConstraint {}                -> error "should not be seeing other kinds of RPConstraint in head"
  RPBoolStructR {}               -> error "should not be seeing RPBoolStructR in head"
  RPParamText _                  -> error "should not be seeing RPParamText in head"
  RPnary {}                      -> error "I don't see any RPnary in the head in Joe's encoding, so."


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

-- | Assumes it's an RPis
simpheadRPC :: [MTExpr] -> [MTExpr] -> L4AtomicBP
simpheadRPC exprsl exprsr = ABPatomic (mtes2cells exprsl <> [MkCellIs] <> mtes2cells exprsr)


{-------------------------------------------------------------------------------
    simplifying body of L4 HC
-------------------------------------------------------------------------------}

simplifyHcBodyBsr :: L4.BoolStructR -> BoolPropn L4AtomicBP
simplifyHcBodyBsr = \case
  AA.Leaf rp      -> simplifybodyRP rp
  AA.All _ propns -> And (map simplifyHcBodyBsr propns)
  AA.Any _ propns -> Or (map simplifyHcBodyBsr propns)
  AA.Not propn    -> Not (simplifyHcBodyBsr propn)
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
pattern TermIsMax maxE atomargs = TermIsOpOfAtomicTerms RPgt maxE atomargs
pattern TermIsMin minE atomargs = TermIsOpOfAtomicTerms RPlt minE atomargs


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

pattern TermIsMaxXWhere term φx = TermIsOpSuchThat RPgt term φx
pattern TermIsMinXWhere term φx = TermIsOpSuchThat RPlt term φx
pattern TermIsSumXWhere term φx = TermIsOpSuchThat RPsum term φx

{- ^ 
Examples of the L4 patterns
    TermIsMaxXWhere:
          ( RPnary RPis
              [ RPMT
                  [ MTT "savings" ]
              , RPnary RPgt
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
                , RPnary RPlt
                    [ RPMT
                        [ MTT "1.5 * initial savings" ]
                    , RPMT
                        [ MTI 1000 ]
                    ]])
-}

simplifybodyRP :: RelationalPredicate -> BoolPropn L4AtomicBP
simplifybodyRP = \case
  RPMT exprs                         -> MkTrueAtomicBP (mtes2cells exprs)
                                     -- ^ this is the same for both the body and head
  RPConstraint exprsl rel exprsr     -> case rel of
                                          RPis  -> simpbodRPC @RPis exprsl exprsr
                                          RPor  -> simpbodRPC @RPor exprsl exprsr
                                          RPand -> simpbodRPC @RPand exprsl exprsr
                                          _     -> error "shouldn't be seeing other rel ops in rpconstraint in body"
                                          {- ^ Special case to handle for RPConstraint in the body but not the head: non-propositional connectives / anaphora!
                                              EG: ( Leaf
                                                    ( RPConstraint
                                                        [ MTT "data breach" , MTT "came about from"] 
                                                        RPor
                                                        [ MTT "luck, fate", MTT "acts of god or any similar event"]
                                                    )
                                                  )                           -}

  -- max / min / sum x where φ(x)
  TermIsMaxXWhere term φx            -> MkIsOpSuchTtBP (mte2cell term) MaxXSuchThat (mtes2cells φx)
  TermIsMinXWhere term φx            -> MkIsOpSuchTtBP (mte2cell term) MinXSuchThat (mtes2cells φx)
  TermIsSumXWhere total φx           -> MkIsOpSuchTtBP (mte2cell total) SumEachXSuchThat (mtes2cells φx)

  -- max / min / sum of terms
  TermIsMax term maxargRPs           -> termIsNaryOpOf MaxOf term maxargRPs
  TermIsMin term minargRPs           -> termIsNaryOpOf MinOf term minargRPs
  TotalIsSumTerms total summandRPs   -> termIsNaryOpOf SumOf total summandRPs
  TotalIsProductTerms total argRPs   -> termIsNaryOpOf ProductOf total argRPs

  T1IsNotT2 t1 t2                     -> MkIsDiffFr (mte2cell t1) (mte2cell t2)

  RPnary{}                              -> error "The spec doesn't support other RPnary constructs in the body of a HC"
  RPBoolStructR {}                      -> error "The spec does not support a RPRel other than RPis in a RPBoolStructR"
  RPParamText _                         -> error "should not be seeing RPParamText in body"


termIsNaryOpOf :: Foldable seq => OpOf -> MTExpr -> seq RelationalPredicate -> BoolPropn L4AtomicBP
termIsNaryOpOf op mteTerm rpargs = MkIsOpOf term op argterms
  where term     = mte2cell mteTerm
        argterms = concatMap atomRPoperand2cell rpargs


atomRPoperand2cell :: RelationalPredicate -> [Cell]
atomRPoperand2cell = \case
  RPMT mtexprs    -> mtes2cells mtexprs
  RPParamText _pt -> error "not sure if we rly need this case (RPParamText in fn atomRPoperand2cell); erroring as a diagnostic tool"
                    -- mtes2cells (concatMap (NE.toList . fst) (NE.toList pt)) 
  _              -> error "input rp supposed to be atomic"


--------- simplifying RPConstraint in body of L4 HC ------------------------------------

-- https://www.tweag.io/blog/2022-11-15-unrolling-with-typeclasses/
class SimpBodyRPConstrntRPrel (rp :: RPRel) where
  simpbodRPC :: [MTExpr] -> [MTExpr] -> BoolPropn L4AtomicBP

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
  MTI i -> MkCellNum (MkInteger i)
  MTF f -> MkCellNum (MkFloat f)
  _     -> error "Booleans in cells currently not supported"

-- | convenience function for when `map mte2cell` too wordy 
mtes2cells :: [L4.MTExpr] -> [Cell]
mtes2cells = map mte2cell


------

--- misc notes
-- wrapper :: L4Rules ValidHornls -> [(NE.NonEmpty MTExpr, Maybe TypeSig)]
-- wrapper = concat . map extractGiven . coerce
