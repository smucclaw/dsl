{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

{-|

We're trying to work with the rules / AST instead, 
in part because we don't need most of the stuff Interpreter.hs provides,
and in part to avoid breaking if the spec / API for Interpreter.hs changes.
After all, the design intentions for this short-term LE transpiler aren't the same as those for the analzyer (which is part of what will hopefully become an effort in longer-term, more principled language development).
-}

module LS.XPile.LogicalEnglish.LogicalEnglish (toLE)  where

import LS.PrettyPrinter
    ( myrender, vvsep, (</>), tildes, (<//>), srchs )
import Prettyprinter
    ( vsep, viaShow, hsep, emptyDoc, (<+>), Pretty(pretty), Doc, indent, line )
import Text.Pretty.Simple   ( pShowNoColor )
import Data.Text qualified as T
import Data.Bifunctor       ( first )
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.HashMap.Strict qualified as Map
import Control.Monad.Identity ( Identity )
import Data.String (IsString)

import LS.Types qualified as L4
import LS.Types (RelationalPredicate(..), RPRel(..))
import LS.Rule qualified as L4 (Rule(..))
import LS.XPile.LogicalEnglish.Types
import LS.XPile.LogicalEnglish.Internal
      (L4Rules, ValidHornls, Unvalidated,
      check, refine, loadRawL4AsUnvalid,
      gvarsFromL4Rule, mtexpr2cell, mtes2cells)
import LS.XPile.LogicalEnglish.Common (
    L4Prog,
    (|>)
    )

import LS.XPile.LogicalEnglish.UtilsLEReplDev -- for prototyping

{- TODO: After we get a v simple end-to-end prototype out, 
we'll add functionality for checking the L4 input rules __upfront__ for things like whether it's using unsupported keywords, whether the input is well-formed by the lights of the translation rules, and so forth. 
(This should be done with Monad.Validate or Data.Validation -- XPileLog isn't as good a fit for this.)
The thought is that if the upfront checks fail, we'll be able to exit gracefully and provide more helpful diagnostics / error messages. 

But for now, we will help ourselves, undeservedly, to the assumption that the L4 input is wellformed. -}


{-------------------------------------------------------------------------------
   L4 rules -> SimpleL4HCs -> LamAbsRules
-------------------------------------------------------------------------------}

-- | TODO: Work on implementing this and adding the Monad Validate or Data.Validation stuff instead of Maybe (i.e., rly doing checks upfront and carrying along the error messages and potential warnings) after getting enoguh of the main transpiler out
checkAndRefine :: L4Rules Unvalidated -> Maybe (L4Rules ValidHornls)
checkAndRefine rawrules = do
  validatedL4rules <- check rawrules
  return $ refine validatedL4rules


-- TODO: Switch over to this, e.g. with coerce or with `over` from new-type generic when have time: simplifyL4rule :: L4Rules ValidHornls -> SimpleL4HC
{- | 
  Precondition: assume that the input L4 rules only have 1 HC in their Horn clauses. 
  TODO: This invariant will have to be established in the next iteration of work on this transpiler (mainly by desugaring the 'ditto'/decision table stuff accordingly first) 
-}
simplifyL4rule :: L4.Rule -> SimpleL4HC
simplifyL4rule l4r =
  let gvars = gvarsFromL4Rule l4r
      (rhead, rbody) = simplifyL4HC (Prelude.head $ L4.clauses l4r)
                      -- this use of head will be safe in the future iteration when we do validation and make sure that there will be exactly one HC in every L4 rule that this fn gets called on
  in MkSL4hc { givenVars = gvars, head = rhead, body = rbody }


lamAbstract :: SimpleL4HC -> LamAbsRule
lamAbstract = undefined


----- helper funcs -----------------
simplifyL4HC :: L4.HornClause2 -> ([Cell], ComplexPropn [Cell])
simplifyL4HC l4hc = (simplifyHead l4hc.hHead, simplifyBody l4hc.hBody)
  
simplifyHead :: L4.RelationalPredicate -> [Cell]
simplifyHead = \case
  (RPMT exprs)                     -> mtes2cells exprs
  (RPConstraint exprsl rel exprsr) -> if rel == RPis 
                                      then mtes2cells exprsl <> [MkCellIs] <> mtes2cells exprsr
                                      --TODO: Need to account for / stash info for IS NOT --- I think that would be in this variant, but need to check 
                                         {- ^ Can't just lowercase IS and transform the mtexprs to (either Text or Integer) Cells 
                                         because it could be a IS-number, 
                                         and when making template vars later, we need to be able to disambiguate between something tt was an IS-kw and smtg tt was originally lowercase 'is'. 
                                         TODO: But do think more abt this when we implement the intermed stage
                                        -}
                                      else error "shouldn't be seeing any other operator for RPConstraint in our encoding"
                                          
  (RPnary _rel _rps)                -> error "I don't see any RPnary in the head in Joe's encoding, so."
  _                               -> error "(simplifyHead cases) not yet implemented / may not even need to be implemented"

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




{-
data RPRel = RPis | RPhas | RPeq | RPlt | RPlte | RPgt | RPgte | RPelem | RPnotElem | RPnot | RPand | RPor | RPsum | RPproduct | RPsubjectTo | RPmap
-}

{-
inspiration:

from types.hs
rp2mt :: RelationalPredicate -> MultiTerm
rp2mt (RPParamText    pt)            = pt2multiterm pt
rp2mt (RPMT           mt)            = mt
rp2mt (RPConstraint   mt1 rel mt2)   = mt1 ++ [MTT $ rel2txt rel] ++ mt2
rp2mt (RPBoolStructR  mt1 rel bsr)   = mt1 ++ [MTT $ rel2txt rel] ++ [MTT $ bsr2text bsr] -- [TODO] is there some better way to bsr2mtexpr?
rp2mt (RPnary         rel rps)       = MTT (rel2txt rel) : concatMap rp2mt rps

From md transpiler:
rpFilter :: RelationalPredicate -> MultiTerm
rpFilter (RPParamText pt) = pt2multiterm pt
rpFilter (RPConstraint mt1 rel mt2) = mt1 ++ mt2
rpFilter (RPBoolStructR mt1 rel bsr) = mt1++ bs2mt bsr
rpFilter (RPnary rel rps) = concatMap rpFilter rps
rpFilter (RPMT mt) = mt


typescript:
hc2ts :: Interpreted -> HornClause2 -> Doc ann
hc2ts _l4i  hc2@HC { hHead = RPMT        _ }                 = "value" <+> colon <+> dquotes (pretty (hHead hc2))
hc2ts _l4i _hc2@HC { hHead = RPConstraint  mt1 _rprel mt2 }  = snake_case mt1 <+> colon <+> dquotes (snake_case mt2) <+> "// hc2ts RPConstraint"
hc2ts _l4i _hc2@HC { hHead = RPBoolStructR mt1 _rprel _bsr } = snake_case mt1 <+> colon <+> "(some => lambda)" 
hc2ts  l4i _hc2@HC { hHead = RPParamText pt }                = pretty (PT4 pt l4i) <+> "// hc2ts RPParamText"
hc2ts  l4i  hc2@HC { hHead = RPnary      _rprel [] }         = error "TypeScript: headless RPnary encountered"
hc2ts  l4i  hc2@HC { hHead = RPnary      _rprel rps }        = hc2ts l4i hc2 {hHead = head rps} <+> "// hc2ts RPnary"

-}

simplifyBody :: Maybe L4.BoolStructR -> ComplexPropn [Cell]
simplifyBody = undefined

{-------------------------------------------------------------------------------
   LamAbsRules -> LE Nat Lang Annotations 
-------------------------------------------------------------------------------}


-- | Generate natural language annotations from a LamAbsRule
nlasFromLamAbsRule :: LamAbsRule -> HS.HashSet LENatLangAnnot
nlasFromLamAbsRule = undefined
{- for each base template (bt) in the LamAbsRule, across the head and body,
  we take its sequence of original variable names <"v1", "v2", ..., "vn">,
  make a new sequence <"*a v1", "a v2", ..., "a vn">,
 and then instantiate the bt with that new sequence. 
-}

allNLAs :: [LamAbsRule] -> HS.HashSet LENatLangAnnot
allNLAs lamAbsRules = HS.unions $ map nlasFromLamAbsRule lamAbsRules


{-------------------------------------------------------------------------------
    LamAbsRules -> LE rules
-------------------------------------------------------------------------------}


leruleFromLamAbsRule :: LamAbsRule -> LERule
leruleFromLamAbsRule = undefined

allLERules :: [LamAbsRule] -> [LERule]
allLERules = map leruleFromLamAbsRule

{-------------------------------------------------------------------------------
   Orchestrating and pretty printing
-------------------------------------------------------------------------------}

toLE :: L4Prog -> String
toLE = const "some output"

{-
note
------

Key types from codebase:
  type ParamText = NonEmpty TypedMulti
  type TypedMulti = (NonEmpty MTExpr, Maybe TypeSig)

  data MTExpr = MTT Text.Text -- ^ Text string
              | MTI Integer   -- ^ Integer
              | MTF Float     -- ^ Float
              | MTB Bool      -- ^ Boolean
            deriving (Eq, Ord, Show, Generic, ToJSON)

    -- | the parser returns a list of MTExpr, to be parsed further at some later point
  type MultiTerm = [MTExpr] --- | apple | banana | 100 | $100 | 1 Feb 1970

  given    :: Maybe ParamText
  aka the stuff in the given field is a non-mt list of (NonEmpty MTExpr, Maybe TypeSig)

-}