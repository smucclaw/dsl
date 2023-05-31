{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.CoreL4.LogicProgram.Pretty () where

import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Hashable (Hashable)
import Flow ((|>))
import GHC.Generics (Generic)
import L4.PrintProg
  ( PrintConfig (PrintCurried, PrintVarCase),
    PrintCurried (MultiArg),
    PrintVarCase (CapitalizeLocalVar),
    ShowL4 (showL4),
  )
import L4.Syntax (Expr)
import LS.XPile.CoreL4.LogicProgram.Common
  ( ASPProgram,
    ASPRule,
    EpilogProgram,
    EpilogRule,
    LPRule (..),
    LogicProgram (..),
    OpposesClause (..),
  )
import LS.XPile.CoreL4.LogicProgram.Pretty.Utils
  ( my_str_trans_list,
    preconToVarStrList,
    skolemize2,
    toBrackets,
    toBrackets2,
    varDeclToVarStrList,
  )
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    comma,
    hsep,
    line,
    parens,
    punctuate,
    viaShow,
    vsep,
    (<+>),
  )
import Prettyprinter.Interpolate (__di)

data TranslationMode t
  = AccordingToR t
  | CausedByR t
  | ExplainsR t
  | VarSubs1R t
  | VarSubs2R t
  | VarSubs3R t
  | AccordingToE
      { ruleName :: String,
        postcond :: t
      }
  | LegallyHoldsE t
  | QueryE t
  | VarSubs4R t
  | AddFacts t
  -- | FixedCode t
  deriving (Eq, Generic, Ord, Read, Show)

instance Hashable t => Hashable (TranslationMode t)

instance Show t => Pretty (Expr t) where
  pretty = showL4 [PrintVarCase CapitalizeLocalVar, PrintCurried MultiArg]

instance Show t => Pretty (OpposesClause t) where
  pretty OpposesClause {..} =
    [__di|
      opposes(#{pos'}, #{neg'}) :-
        #{accordingToPos}.

      opposes(#{pos'}, #{neg'}) :-
        #{accordingToNeg}.

      opposes(#{pos'}, #{neg'}) :-
        #{pretty $ LegallyHoldsE posLit}.

      opposes(#{pos'}, #{neg'}) :-
        #{pretty $ LegallyHoldsE negLit}.

      opposes(#{pos'}, #{neg'}) :-
        query(#{pos'}, _N).

      opposes(#{pos'}, #{neg'}) :-
        query(#{neg'}, _N).
    |]
    where
      pos' = pretty posLit
      neg' = pretty negLit
      (accordingToPos, accordingToNeg) =
        (posLit, negLit) |> join bimap (pretty . AccordingToE "R")

instance Show t => Pretty (TranslationMode (Expr t)) where
    pretty AccordingToE {..} =
      [__di|
        according_to(#{ruleName}, #{pretty postcond})
      |]

    -- predicates (App expressions) are written wrapped into legally_holds,
    -- whereas any other expressions are written as is.
    -- showASP LegallyHoldsE e@AppE{} =
    --   "legally_holds" <> parens (showASP RawL4 e)

    pretty (LegallyHoldsE e) =
      [__di|
        legally_holds(#{pretty e})
      |]

    pretty (QueryE e) =
      [__di|
        query(#{pretty e}, L)
      |]

    pretty _ = mempty   -- not implemented

-- prettyLPRuleCommon :: Show t => TranslationMode (LPRule lpLang t) -> Doc ann
-- prettyLPRuleCommon (ExplainsR (LPRule _rn _env _vds preconds postcond)) =
--   vsep $ do
--     precond <- preconds
--     pure [__di|
--       explains(#{pretty precond}, #{pretty postcond}, _N + 1) :-
--         query(#{pretty postcond}, _N),
--         _N < M, max_ab_lvl(M).
--     |]

instance Show t => Pretty (TranslationMode (ASPRule t)) where
  pretty (AccordingToR LPRule {ruleName, preconds, postcond}) =
    [__di|
      #{accordingToPostcond} :-
        #{hsep $ punctuate ", " $ pretty . LegallyHoldsE <$> preconds}.
    |]
    where
      accordingToPostcond = pretty AccordingToE {..}

  pretty (CausedByR LPRule {ruleName, preconds, postcond}) =
    vsep $ do
      precond <- preconds
      pure [__di|
        caused_by(pos, #{pretty $ LegallyHoldsE precond}, #{accordingToPostcond}, _N + 1) :-
          #{accordingToPostcond},
          #{hsep $ punctuate comma $ pretty . LegallyHoldsE <$> preconds},
          justify(#{accordingToPostcond}, _N).
      |]
    where
      accordingToPostcond = pretty AccordingToE {..}

  pretty (AddFacts LPRule {ruleName, postcond}) =
    [__di|
      user_input(#{pretty postcond}, #{ruleName}).
    |]

  pretty (VarSubs1R LPRule {ruleName, localVarDecls, preconds, postcond}) =
    vsep $ do
      precond <- preconds
      pure [__di|
        explains(#{pretty precond}, #{pretty postcond}, _N) :-
          createSub(subInst_#{ruleName}#{toBrackets localVarDecls}, _N).
      |]

  pretty (VarSubs2R LPRule {..}) =
    vsep $ do
      cond <- postcond : preconds
      pure [__di|
        createSub(subInst_#{ruleName}#{toBrackets2 (my_str_trans_list (preconToVarStrList cond varDecls) (varDeclToVarStrList localVarDecls))}, _N) :-
          createSub(subInst_#{ruleName}#{toBrackets2 (my_str_trans_list [] (varDeclToVarStrList localVarDecls))}, _N), #{pretty $ LegallyHoldsE cond}.
      |]
    where
      varDecls = localVarDecls <> globalVarDecls

-- TODO: weird: var pc not used in map
  pretty (VarSubs3R LPRule {..}) =
    case preconds of
      precond : _ ->
        [__di|
          createSub(subInst_#{ruleName}#{skolemize2 varDecls localVarDecls postcond ruleName}, _N + 1) :-
            query(#{pretty postcond}, _N),
            _N < M, max_ab_lvl(M).
        |]
        where
          varDecls = localVarDecls <> globalVarDecls
      _ -> mempty

  pretty (VarSubs4R LPRule {..}) =
    vsep $ do
      cond <- postcond : preconds
      pure [__di|
        createSub(subInst_#{ruleName}#{toBrackets2 (my_str_trans_list (preconToVarStrList cond varDecls) (varDeclToVarStrList localVarDecls))}, _N) :-
          createSub(subInst_#{ruleName}#{toBrackets2 (my_str_trans_list [] (varDeclToVarStrList localVarDecls))}, _N),
          #{pretty $ QueryE cond}.
      |]
    where
      varDecls = localVarDecls <> globalVarDecls

  pretty _ = mempty

instance Show t => Pretty (TranslationMode (EpilogRule t)) where
  pretty (AccordingToR LPRule {ruleName, preconds, postcond}) =
    [__di|
      #{accordingToPostcond} :-
        #{hsep $ punctuate " & " $ pretty . LegallyHoldsE <$> preconds}
    |]
    where
      accordingToPostcond = pretty AccordingToE {..}

  pretty (CausedByR LPRule {ruleName, preconds, postcond}) =
    vsep $ do
      precond <- preconds
      pure [__di|
        caused_by(pos, #{pretty $ LegallyHoldsE precond}, #{accordingToPostcond}, 0) :-
          #{accordingToPostcond} &
          #{hsep $ punctuate " & " $ pretty . LegallyHoldsE <$> preconds} &
          justify(#{accordingToPostcond}, 0)
      |]
    where
      accordingToPostcond = pretty AccordingToE {..}

  -- pretty (FixedCode _) =
  --   [__di|
  --     defeated(R2,C2) :-
  --       overrides(R1,R2) & according_to(R2,C2) & legally_enforces(R1,C1) & opposes(C1,C2)

  --     opposes(C1,C2) :- opposes(C2,C1)

  --     legally_enforces(R,C) :- according_to(R,C) & ~defeated(R,C)

  --     legally_holds(C) :- legally_enforces(R,C)

  --     legally_holds(contradiction_entailed) :-
  --       opposes(C1,C2) & legally_holds(C1) & legally_holds(C2)

  --     caused_by(pos,overrides(R1,R2),defeated(R2,C2),0) :-
  --       defeated(R2,C2) & overrides(R1,R2) & according_to(R2,C2) & legally_enforces(R1,C1) & opposes(C1,C2) & justify(defeated(R2,C2),0)

  --     caused_by(pos,according_to(R2,C2),defeated(R2,C2),0) :-
  --       defeated(R2,C2) & overrides(R1,R2) & according_to(R2,C2) & legally_enforces(R1,C1) & opposes(C1,C2) & justify(defeated(R2,C2),0)

  --     caused_by(pos,legally_enforces(R1,C1),defeated(R2,C2),0) :-
  --       defeated(R2,C2) & overrides(R1,R2) & according_to(R2,C2) & legally_enforces(R1,C1) & opposes(C1,C2) & justify(defeated(R2,C2),0)

  --     caused_by(pos,opposes(C1,C2),defeated(R2,C2),0) :-
  --       defeated(R2,C2) & overrides(R1,R2) & according_to(R2,C2) & legally_enforces(R1,C1) & opposes(C1,C2) & justify(defeated(R2,C2),0)

  --     caused_by(pos,according_to(R,C),legally_enforces(R,C),0) :-
  --       legally_enforces(R,C) & according_to(R,C) & ~defeated(R,C) & justify(legally_enforces(R,C),0)

  --     caused_by(neg,defeated(R,C),legally_enforces(R,C),0) :-
  --       legally_enforces(R,C) & according_to(R,C) & ~defeated(R,C) & justify(legally_enforces(R,C),0)

  --     caused_by(pos,legally_enforces(R,C),legally_holds(C),0) :-
  --       legally_holds(C) & legally_enforces(R,C) & justify(legally_holds(C),0)

  --     justify(X,0) :- caused_by(pos,X,Y,0)

  --     directedEdge(Sgn,X,Y) :- caused_by(Sgn,X,Y,0)

  --     justify(X,0) :- gen_graph(X)
  --   |]

  pretty _ = mempty

instance Show t => Pretty (ASPProgram t) where
  pretty LogicProgram {..} =
    [__di|
      #{vsep $ pretty . AccordingToR <$> lpRulesNoFact}

      #{vsep $ pretty . VarSubs1R <$> lpRulesNoFact}

      #{vsep $ pretty . AddFacts <$> lpRulesFact}

      #{vsep $ pretty . VarSubs3R <$> lpRulesNoFact}

      #{vsep $ pretty . VarSubs2R <$> lpRulesNoFact}

      #{vsep $ pretty . VarSubs4R <$> lpRulesNoFact}

      #{vsep $ pretty . CausedByR <$> lpRulesNoFact}

      #{vsep $ pretty <$> oppClauses}
    |]

instance Show t => Pretty (EpilogProgram t) where
  pretty LogicProgram {..} =
    [__di|
      #{vsep $ pretty . AccordingToR <$> lpRulesNoFact}

      #{vsep $ pretty . CausedByR <$> lpRulesNoFact}

      #{vsep $ pretty <$> oppClauses}
    |]