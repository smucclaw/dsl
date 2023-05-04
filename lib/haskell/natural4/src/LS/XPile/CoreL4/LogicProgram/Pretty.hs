{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.CoreL4.LogicProgram.Pretty () where

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
    LPRule (LPRule),
    LogicProgram(..),
    OpposesClause (OpposesClause)
  )
-- import LS.XPile.CoreL4.LogicProgram.Skolemize (toBrackets)
import LS.XPile.CoreL4.LogicProgram.Pretty.Utils
  ( my_str_trans_list,
    preconToVarStrList,
    skolemize2,
    toBrackets2,
    varDeclToVarStrList, toBrackets,
  )
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    comma,
    hsep,
    line,
    parens,
    punctuate,
    vsep,
    (<+>), viaShow,
  )

data TranslationMode t
  = AccordingToR t
  | CausedByR t
  | ExplainsR t
  | VarSubs1R t
  | VarSubs2R t
  | VarSubs3R t
  | AccordingToE
      { accordingToStr :: String,
        accordingToItem :: t
      }
  | LegallyHoldsE t
  | QueryE t
  | VarSubs4R t
  | RawL4 t
  | AddFacts t
  -- FixedCode was originally used by the ToEpilog transpiler.
  | FixedCode t
  deriving (Eq, Ord, Read, Show)

aspPrintConfig :: [PrintConfig]
aspPrintConfig = [PrintVarCase CapitalizeLocalVar, PrintCurried MultiArg]

instance Show t => Pretty (OpposesClause t) where
  pretty (OpposesClause pos neg) =
    "opposes" <>
      parens (pretty (RawL4 pos) <> "," <+> pretty (RawL4 neg)) <+>
    ":-" <+>
        pretty (AccordingToE "R" pos) <> "." <> line <>
    "opposes" <>
      parens (pretty (RawL4 pos) <> "," <+> pretty (RawL4 neg)) <+>
    ":-" <+>
      pretty (AccordingToE "R" neg) <> "." <> line <>

    "opposes" <>
      parens (pretty (RawL4 pos) <> "," <+> pretty (RawL4 neg)) <+>

    ":-" <+>
      pretty (LegallyHoldsE pos) <> "." <> line <>

    "opposes" <>
      parens (pretty (RawL4 pos) <> "," <+> pretty (RawL4 neg)) <+>

    ":-" <+>
      pretty (LegallyHoldsE neg) <> "." <> line <>

    "opposes" <>
      parens (pretty (RawL4 pos) <> "," <+> pretty (RawL4 neg)) <+>

    ":-" <+>
    "query" <>
    parens (
            pretty (RawL4 pos) <+>
            "," <>
            "_N"
            ) <> "." <> line <>

    "opposes" <>
      parens (pretty (RawL4 pos) <> "," <+> pretty (RawL4 neg)) <+>

    ":-" <+>
    "query" <>
    parens (
            pretty (RawL4 neg) <+>
            "," <>
            "_N"
            ) <> "."

instance Show t => Pretty (TranslationMode (Expr t)) where
    pretty (AccordingToE rn e) =
      "according_to" <> parens (pretty rn <> "," <+> pretty (RawL4 e))

    -- predicates (App expressions) are written wrapped into legally_holds,
    -- whereas any other expressions are written as is.
    -- showASP LegallyHoldsE e@AppE{} =
    --   "legally_holds" <> parens (showASP RawL4 e)

    pretty (LegallyHoldsE e) =
      "legally_holds" <> parens (pretty $ RawL4 e)
    -- showASP QueryE e@AppE{} =
    --   "query" <> parens (showASP RawL4 e <> "," <> "L")
    pretty (QueryE e) =
      "query" <> parens (pretty (RawL4 e) <> "," <> "L")

    -- showASP LegallyHoldsE e =
    --     showASP RawL4 e
    -- showASP QueryE e@AppE{} =
    --     "query" <> parens (showASP RawL4 e <> "," <> "L")
    -- showASP QueryE e =
    --     showASP RawL4 e

    pretty (RawL4 e) = showL4 aspPrintConfig e
    pretty _ = mempty   -- not implemented

{-     showASP ExplainsSkolemR (LPRule rn vds preconds postcond)=
                          let new_rn = rn
                              new_vds = skolemizedLPRuleVardecls (LPRule rn vds preconds postcond)
                              new_preconds = skolemizedLPRulePrecond (LPRule rn vds preconds postcond)
                              new_precond = postcond
                          in showASP ExplainsR (LPRule rn new_vds new_preconds postcond)
-}

prettyLPRuleCommon :: Show t => TranslationMode (LPRule lpLang t) -> Doc ann
prettyLPRuleCommon (ExplainsR (LPRule _rn _env _vds preconds postcond)) =
    vsep (map (\pc ->
                "explains" <>
                parens (
                    pretty (RawL4 pc) <> "," <+>
                    pretty (RawL4 postcond) <+>
                    "," <>
                    "_N+1"
                    ) <+>
                ":-" <+>
                "query" <>
                parens (
                        pretty (RawL4 postcond) <+>
                        "," <>
                        "_N"
                        ) <>
                "_N < M, max_ab_lvl(M)" <>
                "."
                )
        preconds)

-- TODO: weird: var pc not used in map
prettyLPRuleCommon (VarSubs3R (LPRule _rn _env _vds preconds postcond)) =
    vsep (map (\pc ->
                ("createSub(subInst" <> "_" <> viaShow _rn <> skolemize2 (_vds <> _env) _vds postcond _rn <> "," <> "_N+1" <> ")") <+>
                ":-" <+>
                "query" <>
                parens (
                        pretty (RawL4 postcond) <+>
                        "," <>
                        "_N"
                        ) <>
                ", _N < M, max_ab_lvl(M)" <>
                "."
                )
        [head preconds])

prettyLPRuleCommon (VarSubs1R (LPRule _rn _env _vds preconds postcond)) =
    vsep (map (\pc ->
                "explains" <>
                parens (
                    pretty (RawL4 pc) <> "," <+>
                    pretty (RawL4 postcond) <+>
                    "," <>
                    "_N"
                    ) <+>
                ":-" <+>
                ("createSub(subInst" <> "_" <> viaShow _rn <> toBrackets _vds <> "," <> "_N" <> ").")
                )
        preconds)

prettyLPRuleCommon (AddFacts (LPRule _rn _env _vds _preconds postcond)) =
    vsep (map (\pc ->
                "user_input" <>
                parens (
                    pretty (RawL4 pc) <> "," <+>
                    pretty _rn

                    ) <>
                "."
                )
        [postcond])

prettyLPRuleCommon (VarSubs2R (LPRule _rn _env _vds preconds postcond)) =
    vsep (map (\pc ->
                ("createSub(subInst" <> "_" <> viaShow _rn <> toBrackets2 (my_str_trans_list (preconToVarStrList pc (_vds ++ _env)) (varDeclToVarStrList _vds)) <> "," <> "_N" <> ")")
                      <+>
                ":-" <+>
                ("createSub(subInst" <> "_" <> viaShow _rn <> toBrackets2 (my_str_trans_list [] (varDeclToVarStrList _vds)) <> "," <> "_N" <> ")" <> ",") <+>
                pretty (LegallyHoldsE pc) <> "."
                )
        (postcond : preconds))

prettyLPRuleCommon (VarSubs4R (LPRule rn _env _vds preconds postcond)) =
    vsep (map (\pc ->
                ("createSub(subInst" <> "_" <> viaShow rn <> toBrackets2 (my_str_trans_list (preconToVarStrList pc (_vds <> _env)) (varDeclToVarStrList _vds)) <> "," <> "_N" <> ")")
                      <+>
                ":-" <+>
                ("createSub(subInst" <> "_" <> viaShow rn <> toBrackets2 (my_str_trans_list [] (varDeclToVarStrList _vds)) <> "," <> "_N" <> ")" <> ",") <+>
                pretty (QueryE pc) <> "."
                )
        (postcond : preconds))

prettyLPRuleCommon _ = mempty -- not implemented

instance Show t => Pretty (TranslationMode (ASPRule t)) where
  pretty (AccordingToR (LPRule rn _env _vds preconds postcond)) =
    pretty (AccordingToE rn postcond) <+> ":-" <+>
      hsep (punctuate comma (map (pretty . LegallyHoldsE) preconds)) <> "."

  pretty (CausedByR (LPRule rn _env _vds preconds postcond)) =
      vsep (map (\pc ->
                  "caused_by" <>
                      parens (
                          "pos," <+>
                          pretty (LegallyHoldsE pc) <> "," <+>
                          pretty (AccordingToE rn postcond) <> "," <+>
                          "_N+1"
                          ) <+>
                      ":-" <+>
                      pretty (AccordingToE rn postcond) <> "," <+>
                      hsep (punctuate comma (map (pretty . LegallyHoldsE) preconds)) <>  "," <+>
                      "justify" <>
                      parens (
                          pretty (AccordingToE rn postcond) <>  "," <+>
                          "_N") <>
                      "."
                  )
          preconds)

  pretty translationMode = prettyLPRuleCommon translationMode

instance Show t => Pretty (TranslationMode (EpilogRule t)) where
  pretty (AccordingToR (LPRule rn _env _vds preconds postcond)) =
    pretty (AccordingToE rn postcond) <+> ":-" <+>
      hsep (punctuate "&" (map (pretty . LegallyHoldsE) preconds))

  pretty (CausedByR (LPRule rn _env _vds preconds postcond)) =
        vsep (map (\pc ->
                    "caused_by" <>
                        parens (
                            "pos," <+>
                            pretty (LegallyHoldsE pc) <> "," <+>
                            pretty (AccordingToE rn postcond) <> "," <+>
                            "0"
                            ) <+>
                        ":-" <+>
                        pretty (AccordingToE rn postcond) <> "&" <+>
                        hsep (punctuate "&" (map (pretty . LegallyHoldsE) preconds)) <>  "&" <+>
                         "justify" <>
                        parens (
                            pretty (AccordingToE rn postcond) <>  "," <+>
                            "0")
                    )
            preconds)

  pretty (FixedCode (LPRule _rn _env _vds preconds postcond)) =
    vsep [ "defeated(R2,C2):-overrides(R1,R2) & according_to(R2,C2) & legally_enforces(R1,C1) & opposes(C1,C2)"
        , "opposes(C1,C2):-opposes(C2,C1)"
        , "legally_enforces(R,C):-according_to(R,C) & ~defeated(R,C) "
        , "legally_holds(C):-legally_enforces(R,C)"
        , "legally_holds(contradiction_entailed):-opposes(C1,C2) & legally_holds(C1) & legally_holds(C2)"
        , "caused_by(pos,overrides(R1,R2),defeated(R2,C2),0):-defeated(R2,C2) & overrides(R1,R2) & according_to(R2,C2) & legally_enforces(R1,C1) & opposes(C1,C2) & justify(defeated(R2,C2),0)"
        , "caused_by(pos,according_to(R2,C2),defeated(R2,C2),0):-defeated(R2,C2) & overrides(R1,R2) & according_to(R2,C2) & legally_enforces(R1,C1) & opposes(C1,C2) & justify(defeated(R2,C2),0)"
        , "caused_by(pos,legally_enforces(R1,C1),defeated(R2,C2),0):-defeated(R2,C2) & overrides(R1,R2) & according_to(R2,C2) & legally_enforces(R1,C1) & opposes(C1,C2) & justify(defeated(R2,C2),0)"
        , "caused_by(pos,opposes(C1,C2),defeated(R2,C2),0):-defeated(R2,C2) & overrides(R1,R2) & according_to(R2,C2) & legally_enforces(R1,C1) & opposes(C1,C2) & justify(defeated(R2,C2),0)"
        , "caused_by(pos,according_to(R,C),legally_enforces(R,C),0):-legally_enforces(R,C) & according_to(R,C) & ~defeated(R,C) & justify(legally_enforces(R,C),0)"
        , "caused_by(neg,defeated(R,C),legally_enforces(R,C),0):-legally_enforces(R,C) & according_to(R,C) & ~defeated(R,C) & justify(legally_enforces(R,C),0)"
        , "caused_by(pos,legally_enforces(R,C),legally_holds(C),0):-legally_holds(C) & legally_enforces(R,C) & justify(legally_holds(C),0)"
        , "justify(X,0):-caused_by(pos,X,Y,0)"
        , "directedEdge(Sgn,X,Y):-caused_by(Sgn,X,Y,0)"
        , "justify(X,0):-gen_graph(X)" ]

  pretty translationMode = prettyLPRuleCommon translationMode

instance Show t => Pretty (ASPProgram t) where
  pretty (LogicProgram {..}) =
    vsep (map (pretty . AccordingToR) lpRulesNoFact) <> line <> line <>
    vsep (map (pretty . VarSubs1R) lpRulesNoFact) <> line <> line <>
    vsep (map (pretty . AddFacts) lpRulesFact) <> line <> line <>
    vsep (map (pretty . VarSubs3R) lpRulesNoFact) <> line <> line <>
    vsep (map (pretty . VarSubs2R) lpRulesNoFact) <> line <> line <>
    vsep (map (pretty . VarSubs4R) lpRulesNoFact) <> line <> line <>
    vsep (map (pretty . CausedByR) lpRulesNoFact) <> line <> line <>
    vsep (map pretty oppClauses) <> line

instance Show t => Pretty (EpilogProgram t) where
  pretty (LogicProgram {..}) =
    vsep (map (pretty . AccordingToR) lpRulesNoFact) <> line <> line <>
    vsep (map (pretty . CausedByR) lpRulesNoFact) <> line <> line <>
    vsep (map pretty oppClauses) <> line