{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.CoreL4.LogicProgram
  ( LogicProgram,
    LPType(..),
    astToLp,
  )
where

-- import RuleTransfo (ruleDisjL, clarify) -- TODO: Not needed here, and module RuleTransfo not visible here

import Control.Applicative (Applicative (..))
import Control.Monad.Validate (MonadValidate (refute), Validate, runValidate)
import Data.Coerce (coerce)
import Data.Either (fromRight, rights)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Monoid (Ap (Ap))
import Data.Set qualified as Set
import Data.Tuple.All (Curry (uncurryN), SequenceT (sequenceT))
import Flow ((|>))
import L4.KeyValueMap (ValueKVM)
import L4.PrintProg
  ( PrintConfig (PrintCurried, PrintVarCase),
    PrintCurried (MultiArg),
    PrintVarCase (CapitalizeLocalVar),
    capitalise,
    showL4,
  )
import L4.Syntax
import L4.SyntaxManipulation
  ( appToFunArgs,
    applyVars,
    applyVarsNoType,
    decomposeBinop,
    funArgsToAppNoType,
    fv,
    globalVarsOfProgram,
    isLocalVar,
  )
import LS.Utils (maybe2validate, (|$>))
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

data LPType
  = ASP
  | Epilog
  deriving (Eq, Read, Ord, Show)

{-
  Family of types for logic program rules, indexed by:
    - a type in the (closed) type universe LPType, ie ASP or Epilog
    - a babyl4 type annotation t 
-}
data LPRule (lpType :: LPType) t = LPRule
  { nameOfLPRule :: String,
    globalVarDeclsOfLPRule :: [VarDecl t],
    localVarDeclsOfLPRule :: [VarDecl t],
    precondOfLPRule :: [Expr t],
    postcondOfLPRule :: Expr t
  }
  deriving (Eq, Ord, Show, Read)

-- Skolemized LP rules code
--Additional function when starting at natural4. 
--addinVarDecls :: LPRule -> LPRule


skolemizeLPRuleGlobals :: LPRule lpType t -> [VarDecl t]
skolemizeLPRuleGlobals = globalVarDeclsOfLPRule
skolemizedLPRuleName :: LPRule lpType t -> String
skolemizedLPRuleName = nameOfLPRule
skolemizedLPRulePostcond :: LPRule lpType t -> Expr t
skolemizedLPRulePostcond = postcondOfLPRule
skolemizedLPRulePrecond :: Eq t => LPRule lpType t -> [Expr t]
skolemizedLPRulePrecond r = [transformPrecond precon (postcondOfLPRule r) (localVarDeclsOfLPRule r ++ globalVarDeclsOfLPRule r) (globalVarDeclsOfLPRule r) (nameOfLPRule r) | precon <- precondOfLPRule r]
--skolemizedLPRuleVardecls r = genSkolemList (localVarDeclsOfLPRule r) ([varExprToDecl expr (localVarDeclsOfLPRule r) | expr <- snd (appToFunArgs [] (postcondOfLPRule r))]) (nameOfLPRule r)

skolemizedLPRuleVardecls :: Eq t => LPRule lpType t -> [VarDecl t]
skolemizedLPRuleVardecls r =
  genSkolemList (localVarDeclsOfLPRule r) (map (convertVarExprToDecl (localVarDeclsOfLPRule r)) (snd (appToFunArgs [] (postcondOfLPRule r)))) (globalVarDeclsOfLPRule r) (nameOfLPRule r)

-- skolemizeLPRule :: LPRule t -> LPRule t

skolemizeLPRule :: Eq t => LPRule lpType t -> LPRule lpType t
skolemizeLPRule r = LPRule (skolemizedLPRuleName r)  (skolemizeLPRuleGlobals r) (skolemizedLPRuleVardecls r) (skolemizedLPRulePrecond r) (skolemizedLPRulePostcond r)


findVarDecl :: VarName -> [VarDecl t2] -> Maybe (VarDecl t2)
findVarDecl varname decls = find (\d -> varname == nameOfVarDecl d) decls

-- when exactly is this error triggered?
convertVarExprToDecl :: [VarDecl t2] -> Expr t ->VarDecl t2
convertVarExprToDecl decls (VarE _ v) = fromMaybe (error $ "convertVarExprToDecl: couldn't find " ++ nameOfQVarName (nameOfVar v)) $
                                        findVarDecl (nameOfQVarName (nameOfVar v)) decls
convertVarExprToDecl _decls _ = error "trying to convert a non-variable expression to a declaration"

--transformPrecond :: Expr t -> Expr t ->[VarDecl t] -> String -> Expr t
-- Takes in precon, lifts to var decl, transforms var decl, pushes back down to var expr, doesn't typecheck
-- because transformed predicates (ie function applications) have type Expr (Tp()) rather than Expr t
-- Need to change this !!! First : Check if precon occurs among vardecls, then check if postcon occurs among vardecls

transformPrecond :: Eq t => Expr t -> Expr t -> [VarDecl t] -> [VarDecl t] -> [Char] -> Expr t
transformPrecond precon postcon vardecls vardeclsGlobal ruleid =
                    -- [varExprToDecl expr vardecls | expr <- snd (appToFunArgs [] precon)]
                let preconvar_dec = map (convertVarExprToDecl vardecls) (snd (appToFunArgs [] precon))
                    -- [varExprToDecl expr vardecls | expr <- snd (appToFunArgs [] postcon)]
                    postconvar_dec = map (convertVarExprToDecl vardecls) (snd (appToFunArgs [] postcon))
                    new_preconvar_dec = genSkolemList preconvar_dec postconvar_dec vardeclsGlobal ruleid
                    new_precond = funArgsToAppNoType (fst (appToFunArgs [] precon)) (map varDeclToExpr new_preconvar_dec)

                in new_precond


--genSkolem ::  VarDecl t -> [VarDecl t] -> [VarDecl t] -> String -> VarDecl t
-- Takes in an existing precondition var_decl, list of postcon var_decls, list of global varDecls and returns skolemized precon var_decl
genSkolem :: Eq t => VarDecl t -> [VarDecl t] -> [VarDecl t] -> String -> VarDecl t
genSkolem (VarDecl t vn u) y w z
  | VarDecl t vn u `elem` w = VarDecl t  vn u
  | VarDecl t vn u `elem` y = VarDecl t (capitalise vn) u
  | otherwise = VarDecl t "extVar" u

-- List version of genSkolem
genSkolemList :: Eq t => [VarDecl t] -> [VarDecl t] -> [VarDecl t] -> String -> [VarDecl t]
genSkolemList x y w z = [genSkolem xs y w z | xs <- x]

toBrackets :: [VarDecl t] -> String
toBrackets [] = "()"
toBrackets [VarDecl _t vn _u] = "(" ++ capitalise vn ++ ")"
toBrackets ((VarDecl _t vn _u):xs) = "(" ++ capitalise vn ++ "," ++ tail (toBrackets xs)

-- var to var expr
mkVarE :: Var t -> Expr t
mkVarE v = VarE {annotOfExpr =  annotOfQVarName (nameOfVar v), varOfExprVarE = v}
--Takes in a var_decl and turns it into a var_expr
varDeclToExpr :: VarDecl t -> Expr t
varDeclToExpr (VarDecl x y _z) = mkVarE (GlobalVar (QVarName x y))


varDeclToExprTup :: VarDecl t -> (Expr t, VarDecl t)
varDeclToExprTup (VarDecl x y z) = (varDeclToExpr (VarDecl x y z), VarDecl x y z)

-- Matching function

match :: (Eq t, Show t) => t -> [(t, p)] -> p
match x (y:ys) = if x == fst y
     then snd y
     else match x ys
match x [] = error ("looking for " ++ show x)

-- var expr to var decl, takes in a var expr and returns its corresponding var decl
varExprToDecl :: (Eq t, Show t) => Expr t -> [VarDecl t] -> VarDecl t
varExprToDecl expr listdec = match expr (map varDeclToExprTup listdec)


-- Expression data structure in Syntax.hs
-- many additional functions in SyntaxManipuation.hs

-- End of Skoemized ASP rules code

data OpposesClause t = OpposesClause
  { posLit :: Expr t,
    negLit :: Expr t
  }
  deriving (Eq, Ord, Show, Read)


negationVarname :: QVarName t -> QVarName t
negationVarname (QVarName t vn) = QVarName t ("not"++vn)


negationPredicate :: Expr t -> Ap (Validate (Doc ann)) (Expr t, Maybe (Var t, Var t, Int))
negationPredicate (UnaOpE _ (UBool UBnot) e@AppE{}) =
    let (f, args) = appToFunArgs [] e in
        case f of
            VarE t posvar@(GlobalVar vn) ->
                let negvar = GlobalVar (negationVarname vn)
                in pure (funArgsToAppNoType (VarE t negvar) args, Just (posvar, negvar, length args))
            _ -> refute "negationPredicate: ill-formed negation"
negationPredicate e = pure (e, Nothing)

ruleToLPRule ::
  forall ann lpType t.
  (Show t, Ord t) =>
  Rule t ->
  Ap (Validate (Doc ann)) (LPRule lpType t, [(Var t, Var t, Int)])
ruleToLPRule r =
    let precondsNeg :: Ap (Validate (Doc ann)) [(Expr t, Maybe (Var t, Var t, Int))]
        precondsNeg =
          traverse negationPredicate (decomposeBinop (BBool BBand) (precondOfRule r))

        postcondNeg :: Ap (Validate (Doc ann)) (Expr t, Maybe (Var t, Var t, Int))
        postcondNeg = negationPredicate $ postcondOfRule r

        preconds :: Ap (Validate (Doc ann)) [Expr t]
        preconds = map fst <$> precondsNeg

        postcond :: Ap (Validate (Doc ann)) (Expr t)
        postcond = fst <$> postcondNeg

        negpreds :: Ap (Validate (Doc ann)) [(Var t, Var t, Int)]
        negpreds =
          (postcondNeg, precondsNeg)
            |> sequenceT
            |$> uncurry (:)
            |$> mapMaybe snd

        allVars :: Ap (Validate (Doc ann)) (Set.Set (Var t))
        allVars =
          (postcond, preconds)
            |> sequenceT
            |$> uncurry (:)
            |$> (Set.unions . map fv)

        globalvars :: Ap (Validate (Doc ann)) [VarDecl t]
        globalvars =
          allVars
            |$> map varTovarDecl . Set.toList . Set.filter (not . isLocalVar)

        localvars :: Ap (Validate (Doc ann)) [VarDecl t]
        localvars =
          allVars
            |$> map varTovarDecl . Set.toList . Set.filter isLocalVar

        ruleName :: Ap (Validate (Doc ann)) String
        ruleName =
          r
            |> nameOfRule
            |> maybe2validate
                ("ToASP: ruleToLPRule: nameOfRule is a Nothing :-(\n" <>
                  viaShow r <> "\n" <>
                  "To exclude the ToASP transpiler from a --workdir run, run natural4-exe with the --toasp option.")
    in
      (ruleName, globalvars, localvars, preconds, postcond)
        |> sequenceT
        |$> uncurryN LPRule
        |> (, negpreds)
        |> sequenceT

--varTovarDecl :: Var (Tp()) -> VarDecl (Tp())
--varTovarDecl :: Var t -> VarDecl t
varTovarDecl :: Var t -> VarDecl t
varTovarDecl (GlobalVar (QVarName a vn)) = VarDecl a vn OkT
varTovarDecl (LocalVar (QVarName a vn) _ind) = VarDecl a vn OkT

-- data TranslationMode
--   = AccordingToR
--   | CausedByR
--   | ExplainsR
--   | VarSubs1R
--   | VarSubs2R
--   | VarSubs3R
--   | AccordingToE String
--   | LegallyHoldsE
--   | QueryE
--   | VarSubs4R
--   | RawL4
--   | AddFacts
--   -- FixedCode was originally used by the ToEpilog transpiler.
--   | FixedCode
--   deriving (Eq, Ord, Read, Show)

data TranslationMode t
  = AccordingToR t
  | CausedByR t
  | ExplainsR t
  | VarSubs1R t
  | VarSubs2R t
  | VarSubs3R t
  | AccordingToE String t
  | LegallyHoldsE t
  | QueryE t
  | VarSubs4R t
  | RawL4 t
  | AddFacts t
  -- FixedCode was originally used by the ToEpilog transpiler.
  | FixedCode t
  deriving (Eq, Ord, Read, Show)

-- class PrettyLp x where
--   prettyLp :: TranslationMode -> x -> Doc ann

-- class ShowOppClause x where
--   showOppClause :: x -> Doc ann

aspPrintConfig :: [PrintConfig]
aspPrintConfig = [PrintVarCase CapitalizeLocalVar, PrintCurried MultiArg]

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

-- prettyLpRule ::
--   (Show t, Pretty (LPRule lpType t)) =>
--   TranslationMode ->
--   LPRule lpType t ->
--   Doc ann
{-     showASP ExplainsSkolemR (LPRule rn vds preconds postcond)=
                          let new_rn = rn
                              new_vds = skolemizedLPRuleVardecls (LPRule rn vds preconds postcond)
                              new_preconds = skolemizedLPRulePrecond (LPRule rn vds preconds postcond)
                              new_precond = postcond
                          in showASP ExplainsR (LPRule rn new_vds new_preconds postcond)
-}

prettyLpGeneric (ExplainsR (LPRule _rn _env _vds preconds postcond)) =
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
prettyLpGeneric (VarSubs3R (LPRule _rn _env _vds preconds postcond)) =
    vsep (map (\pc ->
                pretty ("createSub(subInst" <> "_" <> show _rn <> skolemize2 (_vds <> _env) _vds postcond _rn <> "," <> "_N+1" <> ")") <+>
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

prettyLpGeneric (VarSubs1R (LPRule _rn _env _vds preconds postcond)) =
    vsep (map (\pc ->
                "explains" <>
                parens (
                    pretty (RawL4 pc) <> "," <+>
                    pretty (RawL4 postcond) <+>
                    "," <>
                    "_N"
                    ) <+>
                ":-" <+>
                pretty ("createSub(subInst" ++ "_" ++ _rn ++ toBrackets _vds ++ "," ++ "_N" ++ ").")
                )
        preconds)

prettyLpGeneric (AddFacts (LPRule _rn _env _vds _preconds postcond)) =
    vsep (map (\pc ->
                "user_input" <>
                parens (
                    pretty (RawL4 pc) <> "," <+>
                    pretty _rn

                    ) <>
                "."
                )
        [postcond])

prettyLpGeneric (VarSubs2R (LPRule _rn _env _vds preconds postcond)) =
    vsep (map (\pc ->
                pretty ("createSub(subInst" ++ "_" ++ _rn ++ toBrackets2 (my_str_trans_list (preconToVarStrList pc (_vds ++ _env)) (varDeclToVarStrList (_vds))) ++ "," ++ "_N" ++ ")")
                      <+>
                ":-" <+>
                pretty ("createSub(subInst" ++ "_" ++ _rn ++ toBrackets2 (my_str_trans_list [] (varDeclToVarStrList (_vds))) ++ "," ++ "_N" ++ ")" ++ ",") <+>
                pretty (LegallyHoldsE pc) <> "."
                )
        (postcond : preconds))

prettyLpGeneric (VarSubs4R (LPRule rn _env _vds preconds postcond)) =
    vsep (map (\pc ->
                pretty ("createSub(subInst" ++ "_" ++ rn ++ toBrackets2 (my_str_trans_list (preconToVarStrList pc (_vds ++ _env)) (varDeclToVarStrList (_vds))) ++ "," ++ "_N" ++ ")")
                      <+>
                ":-" <+>
                pretty ("createSub(subInst" ++ "_" ++ rn ++ toBrackets2 (my_str_trans_list [] (varDeclToVarStrList (_vds))) ++ "," ++ "_N" ++ ")" ++ ",") <+>
                pretty (QueryE pc) <> "."
                )
        (postcond : preconds))

-- showASP CausedByR (LPRule rn _env _vds preconds postcond) =
--     vsep (map (\pc ->
--                 "caused_by" <>
--                     parens (
--                         "pos," <+>
--                         showASP LegallyHoldsE pc <> "," <+>
--                         showASP (AccordingToE rn) postcond <> "," <+>
--                         "_N+1"
--                         ) <+>
--                     ":-" <+>
--                     showASP (AccordingToE rn) postcond <> "," <+>
--                     hsep (punctuate comma (map (showASP LegallyHoldsE) preconds)) <>  "," <+>
--                     "justify" <>
--                     parens (
--                         showASP (AccordingToE rn) postcond <>  "," <+>
--                         "_N") <>
--                     "."
--                 )
--         preconds)

prettyLpGeneric (FixedCode (LPRule _rn _env _vds preconds postcond)) =
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

-- prettyLpRuleGeneric prettyLpRule transMode@((`elem` [AccordingToR, CausedByR]) -> True) lpRule =
--   prettyLpRule transMode lpRule

prettyLpGeneric _ = mempty -- not implemented

instance Show t => Pretty (TranslationMode (LPRule ASP t)) where
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

  pretty translationMode = prettyLpGeneric translationMode

instance Show t => Pretty (TranslationMode (LPRule Epilog t)) where
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

  pretty translationMode = prettyLpGeneric translationMode

genOppClause :: (Var (Tp ()), Var (Tp ()), Int) -> OpposesClause (Tp ())
genOppClause (posvar, negvar, n) =
    let args = zipWith (\ vn i -> LocalVar (QVarName IntegerT (vn ++ show i)) i) (replicate n "V") [0 .. n-1]
    in OpposesClause (applyVars posvar args) (applyVars negvar args)

genOppClauseNoType :: (Var t, Var t, Int) -> OpposesClause t
genOppClauseNoType (posvar, negvar, n) =
    let vart = annotOfQVarName (nameOfVar posvar) in
    let args = zipWith (\ vn i -> LocalVar (QVarName vart (vn ++ show i)) i) (replicate n "V") [0 .. n-1]
    in OpposesClause (applyVarsNoType posvar args) (applyVarsNoType negvar args)

-- TODO: type of function has been abstracted, is not Program t and not Program (Tp())
-- The price to pay: No more preprocessing of rules (simplification with clarify and ruleDisjL)
-- This could possibly be remedied with NoType versions of these tactics
-- astToASP :: (Eq t, Ord t, Show t) => Program t -> IO ()
-- astToASP prg = do
--     -- let rules = concatMap ruleDisjL (clarify (rulesOfProgram prg))
--     let rules = rulesOfProgram prg
--     -- putStrLn "Simplified L4 rules:"
--     -- putDoc $ vsep (map (showL4 []) rules) <> line
--     let aspRulesWithNegs = map (ruleToLPRule ) rules
--     let aspRules = map fst aspRulesWithNegs
--     let aspRulesNoFact = removeFacts aspRules
--     let aspRulesFact = keepFacts aspRules
--     let skolemizedLPRules = map skolemizeLPRule aspRulesNoFact  -- TODO: not used ??
--     let oppClausePrednames = nub (concatMap snd aspRulesWithNegs)
--     let oppClauses = map genOppClauseNoType oppClausePrednames

--     -- putStrLn "ASP rules:"
--     putDoc $ vsep (map (showASP AccordingToR) aspRulesNoFact) <> line <> line
--     putDoc $ vsep (map (showASP VarSubs1R) aspRulesNoFact) <> line <> line
--     putDoc $ vsep (map (showASP AddFacts) aspRulesFact) <> line <> line
--     putDoc $ vsep (map (showASP VarSubs3R) aspRulesNoFact) <> line <> line
--     putDoc $ vsep (map (showASP VarSubs2R) aspRulesNoFact) <> line <> line
--     putDoc $ vsep (map (showASP VarSubs4R) aspRulesNoFact) <> line <> line
--     -- putDoc $ vsep (map (showASP VarSubs2R) aspRules) <> line <> line
--     -- putDoc $ vsep (map (showASP ExplainsR) aspRules) <> line <> line
--     -- putDoc $ vsep (map (showASP ExplainsR) skolemizedLPRules) <> line <> line
--     putDoc $ vsep (map (showASP CausedByR) aspRulesNoFact) <> line <> line
--     putDoc $ vsep (map showOppClause oppClauses) <> line

data LogicProgram lpType t where
  LogicProgram ::
    { lpRulesNoFact :: [LPRule lpType t],
      lpRulesFact :: [LPRule lpType t],
      oppClauses :: [OpposesClause t]
    } ->
    LogicProgram lpType t
  deriving (Eq, Ord, Read, Show)

-- TODO: redundant with the above. Define astToASP as putDoc (astToDoc prg)
astToLp ::
  forall lpType ann t.
  (Show t, Ord t, Eq t) =>
  Program t ->
  LogicProgram lpType t
astToLp prg =
    -- let rules = concatMap ruleDisjL (clarify (rulesOfProgram prg))
    let rules = rulesOfProgram prg
    -- putStrLn "Simplified L4 rules:"
    -- putDoc $ vsep (map (showL4 []) rules) <> line
        lpRulesWithNegs :: [(LPRule lpType t, [(Var t, Var t, Int)])]
        lpRulesWithNegs =
          rules
            |$> ruleToLPRule
            |> (coerce :: [Ap (Validate a) b] ->[Validate a b])
            |$> runValidate
            |> rights

        lpRules :: [LPRule lpType t]
        lpRules = map fst lpRulesWithNegs

        lpRulesNoFact :: [LPRule lpType t]
        lpRulesNoFact = removeFacts lpRules

        lpRulesFact :: [LPRule lpType t]
        lpRulesFact = keepFacts lpRules

        skolemizedLPRules :: [LPRule lpType t]
        skolemizedLPRules = map skolemizeLPRule lpRulesNoFact  -- TODO: not used ??

        oppClausePrednames :: [(Var t, Var t, Int)]
        oppClausePrednames = lpRulesWithNegs |> foldMap snd |> nub

        oppClauses :: [OpposesClause t]
        oppClauses = map genOppClauseNoType oppClausePrednames
    in LogicProgram {lpRulesNoFact, lpRulesFact, oppClauses}

-- astToASP :: (Show t, Eq t, Ord t) => Program t -> LogicProgram ASP t
-- astToASP = astToLp

-- astToEpilog :: (Show t, Eq t, Ord t) => Program t -> LogicProgram Epilog t
-- astToEpilog = astToLp

-- putStrLn "ASP rules:"

instance Show t => Pretty (LogicProgram ASP t) where
  pretty (LogicProgram {..}) =
    vsep (map (pretty . AccordingToR) lpRulesNoFact) <> line <> line <>
    vsep (map (pretty . VarSubs1R) lpRulesNoFact) <> line <> line <>
    vsep (map (pretty . AddFacts) lpRulesFact) <> line <> line <>
    vsep (map (pretty . VarSubs3R) lpRulesNoFact) <> line <> line <>
    vsep (map (pretty . VarSubs2R) lpRulesNoFact) <> line <> line <>
    vsep (map (pretty . VarSubs4R) lpRulesNoFact) <> line <> line <>
    vsep (map (pretty . CausedByR) lpRulesNoFact) <> line <> line <>
    vsep (map pretty oppClauses) <> line

instance Show t => Pretty (LogicProgram Epilog t) where
  pretty (LogicProgram {..}) =
    vsep (map (pretty . AccordingToR) lpRulesNoFact) <> line <> line <>
    vsep (map (pretty . CausedByR) lpRulesNoFact) <> line <> line <>
    vsep (map pretty oppClauses) <> line

-- TODO: details to be filled in
proveAssertionASP :: Show t => Program t -> ValueKVM  -> Assertion t -> IO ()
proveAssertionASP p v asrt = putStrLn "ASP solver implemented"


-- Additional functions to write var substitution code

preconToVarStrList :: Expr t ->[VarDecl t] -> [String]
preconToVarStrList precon vardecls = varDeclToVarStrList (map (convertVarExprToDecl vardecls) (snd (appToFunArgs [] precon)))

varDeclToVarStrList :: [VarDecl t] -> [String]

varDeclToVarStrList [] = []
varDeclToVarStrList ((VarDecl t vn u) : xs) = capitalise vn : varDeclToVarStrList xs

my_str_trans :: [String] -> String -> String
my_str_trans s t = if elem t s
      then t
else "V" ++ "_" ++ t

my_str_trans_list s ts = [my_str_trans s t | t <- ts]



my_str_trans2 :: String -> [String] -> String -> String
my_str_trans2 v postc rulen  =
    if v `elem` postc
    then v
    else "extVar"

my_str_trans_list2 :: [String] -> [String] -> String -> [String]
my_str_trans_list2 s t u = [my_str_trans2 r t u | r <- s]


toBrackets2 :: [String] -> String
toBrackets2 [] = "()"
toBrackets2 [x] = "(" ++ x ++ ")"
toBrackets2 (x:xs) = "(" ++ x ++ "," ++ tail (toBrackets2 xs)


-- toBrackets3 :: [VarDecl t] -> String
-- toBrackets3 [] = "()"
-- toBrackets3 [VarDecl t vn u] = "(" ++ vn ++ ")"
-- toBrackets3 ((VarDecl t vn u):xs) = "(" ++ vn ++ "," ++ tail (toBrackets xs)


--skolemize2 :: Eq t1 => [VarDecl t1] -> [VarDecl t1] -> Expr t2 -> String -> String
skolemize2 :: [VarDecl t1] -> [VarDecl t2] -> Expr t3 -> String -> String
skolemize2 vardecs localvar postc rulename =  toBrackets2 (my_str_trans_list2 (varDeclToVarStrList localvar) (varDeclToVarStrList ((map (convertVarExprToDecl vardecs) (snd (appToFunArgs [] postc))))) rulename)

isFact :: Expr t -> Bool
isFact (ValE _ (BoolV True)) = True
isFact _ = False

removeFacts :: [LPRule lpType t] -> [LPRule lpType t]
removeFacts [] = []
removeFacts (r : rs)
  | isFact (head (precondOfLPRule r)) = removeFacts rs
  | otherwise = r : removeFacts rs


keepFacts :: [LPRule lpType t] -> [LPRule lpType t]
keepFacts [] =[]
keepFacts (r : rs)
      | isFact (head (precondOfLPRule r)) = r : keepFacts rs
      | otherwise = keepFacts rs

filt :: Eq t1 => [VarDecl t1] -> [VarDecl t1] -> [VarDecl t1]
filt [] _ = []
filt (x:xs) ys =
  if x `elem` ys
     then filt xs ys
  else (x: (filt xs ys))
