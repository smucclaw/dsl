{-# LANGUAGE GHC2021 #-}

module LS.XPile.ToASP where

-- import RuleTransfo (ruleDisjL, clarify) -- TODO: Not needed here, and module RuleTransfo not visible here

import Control.Applicative (Applicative (..))
import Data.Either (fromRight, rights)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Tuple.All (SequenceT (sequenceT))
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
import L4.SyntaxManipulation (appToFunArgs, applyVars, applyVarsNoType, decomposeBinop, funArgsToAppNoType, fv, globalVarsOfProgram, isLocalVar)
import LS.XPile.Maude.Utils ((|$>))
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    comma,
    hsep,
    line,
    parens,
    punctuate,
    vsep,
    (<+>),
  )

data ASPRule t = ASPRule {
                     nameOfASPRule :: String
                   , globalVarDeclsOfASPRule :: [VarDecl t]
                   , localVarDeclsOfASPRule :: [VarDecl t]
                   , precondOfASPRule :: [Expr t]
                   , postcondOfASPRule :: Expr t }
  deriving (Eq, Ord, Show, Read)


-- Skolemized ASP rules code
--Additional function when starting at natural4. 
--addinVarDecls :: ASPRule -> ASPRule


skolemizeASPRuleGlobals :: ASPRule t -> [VarDecl t]
skolemizeASPRuleGlobals = globalVarDeclsOfASPRule
skolemizedASPRuleName :: ASPRule t -> String
skolemizedASPRuleName = nameOfASPRule
skolemizedASPRulePostcond :: ASPRule t -> Expr t
skolemizedASPRulePostcond = postcondOfASPRule
skolemizedASPRulePrecond :: Eq t => ASPRule t -> [Expr t]
skolemizedASPRulePrecond r = [transformPrecond precon (postcondOfASPRule r) (localVarDeclsOfASPRule r ++ globalVarDeclsOfASPRule r) (globalVarDeclsOfASPRule r) (nameOfASPRule r) | precon <- precondOfASPRule r]
--skolemizedASPRuleVardecls r = genSkolemList (localVarDeclsOfASPRule r) ([varExprToDecl expr (localVarDeclsOfASPRule r) | expr <- snd (appToFunArgs [] (postcondOfASPRule r))]) (nameOfASPRule r)

skolemizedASPRuleVardecls :: Eq t => ASPRule t -> [VarDecl t]
skolemizedASPRuleVardecls r =
  genSkolemList (localVarDeclsOfASPRule r) (map (convertVarExprToDecl (localVarDeclsOfASPRule r)) (snd (appToFunArgs [] (postcondOfASPRule r)))) (globalVarDeclsOfASPRule r) (nameOfASPRule r)

-- skolemizeASPRule :: ASPRule t -> ASPRule t

skolemizeASPRule :: Eq t => ASPRule t -> ASPRule t
skolemizeASPRule r = ASPRule (skolemizedASPRuleName r)  (skolemizeASPRuleGlobals r) (skolemizedASPRuleVardecls r) (skolemizedASPRulePrecond r) (skolemizedASPRulePostcond r)


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
genSkolem :: Eq t => VarDecl t -> [VarDecl t] -> [VarDecl t] -> [Char] -> VarDecl t
genSkolem (VarDecl t vn u) y w z =
    if VarDecl t vn u `elem` w
       then VarDecl t  vn u
    else if VarDecl t vn u `elem` y
       then VarDecl t (capitalise vn) u
    else VarDecl t "extVar" u

-- List version of genSkolem
genSkolemList :: Eq t => [VarDecl t] -> [VarDecl t] -> [VarDecl t] -> [Char] -> [VarDecl t]
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




data OpposesClause t = OpposesClause {
      posLit :: Expr t
    , negLit :: Expr t}
  deriving (Eq, Ord, Show, Read)


negationVarname :: QVarName t -> QVarName t
negationVarname (QVarName t vn) = QVarName t ("not"++vn)


negationPredicate :: Expr t -> Either (Doc ann) (Expr t, Maybe (Var t, Var t, Int))
negationPredicate (UnaOpE _ (UBool UBnot) e@AppE{}) =
    let (f, args) = appToFunArgs [] e in
        case f of
            VarE t posvar@(GlobalVar vn) ->
                let negvar = GlobalVar (negationVarname vn)
                in Right (funArgsToAppNoType (VarE t negvar) args, Just (posvar, negvar, length args))
            _ -> Left $ pretty "negationPredicate: ill-formed negation"
negationPredicate e = Right (e, Nothing)

ruleToASPRule ::
  forall ann t.
  (Show t, Ord t) =>
  Rule t ->
  Either (Doc ann) (ASPRule t, [(Var t, Var t, Int)])
ruleToASPRule r =
    let precondsNeg :: Either (Doc ann) [(Expr t, Maybe (Var t, Var t, Int))]
        precondsNeg =
          traverse negationPredicate (decomposeBinop (BBool BBand) (precondOfRule r))

        postcondNeg :: Either (Doc ann) (Expr t, Maybe (Var t, Var t, Int))
        postcondNeg = negationPredicate $ postcondOfRule r

        preconds :: Either (Doc ann) [Expr t]
        preconds = map fst <$> precondsNeg

        postcond :: Either (Doc ann) (Expr t)
        postcond = fst <$> postcondNeg

        negpreds :: Either (Doc ann) [(Var t, Var t, Int)]
        negpreds = liftA2 (:) postcondNeg precondsNeg |$> mapMaybe snd

        allVars :: Either (Doc ann) (Set.Set (Var t))
        allVars = liftA2 (:) postcond preconds |$> (Set.unions . map fv)

        globalvars :: Either (Doc ann) [VarDecl t]
        globalvars = allVars
          |$> map varTovarDecl . Set.toList . Set.filter (not . isLocalVar)

        localvars :: Either (Doc ann) [VarDecl t]
        localvars = allVars
          |$> map varTovarDecl . Set.toList . Set.filter isLocalVar

        maybe2either x Nothing = Left x
        maybe2either _ (Just x) = Right x

        ruleName :: Either (Doc ann) String
        ruleName = r
          |> nameOfRule
          |> maybe2either
              (pretty $ "ToASP: ruleToASPRule: nameOfRule is a Nothing :-(\n" <>
                show r <> "\n" <>
                "To exclude the ToASP transpiler from a --workdir run, run natural4-exe with the --toasp option.")

        uncurry5 f (a, b, c, d, e) = f a b c d e

        -- f ruleName =
        --   (ASPRule ruleName globalvars localvars preconds postcond, negpreds)
    in
      (ruleName, globalvars, localvars, preconds, postcond)
        |> sequenceT
        |$> uncurry5 ASPRule
        |> (, negpreds)
        |> sequenceT

--varTovarDecl :: Var (Tp()) -> VarDecl (Tp())
--varTovarDecl :: Var t -> VarDecl t
varTovarDecl :: Var t -> VarDecl t
varTovarDecl (GlobalVar (QVarName a vn)) = VarDecl a vn OkT
varTovarDecl (LocalVar (QVarName a vn) _ind) = VarDecl a vn OkT

data TranslationMode
  = AccordingToR
  | CausedByR
  | ExplainsR
  | VarSubs1R
  | VarSubs2R
  | VarSubs3R
  | AccordingToE String
  | LegallyHoldsE
  | QueryE
  | VarSubs4R
  | RawL4
  | AddFacts

class ShowASP x where
  showASP :: TranslationMode -> x -> Doc ann

class ShowOppClause x where
    showOppClause :: x -> Doc ann

aspPrintConfig :: [PrintConfig]
aspPrintConfig = [PrintVarCase CapitalizeLocalVar, PrintCurried MultiArg]

instance Show t => ShowASP (Expr t) where
    showASP (AccordingToE rn) e =
        pretty "according_to" <> parens (pretty rn <> pretty "," <+> showASP RawL4 e)

    -- predicates (App expressions) are written wrapped into legally_holds,
    -- whereas any other expressions are written as is.
    showASP LegallyHoldsE e@AppE{} =
        pretty "legally_holds" <> parens (showASP RawL4 e)

    showASP LegallyHoldsE e =
        pretty "legally_holds" <> parens (showASP RawL4 e)
    showASP QueryE e@AppE{} =
        pretty "query" <> parens (showASP RawL4 e <> pretty "," <> pretty "L")
    showASP QueryE e =
        pretty "query" <> parens (showASP RawL4 e <> pretty "," <> pretty "L")

    -- showASP LegallyHoldsE e =
    --     showASP RawL4 e
    -- showASP QueryE e@AppE{} =
    --     pretty "query" <> parens (showASP RawL4 e <> pretty "," <> pretty "L")
    -- showASP QueryE e =
    --     showASP RawL4 e

    showASP RawL4 e = showL4 aspPrintConfig e
    showASP _ _ = pretty ""   -- not implemented

instance Show t => ShowOppClause (OpposesClause t) where
    showOppClause (OpposesClause pos neg) =
        pretty "opposes" <>
            parens (showASP RawL4 pos <> pretty "," <+> showASP RawL4 neg) <+>
        pretty ":-" <+>
            showASP (AccordingToE "R") pos <> pretty "." <> line <>
        pretty "opposes" <>
            parens (showASP RawL4 pos <> pretty "," <+> showASP RawL4 neg) <+>
        pretty ":-" <+>
            showASP (AccordingToE "R") neg <> pretty "." <> line <>

        pretty "opposes" <>
            parens (showASP RawL4 pos <> pretty "," <+> showASP RawL4 neg) <+>


        pretty ":-" <+>
            showASP LegallyHoldsE pos <> pretty "." <> line <>

        pretty "opposes" <>
            parens (showASP RawL4 pos <> pretty "," <+> showASP RawL4 neg) <+>

        pretty ":-" <+>
            showASP LegallyHoldsE neg <> pretty "." <> line <>

        pretty "opposes" <>
            parens (showASP RawL4 pos <> pretty "," <+> showASP RawL4 neg) <+>

        pretty ":-" <+>
        pretty "query" <>
        parens (
                showASP RawL4 pos <+>
                pretty "," <>
                pretty "_N"
                ) <> pretty "." <> line <>




        pretty "opposes" <>
            parens (showASP RawL4 pos <> pretty "," <+> showASP RawL4 neg) <+>

        pretty ":-" <+>
        pretty "query" <>
        parens (
                showASP RawL4 neg <+>
                pretty "," <>
                pretty "_N"
                ) <> pretty "."





instance Show t => ShowASP (ASPRule t) where
    showASP AccordingToR (ASPRule rn _env _vds preconds postcond) =
        showASP (AccordingToE rn) postcond <+> pretty ":-" <+>
            hsep (punctuate comma (map (showASP LegallyHoldsE) preconds)) <>  pretty "."

{-     showASP ExplainsSkolemR (ASPRule rn vds preconds postcond)=
                             let new_rn = rn
                                 new_vds = skolemizedASPRuleVardecls (ASPRule rn vds preconds postcond)
                                 new_preconds = skolemizedASPRulePrecond (ASPRule rn vds preconds postcond)
                                 new_precond = postcond
                             in showASP ExplainsR (ASPRule rn new_vds new_preconds postcond)
 -}

    showASP ExplainsR (ASPRule _rn _env _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty "explains" <>
                    parens (
                        showASP RawL4 pc <> pretty "," <+>
                        showASP RawL4 postcond <+>
                        pretty "," <>
                        pretty "_N+1"
                        ) <+>
                    pretty ":-" <+>
                    pretty "query" <>
                    parens (
                            showASP RawL4 postcond <+>
                            pretty "," <>
                            pretty "_N"
                            ) <>
                    pretty "_N < M, max_ab_lvl(M)" <>
                    pretty "."
                    )
            preconds)



-- TODO: weird: var pc not used in map
    showASP VarSubs3R (ASPRule _rn _env _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty ("createSub(subInst" ++ "_" ++ _rn ++ skolemize2 (_vds ++ _env) _vds postcond _rn ++ "," ++ "_N+1" ++ ")") <+>
                    pretty ":-" <+>
                    pretty "query" <>
                    parens (
                            showASP RawL4 postcond <+>
                            pretty "," <>
                            pretty "_N"
                            ) <>
                    pretty ", _N < M, max_ab_lvl(M)" <>
                    pretty "."
                    )
            [head preconds])

    showASP VarSubs1R (ASPRule _rn _env _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty "explains" <>
                    parens (
                        showASP RawL4 pc <> pretty "," <+>
                        showASP RawL4 postcond <+>
                        pretty "," <>
                        pretty "_N"
                        ) <+>
                    pretty ":-" <+>
                    pretty ("createSub(subInst" ++ "_" ++ _rn ++ toBrackets _vds ++ "," ++ "_N" ++ ").")
                    )
            preconds)


    showASP AddFacts (ASPRule _rn _env _vds _preconds postcond) =
        vsep (map (\pc ->
                    pretty "user_input" <>
                    parens (
                        showASP RawL4 pc <> pretty "," <+>
                        pretty _rn

                        ) <>
                    pretty "."
                    )
            [postcond])





    showASP VarSubs2R (ASPRule _rn _env _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty ("createSub(subInst" ++ "_" ++ _rn ++ toBrackets2 (my_str_trans_list (preconToVarStrList pc (_vds ++ _env)) (varDeclToVarStrList (_vds))) ++ "," ++ "_N" ++ ")")
                         <+>
                    pretty ":-" <+>
                    pretty ("createSub(subInst" ++ "_" ++ _rn ++ toBrackets2 (my_str_trans_list [] (varDeclToVarStrList (_vds))) ++ "," ++ "_N" ++ ")" ++ ",") <+>
                    showASP LegallyHoldsE pc <> pretty "."
                    )
            (postcond : preconds))


    showASP VarSubs4R (ASPRule rn _env _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty ("createSub(subInst" ++ "_" ++ rn ++ toBrackets2 (my_str_trans_list (preconToVarStrList pc (_vds ++ _env)) (varDeclToVarStrList (_vds))) ++ "," ++ "_N" ++ ")")
                         <+>
                    pretty ":-" <+>
                    pretty ("createSub(subInst" ++ "_" ++ rn ++ toBrackets2 (my_str_trans_list [] (varDeclToVarStrList (_vds))) ++ "," ++ "_N" ++ ")" ++ ",") <+>
                    showASP QueryE pc <> pretty "."
                    )
            (postcond : preconds))

    showASP CausedByR (ASPRule rn _env _vds preconds postcond) =
        vsep (map (\pc ->
                    pretty "caused_by" <>
                        parens (
                            pretty "pos," <+>
                            showASP LegallyHoldsE pc <> pretty "," <+>
                            showASP (AccordingToE rn) postcond <> pretty "," <+>
                            pretty "_N+1"
                            ) <+>
                        pretty ":-" <+>
                        showASP (AccordingToE rn) postcond <> pretty "," <+>
                        hsep (punctuate comma (map (showASP LegallyHoldsE) preconds)) <>  pretty "," <+>
                        pretty "justify" <>
                        parens (
                            showASP (AccordingToE rn) postcond <>  pretty "," <+>
                            pretty "_N") <>
                        pretty "."
                    )
            preconds)
    showASP _ _ = pretty ""  -- not implemented

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
--     let aspRulesWithNegs = map (ruleToASPRule ) rules
--     let aspRules = map fst aspRulesWithNegs
--     let aspRulesNoFact = removeFacts aspRules
--     let aspRulesFact = keepFacts aspRules
--     let skolemizedASPRules = map skolemizeASPRule aspRulesNoFact  -- TODO: not used ??
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
--     -- putDoc $ vsep (map (showASP ExplainsR) skolemizedASPRules) <> line <> line
--     putDoc $ vsep (map (showASP CausedByR) aspRulesNoFact) <> line <> line
--     putDoc $ vsep (map showOppClause oppClauses) <> line

-- TODO: redundant with the above. Define astToASP as putDoc (astToDoc prg)
astToDoc :: forall ann t. (Show t, Ord t, Eq t) => Program t -> Doc ann
astToDoc prg =
    -- let rules = concatMap ruleDisjL (clarify (rulesOfProgram prg))
    let rules = rulesOfProgram prg 
    -- putStrLn "Simplified L4 rules:"
    -- putDoc $ vsep (map (showL4 []) rules) <> line
    -- aspRulesWithNegs :: Either (Doc ann) [(ASPRule t, [(Var t, Var t, Int)])]
        aspRulesWithNegs = traverse ruleToASPRule rules
        aspRules = map fst <$> aspRulesWithNegs 
        aspRulesNoFact = removeFacts <$> aspRules
        aspRulesFact = keepFacts <$> aspRules 
        skolemizedASPRules = map skolemizeASPRule <$> aspRulesNoFact  -- TODO: not used ??
        oppClausePrednames = nub . concatMap snd <$> aspRulesWithNegs
        oppClauses = map genOppClauseNoType <$> oppClausePrednames 

        toDoc :: ([ASPRule t], [ASPRule t], [OpposesClause t]) -> Doc ann
        toDoc (aspRulesNoFact, aspRulesFact, oppClauses) =
          vsep (map (showASP AccordingToR) aspRulesNoFact) <> line <> line <>
          vsep (map (showASP VarSubs1R) aspRulesNoFact) <> line <> line <>
          vsep (map (showASP AddFacts) aspRulesFact) <> line <> line <>
          vsep (map (showASP VarSubs3R) aspRulesNoFact) <> line <> line <>
          vsep (map (showASP VarSubs2R) aspRulesNoFact) <> line <> line <>
          vsep (map (showASP VarSubs4R) aspRulesNoFact) <> line <> line <>
          vsep (map (showASP CausedByR) aspRulesNoFact) <> line <> line <>
          vsep (map showOppClause oppClauses) <> line
    in
      (aspRulesNoFact, aspRulesFact, oppClauses)
        |> ( sequenceT ::
               ( Either (Doc ann) [ASPRule t],
                 Either (Doc ann) [ASPRule t],
                 Either (Doc ann) [OpposesClause t]
               ) ->
               Either (Doc ann) ([ASPRule t], [ASPRule t], [OpposesClause t])
           )
        |> either (const mempty) toDoc

    -- putStrLn "ASP rules:"

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

my_str_trans_list2 s t u = [my_str_trans2 r t u | r <- s]



toBrackets2 :: [String] -> String

toBrackets2 [] = "()"
toBrackets2 [x] = "(" ++ x ++ ")"
toBrackets2 (x:xs) = "(" ++ x ++ "," ++ tail (toBrackets2 xs)


toBrackets3 :: [VarDecl t] -> String
toBrackets3 [] = "()"
toBrackets3 [VarDecl t vn u] = "(" ++ vn ++ ")"
toBrackets3 ((VarDecl t vn u):xs) = "(" ++ vn ++ "," ++ tail (toBrackets xs)


--skolemize2 :: Eq t1 => [VarDecl t1] -> [VarDecl t1] -> Expr t2 -> String -> String
skolemize2 vardecs localvar postc rulename =  toBrackets2 (my_str_trans_list2 (varDeclToVarStrList localvar) (varDeclToVarStrList ((map (convertVarExprToDecl vardecs) (snd (appToFunArgs [] postc))))) rulename)

isFact :: Expr t -> Bool
isFact (ValE _ (BoolV True)) = True
isFact _ = False

removeFacts :: [ASPRule t] -> [ASPRule t]
removeFacts [] = []
removeFacts (r : rs)
      | isFact (head (precondOfASPRule r)) = removeFacts rs
      | otherwise = r : removeFacts rs


keepFacts :: [ASPRule t] -> [ASPRule t]
keepFacts [] =[]
keepFacts (r : rs)
      | isFact (head (precondOfASPRule r)) = r : keepFacts rs
      | otherwise = keepFacts rs

filt :: Eq t1 => [VarDecl t1] -> [VarDecl t1] -> [VarDecl t1]
filt [] _ = []
filt (x:xs) ys =
  if x `elem` ys
     then filt xs ys
  else (x: (filt xs ys))
