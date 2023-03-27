{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.ToDMN.FromL4 where

import qualified Debug.Trace as Debug
import qualified Data.Function as Fn
import qualified Data.List as List
import qualified Data.Map as Map

import Control.Monad.Trans.State (runState)

import Text.XML.HXT.Core

import L4.PrintProg
import L4.Syntax
import L4.SyntaxManipulation
import L4.Typing

import ToDMN.FromSimpleToReg
import ToDMN.Types


-- extracts the rules of a program and translates each rule into a decision table
rulesToDecTables :: Show t => Program t -> [SimpleDecision]
rulesToDecTables pg =
    let rs = rulesOfProgram pg
        filtered = filterRules rs

        classDecls = classDeclsOfProgram pg
        varDecls = varDeclsOfProgram pg
        env = initialEnvOfProgram classDecls varDecls
        allPreds = globalsOfEnv env

        -- groupedRules = groupBy' headPredOf filtered
        -- e.g. [(I, [r6, facti]), ...]

        -- let predMap = ruleGpsToPredGps groupedRules

        allTables = allRulesToTables allPreds (groupBy' headPredOf filtered)
    -- in Debug.trace ("filtered rules" ++ show (map (showL4 []) filtered)) allTables
    in allTables

rulesToDecTablesNoType :: Show t => Program t -> [SimpleDecision]
rulesToDecTablesNoType pg =
  let rs = rulesOfProgram pg
      filtered = filterRules rs
      allTablesNoType = allRulesToTablesNoType (groupBy' headPredOf filtered)
  in allTablesNoType


-- wraps the list of tables in an xml definitions element
decTablesToXMLDefs :: [SimpleDecision] -> Definitions
decTablesToXMLDefs sds =
  let (decisions, _decIDState) = runState (mapM sDecisionToDecision sds) Map.empty
      iddefs = decisionsToDefs decisions
      (definitions, _defIDState) = runState iddefs Map.empty
  in definitions


-- genDMN :: Program (Tp ()) -> IO ()
genDMN :: Show t => Program t -> IO ()
genDMN x = -- do

    -- let defs = (decTablesToXMLDefs . rulesToDecTables) x

    -- runX (

    --   constA defs
    --   >>>
    --   xpickleDocument xpDefinitions
    --                   [ withIndent yes
    --                   ] "" -- "src/ToDMN/out/minimal.dmn"
    --   )

    -- return ()
  writeTreeToDoc $ genXMLTree x

genXMLTree :: (ArrowXml cat, Show t) => Program t -> cat a XmlTree
genXMLTree x =
  let defs = decTablesToXMLDefs $ rulesToDecTables x
      arrDefs = constA defs
      pickledXML = xpickleVal xpDefinitions
  in arrDefs >>> pickledXML

genXMLTreeNoType :: (ArrowXml cat, Show t) => Program t -> cat a XmlTree
genXMLTreeNoType x =
  let defs = decTablesToXMLDefs $ rulesToDecTablesNoType x
      arrDefs = constA defs
      pickledXML = xpickleVal xpDefinitions
  in arrDefs >>> pickledXML

writeTreeToDoc :: IOSLA (XIOState ()) XmlTree XmlTree -> IO ()
writeTreeToDoc tree = do
  _ <- runX ( tree >>> writeDocument [ withIndent yes ] "src/ToDMN/out/minimal.dmn" )
  defs <- loadDefs
  print defs
  -- return ()

loadDefs :: IO Definitions
loadDefs = do
  [defs] <- runX ( xunpickleDocument xpDefinitions
             [
               withRemoveWS yes
             , withValidate no
             , withTrace 4
             ] "src/ToDMN/out/minimal.dmn" )
  return defs

-- constA :: c -> a b c
-- constA :: Defs -> IOSArrow b Defs

-- xpickleDocument :: PU a -> SysConfigList -> String -> IOSArrow a XmlTree
-- xpickleDocument :: PU Defs -> [] -> "" -> IOSArrow Defs XmlTree

-- (>>>) :: cat a b -> cat b c -> cat a c
-- (>>>) :: IOSArrow b Defs -> IOSArrow Defs XmlTree -> IOSArrow b XmlTree


-- xpickleVal :: ArrowXml a => PU b -> a b XmlTree
-- xpickleVal :: ArrowXml a => PU Defs -> a Defs XmlTree

-- (>>>) :: cat a b -> cat b c -> cat a c
-- (>>>) :: arr x Defs -> arr Defs XmlTree -> arr x XmlTree


isValE :: Expr t -> Bool
isValE ValE {} = True
isValE _       = False

isVarE :: Expr t -> Bool
isVarE VarE {} = True
isVarE _  = False

-- does AppE have only 1 arg?
-- is each arg in AppE a value?
-- appToFunArgs :: [Expr t] -> Expr t -> (Expr t, [Expr t])
unaryApp :: Show t => Expr t -> Bool
unaryApp appE =
  let (f, es) = appToFunArgs [] appE
  -- in Debug.trace ("\n--- DEBUG unary app --- \n" ++ show f ++ "\n" ++ show es ++ "\n--- DEBUG unary app --- \n" )
  in isVarE f && length es == 1 && all isValE es


-- is precond a conj of unary function applications?
isValidPrecond :: Show t => Rule t -> Bool
isValidPrecond = conjHasSimplePreds . precondOfRule

-- is each pred in conjE a unary function application?
-- decomposeBinop :: BinOp -> Expr t -> [Expr t]
conjHasSimplePreds :: Show t => Expr t -> Bool
conjHasSimplePreds bop =
  -- Debug.trace
  -- ("simple preds" ++ show (decomposeBinopClean (BBool BBand) bop) )
   all unaryApp (decomposeBinopClean (BBool BBand) bop)


-- is postcond a unary function application?
isValidPostcond :: Show t => Rule t -> Bool
isValidPostcond = unaryApp . postcondOfRule


-- filter for well-formed rules
-- preconds should be conjs of app of single pred to single arg
-- postcond should app of single pred to single arg
filterRules :: Show t => [Rule t] -> [Rule t]
-- filterRules = filter (\r -> isValidPrecond r)
filterRules = filter (\r -> isValidPrecond r && isValidPostcond r)


-- classify :: Eq a => [a] -> [[a]]
-- classify = classifyBy (==)

-- classifyBy :: (a -> a -> Bool) -> [a] -> [[a]]
-- classifyBy eq = List.foldl' f [ ]
--   where
--     f [ ] y = [[y]]
--     f (xs@ (x: _): xss) y | x `eq` y  = (y: xs): xss
--                           | otherwise = xs: f xss y

classifyHeadPred :: [Rule t] -> [[Rule t]]
-- classifyHeadPred = classifyBy hasSameHeadPred
classifyHeadPred = List.groupBy hasSameHeadPred

hasSameHeadPred :: Rule t -> Rule t -> Bool
hasSameHeadPred rx ry = headPredOf rx == headPredOf ry

-- group rules that have same output preds together
-- should also include facts
-- decompose postcond into fn and arg and extract fn name
headPredOf :: Rule t -> String
headPredOf r =
  let (f, _) = appToFunArgs [] (postcondOfRule r)
  in getNameOfPred f

groupBy' :: (Rule t -> String) -> [Rule t] ->[(String, [Rule t])]
groupBy' f = map (f . head &&& id)
             . List.groupBy ((==) `Fn.on` f)
             . List.sortOn f -- sortBy (compare `on` f)

-- sortOn :: (Rule t -> String) -> [Rule t] -> [Rule t]
-- groupBy :: (Rule t -> Rule t -> Bool) ->[Rule t] -> [[Rule t]]

-- getNameOfVal :: Expr t -> String
-- getnameOfVal = printVal . valOfExprValE

getNameOfPred :: Expr t -> String
getNameOfPred = nameOfQVarName . nameOfVar . varOfExprVarE

-- does different things depending on whether pred is a bool or funApp
-- mkPredName :: Expr t -> String
-- mkPredName (ValE _ (BoolV True)) = ""
-- mkPredName fa@AppE {} = (getNameOfPred . fst . appToFunArgs []) fa
-- mkPredName _ = error "Precondition should be either a function application or True"


-- form a schema for each equivalence class
-- fns need better names
-- ("O", [r2, r3]) -> ("O", ["P1", "P3", "P2"])
-- schema list : ("O", ["P1", "P3", "P2"])
-- given an output pred and all the rules that produce it
-- generates a list of input preds from those rules
ruleGpToPredGp :: Show t => (String, [Rule t]) -> (String, [String])
ruleGpToPredGp (hp, rs) =
  let decomp = decomposeBinopClean (BBool BBand)
      debops = concatMap (decomp . precondOfRule) rs
      getPN = getNameOfPred . fst . appToFunArgs []
  in (hp, List.nub (map getPN debops))

ruleGpsToPredGps :: Show t => [(String, [Rule t])] -> [(String, [String])]
ruleGpsToPredGps = map ruleGpToPredGp


-- pred types need to be extracted too
schematize ::  VarEnvironment -> (String, [String]) -> SimpleSchema
schematize env (hp, body) =
  SimpleSchema
  (map (mkSimpleInputSchema env) body)
  (mkSimpleOutputSchema env hp)

schematizeNoType :: (String, [String]) -> SimpleSchema
schematizeNoType (hp, body) =
  SimpleSchema (map mkSimpleInputSchemaNoType body) (mkSimpleOutputSchemaNoType hp)

mkSimpleInputSchema :: VarEnvironment -> String -> SimpleInputSchema
mkSimpleInputSchema _ ""   = undefined
mkSimpleInputSchema env pd = SimpleInputSchema pd (stringTpToFEELTp (lookupPredType pd env))

mkSimpleInputSchemaNoType :: String -> SimpleInputSchema
mkSimpleInputSchemaNoType "" = undefined
mkSimpleInputSchemaNoType pd = SimpleInputSchema pd "Any"

mkSimpleOutputSchema :: VarEnvironment -> String -> SimpleOutputSchema
mkSimpleOutputSchema env pd = SimpleOutputSchema pd (stringTpToFEELTp (lookupPredType pd env))

mkSimpleOutputSchemaNoType :: String -> SimpleOutputSchema
mkSimpleOutputSchemaNoType pd = SimpleOutputSchema pd "Any"

-- TODO
-- how to deal with predicates of arbitrary classes (invalid types)
stringTpToFEELTp :: String -> FEELType
stringTpToFEELTp "String" = "string"
stringTpToFEELTp "Boolean" = "bool" -- is this actually "Boolean"?
stringTpToFEELTp "Integer" = "number"
stringTpToFEELTp _ = error "not a valid FEELType"


-- given pred name, looks up its type
-- varenv is [(predname, FunT)]
-- lookup should always succeed
lookupPredType :: String -> VarEnvironment -> String
lookupPredType nm env =
  -- let (Just funt) = lookup nm env
  -- in (stringOfClassName . classNameOfTp . paramTp) funt

  -- debug empty varnev
  case lookup nm env of
    Just funt -> (stringOfClassName . classNameOfTp . paramTp) funt
    _ -> error ("pred " ++ nm ++ " not found in globalsOfEnv "  ++ show env ++ "\n")


-- input is a single element of this
-- [
--     ( "O"
--     ,
--         [ "P1"
--         , "P3"
--         , "P2"
--         ]
--     )
-- ,
--     ( "O2"
--     ,
--         [ "P1"
--         , "P2"
--         ]
--     )
-- ]

-- table O
-- P1 | P3 | P2    | O
-- 2  | 4  | False | 11 (r2)
-- 1  | -  | -     | 10 (r3)
-- 11 | 33 | True  | 44 (r5)

-- [("O", [r2, r3]), ("O2", [r1])]
allRulesToTables :: Show t => VarEnvironment -> [(String, [Rule t])] -> [SimpleDecision]
allRulesToTables env = map (ruleGroupToTable env)

allRulesToTablesNoType :: Show t => [(String, [Rule t])] -> [SimpleDecision]
allRulesToTablesNoType = map ruleGroupToTableNoType

-- given a varenv, output pred and corresponding rules e.g. ("O", [r2, r3])
-- generates a simple decision table
ruleGroupToTable :: Show t => VarEnvironment -> (String, [Rule t]) -> SimpleDecision
ruleGroupToTable env rg@(_, rs) =
  let predGp = ruleGpToPredGp rg
      schema = schematize env predGp
      ruleLines = map (mkRuleLine predGp) rs
      inputPreds = snd predGp
  in SimpleDecTableEl (map (SimpleReqInputEl . ("#"++)) inputPreds) schema ruleLines

ruleGroupToTableNoType :: Show t => (String, [Rule t]) -> SimpleDecision
ruleGroupToTableNoType rg@(_, rs) =
  let predGp = ruleGpToPredGp rg
      schema = schematizeNoType predGp
      ruleLines = map (mkRuleLine predGp) rs
      inputPreds = snd predGp
  in SimpleDecTableEl (map (SimpleReqInputEl . ("#"++)) inputPreds) schema ruleLines

-- given a list of input preds and a rule, generates a list of input entries
-- schemaPreds (from O): ["P1", "P3", "P2"]
-- funArgs: [(P1, [11]), (P2, [True]), (P3, [33])]
-- delistedFunArgs : [(P1, 11), (P2, True), (P3, 33)]
mkInputEntries :: Show t => [String] -> Rule t -> [SimpleInputEntry]
mkInputEntries schemaInpPreds r =
  let funApps = decomposeBinopClean (BBool BBand) (precondOfRule r)
      funArgs = map (appToFunArgs []) funApps
      delistedFunArgs = map (\(fn, args) -> (getNameOfPred fn, head args)) funArgs
  in map (foo delistedFunArgs) schemaInpPreds

-- looks up schema pred against map of rule pred-vals
foo :: Show t => [(String, Expr t)] -> String -> SimpleInputEntry
foo delistedFunArgs pd =
  case lookup pd delistedFunArgs of
    Just val -> SimpleInputEntry (Just (XMLText (printExpr val)))
    Nothing -> SimpleInputEntry Nothing

-- given an output pred and a rule, generates an output entry
mkOutputEntry :: Show t => String -> Rule t -> SimpleOutputEntry
mkOutputEntry _ r =
  let funApp = postcondOfRule r
      (fn, args) = appToFunArgs [] funApp
      -- delistedFunArgs = (getNameOfPred fn, head args)
  in bar (head args)

bar :: Show t => Expr t -> SimpleOutputEntry
bar val = SimpleOutputEntry (XMLText (printExpr val))

-- given a schema ("O2", ["P1", "P2"]) and a rule
mkRuleLine :: Show t => (String, [String]) -> Rule t -> SimpleDMNRule
mkRuleLine (_, inPreds) r =
  let
    bopToApps = decomposeBinopClean (BBool BBand)
    appToPreds = getNameOfPred . fst . appToFunArgs []

    outFunApps = bopToApps (postcondOfRule r)
    -- outpred is always a singleton
    outPred = head $ map appToPreds outFunApps

  in SimpleDMNRule (mkInputEntries inPreds r) (mkOutputEntry outPred r)


-- decomposeBinop :: BinOp -> Expr t -> [Expr t]
-- appToFunArgs :: [Expr t] -> Expr t -> (Expr t, [Expr t])


-- assume that all variables come from tables
