
module LS.XPile.ToDMN.FromSimpleToReg where

import ToDMN.Types
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Maybe

type ID = State (Map.Map String Int)

mkID :: String -> ID String
mkID pfx = do
  mymap <- get
  let n = fromMaybe 1 (Map.lookup pfx mymap)
  put (Map.insert pfx (n+1) mymap)
  return $ pfx ++ "_" ++ show n

decisionsToDefs :: [Decision] -> ID Definitions
decisionsToDefs decs = do
  iddefs <- mkID "Definitions"
  return $ Definitions {
    --   sXmlns = "https://www.omg.org/spec/DMN/20191111/MODEL/"
    -- , sXmlnsDmndi = "https://www.omg.org/spec/DMN/20191111/DMNDI/"
    -- , sXmlnsDc = "http://www.omg.org/spec/DMN/20180521/DC/"
    -- , sXmlnsModeler = "http://camunda.org/schema/modeler/1.0"
    -- , sXmlnsDi = "http://www.omg.org/spec/DMN/20180521/DI/"
      sDefId = iddefs
    , sDefName = "myDRD"
    , sNamespace = "http://camunda.org/schema/1.0/dmn"
    , sExporter = "Camunda Modeler"
    , sExporterVersion = "5.1.0"
    , sModelerExPlat = "Camunda Cloud"
    , sModelerExPlatVer = "8.0.0"
    , sDecisions = decs
    }

sDecisionToDecision :: SimpleDecision -> ID Decision
sDecisionToDecision (SimpleDecTableEl sInfoReqs simSchema sDMNRules) = do
  sch         <- sSchemaToSchema simSchema
  let outsch  =  sOutputSchema sch
  let outname =  sOutputSchemaVarName outsch
  let outtype =  sOutputSchemaFEELType outsch
  iddt        <- mkID "DecisionTable"
  sinforeqs   <- mapM sInfoReqToInfoReq sInfoReqs
  sdmns       <- mapM sDMNRuleToDMNRule sDMNRules
  iddov       <- mkID "InformationItem"
  return $ Decision outname outname (DecOutVar iddov outname outtype) sinforeqs $ DecTable iddt sch sdmns
sDecisionToDecision SimpleLitExprEl {} = error "not yet implemented"

sInfoReqToInfoReq :: SimpleInfoReq -> ID InfoReq
sInfoReqToInfoReq (SimpleReqInputEl reqInput) = do
  idir <- mkID "InformationRequirement"
  return $ ReqInputEl idir reqInput

sSchemaToSchema :: SimpleSchema -> ID Schema
sSchemaToSchema (SimpleSchema sInputSchemas' sOutputSchema') = do
  Schema
    <$> mapM sInputSchemaToInputSchema sInputSchemas'
    <*> sOutputSchemaToOutputSchema sOutputSchema'

sInputSchemaToInputSchema :: SimpleInputSchema -> ID InputSchema
sInputSchemaToInputSchema (SimpleInputSchema sInpExprVarName sInpExprFEELType) = do
  idic <- mkID "InputClause"
  idle <- mkID "LiteralExpression"
  return $
    InputSchema idic (Just "optional input label")
    (InputExprEl idle sInpExprFEELType (XMLText sInpExprVarName))

sOutputSchemaToOutputSchema :: SimpleOutputSchema -> ID OutputSchema
sOutputSchemaToOutputSchema (SimpleOutputSchema sOutSchemaVarName sOutSchemaFEELType) = do
  idoc <- mkID "OutputClause"
  return $ OutputSchema idoc (Just "optional output label") sOutSchemaVarName sOutSchemaFEELType

sDMNRuleToDMNRule :: SimpleDMNRule -> ID DMNRule
sDMNRuleToDMNRule (SimpleDMNRule sInpEntries sOutputEntry') = do
  DMNRule
    <$> mkID "DecisionRule"
    <*> mapM sInputEntryToInputEntry sInpEntries
    <*> sOutputEntryToOutputEntry sOutputEntry'

sInputEntryToInputEntry :: SimpleInputEntry -> ID InputEntry
sInputEntryToInputEntry (SimpleInputEntry mCondition) = do
  idut <- mkID "UnaryTests"
  return $ InputEntry idut mCondition

sOutputEntryToOutputEntry :: SimpleOutputEntry -> ID OutputEntry
sOutputEntryToOutputEntry (SimpleOutputEntry feelExpr) = do
  OutputEntry <$> mkID "LiteralExpression" <*> pure feelExpr
