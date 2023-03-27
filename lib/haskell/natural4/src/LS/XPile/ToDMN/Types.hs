
module LS.XPile.ToDMN.Types where

import qualified Text.XML.HXT.Core as HXT
-- import L4.Syntax (BComparOp, Val)

type Id = String
type VarName = XMLText
type Label = String
type DecName = String

-- TODO
-- should FEELExpr be XMLText? see XMLText def below
-- type FEELExpr = String

data DecOutVar = DecOutVar
  { sDecVarId :: Id
  , sDecVarName :: DecName
  , sDecVarFEELType :: FEELType }
  deriving Show

instance HXT.XmlPickler DecOutVar where
  xpickle = xpDecOutVar

xpDecOutVar :: HXT.PU DecOutVar
xpDecOutVar = HXT.xpElem "variable" $
              HXT.xpWrap ( HXT.uncurry3 DecOutVar
                     , \v -> ( sDecVarId v
                             , sDecVarName v
                             , sDecVarFEELType v )) $
              HXT.xpTriple (HXT.xpAttr "id" HXT.xpText) (HXT.xpAttr "name" HXT.xpText) (HXT.xpAttr "typeRef" HXT.xpText)


data Definitions = Definitions
  {
  --   sXmlns :: String
  -- , sXmlnsDmndi :: String
  -- , sXmlnsDc :: String
  -- , sXmlnsModeler :: String
  -- , sXmlnsDi :: String
    sDefId :: Id
  , sDefName :: String
  , sNamespace :: String
  , sExporter :: String
  , sExporterVersion :: String
  , sModelerExPlat :: String
  , sModelerExPlatVer :: String
  , sDecisions :: [Decision] }
  deriving Show

withNS :: HXT.PU a -> HXT.PU a
withNS = HXT.xpAddNSDecl "" "https://www.omg.org/spec/DMN/20191111/MODEL/"
       . HXT.xpAddNSDecl "dmndi" "https://www.omg.org/spec/DMN/20191111/DMNDI/"
       . HXT.xpAddNSDecl "dc" "http://www.omg.org/spec/DMN/20180521/DC/"
       . HXT.xpAddNSDecl "modeler" "http://camunda.org/schema/modeler/1.0"
       . HXT.xpAddNSDecl "di" "http://www.omg.org/spec/DMN/20180521/DI/"

xpDefinitions :: HXT.PU Definitions
xpDefinitions = HXT.xpElem "definitions"
              . withNS
              $ HXT.xpWrap ( uncurry8 Definitions
                       , \d -> ( -- sXmlns d
                               -- , sXmlnsDmndi d
                               -- , sXmlnsDc d
                               -- , sXmlnsModeler d
                               -- , sXmlnsDi d
                                 sDefId d
                               , sDefName d
                               , sNamespace d
                               , sExporter d
                               , sExporterVersion d
                               , sModelerExPlat d
                               , sModelerExPlatVer d
                               , sDecisions d
                               )) $
                HXT.xp8Tuple
                          -- ( HXT.xpAttr "xmlns" HXT.xpText )
                          -- ( HXT.xpAttrNS "http://camunda.org/schema/1.0/dmn" "xmlns" "dmndi" HXT.xpText )
                          -- ( HXT.xpAttrNS "http://camunda.org/schema/1.0/dmn" "xmlns" "dc" HXT.xpText )
                          -- ( HXT.xpAttrNS "http://camunda.org/schema/1.0/dmn" "xmlns" "modeler" HXT.xpText )
                          -- ( HXT.xpAttrNS "http://camunda.org/schema/1.0/dmn" "xmlns" "di" HXT.xpText )
                          ( HXT.xpAttr "id" HXT.xpText )
                          ( HXT.xpAttr "name" HXT.xpText )
                          ( HXT.xpAttr "namespace" HXT.xpText )
                          ( HXT.xpAttr "exporter" HXT.xpText )
                          ( HXT.xpAttr "exporterVersion" HXT.xpText )
                          ( HXT.xpAttrNS "" "modeler" "executionPlatform" HXT.xpText )
                          ( HXT.xpAttrNS "" "modeler" "executionPlatformVersion" HXT.xpText )
                          ( HXT.xpList xpDecision )


-- Label is tagged name in a decision element
-- TODO: crosscheck with DMN standard to see if classification scheme matches
data Decision = Decision
  { sDecId :: Id
  , sDecName :: DecName
  , sDecOutVar :: DecOutVar
  , sDecTableInfoReqs :: [InfoReq]
  , sDecTableOrLitExpr :: DecTableOrLitExpr }
  deriving Show

instance HXT.XmlPickler Decision where
  xpickle = xpDecision
    -- xpElem "decision" $
    -- xpAlt tag ps
    -- where
    --   tag (LitExprEl _ _ _ _ _) = 0
    --   tag (DecTableEl _ _ _ _ _) = 1
    --   ps = [ xpElem "literalExpression" $
    --          xpWrap ( uncurry5 LitExprEl
    --                 , \e -> (e, e, e, e, e) ) $
    --          xp5Tuple (xpAttr "id" xpText) (xpAttr "label" xpText) (xpList xpInfoReq)
    --        -- for now
    --        , xpElem "decisionTable" $
    --          xpWrap ( uncurry5 DecTableEl
    --                 , \t -> (sDecTableId t, sDecTableLabel t, sDecTableInfoReqs t, sSchema t, sRules t) ) $
    --          xp5Tuple (xpAttr "id" xpText) (xpAttr "label" xpText) (xpList xpInfoReq) xpSchema (xpList xpDMNRule)
    --        ]

xpDecision :: HXT.PU Decision
xpDecision = HXT.xpElem "decision" $
             HXT.xpWrap ( uncurry5 Decision
                    , \d -> ( sDecId d
                            , sDecName d
                            , sDecOutVar d
                            , sDecTableInfoReqs d
                            , sDecTableOrLitExpr d )) $
             HXT.xp5Tuple (HXT.xpAttr "id" HXT.xpText) (HXT.xpAttr "name" HXT.xpText) xpDecOutVar (HXT.xpList xpInfoReq) xpDecTableOrLitExpr


data DecTableOrLitExpr =
  DecTable
  { sDecTableId :: Id
  , sSchema :: Schema
  , sRules :: [DMNRule] }
  -- | LitExpr Id Label DecOutVar [InfoReq] FEELExpr
  deriving Show

instance HXT.XmlPickler DecTableOrLitExpr where
  xpickle = xpDecTableOrLitExpr

xpDecTableOrLitExpr :: HXT.PU DecTableOrLitExpr
xpDecTableOrLitExpr = HXT.xpElem "decisionTable" $
                      HXT.xpWrap ( HXT.uncurry3 DecTable
                             , \t -> ( sDecTableId t
                                     , sSchema t
                                     , sRules t ) ) $
                      HXT.xpTriple (HXT.xpAttr "id" HXT.xpText) xpSchema (HXT.xpList xpDMNRule)


data Schema = Schema
  { sInputSchemas :: [InputSchema]
  , sOutputSchema :: OutputSchema }
  deriving Show

instance HXT.XmlPickler LS.XPile.ToDMN.Types.Schema where
  xpickle = xpSchema

xpSchema :: HXT.PU LS.XPile.ToDMN.Types.Schema
xpSchema = HXT.xpWrap ( uncurry Schema
                  , \s -> (sInputSchemas s, sOutputSchema s) ) $
           HXT.xpPair (HXT.xpList xpInputSchema) xpOutputSchema


-- TypeRef is a String representation of FEELExpr types
-- The FEELExpr types enumerated here only reflect the possible options on Camunda Modeler
-- The docs on FEEL data types lists an additional 2 types: lists, maps
-- Other types of FEEL expressions listed on the docs are: variables, control flow, functions
-- See https://docs.camunda.io/docs/components/modeler/feel/language-guide/feel-expressions-introduction
-- Maybe this is what the Modeler Any type covers?
data InputExprEl = InputExprEl
  { sInputExprElId :: Id
  , sInputExprFEELType :: FEELType
  , sInputExprVarName :: VarName }
  deriving Show

instance HXT.XmlPickler InputExprEl where
  xpickle = xpInputExprEl

xpInputExprEl :: HXT.PU InputExprEl
xpInputExprEl = HXT.xpElem "inputExpression" $
                HXT.xpWrap ( HXT.uncurry3 InputExprEl
                       , \ie -> (sInputExprElId ie, sInputExprFEELType ie, sInputExprVarName ie) ) $
                HXT.xpTriple (HXT.xpAttr "id" HXT.xpText) (HXT.xpAttr "typeRef" HXT.xpText) xpXMLText

-- data FEELType =
--     String
--   | Bool
--   | Number
--   -- | DateTime
--   -- | DayTimeDuration
--   -- | YearMonthDuration
--   deriving (Show, Read)
-- The above has been discarded in favour of the String type alias because
-- FeelType has to be converted to lowercase, which is not possible at the time of pickling
type FEELType = String


-- Table Headers
-- Label is tagged label in input/output elements
data InputSchema = InputSchema
  { sInputSchemaId :: Id
  , sInputLabel :: Maybe Label
  , sInputExprEl :: InputExprEl }
  deriving Show

instance HXT.XmlPickler InputSchema where
  xpickle = xpInputSchema

xpInputSchema :: HXT.PU InputSchema
xpInputSchema = HXT.xpElem "input" $
                HXT.xpWrap ( HXT.uncurry3 InputSchema
                       , \s -> (sInputSchemaId s, sInputLabel s, sInputExprEl s) ) $
                HXT.xpTriple (HXT.xpAttr "id" HXT.xpText) (HXT.xpAttrImplied "label" HXT.xpText) xpInputExprEl


data OutputSchema = OutputSchema
  { sOutputSchemaId :: Id
  , sOutputLabel :: Maybe Label
  , sOutputSchemaVarName :: String
  , sOutputSchemaFEELType :: FEELType }
  deriving Show

instance HXT.XmlPickler OutputSchema where
  xpickle = xpOutputSchema

xpOutputSchema :: HXT.PU OutputSchema
xpOutputSchema = HXT.xpElem "output" $
                 HXT.xpWrap ( HXT.uncurry4 OutputSchema
                        , \s -> (sOutputSchemaId s, sOutputLabel s, sOutputSchemaVarName s, sOutputSchemaFEELType s) ) $
                 HXT.xp4Tuple (HXT.xpAttr "id" HXT.xpText) (HXT.xpAttrImplied "label" HXT.xpText) (HXT.xpAttr "name" HXT.xpText)
                 (HXT.xpAttr "typeRef" HXT.xpText)


type DRD = [Decision]


data InfoReq = ReqInputEl
  { sReqInputId :: Id
  , sReqInput :: ReqInput }
  -- Id ReqInput
  deriving Show
type ReqInput = String

instance HXT.XmlPickler InfoReq where
  xpickle = xpInfoReq

xpInfoReq :: HXT.PU InfoReq
xpInfoReq = HXT.xpElem "informationRequirement" $
            HXT.xpWrap ( uncurry ReqInputEl
                   , \i -> (sReqInputId i, sReqInput i) ) $
            HXT.xpPair (HXT.xpAttr "id" HXT.xpText) (HXT.xpElem "requiredDecision" (HXT.xpAttr "href" HXT.xpText))


data DMNRule = DMNRule
  { sRuleId :: Id
  , sInputEntries :: [InputEntry]
  , sOutputEntry :: OutputEntry }
  deriving Show

instance HXT.XmlPickler DMNRule where
  xpickle = xpDMNRule

xpDMNRule :: HXT.PU DMNRule
xpDMNRule = HXT.xpElem "rule" $
            HXT.xpWrap ( HXT.uncurry3 DMNRule
                   , \r -> (sRuleId r, sInputEntries r, sOutputEntry r) ) $
            HXT.xpTriple (HXT.xpAttr "id" HXT.xpText) (HXT.xpList xpInputEntry) xpOutputEntry


data InputEntry = InputEntry
  { sInputEntryId :: Id
  , sMaybeCondition :: Maybe Condition }
  deriving Show

instance HXT.XmlPickler InputEntry where
  xpickle = xpInputEntry

xpInputEntry :: HXT.PU InputEntry
xpInputEntry = HXT.xpElem "inputEntry" $
               HXT.xpWrap ( uncurry InputEntry
                      , \ip -> (sInputEntryId ip, sMaybeCondition ip)) $
               HXT.xpPair (HXT.xpAttr "id" HXT.xpText) (HXT.xpOption xpXMLText)


-- Condition is a FEEL Unary Test
-- See notes on FEELTypes above
-- TODO: can input expression be <= y + 3?
-- data Condition
--   = CompVal BComparOp Val
--   | CompVar BComparOp VarName
--   -- | Intv
--   -- | Disj
--   -- | Neg
--   -- | FEELExpr

newtype XMLText = XMLText {sText :: String}
  deriving Show

instance HXT.XmlPickler XMLText where
  xpickle = xpXMLText

xpXMLText :: HXT.PU XMLText
xpXMLText = HXT.xpElem "text" $ HXT.xpWrap (XMLText, \ (XMLText b) -> b) HXT.xpText

type Condition = XMLText

-- should FEELExpr be XMLText?
type FEELExpr = XMLText


-- Conclusion type is constrained by OutputSchema TypeRef? Which is a String representation of FEELExpr types
-- data OutputEntry = OutputEntry { sOutputId :: Id, sExpr :: FEELExpr }
data OutputEntry = OutputEntry
  { sOutputId :: Id
  , sExpr :: FEELExpr }
  deriving Show

instance HXT.XmlPickler OutputEntry where
  xpickle = xpOutputEntry

xpOutputEntry :: HXT.PU OutputEntry
xpOutputEntry = HXT.xpElem "outputEntry" $
               HXT.xpWrap ( uncurry OutputEntry
                      , \op -> (sOutputId op, sExpr op) ) $
               HXT.xpPair (HXT.xpAttr "id" HXT.xpText) xpXMLText


-- to investigate: how much of this DMN syntax is part of the DMN standard, and how much is Camunda's implementation?
-- paste findings from slack threads


-- Constraints:
-- to resolve difficulties around name/id being used interchangeably as variables
-- name and id of a component (where both are required) will be the same
-- tables will only have 1 output column
-- name of output column is the name of the table and its decision ID &
-- a pred P1 will have input expr "P1" and be produced from a table (with name and id) "P1" with output name "P1"

-- Simple Types (for removing ids and names)

newtype SimpleInfoReq = SimpleReqInputEl ReqInput deriving Show

-- when inforeq is an empty list, inforeq element is not generated
data SimpleDecision
  = SimpleLitExprEl DecOutVar [InfoReq] FEELExpr
  | SimpleDecTableEl [SimpleInfoReq] SimpleSchema [SimpleDMNRule]
  deriving Show

data SimpleSchema = SimpleSchema [SimpleInputSchema] SimpleOutputSchema
  deriving Show

data SimpleInputSchema = SimpleInputSchema
  { sSimpleInputExprVarName :: String
  , sSimpleInputExprFEELType :: FEELType }
  deriving Show

data SimpleOutputSchema = SimpleOutputSchema
  { sSimpleOutputSchemaVarName :: String
  , sSimpleOutputSchemaFEELType :: FEELType }
  deriving Show


data SimpleDMNRule = SimpleDMNRule
  { sSimpleInputEntries :: [SimpleInputEntry]
  , sSimpleOutputEntry :: SimpleOutputEntry }
  deriving Show

newtype SimpleInputEntry = SimpleInputEntry
  { sSimpleMaybeCondition :: Maybe Condition }
  deriving Show

newtype SimpleOutputEntry = SimpleOutputEntry
  { sSimpleExpr :: FEELExpr }
  deriving Show


-- desired output
-- to be transferred to test file
schemaO :: SimpleSchema
schemaO =
  SimpleSchema
    [ SimpleInputSchema "P1" "number"
    , SimpleInputSchema "P2" "number"
    , SimpleInputSchema "P3" "number" ]
    ( SimpleOutputSchema "O" "number" )

schemaO2 :: SimpleSchema
schemaO2 =
  SimpleSchema
    [ SimpleInputSchema "P1" "number" ]
    ( SimpleOutputSchema "O2" "number" )


r1 :: SimpleDMNRule
r1 =
  SimpleDMNRule
    [ SimpleInputEntry (Just (XMLText "1")),
      SimpleInputEntry (Just (XMLText "True")),
      SimpleInputEntry Nothing ]
    ( SimpleOutputEntry (XMLText "10"))

r2 :: SimpleDMNRule
r2 =
  SimpleDMNRule
    [ SimpleInputEntry (Just (XMLText "2")),
      SimpleInputEntry (Just (XMLText "4")),
      SimpleInputEntry (Just (XMLText "False")) ]
    ( SimpleOutputEntry (XMLText "11") )

r3 :: SimpleDMNRule
r3 =
  SimpleDMNRule
    [ SimpleInputEntry (Just (XMLText "1"))
    , SimpleInputEntry Nothing
    , SimpleInputEntry Nothing ]
    ( SimpleOutputEntry (XMLText "10") )


-- Utils
uncurry1 :: (a -> b) -> a -> b
uncurry1 b a = b a

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f ~(a, b, c, d, e) = f a b c d e

uncurry13 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n)
          -> (a, b, c, d, e, f, g, h, i, j, k, l, m)
          -> n
uncurry13 fn ~(a, b, c, d, e, f, g, h, i, j, k, l, m) = fn a b c d e f g h i j k l m


uncurry8 :: (a -> b -> c -> d -> e -> f -> g -> h -> i)
         -> (a, b, c, d, e, f, g, h)
         -> i
uncurry8 fn ~(a, b, c, d, e, f, g, h) = fn a b c d e f g h
