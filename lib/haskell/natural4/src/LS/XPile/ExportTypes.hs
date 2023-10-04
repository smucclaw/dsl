{-# OPTIONS_GHC -W #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, TemplateHaskell #-}
{-# LANGUAGE  DerivingVia, DeriveAnyClass #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-
Provide export from Natural4 type declarations of record types of the form
DECLARE T
HAS field1 is T1
    ...
    fieldn is Tn

or of enum types of the form:
DECLARE T IS ONE OF enum1 .. enumn

These declarations are meant to be exported to JSON and to Prolog.
The approach is strictly ad-hoc and not systematic and agnostic of
other translations (in particular to JSON) that may have been developed.
-}

module LS.XPile.ExportTypes where
    -- TODO: Add export list!

import Data.Text qualified as T
import Prettyprinter
import Prettyprinter.Render.Text ()
import LS.Rule as SFL4
  ( Rule (Hornlike, TypeDecl, keyword, clauses, has, name, super),
    extractMTExprs
  )
import LS.Types as SFL4
  (
    -- BoolStructR,
    HornClause (..),
    -- HornClause2,
    MTExpr (..),
    -- ParamText,
    ParamType (TList1, TOne),
    RelationalPredicate (..),
    RPRel(..),
    TypeSig (..),
    -- mt2text,
    mtexpr2text,
    -- rel2txt,
    -- untypePT, 
    RuleName,
    MyToken(Means)
  )
import AnyAll qualified as AA (BoolStruct(Leaf))
import Data.Text (unpack)
import Debug.Trace (trace)
import Data.List (isSuffixOf, intercalate, partition)
import Data.Char (toLower)
-- import Optics hiding (has)
import Data.Generics.Product.Types (HasTypes)
import Optics.TH

-- for json types -----------------------------------
type TypeName = String
type ConstructorName = String

data FieldType =
      FTBoolean
    | FTNumber
    | FTString
    | FTRef TypeName
    | FTList FieldType
    | FTDate
    deriving (Eq, Ord, Show, Read)

data Field = Field
    { fieldName :: String
    , fieldType :: FieldType
    }
    deriving (Eq, Ord, Show, Read)

-- for json metadata annotations -----------------------------------
{- 
Right now we're just adding a __global__ metadata dict to the json schema

EG:

```L4
§	Global definition dictionary	
§§	High risk activity definitions	
		
§§§	Clause 2.1	
	Clause 2.1	
MEANS	ok 1	
		
§§§	Clause 2.2	
	Clause 2.2 	
MEANS	whatever	123
```

will become, in the json schema,

```json
{
    x_global_metadata_definitions": {"type": "object",
                                     "properties": 
                                            { "Clause 2.1": ["ok 1"],
                                              "Clause 2.2": ["whatever", "123"] }}
}
```


Things to note:
    * https://json-schema.org/blog/posts/custom-annotations-will-continue
-}


data MdataKV = MdataKV
    { key :: String
    , annotatn :: [String]
    }
    deriving (Eq, Ord, Show, Read)
makePrisms ''MdataKV 

type MdGroupName = String
data MetadataGrp = MkMetadata
    { mdGroupName :: MdGroupName
    -- ^ e.g.: "x_global_metadata_definitions"
    , metadata      :: [MdataKV]
    }
    deriving (Eq, Ord, Show, Read)
makePrisms ''MetadataGrp

-- data TypeRec = MkTypeRecord 
--         { typeName :: TypeName
--         , fields :: [Field]
--         }
--     deriving (Eq, Ord, Show, Read)

-- data ExpTypeEnum = MkTypeEnum
--         { typeName :: TypeName
--         , enums :: [ConstructorName]
--         }
--     deriving (Eq, Ord, Show, Read)

-- Expression types, coming
-- either as records (DECLARE .. HAS)
-- or as enums (DECLARE .. IS ONE OF)
data JSchemaExp
    = ExpTypeRecord
        { typeName :: TypeName
        , fields :: [Field]
        }
    | ExpTypeEnum
        { typeName :: TypeName
        , enums :: [ConstructorName]
        }
    | ExpMetadataGrp MetadataGrp
    deriving stock (Eq, Ord, Show, Read)

pattern MkMetadata :: MdGroupName -> [MdataKV] -> JSchemaExp
pattern MkMetadata{grpName, mdata} = 
    ExpMetadataGrp ( MkMetadataGrp { mdGroupName = grpName
                                   , metadata = mdata })

processTextForJsonSchema :: T.Text -> T.Text
processTextForJsonSchema = T.toLower . T.intercalate (T.pack "_") . T.words

stringifyMdataMTExpr :: MTExpr -> String
stringifyMdataMTExpr = T.unpack . processTextForJsonSchema . mtexpr2text

-- TODO: Rewrite this in terms of processTextForJsonSchema
typeDeclNameToTypeName :: RuleName -> TypeName
typeDeclNameToTypeName [ mtt@(MTT n) ] = 
    typeDeclExprToTypeName mtt
        where
            -- | TODO: This will need cleaning up 
            typeDeclExprToTypeName :: MTExpr -> TypeName
            typeDeclExprToTypeName (MTT n) = map toLower . intercalate "_" . words . T.unpack $ n
            typeDeclExprToTypeName _       = ""
typeDeclNameToTypeName _ = "" -- TODO: should be an error case

typeDeclNameToFieldName :: RuleName -> String
typeDeclNameToFieldName = typeDeclNameToTypeName

-- | Collect the enums into a list of strings
getEnums :: forall s. (HasTypes s MTExpr) => s -> [String]
getEnums = map (T.unpack . mtexpr2text) . extractMTExprs

textToFieldType :: T.Text -> FieldType
textToFieldType tn = case unpack tn of
        "Boolean" -> FTBoolean
        "Number" -> FTNumber
        "String" -> FTString
        "Date" -> FTDate
        n -> FTRef n

typeDeclSuperToFieldType :: Maybe TypeSig -> FieldType
typeDeclSuperToFieldType (Just (SimpleType TOne tn)) = textToFieldType tn
-- TODO: There somehow cannot be lists of lists (problem both of the parser and of data structures).

typeDeclSuperToFieldType (Just (SimpleType TList1 tn)) = FTList (FTRef (map toLower $ intercalate "_" $ words $ unpack tn))
typeDeclSuperToFieldType other = do
    trace ("Unhandled case: " ++ show other) FTString

ruleFieldToField :: Rule -> [Field]
ruleFieldToField (TypeDecl{name=n, super=sup}) =
    [Field (typeDeclNameToFieldName n) (typeDeclSuperToFieldType sup)]
ruleFieldToField _ = []

-- unpackHierarchy :: Rule -> [Field]
-- unpackHierarchy (TypeDecl{name=n}) =
--     [Field (typeDeclNameToFieldName n) (FTList (FTRef (unpack n)))]
-- unpackHierarchy _ = []

{- | Pattern for matching on L4 `MEANS`
Example:
    ```
    Hornlike
        { name =
            [ MTT "clause 15.1" ]
        , super = Nothing
        , keyword = Means
        , given = Nothing , giveth = Nothing , upon = Nothing
        , clauses =
            [ HC { hHead = RPBoolStructR
                    [ MTT "clause 15.1" ] RPis
                    ( Leaf
                        ( RPMT [ MTT "text that we want to be transferred over to the json so that it can be displayed on mouseover in the web UI" ] ))
                , hBody = Nothing
                }
            ]
        , rlabel = Nothing
    ```

below matching MultiTerm instead of Text, because
typeDeclNameToTypeName validates the shape of the MultiTerm (singleton list)
-}
pattern TermMeansThat :: [MTExpr] -> [MTExpr] -> Rule
pattern TermMeansThat term defnMtexprs <- Hornlike{keyword=Means,
                                                   clauses= [ HC { hHead = RPBoolStructR term RPis ( AA.Leaf ( RPMT defnMtexprs ) ) } ]}

ruleIsMeans :: Rule -> Bool
ruleIsMeans = \case TermMeansThat _ _ -> True; _others -> False 

rule2JsonExp :: Rule -> [JSchemaExp]
rule2JsonExp = \case
    TypeDecl{name=[MTT n], has=fields, super=Nothing}
      -> case unpack n of
           n' | "Hierarchy" `isSuffixOf` n' -> concatMap rule2HierarchyBool fields
           _ ->  [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (concatMap ruleFieldToField fields)]
    TypeDecl{name=n, has=[], super=Just (InlineEnum TOne enums)}
      -> [ExpTypeEnum (typeDeclNameToTypeName n) (getEnums enums)]
    TermMeansThat term def
      -> undefined
        -- TODO: Stopped because I realized https://smucclaw.slack.com/archives/C0164JRERL0/p1696347657385909
        --  [MetadataGrp 
        --     MkMetadata { typeName = typeDeclNameToTypeName term
        --                 , fields = [MkAnnotField {}]
        --                 )
        
        -- [ExpTypeRecord 
        --     (makeMetadataTypeName term) 
        --     [Field {fieldName = T.unpack $ mt2text def, 
        --             fieldType = textToFieldType (T.pack "object")}]]

    _ -> []
        -- where
            -- makeMetadataTypeName term = term <> MTT (T.pack "metadata")


-- TODO: do we want to split long lines e.g. like below?
-- "winter sports or ice hockey; horse riding or playing polo; canoeing, sailing or windsurfing"
-- -> [MTT "winter sports or ice hockey", MTT "horse riding or playing polo", MTT "canoeing, sailing or windsurfing"]

-- rule2JsonExp :: Rule -> [JSchemaExp]
-- rule2JsonExp (TypeDecl{name=[MTT n], has=fields, super=Nothing}) =
--     let hierarchyName = (unpack n) ++ " Hierarchy"
--     in case findNonHierarchyRule hierarchyName (TypeDecl{name=[MTT n], has=fields, super=Nothing}) of
--         True  -> [ExpTypeRecord (typeDeclNameToTypeName [MTT (T.pack "hi")]) (concatMap ruleFieldToField fields)]
--         False | " Hierarchy" `isSuffixOf` (unpack n) -> concatMap rule2HierarchyBool fields
--                 | otherwise -> [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (concatMap ruleFieldToField fields)]
--         _ ->  [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (concatMap ruleFieldToField fields)]
-- rule2JsonExp (TypeDecl{name=n, has=[], super=Just (InlineEnum TOne enums)}) =
--     [ExpTypeEnum (typeDeclNameToTypeName (n)) (unpackEnums enums)]
-- rule2JsonExp _ = []

rule2ExpType :: Rule -> [JSchemaExp]
rule2ExpType (TypeDecl{name=[MTT n], has=fields, super=Nothing}) = [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (concatMap ruleFieldToField fields)]
rule2ExpType (TypeDecl{name=n, has=[], super=Just (InlineEnum TOne enums)}) =
    [ExpTypeEnum (typeDeclNameToTypeName n) (getEnums enums)]
rule2ExpType _ = []

findNonHierarchyRule :: TypeName -> Rule -> Bool
findNonHierarchyRule hierarchyName rule =
   getType rule == hierarchyName

getType:: Rule -> TypeName
getType(TypeDecl{name=[MTT n]}) =
    typeDeclNameToTypeName [MTT n]
getType _ = ""

-- enumToField :: ConstructorName -> Field
-- enumToField enumName = Field enumName FTBoolean

rule2HierarchyBool :: Rule -> [JSchemaExp]
rule2HierarchyBool (TypeDecl{name=[MTT n], has=fields, super=Nothing}) = [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (concatMap ruleBool fields)]
    where
        ruleBool ((TypeDecl{name=n})) = [Field (typeDeclNameToFieldName n) FTBoolean]
        ruleBool _                    = []

rule2HierarchyBool _ = []

------------------------------------
-- Output of types to Prolog

class ShowTypesProlog x where
    showTypesProlog :: x -> Doc ann

instance ShowTypesProlog FieldType where
    showTypesProlog FTBoolean = pretty "boolean"
    showTypesProlog FTNumber = pretty "number"
    showTypesProlog FTString = pretty "string"
    showTypesProlog FTDate = pretty "string"
    showTypesProlog (FTRef n) = pretty "ref" <> parens (pretty n)
    showTypesProlog (FTList t) = pretty "list" <> parens (showTypesProlog t)

instance ShowTypesProlog Field where
    showTypesProlog (Field fn ft) =
        parens (pretty fn <> pretty ", " <> showTypesProlog ft)

showEnumProlog :: TypeName -> ConstructorName -> Doc ann
showEnumProlog tn en =
    pretty "typedecl" <> parens (pretty tn <> pretty ", " <> pretty en <> pretty ", " <> pretty en) <> pretty "."
instance ShowTypesProlog JSchemaExp where
    showTypesProlog (ExpTypeRecord tn fds) =
        pretty "typedecl" <>
        nest 4
        (parens (
            pretty tn <> pretty ", " <> pretty tn <> pretty ", " <>
            brackets (vsep (punctuate comma (map showTypesProlog fds)))
            )
        ) <>
        pretty "."

    showTypesProlog (ExpTypeEnum tn enums) =
        vsep (map (showEnumProlog tn) enums)


-- the root data type of a Json Schema is always embedded in an entrypoint,
-- here called "toplevel"
entrypointName :: String
entrypointName = "toplevel"

rulesToPrologTp :: [SFL4.Rule] -> String
rulesToPrologTp rs =
    let ets = concatMap rule2ExpType rs in
        (case ets of
            [] -> show emptyDoc
            rt : _rts ->
                let entry = ExpTypeRecord entrypointName [Field entrypointName (FTRef rt.typeName)] in
                show (vsep (map showTypesProlog (entry:ets))))

------------------------------------
-- Output of types to Json Schema
-- also see https://json-schema.org/understanding-json-schema/

class ShowTypesJson x where
    showTypesJson :: x -> Doc ann

-- definitions are located in a section called $defs according to the Json Schema standard,
-- but the choice seems to be arbitrary.
defsLocationName :: String
defsLocationName = "$defs"

defsLocation :: String -> String
defsLocation n = "#/" ++ defsLocationName ++ "/" ++ (map toLower $ intercalate "_" $ words n)

jsonType :: Pretty a => a -> Doc ann
jsonType t =
    dquotes (pretty "type") <> pretty ": " <> dquotes (pretty t)

showRequireds :: [Field] -> Doc ann
showRequireds fds =
    dquotes (pretty "required") <> pretty ": " <>
    brackets (hsep (punctuate comma (map (dquotes . pretty . (.fieldName)) fds)))

showRef :: TypeName -> Doc ann
showRef n =
        dquotes (pretty "$ref") <> pretty ": " <>
        dquotes (pretty (defsLocation n))

bracketArgs :: Pretty a => [a] -> Doc ann
bracketArgs = brackets . hsep . punctuate comma . map (dquotes . pretty) 


-- Due to limitations of the JSON Form Web UI builder,
-- single references are not represented as single objects,
-- but arrays of length 1.
-- List types can only have nesting level 1 (a limitation inherited from Natural4)
instance ShowTypesJson FieldType where
    showTypesJson FTBoolean =
        jsonType "boolean"
    showTypesJson FTNumber =
        jsonType "number"
    showTypesJson FTString =
        jsonType "string"
    showTypesJson FTDate =
        jsonType "string" <> pretty "," <>
        dquotes (pretty "format") <> pretty ": " <> dquotes (pretty "date")
    showTypesJson (FTRef n) =
        -- jsonType "array" <> pretty "," <>
        -- dquotes (pretty "minItems") <> pretty ": 1," <>
        -- dquotes (pretty "maxItems") <> pretty ": 1," <>
        -- dquotes
        -- (pretty "items") <> pretty ": " <>
        -- braces
        showRef n
    showTypesJson (FTList (FTRef n)) =
        jsonType "array" <> pretty "," <>
        dquotes (pretty "items") <> pretty ": " <>
        braces (showRef n)
    showTypesJson _ =
        jsonType "string"

instance ShowTypesJson Field where
    showTypesJson :: Field -> Doc ann
    showTypesJson (Field fn ft) =
        dquotes (pretty fn) <> pretty ": " <> braces (showTypesJson ft)

instance ShowTypesJson MdataKV where
    showTypesJson :: MdataKV -> Doc ann
    showTypesJson (MdataKV key metadata) = 
        dquotes (pretty key) <> pretty ": " <> bracketArgs metadata
       


instance ShowTypesJson JSchemaExp where
    showTypesJson (ExpTypeEnum tn enums) =
        dquotes (pretty tn) <> pretty ": " <>
        nest 4
        (braces (
            jsonType "string" <> pretty "," <>
            dquotes (pretty "enum") <> pretty ": " <>
            nest 4 (brackets (hsep (punctuate comma (map (dquotes . pretty) enums))))
        ))
    showTypesJson (ExpTypeRecord tn fds) =
        dquotes (pretty tn) <> pretty ": " <>
        nest 4
        (braces (
            jsonType "object" <> pretty "," <>
            dquotes (pretty "properties") <>  pretty ": " <>
            nest 4
            (braces (vsep (punctuate comma (map showTypesJson fds)))) <>
            pretty "," <>
            nest 4
            (showRequireds fds)
        ))
    -- showTypesJson (MetadataGrp (MkMetadata tn annotfields)) = 
    --     -- TODO: Abstract common functionality between this and ExpTypeRecord out later
    --     dquotes (pretty $ "metadata" <> tn) <> pretty ": " <>
    --     nest 4
    --     (braces (
    --         jsonType "object" <> pretty "," <>
    --         dquotes (pretty "properties") <>  pretty ": " <>
    --         nest 4
    --         (braces (vsep (punctuate comma (map showTypesJson annotfields)))) <>
    --         pretty ","
    --     ))


jsonPreamble :: TypeName -> [Doc ann]
jsonPreamble tn = [
    dquotes (pretty "$schema") <> pretty ":" <> dquotes (pretty "http://json-schema.org/draft-07/schema#"),
    jsonType "object",
    dquotes (pretty "properties") <> pretty ": " <>
    braces (dquotes (pretty entrypointName) <> pretty ": " <>
        braces (showTypesJson (FTRef tn)))
    ]

jsonifyMeans :: [Rule] -> JSchemaExp
jsonifyMeans = undefined

rulesToJsonSchema :: [SFL4.Rule] -> String
rulesToJsonSchema rs =
    let 
        -- TODO: Would be better to avoid bullion blindness here; improve when time permits
        -- Partitioning in advance because we want to group the means HLikes into one object
        (meansRules, nonMeansRules) = partition ruleIsMeans rs
        globalMetadataDefs = jsonifyMeans meansRules
        ets = concatMap rule2JsonExp nonMeansRules
    in
        (case ets of
            [] -> show (braces emptyDoc)
            (rt : _rts) ->
                trace ("ets: " ++ show ets) $
                show
                (braces
                    (vsep (punctuate comma
                    (
                        jsonPreamble rt.typeName ++
                        [dquotes (pretty defsLocationName) <> pretty ": " <>
                        braces (
                            nest 4
                            (vsep (punctuate comma (map showTypesJson ets)))
                        )
                        ]
                    )
                    ) )
                )
        )

rulesToUISchema :: [SFL4.Rule] -> String
rulesToUISchema rs =
    let ets = concatMap rule2JsonExp rs in
        (case ets of
            [] -> show (braces emptyDoc)
            rts ->
                trace ("ets: " ++ show ets) $
                show
                (braces
                    (vsep (punctuate comma
                    (
                        jsonPreamble (last rts).typeName ++
                        [dquotes (pretty defsLocationName) <> pretty ": " <>
                        braces (
                            nest 4
                            (vsep (punctuate comma (map showTypesJson ets)))
                        )
                        ]
                    )
                    ) )
                )
        )