{-# OPTIONS_GHC -W #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric, FlexibleContexts, TypeFamilies, TypeApplications, DataKinds #-}
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
    mt2text,
    mtexpr2text,
    -- rel2txt,
    -- untypePT, 
    RuleName,
    MyToken(Means)
  )
import AnyAll qualified as AA (BoolStruct(Leaf))
import Data.Text (unpack)
import Debug.Trace (trace)
import Data.List (isSuffixOf, intercalate)
import Data.Char (toLower)
import Optics hiding (has)
import Data.Text.Optics (unpacked)
import Data.Generics.Product.Types (types, HasTypes)


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

-- | c.f. https://json-schema.org/blog/posts/custom-annotations-will-continue
data AnnotatField = MkAnnotField 
    { fieldName :: String
    , annotatn :: String}

-- Expression types, coming
-- either as records (DECLARE .. HAS)
-- or as enum s(DECLARE .. IS ONE OF)
data ExpType
    = ExpTypeRecord
        { typeName :: TypeName
        , fields :: [Field]
        }
    | ExpTypeEnum
        { typeName :: TypeName
        , enums :: [ConstructorName]
        }
    -- TODO: I think we want something like this, but I don't like how this is a sum of records with different fields
    -- | ExpTypeMetadata
    --     { typeName :: TypeName
    --     , fields :: [AnnotatField]
    --     }
    deriving (Eq, Ord, Show, Read)

typeDeclNameToTypeName :: RuleName -> TypeName
typeDeclNameToTypeName [MTT n] =  map toLower . intercalate "_" . words . unpack $ n
typeDeclNameToTypeName _ = "" -- TODO: should be an error case

typeDeclNameToFieldName :: RuleName -> String
typeDeclNameToFieldName = typeDeclNameToTypeName

-- | Collect the enums into a list of strings
getEnums :: forall s. (HasTypes s MTExpr) => s -> [String]
getEnums enums = enums ^.. types @MTExpr % to mtexpr2text % unpacked

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

{- |
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

rule2JsonExp :: Rule -> [ExpType]
rule2JsonExp = \case
    TypeDecl{name=[MTT n], has=fields, super=Nothing}
      -> case unpack n of
           n' | "Hierarchy" `isSuffixOf` n' -> concatMap rule2HierarchyBool fields
           _ ->  [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (concatMap ruleFieldToField fields)]
    TypeDecl{name=n, has=[], super=Just (InlineEnum TOne enums)}
      -> [ExpTypeEnum (typeDeclNameToTypeName n) (getEnums enums)]
    TermMeansThat term def
      -> undefined
        -- I think that we should use something like the ExpTypeMetadata type sketched in comments above
        -- [ExpTypeRecord 
        --     (makeMetadataTypeName term) 
        --     [Field {fieldName = T.unpack $ mt2text def, 
        --             fieldType = textToFieldType (T.pack "object")}]]
      
    _ -> []
        where
            makeMetadataTypeName term = term <> MTT "metadata"   


-- TODO: do we want to split long lines e.g. like below?
-- "winter sports or ice hockey; horse riding or playing polo; canoeing, sailing or windsurfing"
-- -> [MTT "winter sports or ice hockey", MTT "horse riding or playing polo", MTT "canoeing, sailing or windsurfing"]

-- rule2JsonExp :: Rule -> [ExpType]
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

rule2ExpType :: Rule -> [ExpType]
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

rule2HierarchyBool :: Rule -> [ExpType]
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
instance ShowTypesProlog ExpType where
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
                let entry = ExpTypeRecord entrypointName [Field entrypointName (FTRef (typeName rt))] in
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
    brackets (hsep (punctuate comma (map (dquotes . pretty . fieldName) fds)))

showRef :: TypeName -> Doc ann
showRef n =
        dquotes (pretty "$ref") <> pretty ": " <>
        dquotes (pretty (defsLocation n))


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

instance ShowTypesJson ExpType where
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

jsonPreamble :: TypeName -> [Doc ann]
jsonPreamble tn = [
    dquotes (pretty "$schema") <> pretty ":" <> dquotes (pretty "http://json-schema.org/draft-07/schema#"),
    jsonType "object",
    dquotes (pretty "properties") <> pretty ": " <>
    braces (dquotes (pretty entrypointName) <> pretty ": " <>
        braces (showTypesJson (FTRef tn)))
    ]


rulesToJsonSchema :: [SFL4.Rule] -> String
rulesToJsonSchema rs =
    let ets = concatMap rule2JsonExp rs in
        (case ets of
            [] -> show (braces emptyDoc)
            (rt : _rts) ->
                trace ("ets: " ++ show ets) $
                show
                (braces
                    (vsep (punctuate comma
                    (
                        jsonPreamble (typeName rt) ++
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
                        jsonPreamble (typeName (last rts)) ++
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