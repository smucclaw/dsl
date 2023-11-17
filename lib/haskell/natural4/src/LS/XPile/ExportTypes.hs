{-# OPTIONS_GHC -W #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE DerivingVia, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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

module LS.XPile.ExportTypes (
      rulesToUISchema
    , rulesToJsonSchema
    , rulesToHaskellTp
    , rulesToPrologTp
) where

import Data.Text qualified as T
import Prettyprinter
import Prettyprinter.Render.Text ()
import Prettyprinter.Interpolate (__di, di)
import L4.PrintProg (capitalise)

import LS.Rule as SFL4
  ( Rule (TypeDecl, has, name, super),
    extractMTExprs
  )
import LS.Types as SFL4
  (
    MTExpr (..),
    -- ParamText,
    ParamType (TList1, TOne),
    TypeSig (..),
    mtexpr2text,
    RuleName
  )
import Debug.Trace (trace)
-- import Data.List (isSuffixOf, intercalate)
import Data.Char (isAlphaNum)
import Data.Generics.Product.Types (HasTypes)
import Data.HashMap.Strict qualified as M
import Text.Regex.PCRE.Heavy qualified as PCRE

-- for json types -----------------------------------
type ConstructorName = T.Text
type FieldName = T.Text
type TypeName = T.Text

data FieldType =
      FTBoolean
    | FTNumber
    | FTString
    | FTRef TypeName
    | FTList FieldType
    | FTDate
    | FTInteger
    | FTEnum [FieldName]
    deriving (Eq, Ord, Show, Read)

data Field = Field
    { fieldName :: FieldName
    , fieldType :: FieldType
    }
    deriving (Eq, Ord, Show, Read)

-- non-json-schema-specific metadata will be moved to another module

-- can think about moving to using references to these records instead of a sum of records with different fieldnames
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
    deriving stock (Eq, Ord, Show, Read)

processTopLvlNameTextForJsonSchema :: T.Text -> T.Text
processTopLvlNameTextForJsonSchema = PCRE.gsub [PCRE.re|\s+|] ("_" :: T.Text)
  -- T.intercalate (T.pack "_") . T.words

-- stringifyMTEwrapper :: (T.Text -> T.Text) -> MTExpr -> String
-- stringifyMTEwrapper f = T.unpack . f . mtexpr2text

typeDeclNameToTypeName :: RuleName -> TypeName
typeDeclNameToTypeName [ MTT n ] = processTopLvlNameTextForJsonSchema n
typeDeclNameToTypeName _ = "" -- TODO: should be an error case

typeDeclNameToFieldName :: RuleName -> T.Text
typeDeclNameToFieldName = typeDeclNameToTypeName

-- | Collect the enums into a list of strings
getEnums :: forall s. (HasTypes s MTExpr) => s -> [T.Text]
getEnums = map mtexpr2text . SFL4.extractMTExprs

textToFieldType :: T.Text -> FieldType
textToFieldType tn = case tn of
        "Integer" -> FTInteger
        "Boolean" -> FTBoolean
        "Number" -> FTNumber
        "String" -> FTString
        "Date" -> FTDate
        n -> FTRef n

typeDeclSuperToFieldType :: Maybe TypeSig -> FieldType
typeDeclSuperToFieldType (Just (SimpleType TOne tn)) = textToFieldType tn
-- TODO: There somehow cannot be lists of lists (problem both of the parser and of data structures).

typeDeclSuperToFieldType (Just (SimpleType TList1 tn)) =
  FTList (FTRef $ PCRE.gsub [PCRE.re|\s+|] ("_" :: T.Text) tn)

typeDeclSuperToFieldType (Just (InlineEnum TOne enums)) = FTEnum (getEnums enums)

typeDeclSuperToFieldType other = do
    trace ("Unhandled case: " ++ show other) FTString

ruleFieldToField :: SFL4.Rule -> [Field]
ruleFieldToField (SFL4.TypeDecl{name=n, super=sup}) =
    [Field (typeDeclNameToFieldName n) (typeDeclSuperToFieldType sup)]
ruleFieldToField _ = []

-- unpackHierarchy :: Rule -> [Field]
-- unpackHierarchy (TypeDecl{name=n}) =
--     [Field (typeDeclNameToFieldName n) (FTList (FTRef (unpack n)))]
-- unpackHierarchy _ = []

rule2ExpType :: SFL4.Rule -> [JSchemaExp]
rule2ExpType (SFL4.TypeDecl{name=[MTT n], has=fields, super=Nothing}) = [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (foldMap ruleFieldToField fields)]
rule2ExpType (SFL4.TypeDecl{name=n, has=[], super=Just (InlineEnum TOne enums)}) =
    [ExpTypeEnum (typeDeclNameToTypeName n) (getEnums enums)]
rule2ExpType _ = []


-- rule2NonmdJsonExp :: Rule -> [JSchemaExp]
-- rule2NonmdJsonExp (TypeDecl{name=[MTT n], has=fields, super=Nothing}) =
--     let hierarchyName = (unpack n) ++ " Hierarchy"
--     in case findNonHierarchyRule hierarchyName (TypeDecl{name=[MTT n], has=fields, super=Nothing}) of
--         True  -> [ExpTypeRecord (typeDeclNameToTypeName [MTT (T.pack "hi")]) (foldMap ruleFieldToField fields)]
--         False | " Hierarchy" `isSuffixOf` (unpack n) -> foldMap rule2HierarchyBool fields
--                 | otherwise -> [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (foldMap ruleFieldToField fields)]
--         _ ->  [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (foldMap ruleFieldToField fields)]
-- rule2NonmdJsonExp (TypeDecl{name=n, has=[], super=Just (InlineEnum TOne enums)}) =
--     [ExpTypeEnum (typeDeclNameToTypeName (n)) (unpackEnums enums)]
-- rule2NonmdJsonExp _ = []

-- findNonHierarchyRule :: TypeName -> Rule -> Bool
-- findNonHierarchyRule hierarchyName rule =
--    getType rule == hierarchyName

-- getType:: Rule -> TypeName
-- getType(TypeDecl{name=[MTT n]}) =
--     typeDeclNameToTypeName [MTT n]
-- getType _ = ""

-- enumToField :: ConstructorName -> Field
-- enumToField enumName = Field enumName FTBoolean


------------------------------------
-- Output of types to Haskell

-- somewhat brutal methods to convert arbitrary strings to Haskell identifiers
convertToAlphaNum :: Char -> Char
convertToAlphaNum c =
    if isAlphaNum c
    then c
    else '_'

haskellise :: String -> String
haskellise = map convertToAlphaNum
    -- PCRE.gsub [PCRE.re|[^a-zA-Z\d\s:]|] ("_" :: T.Text)

haskelliseTxt :: T.Text -> T.Text
haskelliseTxt = T.pack . haskellise . T.unpack

capitaliseTxt :: T.Text -> T.Text
capitaliseTxt = T.pack . capitalise . T.unpack

derivingClause :: T.Text
derivingClause = "deriving (Eq, Ord, Show, Read, Generic)"

hConstructorName :: ConstructorName -> ConstructorName
hConstructorName = capitaliseTxt . haskelliseTxt
hFieldName :: FieldName -> FieldName
hFieldName = haskelliseTxt
hTypeName :: TypeName -> TypeName
hTypeName = capitaliseTxt . haskelliseTxt
hTypeNameAsConstructorName :: TypeName -> ConstructorName
hTypeNameAsConstructorName = hTypeName
class ShowTypesHaskell x where
    showTypesHaskell :: x -> Doc ann


instance ShowTypesHaskell FieldType where
    showTypesHaskell FTBoolean = "Bool"
    showTypesHaskell FTNumber = "Double"
    showTypesHaskell FTInteger = "Int"
    showTypesHaskell FTString = "String"
    showTypesHaskell FTDate = "String"
    showTypesHaskell (FTRef n) = pretty $ hTypeName n
    showTypesHaskell (FTList t) = brackets $ showTypesHaskell t
    showTypesHaskell (FTEnum t) = brackets $ (pretty $ getEnums t)


instance ShowTypesHaskell Field where
    showTypesHaskell (Field fn ft) =
        -- trace ("Field: " ++ (show f) ) $
        pretty (hFieldName fn) <> " :: " <> showTypesHaskell ft

showInstanceDecl :: TypeName -> Doc ann
showInstanceDecl tn =
    vsep [
          pretty ("instance FromJSON " :: T.Text) <> pretty (hTypeName tn)
        , pretty ("instance ToJSON " :: T.Text) <> pretty (hTypeName tn)
    ]

instance ShowTypesHaskell JSchemaExp where
    showTypesHaskell :: JSchemaExp -> Doc ann
    showTypesHaskell (ExpTypeRecord tn fds) =
        -- trace ("Record: " ++ (show tn) ++ (show (hTypeName tn))) $
        vsep
        [ "data " <> pretty (hTypeName tn) <> " = " <> pretty (hTypeNameAsConstructorName tn) <>
            nest 4
            (vsep
                [braces (vsep (punctuate comma (map showTypesHaskell fds))),
                 pretty derivingClause])
        , showInstanceDecl tn
        ]

    showTypesHaskell (ExpTypeEnum tn enums) =
        -- trace ("Enum: " ++ (show tn) ++ (show (hTypeName tn))) $
        vsep
        [ "data " <> pretty (hTypeName tn) <>
            nest 4
            (" = " <>
            vsep
                (punctuate " | " (map (pretty . hConstructorName) enums) ++
                [pretty derivingClause])
            )
        , showInstanceDecl tn
        ]

rulesToHaskellTp :: [SFL4.Rule] -> String
rulesToHaskellTp rs =
    let ets = foldMap rule2ExpType rs in
        (case ets of
            [] -> ""
            rt : _rts ->
                let entry = ExpTypeRecord entrypointName [Field entrypointName (FTRef (typeName rt))]
                    entries = entry:ets
                in trace ("Entries: " ++ show entries) $
                   show (vsep (map showTypesHaskell entries ++
                        translationTable (genTranslationTable entries))))

translationTable :: forall ann. [(T.Text, T.Text)] -> [Doc ann]
translationTable tab =
    [vsep [
            "translations :: [(String, String)]"
          , "translations = " <>
            nest 4
                (brackets (vsep (punctuate comma (map (\(p1, p2) ->  parens (dquotes (pretty p1) <> ", " <> dquotes (pretty p2))) tab))))
        ]]

genTranslationTable :: [JSchemaExp] -> [(T.Text, T.Text)]
genTranslationTable tab = M.toList (M.unions (map tableOfSchema tab))

tableOfSchema :: JSchemaExp -> M.HashMap T.Text T.Text
tableOfSchema (ExpTypeRecord tn fds) =
    M.insert tn (hTypeName tn) (M.unions (map tableOfField fds))
tableOfSchema (ExpTypeEnum tn enums) =
    M.insert tn (hTypeName tn) (M.unions (map tableOfEnum enums))

tableOfField :: Field -> M.HashMap FieldName FieldName
tableOfField (Field fn ft) = M.insert fn (hFieldName fn) (tableOfType ft)

tableOfEnum :: ConstructorName -> M.HashMap ConstructorName ConstructorName
tableOfEnum e = M.singleton e $ hConstructorName e

tableOfType :: FieldType -> M.HashMap TypeName TypeName
tableOfType (FTRef n) = M.singleton n $ hTypeName n
tableOfType (FTList t) = tableOfType t
tableOfType _ = M.empty

------------------------------------
-- Output of types to Prolog

class ShowTypesProlog x where
    showTypesProlog :: x -> Doc ann

instance ShowTypesProlog FieldType where
    showTypesProlog FTBoolean = "boolean"
    showTypesProlog FTNumber = "number"
    showTypesProlog FTInteger = "integer"
    showTypesProlog FTString = "string"
    showTypesProlog FTDate = "string"
    showTypesProlog (FTRef n) = "ref" <> parens (pretty n)
    showTypesProlog (FTList t) = "list" <> parens (showTypesProlog t)
    showTypesProlog (FTEnum t) = "list" <> parens (pretty $ getEnums t)

instance ShowTypesProlog Field where
    showTypesProlog (Field fn ft) =
        parens (pretty fn <> ", " <> showTypesProlog ft)

showEnumProlog :: TypeName -> ConstructorName -> Doc ann
showEnumProlog tn en =
    "typedecl" <> parens (pretty tn <> ", " <> pretty en <> ", " <> pretty en) <> "."
instance ShowTypesProlog JSchemaExp where
    showTypesProlog (ExpTypeRecord tn fds) =
        "typedecl" <>
        nest 4
        (parens (
            pretty tn <> ", " <> pretty tn <> ", " <>
            brackets (vsep (punctuate comma (map showTypesProlog fds)))
            )
        ) <>
        "."

    showTypesProlog (ExpTypeEnum tn enums) =
        vsep (map (showEnumProlog tn) enums)


-- the root data type of a Json Schema is always embedded in an entrypoint,
-- here called "toplevel"
entrypointName :: T.Text
entrypointName = "toplevel"

rulesToPrologTp :: [SFL4.Rule] -> String
rulesToPrologTp rs =
    let ets = foldMap rule2ExpType rs in
        (case ets of
            [] -> ""
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
defsLocationName :: T.Text
defsLocationName = "$defs"

defsLocation :: T.Text -> Doc ann
defsLocation n =
  [di|\#/#{defsLocationName}/#{n'}|]
  where
    -- Replace multiple whitespaces with a single underscore.
    n' = PCRE.gsub [PCRE.re|\s+|] ("_" :: T.Text) n

-- defsLocation n = pretty $ "#/" ++ defsLocationName ++ "/" ++ (map toLower $ intercalate "_" $ words n)

jsonType :: T.Text -> Doc ann
jsonType t =
    dquotes "type" <> ": " <> dquotes (pretty t)

-- showRequireds :: [Field] -> Doc ann
-- showRequireds fds =
--     dquotes "required" <> ": " <>
--     brackets (hsep (punctuate comma (map (dquotes . pretty . (.fieldName)) fds)))

showRef :: TypeName -> Doc ann
showRef n = [__di| "$ref": "#{defsLocation n}"|]


-- Due to limitations of the JSON Form Web UI builder,
-- single references are not represented as single objects,
-- but arrays of length 1.
-- List types can only have nesting level 1 (a limitation inherited from Natural4)
instance ShowTypesJson FieldType where
    showTypesJson FTInteger =
        jsonType "integer"
    showTypesJson FTBoolean =
        jsonType "boolean"
    showTypesJson FTNumber =
        jsonType "number"
    showTypesJson FTString =
        jsonType "string"
    showTypesJson FTDate =
        jsonType "string" <> "," <>
        dquotes "format" <> ": " <> dquotes "date"
    showTypesJson (FTRef n) =
        showRef n
    showTypesJson (FTList (FTRef n)) =
        jsonType "array" <> "," <>
        dquotes "items" <> ": " <>
        braces (showRef n)
    showTypesJson (FTEnum n) =
      brackets (hsep (punctuate comma (map (dquotes . pretty) (map processTopLvlNameTextForJsonSchema n))))
    showTypesJson _ =
        jsonType "string"

instance ShowTypesJson Field where
    showTypesJson :: Field -> Doc ann
    showTypesJson (Field fn ft) =
        dquotes (pretty fn) <> ": " <> braces (showTypesJson ft)


instance ShowTypesJson JSchemaExp where
    showTypesJson :: JSchemaExp -> Doc ann
    showTypesJson (ExpTypeEnum tn enums) =
        dquotes (pretty tn) <> ": " <>
        nest 4
        (braces (
            jsonType "string" <> "," <>
            dquotes "enum" <> ": " <>
            nest 4 (brackets (hsep (punctuate comma (map (dquotes . pretty) enums))))
        ))
    showTypesJson (ExpTypeRecord tn fds) =
        pprintJsonObj tn fds ""
--         --     where requiredFds = "," <> nest 4 (showRequireds fds)

pprintJsonObj :: (Pretty a, ShowTypesJson b) => a -> [b] -> Doc ann -> Doc ann
pprintJsonObj key values final =
    dquotes (pretty key) <> ": " <>
        nest 4
        (braces (
            jsonType "object" <> "," <>
            dquotes "properties" <> ": " <>
                nest 4
                (braces (vsep (punctuate comma (map showTypesJson values))))
                <> final
        ))

jsonPreamble :: TypeName -> Doc ann
jsonPreamble tn =
    [__di|
        "$schema":"http://json-schema.org/draft-07/schema\#",
        "type": "object",
        "properties":{"#{pretty entrypointName}":{#{showTypesJson (FTRef tn)}}}
    |]

rulesToJsonSchema :: [SFL4.Rule] -> String
rulesToJsonSchema rs =
    let
        -- TODO: Would be better to avoid boolean blindness here; improve when time permits
        -- Partitioning in advance because we want to group the means HLikes into one object
        ets = foldMap rule2ExpType rs
        subJsonObjs = map showTypesJson ets
    in (case ets of
            [] -> show $ braces emptyDoc
            (rt : _rts) ->
                -- trace ("ets: " ++ show ets) $
                show [__di|
                {#{jsonPreamble (typeName rt)},
                "#{pretty defsLocationName}":
                {#{vsep (punctuate comma subJsonObjs)}}
                }
                |]
        )

rulesToUISchema :: [SFL4.Rule] -> String
rulesToUISchema rs =
    let ets = foldMap rule2ExpType rs in
        (case ets of
            [] -> show $ braces emptyDoc
            rts ->
                -- trace ("ets: " ++ show ets) $
                show
                (braces
                    (vsep (punctuate comma
                    (
                        jsonPreamble (last rts).typeName :
                        [dquotes (pretty defsLocationName) <> ": " <>
                        braces (
                            nest 4
                            (vsep (punctuate comma (map showTypesJson ets)))
                        )
                        ]
                    )
                    ) )
                )
        )

