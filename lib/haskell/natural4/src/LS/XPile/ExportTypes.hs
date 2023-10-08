{-# OPTIONS_GHC -W #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, TemplateHaskell #-}
{-# LANGUAGE  DerivingVia, DeriveAnyClass #-}


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
  ( Rule (TypeDecl, keyword, clauses, has, name, super),
    extractMTExprs
  )
import LS.Types as SFL4
  (
    -- BoolStructR,
    -- HornClause2,
    MTExpr (..),
    -- ParamText,
    ParamType (TList1, TOne),
    TypeSig (..),
    -- mt2text,
    mtexpr2text,
    RuleName
  )
import Data.Text (unpack)
import Debug.Trace (trace)
import Data.List (isSuffixOf, intercalate)
import Data.Char (toLower, isAlphaNum)
import Data.Generics.Product.Types (HasTypes)
import qualified Text.Regex.PCRE.Heavy as PCRE
-- import Optics.TH
import Data.Set qualified as S

-- for json types -----------------------------------
type ConstructorName = String
type FieldName = String
type TypeName = String

data FieldType =
      FTBoolean
    | FTNumber
    | FTString
    | FTRef TypeName
    | FTList FieldType
    | FTDate
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
processTopLvlNameTextForJsonSchema = T.toLower . T.intercalate (T.pack "_") . T.words

-- stringifyMTEwrapper :: (T.Text -> T.Text) -> MTExpr -> String
-- stringifyMTEwrapper f = T.unpack . f . mtexpr2text

typeDeclNameToTypeName :: RuleName -> TypeName
typeDeclNameToTypeName [ MTT n ] =  T.unpack . processTopLvlNameTextForJsonSchema $ n
typeDeclNameToTypeName _ = "" -- TODO: should be an error case

typeDeclNameToFieldName :: RuleName -> String
typeDeclNameToFieldName = typeDeclNameToTypeName

-- | Collect the enums into a list of strings
getEnums :: forall s. (HasTypes s MTExpr) => s -> [String]
getEnums = map (T.unpack . mtexpr2text) . SFL4.extractMTExprs

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

ruleFieldToField :: SFL4.Rule -> [Field]
ruleFieldToField (SFL4.TypeDecl{name=n, super=sup}) =
    [Field (typeDeclNameToFieldName n) (typeDeclSuperToFieldType sup)]
ruleFieldToField _ = []

-- unpackHierarchy :: Rule -> [Field]
-- unpackHierarchy (TypeDecl{name=n}) =
--     [Field (typeDeclNameToFieldName n) (FTList (FTRef (unpack n)))]
-- unpackHierarchy _ = []

rule2NonmdJsonExp :: SFL4.Rule -> [JSchemaExp]
rule2NonmdJsonExp = \case
    SFL4.TypeDecl{name=[MTT n], has=fields, super=Nothing}
      -> case unpack n of
           n' | "Hierarchy" `isSuffixOf` n' -> concatMap rule2HierarchyBool fields
           _ ->  [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (concatMap ruleFieldToField fields)]
    SFL4.TypeDecl{name=n, has=[], super=Just (InlineEnum TOne enums)}
      -> [ExpTypeEnum (typeDeclNameToTypeName n) (getEnums enums)]
    _ -> []

rule2ExpType :: SFL4.Rule -> [JSchemaExp]
rule2ExpType (SFL4.TypeDecl{name=[MTT n], has=fields, super=Nothing}) = [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (concatMap ruleFieldToField fields)]
rule2ExpType (SFL4.TypeDecl{name=n, has=[], super=Just (InlineEnum TOne enums)}) =
    [ExpTypeEnum (typeDeclNameToTypeName n) (getEnums enums)]
rule2ExpType _ = []

rule2HierarchyBool :: SFL4.Rule -> [JSchemaExp]
rule2HierarchyBool (SFL4.TypeDecl{name=[MTT n], has=fields, super=Nothing}) = [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (concatMap ruleBool fields)]
    where
        ruleBool ((SFL4.TypeDecl{name=n})) = [Field (typeDeclNameToFieldName n) FTBoolean]
        ruleBool _                    = []

rule2HierarchyBool _ = []


-- rule2NonmdJsonExp :: Rule -> [JSchemaExp]
-- rule2NonmdJsonExp (TypeDecl{name=[MTT n], has=fields, super=Nothing}) =
--     let hierarchyName = (unpack n) ++ " Hierarchy"
--     in case findNonHierarchyRule hierarchyName (TypeDecl{name=[MTT n], has=fields, super=Nothing}) of
--         True  -> [ExpTypeRecord (typeDeclNameToTypeName [MTT (T.pack "hi")]) (concatMap ruleFieldToField fields)]
--         False | " Hierarchy" `isSuffixOf` (unpack n) -> concatMap rule2HierarchyBool fields
--                 | otherwise -> [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (concatMap ruleFieldToField fields)]
--         _ ->  [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (concatMap ruleFieldToField fields)]
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

hConstructorName :: ConstructorName -> ConstructorName
hConstructorName = capitalise . haskellise
hFieldName :: FieldName -> FieldName
hFieldName = haskellise
hTypeName :: TypeName -> TypeName
hTypeName = capitalise . haskellise
hTypeNameAsConstructorName :: TypeName -> ConstructorName
hTypeNameAsConstructorName = hTypeName
class ShowTypesHaskell x where
    showTypesHaskell :: x -> Doc ann


instance ShowTypesHaskell FieldType where
    showTypesHaskell FTBoolean = pretty "Bool"
    showTypesHaskell FTNumber = pretty "Int"
    showTypesHaskell FTString = pretty "String"
    showTypesHaskell FTDate = pretty "String"
    showTypesHaskell (FTRef n) = pretty (hTypeName n)
    showTypesHaskell (FTList t) = brackets (showTypesHaskell t)

instance ShowTypesHaskell Field where
    showTypesHaskell f@(Field fn ft) = 
        trace ("Field: " ++ (show f) ) $  
        pretty (hFieldName fn) <> pretty " :: " <> showTypesHaskell ft

instance ShowTypesHaskell JSchemaExp where
    showTypesHaskell :: JSchemaExp -> Doc ann
    showTypesHaskell (ExpTypeRecord tn fds) =
        trace ("Record: " ++ (show tn) ++ (show (hTypeName tn))) $
        pretty "data " <> pretty (hTypeName tn) <> pretty " = " <> pretty (hTypeNameAsConstructorName tn) <>
            nest 4 (braces (vsep (punctuate comma (map showTypesHaskell fds))))

    showTypesHaskell (ExpTypeEnum tn enums) =
        trace ("Enum: " ++ (show tn) ++ (show (hTypeName tn))) $
        pretty "data " <> pretty (hTypeName tn) <>
        nest 4
            (pretty " = " <>
            vsep (punctuate (pretty " | ") (map (pretty . hConstructorName) enums)))

rulesToHaskellTp :: [SFL4.Rule] -> String
rulesToHaskellTp rs =
    let ets = concatMap rule2ExpType rs in
        (case ets of
            [] -> show emptyDoc
            rt : _rts ->
                let entry = ExpTypeRecord entrypointName [Field entrypointName (FTRef (typeName rt))]
                    entries = entry:ets
                in trace ("Entries: " ++ (show entries)) $
                   show (vsep (map showTypesHaskell entries ++
                        translationTable (genTranslationTable entries))))

translationTable :: forall ann. [(String, String)] -> [Doc ann]
translationTable tab =
    [vsep [
            pretty "translations :: [(String, String)]"
          , pretty "translations = " <>
            nest 4
                (brackets (vsep (punctuate comma (map (\(p1, p2) ->  parens (dquotes (pretty p1) <> pretty ", " <> dquotes (pretty p2))) tab))))
        ]]

genTranslationTable :: [JSchemaExp] -> [(String, String)]
genTranslationTable tab = S.toList (S.unions (map tableOfSchema tab))

tableOfSchema :: JSchemaExp -> S.Set (String, String)
tableOfSchema (ExpTypeRecord tn fds) =
    S.insert (tn, hTypeName tn) (S.unions (map tableOfField fds))
tableOfSchema (ExpTypeEnum tn enums) =
    S.insert (tn, hTypeName tn) (S.unions (map tableOfEnum enums))
-- tableOfSchema (MkMetadata{}) = S.empty

tableOfField :: Field -> S.Set (FieldName, FieldName)
tableOfField (Field fn ft) = S.insert (fn, hFieldName fn) (tableOfType ft)

tableOfEnum :: ConstructorName -> S.Set (ConstructorName, ConstructorName)
tableOfEnum e = S.singleton (e, hConstructorName e)

tableOfType :: FieldType -> S.Set (TypeName, TypeName)
tableOfType (FTRef n) = S.singleton (n, hTypeName n)
tableOfType (FTList t) = tableOfType t
tableOfType _ = S.empty

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

defsLocation :: String -> Doc ann
defsLocation n =
  [di|\#/#{defsLocationName}/#{n'}|]
  where
    -- Replace multiple whitespaces with a single underscore.
    n' = PCRE.gsub [PCRE.re|\s+|] "_" n

-- defsLocation n = pretty $ "#/" ++ defsLocationName ++ "/" ++ (map toLower $ intercalate "_" $ words n)

jsonType :: Pretty a => a -> Doc ann
jsonType t =
    dquotes (pretty "type") <> pretty ": " <> dquotes (pretty t)

showRequireds :: [Field] -> Doc ann
showRequireds fds =
    dquotes (pretty "required") <> pretty ": " <>
    brackets (hsep (punctuate comma (map (dquotes . pretty . (.fieldName)) fds)))

showRef :: TypeName -> Doc ann
showRef n = [__di| "$ref": "#{defsLocation n}"|]

-- bracketArgs :: Pretty a => [a] -> Doc ann
-- bracketArgs = brackets . hsep . punctuate comma . map (dquotes . pretty)


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


instance ShowTypesJson JSchemaExp where
    showTypesJson :: JSchemaExp -> Doc ann
    showTypesJson (ExpTypeEnum tn enums) =
        dquotes (pretty tn) <> pretty ": " <>
        nest 4
        (braces (
            jsonType "string" <> pretty "," <>
            dquotes (pretty "enum") <> pretty ": " <>
            nest 4 (brackets (hsep (punctuate comma (map (dquotes . pretty) enums))))
        ))
    showTypesJson (ExpTypeRecord tn fds) =
        pprintJsonObj tn fds requiredFds
            where requiredFds = pretty "," <> nest 4 (showRequireds fds)
    
pprintJsonObj :: (Pretty a, ShowTypesJson b) => a -> [b] -> Doc ann -> Doc ann
pprintJsonObj key values final =
    dquotes (pretty key) <> pretty ": " <>
        nest 4
        (braces (
            jsonType "object" <> pretty "," <>
            dquotes (pretty "properties") <>  pretty ": " <>
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
        ets = concatMap rule2NonmdJsonExp rs
        subJsonObjs = map showTypesJson ets
    in (case ets of
            [] -> show (braces emptyDoc)
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
    let ets = concatMap rule2NonmdJsonExp rs in
        (case ets of
            [] -> show (braces emptyDoc)
            rts ->
                -- trace ("ets: " ++ show ets) $
                show
                (braces
                    (vsep (punctuate comma
                    (
                        jsonPreamble (last rts).typeName :
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

