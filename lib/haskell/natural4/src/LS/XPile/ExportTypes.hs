{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

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

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (unpack)
import Data.Text qualified as Text
import Debug.Trace (trace)
import LS.Rule as SFL4
  ( Rule (Hornlike, TypeDecl, clauses, enums, has, name, super),
  )
import LS.Types as SFL4
  ( BoolStructP,
    BoolStructR,
    EntityType,
    HornClause (hBody, hHead),
    HornClause2,
    MTExpr (..),
    ParamText,
    ParamType (TList0, TList1, TOne, TOptional),
    RPRel (RPeq),
    RelationalPredicate (..),
    RuleName,
    TypeSig (..),
    TypedMulti,
    mt2text,
    mtexpr2text,
    pt2text,
    rel2txt,
    untypePT,
  )
import Prettyprinter
import Prettyprinter.Render.Text (putDoc)

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
    deriving (Eq, Ord, Show, Read)

typeDeclNameToTypeName :: RuleName -> TypeName
typeDeclNameToTypeName [MTT n] = unpack n
typeDeclNameToTypeName _ = "" -- TODO: should be an error case

typeDeclNameToFieldName :: RuleName -> String
typeDeclNameToFieldName = typeDeclNameToTypeName

unpackEnums :: Foldable t => t (NonEmpty MTExpr, b) -> [String]
unpackEnums = map unpack . mapMaybe paramTextToEnumTypeName . toList
  where
    paramTextToEnumTypeName :: (NonEmpty MTExpr, b) -> Maybe Text.Text
    paramTextToEnumTypeName (MTT tn :| _, _) = Just tn
    paramTextToEnumTypeName _ = Nothing

-- unpackEnums :: Nonempty (Nonempty MTExpr, b) -> [String]
-- unpackEnums ((MTT tn :| _, _) :| xs) = unpack tn : map unpackEnum xs

-- unpackEnum :: (NonEmpty MTExpr, b) -> String
-- unpackEnum (MTT tn :| _, _) = unpack tn
-- unpackEnum _ = mempty

typeDeclSuperToFieldType :: Maybe TypeSig -> FieldType
typeDeclSuperToFieldType (Just (SimpleType TOne tn)) =
    case unpack tn of
        "Boolean" -> FTBoolean
        "Number" -> FTNumber
        "String" -> FTString
        "Date" -> FTDate
        n -> FTRef n
-- TODO: There somehow cannot be lists of lists (problem both of the parser and of data structures).
typeDeclSuperToFieldType (Just (SimpleType TList1 tn)) = FTList (FTRef (unpack tn))
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

rule2JsonExp :: Rule -> [ExpType]
rule2JsonExp (TypeDecl{name=[MTT n], has=fields, super=Nothing}) =
    case unpack n of
        "damageType Hierarchy" -> concatMap rule2ExpType fields
        _ ->  [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (concatMap ruleFieldToField fields)]
rule2JsonExp (TypeDecl{name=n, has=[], super=Just (InlineEnum TOne enums)}) =
    [ExpTypeEnum (typeDeclNameToTypeName n) (unpackEnums enums)]
rule2JsonExp _ = []

rule2ExpType :: Rule -> [ExpType]
rule2ExpType (TypeDecl{name=[MTT n], has=fields, super=Nothing}) = [ExpTypeRecord (typeDeclNameToTypeName [MTT n]) (concatMap ruleFieldToField fields)]
rule2ExpType (TypeDecl{name=n, has=[], super=Just (InlineEnum TOne enums)}) =
    [ExpTypeEnum (typeDeclNameToTypeName n) (unpackEnums enums)]
rule2ExpType _ = []



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
            rt : rts ->
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
defsLocation n = "#/" ++ defsLocationName ++ "/" ++ n

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
        (showRef n)
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
        braces (showTypesJson (FTList (FTRef tn))))
    ]


rulesToJsonSchema :: [SFL4.Rule] -> String
rulesToJsonSchema rs =
    let ets = concatMap rule2JsonExp rs in
        (case ets of
            [] -> show (braces emptyDoc)
            rt : rts ->
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
            rt : rts ->
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