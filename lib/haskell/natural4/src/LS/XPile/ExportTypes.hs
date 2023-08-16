{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-
Provide export from Natural4 type declarations of the form
DECLARE T
HAS field1 is T1
    ...
    fieldn is Tn

These declarations are meant to be exported to JSON and to Prolog.
The approach is strictly ad-hoc and not systematic and agnostic of
other translations (in particular to JSON) that may have been developed.
-}

module LS.XPile.ExportTypes where

import Data.Map qualified as Map
import Data.Text qualified as Text
import Prettyprinter
import Prettyprinter.Render.Text (putDoc)
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
    TypeSig (..),
    mt2text,
    mtexpr2text,
    pt2text,
    rel2txt,
    untypePT, RuleName,
  )
import Data.Text (unpack)

type TypeName = String

data FieldType =
      FTBoolean
    | FTNumber
    | FTString
    | FTRef TypeName
    | FTList FieldType
    deriving (Eq, Ord, Show, Read)

data Field = Field
    { fieldName :: String
    , fieldType :: FieldType
    }
    deriving (Eq, Ord, Show, Read)

data ExpType = ExpType
    { typeName :: TypeName
    , fields :: [Field]
    }
    deriving (Eq, Ord, Show, Read)

typeDeclNameToTypeName :: RuleName -> TypeName
typeDeclNameToTypeName [MTT n] = unpack n
typeDeclNameToTypeName _ = "" -- TODO: should be an error case

typeDeclNameToFieldName :: RuleName -> String
typeDeclNameToFieldName = typeDeclNameToTypeName

typeDeclSuperToFieldType :: Maybe TypeSig -> FieldType
typeDeclSuperToFieldType (Just  (SimpleType TOne tn)) =
    case unpack tn of
        "Boolean" -> FTBoolean
        "Number" -> FTNumber
        "String" -> FTString
        n -> FTRef n
-- TODO: There somehow cannot be lists of lists (problem both of the parser and of data structures).
typeDeclSuperToFieldType (Just  (SimpleType TList1 tn)) = FTList (FTRef (unpack tn))
typeDeclSuperToFieldType _ = FTString

ruleFieldToField :: Rule -> [Field]
ruleFieldToField (TypeDecl{name=n, super=sup}) =
    [Field (typeDeclNameToFieldName n) (typeDeclSuperToFieldType sup)]
ruleFieldToField _ = []

rule2ExpType :: Rule -> [ExpType]
rule2ExpType (TypeDecl{name=n, has=fields}) =
    [ExpType (typeDeclNameToTypeName n) (concatMap ruleFieldToField fields)]
rule2ExpType _ = []

------------------------------------
-- Output of types to Prolog 

class ShowTypesProlog x where
    showTypesProlog :: x -> Doc ann

instance ShowTypesProlog FieldType where
    showTypesProlog FTBoolean = pretty "boolean"
    showTypesProlog FTNumber = pretty "number"
    showTypesProlog FTString = pretty "string"
    showTypesProlog (FTRef n) = pretty n
    showTypesProlog (FTList t) = pretty "list" <> parens (showTypesProlog t)


instance ShowTypesProlog Field where
    showTypesProlog (Field fn ft) =
        parens (pretty fn <> pretty ", " <> showTypesProlog ft)

instance ShowTypesProlog ExpType where
    showTypesProlog (ExpType tn fds) =
        pretty "typedecl" <>
        nest 4
        (parens (
            pretty tn <> pretty ", " <> pretty tn <> pretty ", " <>
            brackets (vsep (punctuate comma (map showTypesProlog fds)))
            )
        ) <>
        pretty "."

rulesToPrologTp :: [SFL4.Rule] -> String
rulesToPrologTp rs = show (vsep (map showTypesProlog (concatMap rule2ExpType rs)))


------------------------------------
-- Output of types to Json Schema 
-- also see https://json-schema.org/understanding-json-schema/

class ShowTypesJson x where
    showTypesJson :: x -> Doc ann

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

instance ShowTypesJson FieldType where
    showTypesJson FTBoolean =
        jsonType "boolean"
    showTypesJson FTNumber =
        jsonType "number"
    showTypesJson FTString =
        jsonType "string"
    showTypesJson (FTRef n) = 
        jsonType "array" <> pretty "," <>
        dquotes (pretty "minItems") <> pretty ": 1," <>
        dquotes (pretty "maxItems") <> pretty ": 1," <>
        dquotes (pretty "items") <> pretty ": " <>
        braces (showRef n)
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
    showTypesJson (ExpType tn fds) =
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
    braces (dquotes (pretty "User") <> pretty ": " <>
        braces (showTypesJson (FTList (FTRef tn))))
    ]

rulesToJsonSchema :: [SFL4.Rule] -> String
rulesToJsonSchema rs =
    let ets = concatMap rule2ExpType rs in
        (case ets of
            [] -> show (braces emptyDoc)
            rt : rts ->
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

{-

-}