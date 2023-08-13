{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-
Provide export from Natural4 type declarations of the form
DECLARE T
HAS field1 is T1
    ...
    fieldn is Tn

These declarations are meant to be exported to JSON and to Prolog.
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
        )


rulesToPrologTp :: [SFL4.Rule] -> String
rulesToPrologTp rs = show (vsep (map showTypesProlog (concatMap rule2ExpType rs)))
