-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language TextuaL4.

module TextuaL4.AbsTextuaL where

import Prelude (Double, Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data IsA
    = IsAType Text Text
    | IsAnType Text Text
    | IsAOptional Text Text
    | IsAEnum Text [Text]
    | IsAList Text Text
    | IsASet Text Text
    | IsANoType Text
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Fields = Has [IsA] | EmptyFields
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Rule
    = Rlabel Text Rule
    | TypeDecl IsA Fields
    | Given [IsA] Rule
    | Where Rule [HornClause]
    | RegSimple BoolStruct Deontic BoolStruct
    | RegWho BoolStruct Who Deontic BoolStruct
    | RegWhoInline BoolStruct Who InlineHornlike Deontic BoolStruct
    | HornlikeMeans Text BoolStruct
    | HornlikeDecide [HornClause]
    | HlikeGiveth [IsA] [HornClause]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data HornClause
    = HeadBody RelationalPredicate BoolStruct
    | HeadOtherwise RelationalPredicate
    | HeadOnly RelationalPredicate
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Deontic = Deontic_MUST | Deontic_MAY | Deontic_SHANT
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Who = WhoSimple BoolStruct
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data InlineHornlike = MeansInline BoolStruct
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RelationalPredicate
    = RPMT [MTExpr]
    | RPBoolStructR [MTExpr] RPRel BoolStruct
    | RPnary RPRel [RelationalPredicate]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data MTExpr = MTT Text | MTI Integer | MTF Double | MTB Bool
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Bool = Bool_True | Bool_False
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Text
    = TextString String
    | TextToken Token
    | TextBacktickToken BacktickToken
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data BoolStruct
    = Any [BoolStruct]
    | AnyPrePost Text [BoolStruct] Text
    | AnyPre Text [BoolStruct]
    | All [BoolStruct]
    | AllPre Text [BoolStruct]
    | AllPrePost Text [BoolStruct] Text
    | Not BoolStruct
    | Unless BoolStruct BoolStruct
    | Leaf RelationalPredicate
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RPRel
    = RPis
    | RPhas
    | RPeq
    | RPlt
    | RPlte
    | RPgt
    | RPgte
    | RPelem
    | RPnotElem
    | RPnot
    | RPand
    | RPor
    | RPsum
    | RPproduct
    | RPminus
    | RPdivide
    | RPmodulo
    | RPsubjectTo
    | RPmin
    | RPmax
    | RPmap
    | RPTC TComparison
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TComparison = TBefore | TAfter | TBy | TOn | TVague
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Token = Token String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype BacktickToken = BacktickToken String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

