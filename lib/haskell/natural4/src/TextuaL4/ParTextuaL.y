-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.5).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module TextuaL4.ParTextuaL
  ( happyError
  , myLexer
  , pRule
  ) where

import Prelude

import qualified TextuaL4.AbsTextuaL
import TextuaL4.LexTextuaL

}

%name pRule Rule
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '('       { PT _ (TS _ 1)    }
  ')'       { PT _ (TS _ 2)    }
  ','       { PT _ (TS _ 3)    }
  '<'       { PT _ (TS _ 4)    }
  '<='      { PT _ (TS _ 5)    }
  '>'       { PT _ (TS _ 6)    }
  '>='      { PT _ (TS _ 7)    }
  'ABOUT'   { PT _ (TS _ 8)    }
  'AFTER'   { PT _ (TS _ 9)    }
  'ALL'     { PT _ (TS _ 10)   }
  'AND'     { PT _ (TS _ 11)   }
  'ANY'     { PT _ (TS _ 12)   }
  'BEFORE'  { PT _ (TS _ 13)   }
  'BY'      { PT _ (TS _ 14)   }
  'DIVIDE'  { PT _ (TS _ 15)   }
  'EQUALS'  { PT _ (TS _ 16)   }
  'EVERY'   { PT _ (TS _ 17)   }
  'False'   { PT _ (TS _ 18)   }
  'HAS'     { PT _ (TS _ 19)   }
  'IN'      { PT _ (TS _ 20)   }
  'IS'      { PT _ (TS _ 21)   }
  'MAP'     { PT _ (TS _ 22)   }
  'MAX'     { PT _ (TS _ 23)   }
  'MAY'     { PT _ (TS _ 24)   }
  'MEANS'   { PT _ (TS _ 25)   }
  'MIN'     { PT _ (TS _ 26)   }
  'MINUS'   { PT _ (TS _ 27)   }
  'MODULO'  { PT _ (TS _ 28)   }
  'MUST'    { PT _ (TS _ 29)   }
  'NOT'     { PT _ (TS _ 30)   }
  'ON'      { PT _ (TS _ 31)   }
  'OR'      { PT _ (TS _ 32)   }
  'PRODUCT' { PT _ (TS _ 33)   }
  'SHANT'   { PT _ (TS _ 34)   }
  'SUBJECT' { PT _ (TS _ 35)   }
  'SUM'     { PT _ (TS _ 36)   }
  'TO'      { PT _ (TS _ 37)   }
  'True'    { PT _ (TS _ 38)   }
  'WHO'     { PT _ (TS _ 39)   }
  L_doubl   { PT _ (TD $$)     }
  L_integ   { PT _ (TI $$)     }
  L_Text    { PT _ (T_Text $$) }

%%

Double  :: { Double }
Double   : L_doubl  { (read $1) :: Double }

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

Text :: { TextuaL4.AbsTextuaL.Text }
Text  : L_Text { TextuaL4.AbsTextuaL.Text $1 }

Rule :: { TextuaL4.AbsTextuaL.Rule }
Rule
  : 'EVERY' BoolStruct Deontic BoolStruct { TextuaL4.AbsTextuaL.RegSimple $2 $3 $4 }
  | 'EVERY' BoolStruct Who Deontic BoolStruct { TextuaL4.AbsTextuaL.RegWho $2 $3 $4 $5 }
  | 'EVERY' BoolStruct Who InlineHornlike Deontic BoolStruct { TextuaL4.AbsTextuaL.RegWhoInline $2 $3 $4 $5 $6 }
  | Text 'MEANS' BoolStruct { TextuaL4.AbsTextuaL.Hornlike $1 $3 }

Deontic :: { TextuaL4.AbsTextuaL.Deontic }
Deontic
  : 'MUST' { TextuaL4.AbsTextuaL.Deontic_MUST }
  | 'MAY' { TextuaL4.AbsTextuaL.Deontic_MAY }
  | 'SHANT' { TextuaL4.AbsTextuaL.Deontic_SHANT }

Who :: { TextuaL4.AbsTextuaL.Who }
Who : 'WHO' BoolStruct { TextuaL4.AbsTextuaL.WhoSimple $2 }

InlineHornlike :: { TextuaL4.AbsTextuaL.InlineHornlike }
InlineHornlike
  : 'MEANS' BoolStruct { TextuaL4.AbsTextuaL.MeansInline $2 }

RelationalPredicate :: { TextuaL4.AbsTextuaL.RelationalPredicate }
RelationalPredicate
  : ListMTExpr { TextuaL4.AbsTextuaL.RPMT $1 }
  | ListMTExpr RPRel BoolStruct { TextuaL4.AbsTextuaL.RPBoolStructR $1 $2 $3 }

MTExpr :: { TextuaL4.AbsTextuaL.MTExpr }
MTExpr
  : Text { TextuaL4.AbsTextuaL.MTT $1 }
  | Integer { TextuaL4.AbsTextuaL.MTI $1 }
  | Double { TextuaL4.AbsTextuaL.MTF $1 }
  | Bool { TextuaL4.AbsTextuaL.MTB $1 }

Bool :: { TextuaL4.AbsTextuaL.Bool }
Bool
  : 'True' { TextuaL4.AbsTextuaL.Bool_True }
  | 'False' { TextuaL4.AbsTextuaL.Bool_False }

ListMTExpr :: { [TextuaL4.AbsTextuaL.MTExpr] }
ListMTExpr : MTExpr { (:[]) $1 } | MTExpr ListMTExpr { (:) $1 $2 }

BoolStruct :: { TextuaL4.AbsTextuaL.BoolStruct }
BoolStruct
  : 'ANY' '(' ListBoolStruct ')' { TextuaL4.AbsTextuaL.Any $3 }
  | Text 'ANY' '(' ListBoolStruct ')' Text { TextuaL4.AbsTextuaL.AnyPrePost $1 $4 $6 }
  | Text 'ANY' '(' ListBoolStruct ')' { TextuaL4.AbsTextuaL.AnyPre $1 $4 }
  | 'ALL' '(' ListBoolStruct ')' { TextuaL4.AbsTextuaL.All $3 }
  | Text 'ALL' '(' ListBoolStruct ')' { TextuaL4.AbsTextuaL.AllPre $1 $4 }
  | Text 'ALL' '(' ListBoolStruct ')' Text { TextuaL4.AbsTextuaL.AllPrePost $1 $4 $6 }
  | 'NOT' BoolStruct { TextuaL4.AbsTextuaL.Not $2 }
  | RelationalPredicate { TextuaL4.AbsTextuaL.Leaf $1 }

ListBoolStruct :: { [TextuaL4.AbsTextuaL.BoolStruct] }
ListBoolStruct
  : BoolStruct { (:[]) $1 }
  | BoolStruct ',' ListBoolStruct { (:) $1 $3 }

RPRel :: { TextuaL4.AbsTextuaL.RPRel }
RPRel
  : 'IS' { TextuaL4.AbsTextuaL.RPis }
  | 'HAS' { TextuaL4.AbsTextuaL.RPhas }
  | 'EQUALS' { TextuaL4.AbsTextuaL.RPeq }
  | '<' { TextuaL4.AbsTextuaL.RPlt }
  | '<=' { TextuaL4.AbsTextuaL.RPlte }
  | '>' { TextuaL4.AbsTextuaL.RPgt }
  | '>=' { TextuaL4.AbsTextuaL.RPgte }
  | 'IN' { TextuaL4.AbsTextuaL.RPelem }
  | 'NOT' 'IN' { TextuaL4.AbsTextuaL.RPnotElem }
  | 'NOT' { TextuaL4.AbsTextuaL.RPnot }
  | 'AND' { TextuaL4.AbsTextuaL.RPand }
  | 'OR' { TextuaL4.AbsTextuaL.RPor }
  | 'SUM' { TextuaL4.AbsTextuaL.RPsum }
  | 'PRODUCT' { TextuaL4.AbsTextuaL.RPproduct }
  | 'MINUS' { TextuaL4.AbsTextuaL.RPminus }
  | 'DIVIDE' { TextuaL4.AbsTextuaL.RPdivide }
  | 'MODULO' { TextuaL4.AbsTextuaL.RPmodulo }
  | 'SUBJECT' 'TO' { TextuaL4.AbsTextuaL.RPsubjectTo }
  | 'MIN' { TextuaL4.AbsTextuaL.RPmin }
  | 'MAX' { TextuaL4.AbsTextuaL.RPmax }
  | 'MAP' { TextuaL4.AbsTextuaL.RPmap }
  | TComparison { TextuaL4.AbsTextuaL.RPTC $1 }

TComparison :: { TextuaL4.AbsTextuaL.TComparison }
TComparison
  : 'BEFORE' { TextuaL4.AbsTextuaL.TBefore }
  | 'AFTER' { TextuaL4.AbsTextuaL.TAfter }
  | 'BY' { TextuaL4.AbsTextuaL.TBy }
  | 'ON' { TextuaL4.AbsTextuaL.TOn }
  | 'ABOUT' { TextuaL4.AbsTextuaL.TVague }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}

