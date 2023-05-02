-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module ParNatural
  ( happyError
  , myLexer
  , pIfPart
  , pBoolStructR
  , pBoolStructR2
  , pBoolStructR1
  , pBSRLeaf2
  , pBSRLeaf1
  , pBSRLeaf
  , pBSRLeaf3
  , pBSRAny
  , pListBSRAny
  , pBSRAll
  , pListBSRAll
  , pRelationalPredicate
  , pRPRel
  , pMultiTerm
  , pMTExpr
  , pMTT
  , pMTN
  , pListMTExpr
  ) where

import Prelude

import qualified AbsNatural
import LexNatural

}

%name pIfPart IfPart
%name pBoolStructR BoolStructR
%name pBoolStructR2 BoolStructR2
%name pBoolStructR1 BoolStructR1
%name pBSRLeaf2 BSRLeaf2
%name pBSRLeaf1 BSRLeaf1
%name pBSRLeaf BSRLeaf
%name pBSRLeaf3 BSRLeaf3
%name pBSRAny BSRAny
%name pListBSRAny ListBSRAny
%name pBSRAll BSRAll
%name pListBSRAll ListBSRAll
%name pRelationalPredicate RelationalPredicate
%name pRPRel RPRel
%name pMultiTerm MultiTerm
%name pMTExpr MTExpr
%name pMTT MTT
%name pMTN MTN
%name pListMTExpr ListMTExpr
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '('        { PT _ (TS _ 1)       }
  ')'        { PT _ (TS _ 2)       }
  'AND'      { PT _ (TS _ 3)       }
  'IF'       { PT _ (TS _ 4)       }
  'IS'       { PT _ (TS _ 5)       }
  'OR'       { PT _ (TS _ 6)       }
  'POST'     { PT _ (TS _ 7)       }
  'PRE'      { PT _ (TS _ 8)       }
  L_PIdent   { PT _ (T_PIdent _)   }
  L_PInteger { PT _ (T_PInteger _) }

%%

PIdent :: { AbsNatural.PIdent }
PIdent  : L_PIdent { AbsNatural.PIdent (mkPosToken $1) }

PInteger :: { AbsNatural.PInteger }
PInteger  : L_PInteger { AbsNatural.PInteger (mkPosToken $1) }

IfPart :: { AbsNatural.IfPart }
IfPart : 'IF' BSRAny { AbsNatural.IfPart $2 }

BoolStructR :: { AbsNatural.BoolStructR }
BoolStructR
  : ListBSRAny 'OR' BoolStructR { AbsNatural.BSRAny $1 $3 }
  | BoolStructR1 { $1 }

BoolStructR2 :: { AbsNatural.BoolStructR }
BoolStructR2
  : BSRLeaf { AbsNatural.BSRLeaf $1 } | '(' BoolStructR ')' { $2 }

BoolStructR1 :: { AbsNatural.BoolStructR }
BoolStructR1
  : ListBSRAll 'AND' BoolStructR { AbsNatural.BSRAll $1 $3 }
  | BoolStructR2 { $1 }

BSRLeaf2 :: { AbsNatural.BSRLeaf }
BSRLeaf2
  : RelationalPredicate { AbsNatural.LeafNoLabel $1 }
  | BSRLeaf3 { $1 }

BSRLeaf1 :: { AbsNatural.BSRLeaf }
BSRLeaf1
  : BSRLeaf 'POST' MultiTerm { AbsNatural.LeafPostLabel $1 $3 }
  | BSRLeaf2 { $1 }

BSRLeaf :: { AbsNatural.BSRLeaf }
BSRLeaf
  : 'PRE' MultiTerm BoolStructR { AbsNatural.LeafPreLabel $2 $3 }
  | BSRLeaf1 { $1 }

BSRLeaf3 :: { AbsNatural.BSRLeaf }
BSRLeaf3 : '(' BSRLeaf ')' { $2 }

BSRAny :: { AbsNatural.BSRAny }
BSRAny : BoolStructR { AbsNatural.BSRAnyList $1 }

ListBSRAny :: { [AbsNatural.BSRAny] }
ListBSRAny
  : BSRAny { (:[]) $1 } | BSRAny 'OR' ListBSRAny { (:) $1 $3 }

BSRAll :: { AbsNatural.BSRAll }
BSRAll : BoolStructR { AbsNatural.BSRAllList $1 }

ListBSRAll :: { [AbsNatural.BSRAll] }
ListBSRAll
  : BSRAll { (:[]) $1 } | BSRAll 'AND' ListBSRAll { (:) $1 $3 }

RelationalPredicate :: { AbsNatural.RelationalPredicate }
RelationalPredicate
  : MultiTerm 'IS' MultiTerm { AbsNatural.RPConstraint $1 $3 }
  | MultiTerm 'IS' BoolStructR { AbsNatural.RPBoolStructR $1 $3 }
  | MultiTerm { AbsNatural.RPMT $1 }

RPRel :: { AbsNatural.RPRel }
RPRel : 'IS' { AbsNatural.RPis }

MultiTerm :: { AbsNatural.MultiTerm }
MultiTerm : ListMTExpr { AbsNatural.MultiTerm $1 }

MTExpr :: { AbsNatural.MTExpr }
MTExpr : MTT { AbsNatural.MTExpr $1 }

MTT :: { AbsNatural.MTT }
MTT : PIdent { AbsNatural.MTT $1 }

MTN :: { AbsNatural.MTN }
MTN : PInteger { AbsNatural.MTN $1 }

ListMTExpr :: { [AbsNatural.MTExpr] }
ListMTExpr : MTExpr { (:[]) $1 } | MTExpr ListMTExpr { (:) $1 $2 }

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

