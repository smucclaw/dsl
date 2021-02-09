{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseExpr,
  parseTokens,
) where

import Lexer
import Syntax

import Control.Monad.Except

}

-- Entry point
%name expr Expr

-- Lexer structure 
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
--    let   { TokenLet }
    true  { TokenTrue }
    false { TokenFalse }
    Bool  {TokenBool}
--    in    { TokenIn }
    Int   {TokenInt}
    NUM   { TokenNum $$ }
    VAR   { TokenSym $$ }
    '\\'  { TokenLambda }
    '->'  { TokenArrow }
--    '='   { TokenEq }
    '<'   { TokenLt }
    '+'   { TokenAdd }
    '-'   { TokenSub }
    '*'   { TokenMul }
    ':'   { TokenColon }
    '('   { TokenLParen }
    ')'   { TokenRParen }

-- Operators
%left '<'
%left '+' '-'
%left '*'
%right '->'
%%

Tp   : Bool                       { BoolT }
     | Int                        { IntT }
     | Tp '->' Tp                 { FunT $1 $3 }
     | '(' Tp ')'                 { $2 }

Expr : '\\' VAR ':' Tp '->' Expr   { FunE () $2 $4 $6 }
     | Form                        { $1 }

Form : Form '<' Form               { BinOpE () (BCompar BClt) $1 $3 }
     | Form '+' Form               { BinOpE () (BArith BAadd) $1 $3 }
     | Form '-' Form               { BinOpE () (BArith BAsub) $1 $3 }
     | Form '*' Form               { BinOpE () (BArith BAmul) $1 $3 }
     | Fact                        { $1 }

Fact : Fact Atom                   { AppE () $1 $2 }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | NUM                         { ValE () (IntV $1) }
     | VAR                         { VarE () $1 0 }
     | true                        { ValE () (BoolV True) }
     | false                       { ValE () (BoolV False) }

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String (Exp ())
parseExpr input = runExcept $ do
  tokenStream <- scanTokens input
  expr tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens
    
}
