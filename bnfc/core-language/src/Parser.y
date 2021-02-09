{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseProgram
--  , parseTokens,
) where

import Lexer
import Syntax

import Control.Monad.Except

}

-- Entry point
%name program Program

-- Lexer structure 
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    class   { TokenClass }
    extends { TokenExtends }
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
    '{'   { TokenLBrace }
    '}'   { TokenRBrace }

-- Operators
%left '<'
%left '+' '-'
%left '*'
%right '->'
%%

Program : ClassDecls               { Program (reverse $1) }

ClassDecls :                       { [] }
           | ClassDecls ClassDecl  { $2 : $1 }
ClassDecl : class VAR ClassDef     { ClassDecl (ClsNm $2) $3 }

ClassDef :  '{' FieldDecls '}'     { ClassDef (Just (ClsNm "Object")) (reverse $2) }
         |   extends VAR '{' FieldDecls '}'    
                                   { ClassDef (Just (ClsNm $2)) (reverse $4) }
FieldDecls :                       { [] }
           | FieldDecls FieldDecl  { $2 : $1 }
FieldDecl : VAR ':' Tp             { FieldDecl (FldNm $1) $3 }

Tp   : Bool                       { BoolT }
     | Int                        { IntT }
     | Tp '->' Tp                 { FunT $1 $3 }
     | '(' Tp ')'                 { $2 }

Expr : '\\' VAR ':' Tp '->' Expr   { FunE () (VarNm $2) $4 $6 }
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
     | VAR                         { VarE () (VarNm $1) }
     | true                        { ValE () (BoolV True) }
     | false                       { ValE () (BoolV False) }


-- Annotations for GF
Annot : '(' NUM ')'                 { $2 }


{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseProgram:: String -> Either String (Program (Maybe ClassName))
parseProgram input = runExcept $ do
  tokenStream <- scanTokens input
  program tokenStream

-- still needed ???
-- parseTokens :: String -> Either String [Token]
-- parseTokens = runExcept . scanTokens
    
}
