{
{-# OPTIONS_GHC -XFlexibleContexts #-}

module Lexer (
  Token(..),
  scanTokens
) where

import Control.Monad.Except

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  "#".*                         ;

  -- Syntax
  -- Structuring elements of an L4 file
  
  assert                        { \s -> TokenAssert }
  class                         { \s -> TokenClass }
  decl                          { \s -> TokenDecl }
  defn                          { \s -> TokenDefn }
  extends                       { \s -> TokenExtends }
  rule                          { \s -> TokenRule }

  -- Types
  Bool                          { \s -> TokenBool }
  Int                           { \s -> TokenInt }

  -- Expressions
  let                           { \s -> TokenLet }
  in                            { \s -> TokenIn 
  not                           { \s -> TokenNot }
  all                           { \s -> TokenAll }
  ex                            { \s -> TokenEx }
  if                            { \s -> TokenIf }
  then                          { \s -> TokenThen }
  else                          { \s -> TokenElse }
  for                           { \s -> TokenFor }
  True                          { \s -> TokenTrue }
  False                         { \s -> TokenFalse }

  -- Symbols
  -- "->"                          { \s -> TokenArrow }
  -- \\                            { \s -> TokenLambda }
  --"-->"                         { \s -> TokenImpl }
  --"||"                          { \s -> TokenOr }
  --"&&"                          { \s -> TokenAnd }
  --\=                            { \s -> TokenEq }
  --\<                            { \s -> TokenLt }
  -- \>                           { \s -> TokenGt }
  --[\+]                          { \s -> TokenAdd }
  --[\-]                          { \s -> TokenSub }
  --[\*]                          { \s -> TokenMul }
  --"/"                           { \s -> TokenDiv }
  -- "%"                           { \s -> TokenMod }
  --\.                            { \s -> TokenDot }
  --\,                            { \s -> TokenComma }
  --\:                            { \s -> TokenColon }
  --\(                            { \s -> TokenLParen }
  --\)                            { \s -> TokenRParen }
  --\{                            { \s -> TokenLBrace }
  --\}                            { \s -> TokenRBrace }
 
  -- Numbers and identifiers
  --$digit+                       { \s -> TokenNum (read s) }
  --$alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

{

data Token 
  =
    TokenAssert
  | TokenClass
  | TokenDecl
  | TokenDefn
  | TokenExtends
  | TokenRule

  | TokenBool
  | TokenInt

  | TokenLet
  | TokenIn
  | TokenNot
  | TokenAll
  | TokenEx
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenFor
  | TokenTrue
  | TokenFalse
  
  | TokenLambda
  | TokenArrow
  | TokenImpl
  | TokenOr
  | TokenAnd
  | TokenEq
  | TokenLt
 -- | TokenGt
  | TokenAdd
  | TokenSub
  | TokenMul
  --| TokenDiv
  --| TokenMod
  | TokenDot
  | TokenComma
  | TokenColon
  | TokenLBrace
  | TokenRBrace
  | TokenLParen
  | TokenRParen
  | TokenEOF

  | TokenNum Integer
  | TokenSym String
  deriving (Eq,Show)

scanTokens :: String -> Except String [Token]
scanTokens str = go ('\n',[],str) where 
  go inp@(_,_bs,str) =
    case alexScan inp 0 of
     AlexEOF -> return []
     AlexError _ -> throwError "Invalid lexeme."
     AlexSkip  inp' len     -> go inp'
     AlexToken inp' len act -> do
      res <- go inp'
      let rest = act (take len str)
      return (rest : res)

}
