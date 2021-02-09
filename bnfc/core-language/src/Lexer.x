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
  let                           { \s -> TokenLet }
  True                          { \s -> TokenTrue }
  False                         { \s -> TokenFalse }
  Bool                          { \s -> TokenBool }
  in                            { \s -> TokenIn }
  Int                           { \s -> TokenInt }
  $digit+                       { \s -> TokenNum (read s) }
  "->"                          { \s -> TokenArrow }
  \\                            { \s -> TokenLambda }
  \=                            { \s -> TokenEq }
  \<                            { \s -> TokenLt }
  [\+]                          { \s -> TokenAdd }
  [\-]                          { \s -> TokenSub }
  [\*]                          { \s -> TokenMul }
  \:                            { \s -> TokenColon }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

{

data Token 
  = TokenLet
  | TokenTrue
  | TokenFalse
  | TokenBool
  | TokenIn
  | TokenInt
  | TokenLambda
  | TokenNum Int
  | TokenSym String
  | TokenArrow
  | TokenEq
  | TokenLt
  | TokenAdd
  | TokenSub
  | TokenMul
  | TokenColon
  | TokenLParen
  | TokenRParen
  | TokenEOF
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
