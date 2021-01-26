{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text, pack, unpack)
import Data.Void
import Data.List (nub, permutations, sort, sortOn, intercalate)
import Data.Char (toLower)
import Control.Monad (forM_)
import qualified Text.PrettyPrint.Boxes as Bx
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

data Conj = AndC | OrC | CommaC | AnyC | UnionC | IntersectC | RunOn
  deriving (Show, Eq)

data TermExpr = TEString NLStr               -- ideally spaceless but can contain spaces
              | TEList Conj [TermExpr]       -- any [foo,bar]   [foo, bar | baz]   [foo & bar]
              | TECompound TermExpr TermExpr -- foo(bar, baz)
              | TEPrep NLStr                 -- "to" "for" "with" "of" "from" "by"
              | TE2 TermExpr BinOp TermExpr  -- foo + bar
  deriving (Show, Eq)

type NLTag = (String,String)
data NLStr = NL { ex :: String, tags :: [NLTag] } deriving (Show, Eq)
data BinOp = Plus | Minus | Times | Divide | Pow | Tilde deriving (Show, Eq)

data Stm = Section          NLStr
         | DeonticRule      NLStr
         | ConstitutiveRule NLStr deriving (Show, Eq)

parseL4 :: Parser [Stm]
parseL4 = do
  stm <- parseStm
  rhs <- parseL4
  eof
  return (stm : rhs)

parseStm :: Parser Stm
parseStm = do
  many newline
  stm <- choice [ try (Section <$> (lexeme "SECTION" *> parseNLStr)) ]
  return stm

parseNLStr :: Parser NLStr
parseNLStr = do
  exPart    <- lexeme $ some alphaNumChar
  tagsPart  <- many parseTag
  return $ NL exPart tagsPart

parseTag :: Parser NLTag
parseTag = do
  lhs <- between (char ':') (char ':') (some alphaNumChar)
  rhs <- stringLiteral
  return (lhs, rhs)
someFunc :: IO ()
someFunc = do
  myinput <- getContents
  let ast = case parse parseL4 "parsing L4 toplevel" (pack myinput) of
              Left  someError  -> error $ errorBundlePretty someError
              Right rhs -> rhs
  -- print ast
  print ast
