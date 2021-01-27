{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
              | TECompound TermExpr TermExpr -- foo(bar, baz) what Prolog calls a compound term, but looks like a function to everyone else
              | TEPrep NLStr                 -- "to" "for" "with" "of" "from" "by"
              | TE2 TermExpr BinOp TermExpr  -- foo + bar, foo = bar
  deriving (Show, Eq)

type NLTag = (String,String)
data NLStr = NL { ex :: String, tags :: [NLTag] } deriving (Show, Eq)
data BinOp = Plus | Minus | Times | Divide | Pow | Tilde deriving (Show, Eq)

data Stm = Section          NLStr
         | DeonticRule      NLStr DRuleBody
         | ConstitutiveRule NLStr CRuleBody deriving (Show, Eq)

data Deontic = DMust | DMay | DShant deriving (Show, Eq)

data DRuleBody = DRule { party  :: NLStr
                       , dmodal :: Deontic
                       , action :: TermExpr
                       , when   :: Maybe TermExpr
                       } deriving (Show, Eq)
  -- no hence, no lest

data CRuleBody = CRule { item :: NLStr
                       , body :: TermExpr
                       , when :: Maybe TermExpr
                       } deriving (Show, Eq)

-- an L4 document is a list of stanzas
parseL4 :: Parser [Stm]
parseL4 = do
  stm <- parseStm
  rhs <- parseL4
  eof
  return (stm : rhs)

-- a stanza
parseStm :: Parser Stm
parseStm = do
  many newline
  stm <- choice [ try (Section          <$> (lexeme "SECTION" *> parseNLStr))
                , try parseDeonticRule
                , try parseConstitutiveRule
                ]
  return stm

-- RULE PARTY X MUST Y
parseDeonticRule :: Parser Stm
parseDeonticRule = do
  ruleName <- space *> lexeme "RULE"    *> parseNLStr <* lexeme "ISA" <* lexeme "DeonticRule" <* some newline
  party    <- space *> lexeme "PARTY"   *> parseNLStr <*                                         some newline
  dmodal   <- space *> lexeme (DMust  <$ "MUST" <|>
                               DMay   <$ "MAY"  <|>
                               DShant <$ "SHANT")
  action   <-          lexeme parseTermExpr <* some newline
  when     <- (Just <$> space *> lexeme "WHEN" *> parseTermExpr) <|> (Nothing <$ empty)
  return (DeonticRule ruleName (DRule party dmodal action when))

-- DEFINE X IS BLAH
parseConstitutiveRule :: Parser Stm
parseConstitutiveRule = do
  ruleName <- space *> lexeme "RULE"    *> parseNLStr <* lexeme "ISA" <* lexeme "ConstitutiveRule" <* some newline
  item     <- space *> lexeme "DEFINE"  *> parseNLStr <*                                         some newline
  body     <-          lexeme parseTermExpr <* some newline
  when     <- (Just <$> space *> lexeme "WHEN" *> parseTermExpr ) <|> (Nothing <$ empty)
  return (DeonticRule ruleName (DRule party dmodal action when))

-- SomeString :en:"Some String"
parseNLStr :: Parser NLStr
parseNLStr = do
  exPart    <- lexeme $ some (alphaNumChar <|> char '.')
  tagsPart  <- many (lexeme parseTag)
  return $ NL exPart tagsPart

-- :en:"Some String"
parseTag :: Parser NLTag
parseTag = do
  lhs <- between (char ':') (char ':') (some alphaNumChar)
  rhs <- stringLiteral
  return (lhs, rhs)

-- sooner or later we will need to think about https://github.com/glebec/left-recursion
parseTermExpr :: Parser TermExpr
parseTermExpr = return $ TEString (NL "burf" [])

someFunc :: IO ()
someFunc = do
  myinput <- getContents
  let ast = case parse parseL4 "parsing L4 toplevel" (pack myinput) of
              Left  someError  -> error $ errorBundlePretty someError
              Right rhs -> rhs
  -- print ast
  print ast
