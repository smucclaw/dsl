{-# LANGUAGE PostfixOperators #-} -- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#postfix-operators
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

data TermExpr = NL String [NLTag]            -- "potato" `TENL` "Solanum tuberosum"
              | TKey Key                     -- "potato"
              | All [TermExpr]               -- and
              | Any [TermExpr]               -- or
              | And [TermExpr]               -- set union
              | Compound TermExpr [TermExpr] -- foo(bar, baz) what Prolog calls a compound term, but looks like a function to everyone else
              | Cons TermExpr TermExpr
              | BinOp TermExpr TermExpr TermExpr -- ace "of" bass
  deriving (Show, Eq)

type Key = String               -- ideally spaceless but can contain spaces. Maybe a Mk to s/ /_/g
type NLTag = (Key,String)

data Stm = Section TermExpr -- first TermExpr should always be an NL
         | DRule { rulename :: TermExpr
                 , party  :: TermExpr
                 , dmodal :: Deontic
                 , action :: TermExpr
                 , when   :: Maybe TermExpr
                 }
         | CRule { rulename :: TermExpr
                 , item :: TermExpr
                 , body :: TermExpr
                 , when :: Maybe TermExpr
                 }
  deriving (Show, Eq)

data Deontic = DMust | DMay | DShant deriving (Show, Eq)

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
  stm <- choice [ try (Section           <$> (lexeme "SECTION" *> parseNLStr))
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
  when     <- optional (space *> lexeme "WHEN" *> parseTermExpr)
  return (DRule ruleName party dmodal action when)

-- DEFINE X IS BLAH
parseConstitutiveRule :: Parser Stm
parseConstitutiveRule = do
  ruleName <- space *> lexeme "RULE"    *> parseNLStr <* lexeme "ISA" <* lexeme "ConstitutiveRule" <* some newline
  item     <- space *> lexeme "DEFINE"  *> parseNLStr <*                                         some newline
  body     <-          lexeme parseTermExpr <* some newline
  when     <- optional (space *> lexeme "WHEN" *> parseTermExpr)
  return (CRule ruleName item body when)

-- SomeString :en:"Some String"
parseNLStr :: Parser TermExpr
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
parseTermExpr = return $ NL "burf" []



section34 :: [Stm]
section34 = [ (§) ("34" 💬 "Thirty Four, part One")
            , section34_1 ]

section34_1 :: Stm
section34_1 = (§§) ("34.1" 💬 "Prohibitions")
                   ("LP" 💬 "Legal Practitioner")
                   mustNot
                   (en_ "accept" ⏩ ["ExecA" 💬 "Executive Appointment"])
                   (Just (All [ "ExecA" 👉 ("assocWith" 💬 "associated with") $ (en_ "Business")
                              , Any [ "Business" 👉 Any [ (en_ "detracts" `_from`)
                                                         , ("incompat" 💬 "is incompatible" `_with`)
                                                         , (en_ "derogates" `_from`)
                                                         ] $ (en_ "dignity") `of_` ("prof" 💬 "legal profession")
                                    , "Business" 👉 (en_ "materially interferes with") $
                                      Any [ "LP" `_'s` ("occ" 💬 "primary occupation") `of_` (en_ "practising") `as_` (en_ "lawyer")
                                          , "LP" `_'s` en_ "availability to those who may seek" `cons` "LP" `_'s` en_ "services as a lawyer"
                                          , en_ "representation" `of_` "LP" `_'s` en_ "clients" ]
                                    , "Business" 👉 (en_ "is likely to") $ (en_ "unfairly attract business in the practice of law")
                                    , "Business" 👉 (en_ "is likely to") $ (en_ "unfairly attract business") `in_` (en_ "the practice of law")
                                    , "Business" 👉 (en_ "involves")     $ (en_ "sharing") `of_` "LP" `_'s` en_ "fees" `with_` (Any [ (en_ "payment" `of_` en_ "commission" `_to`)
                                                                                                                                    , Any [ en_ "unauthorised person" `for_` en_ "legal work performed" `by_` (TKey "LP") ]
                                                                                                                                    ])
                                    , "Business" 👉 ("isIn" 💬 "is in") $ ("Schedule1" 💬 "First Schedule")
                                    , "Business" 👉 ("isProhibitedBy" 💬 "is prohibited by") $
                                      Any [ en_ "Act"
                                          , Any [ en_ "these Rules"
                                                , en_ "any other subsidiary legislation made under the Act" ]
                                          , And [ en_ "practice directions", en_ "guidance notes", en_ "rulings" ]
                                            `cons` en_ "issued under section 71(6) of the Act"
                                          , And [ en_ "practice directions", en_ "guidance notes", en_ "rulings" ]
                                            `cons` en_ "relating" `to_` And [ en_ "professional practice", en_ "etiquette", en_ "conduct", en_ "discipline" ]
                                            `cons` en_ "issued" `by_` Any [ en_ "Council", en_ "Society" ]
                                        ]
                                    ]
                              ]
                         )
                   )



-- syntactic sugar for constructing common types
(💬) :: String -> String -> TermExpr
infixr 7 💬
ex 💬 tag = NL ex [("en",tag)]

-- syntactic sugar for constructing common types
en_ :: String -> TermExpr
en_ ex = NL ex [("en",ex)]

nl_ :: String -> TermExpr
nl_ ex = NL ex []

-- syntactic sugar for Compound
(⏩) :: TermExpr -> [TermExpr] -> TermExpr
infixr 5 ⏩
lhs ⏩ rhs = Compound lhs rhs

-- executive appointment IS associatedWith Something
(👉) :: Key -> TermExpr -> TermExpr -> TermExpr
infixr 5 👉
lhs 👉 compound = \rhs -> Compound compound [TKey lhs, rhs]

(§) :: TermExpr -> Stm
(§) = Section -- this always has to be wrapped in () because it's a section

type MkDRule = TermExpr -> TermExpr -> Deontic -> TermExpr -> Maybe TermExpr -> Stm
(§§) :: MkDRule
(§§) = DRule

cons :: TermExpr -> TermExpr -> TermExpr
cons = Cons
infixr 6 `cons`

_'s :: Key -> TermExpr -> TermExpr
infixr 6 `_'s`
x `_'s` y = TKey x `possessive` y

mustNot = DShant
mayNot = DShant
must = DMust
may = DMay

binop op x y = BinOp x (nl_ op) y

-- this is a sign we need to learn Template Haskell
infixr 6 `of_`;     of_ = binop "of";      infixr 5 `_of`;    _of  x = x `cons` (nl_ "of")
infixr 6 `to_`;     to_ = binop "to";      infixr 5 `_to`;    _to  x = x `cons` (nl_ "to")
infixr 6 `as_`;     as_ = binop "as";      infixr 5 `_as`;    _as  x = x `cons` (nl_ "as")
infixr 6 `in_`;     in_ = binop "in";      infixr 5 `_in`;    _in  x = x `cons` (nl_ "in")
infixr 6 `is_`;     is_ = binop "is";      infixr 5 `_is`;    _is  x = x `cons` (nl_ "is")
infixr 6 `by_`;     by_ = binop "by";      infixr 5 `_by`;    _by  x = x `cons` (nl_ "by")
infixr 6 `for_`;   for_ = binop "for";     infixr 5 `_for`;   _for x = x `cons` (nl_ "for")
infixr 6 `with_`; with_ = binop "with";    infixr 5 `_with`; _with x = x `cons` (nl_ "with")
infixr 6 `from_`; from_ = binop "from";    infixr 5 `_from`; _from x = x `cons` (nl_ "from")

infixr 6 `possessive`
possessive = binop "'s"

someFunc :: IO ()
someFunc = do
  myinput <- getContents
  let ast = case parse parseL4 "parsing L4 toplevel" (pack myinput) of
              Left  someError  -> error $ errorBundlePretty someError
              Right rhs -> rhs
  -- print ast
  print ast
