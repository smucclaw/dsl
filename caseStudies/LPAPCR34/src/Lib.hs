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
import Text.Pretty.Simple (pPrint, pShow)
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

data Stm = Section TermExpr -- first TermExpr should always be an NL expression (:@)
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

data TermExpr = String :@ [NLTag]            -- infix constructor for NL:    "potato" :@ [("en","Solanum Tuberosum")]
              | TKey Key                     -- "potato"
              | All [TermExpr]               -- and -- each element must evaluate true
              | Any [TermExpr]               -- or  -- only one element is needed to be true
              | And [TermExpr]               -- set union! inclusive "and" is not a logical "and"! more of an "or"
              | Compound TermExpr [TermExpr] -- foo(bar, baz) what Prolog calls a compound term, but looks like a function to everyone else
              | Cons TermExpr TermExpr       -- used to linearly string together increasingly lengthy sentences -- you will recognize this as a cons-list from LISP
              | BinOp TermExpr TermExpr TermExpr -- ace "of" bass, useful when either lhs or rhs term is itself an All/Any list
  deriving (Show, Eq)

type Key = String               -- ideally spaceless but can contain spaces. Maybe a Mk to s/ /_/g
type NLTag = (Key,String)

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
  return $ exPart :@ tagsPart

-- :en:"Some String"
parseTag :: Parser NLTag
parseTag = do
  lhs <- between (char ':') (char ':') (some alphaNumChar)
  rhs <- stringLiteral
  return (lhs, rhs)

-- sooner or later we will need to think about https://github.com/glebec/left-recursion
parseTermExpr :: Parser TermExpr
parseTermExpr = return $ "burf" :@ []



section34 :: [Stm]
section34 = [ (§) ("34" :@ [("en","Thirty Four, part One")]) -- subsequently we speechbubble with English as default
            , section34_1 ]

section34_1 :: Stm
section34_1 =
  (§§) ("34.1" 💬 "Prohibitions")
  ("LP" 💬 "Legal Practitioner")
  mustNot
  (en_ "accept" ⏩ ["execA" 💬 "executive appointment", "Business" 💬 "business"]) -- prolog: drule(LP,shant,accept(execA,Business)) :- subRule(Violations, LP, Business)
  (Any [ "Business" 👉 Any [ (en_ "detracts" `_from`)                               -- prolog: subRule(rule1a, LP, Business) :- detracts(Business, dignity(prof));
                              , ("incompat" 💬 "is incompatible" `_with`)           --                                          incompat(Business, dignity(prof));
                              , (en_ "derogates" `_from`)                           --                                          derogates(Business, dignity(prof)).
                              ] $ (en_ "dignity") `of_` ("prof" 💬 "legal profession")  --    detracts(Business,dignity(prof)) :- askUser("does the Business detract from the dignity of the profession?").
         , "Business" 👉 (en_ "materially interferes with") $
           Any [ lp's ("occ" 💬 "primary occupation") `of_` (en_ "practising") `as_` (en_ "lawyer")
               , lp's $ en_ "availability to those who may seek" <> lp's (en_ "services as a lawyer") -- needs more structure? rephrase using BinOp? Cons?
               , en_ "representation" `of_` lp's ( en_ "clients" ) ]
         , "Business" 👉 (en_ "is likely to") $ (en_ "unfairly attract business") `in_` (en_ "practice of law")
         , "Business" 👉 (en_ "involves") $ ( Any [ ((en_ "sharing") `of_` (lp's (en_ "fees")) `_with`) -- how to structure a ditransitive, ternary preposition?
                                                  , (en_ "payment" `of_` en_ "commission" `_to`) ] <>
                                              Any [ en_ "unauthorised person" ] `for_`
                                              en_ "legal work performed" `by_`
                                              (TKey "LP") )
         , "Business" 👉 ("isIn" 💬 "is in") $ ("Schedule1" 💬 "First Schedule")
         , "Business" 👉 ("isProhibitedBy" 💬 "is prohibited by") $
           Any [ en_ "Act"
               , Any [ en_ "these Rules"
                     , en_ "any other subsidiary legislation made under the Act" ] -- not enough structure?
               , And [ en_ "practice directions", en_ "guidance notes", en_ "rulings" ]
                 <> en_ "issued under section 71(6) of the Act" -- should "under" be a preposition like the others?
               , And [ en_ "practice directions", en_ "guidance notes", en_ "rulings" ]
                 <> en_ "relating" `to_` And [ en_ "professional practice", en_ "etiquette", en_ "conduct", en_ "discipline" ]
                 <> en_ "issued" `by_` Any [ en_ "Council", en_ "Society" ]
             ]
         ]
  )

-- by default, we construct an English term, but this could evolve toward wordnet
(💬) :: String -> String -> TermExpr
infixr 7 💬
ex 💬 tag = ex :@ [("en",tag)]

en_ :: String -> TermExpr
en_ ex = ex :@ [("en",ex)]

nl_ :: String -> TermExpr
nl_ ex = ex :@ []

-- syntactic sugar for a prolog-style Compound term: lhs(r,h,s)
(⏩) :: TermExpr -> [TermExpr] -> TermExpr
infixr 5 ⏩
lhs ⏩ rhs = Compound lhs rhs

-- executive appointment IS associatedWith Something
-- becomes, in prolog, associatedWith(ExecutiveAppointment, Something)
-- which is basically a bit of a flip and rearrangement of the above
(👉) :: Key -> TermExpr -> TermExpr -> TermExpr
infixr 5 👉
lhs 👉 compound = \rhs -> compound ⏩ [TKey lhs, rhs]

-- section marker. this always has to be wrapped in () because it's a section?
(§) :: TermExpr -> Stm
(§) = Section

-- deontic rule -- party x must y
type MkDRule = TermExpr -> TermExpr -> Deontic -> TermExpr -> TermExpr -> Stm
(§§) :: MkDRule
(§§) a b c d e = DRule a b c d (Just e)

-- constitutive rule -- define x as y
type MkCRule = TermExpr -> TermExpr -> TermExpr -> TermExpr -> Stm
(§=§) :: MkCRule
(§=§) a b c d = CRule a b c (Just d)

instance Semigroup TermExpr where (<>) = Cons

_'s :: Key -> TermExpr -> TermExpr
infixr 6 `_'s`
x `_'s` y = TKey x `possessive` y

-- we hardcode "LP" to mean Legal Practitioner
lp's = _'s "LP"

mustNot = DShant
mayNot  = DShant
must    = DMust
may     = DMay

binop op x y = BinOp x (nl_ op) y

-- this is a sign we need to learn Template Haskell
infixr 6 `of_`;     of_ = binop "of";      infixr 5 `_of`;    _of  x = x <> (nl_ "of")
infixr 6 `to_`;     to_ = binop "to";      infixr 5 `_to`;    _to  x = x <> (nl_ "to")
infixr 6 `as_`;     as_ = binop "as";      infixr 5 `_as`;    _as  x = x <> (nl_ "as")
infixr 6 `in_`;     in_ = binop "in";      infixr 5 `_in`;    _in  x = x <> (nl_ "in")
infixr 6 `is_`;     is_ = binop "is";      infixr 5 `_is`;    _is  x = x <> (nl_ "is")
infixr 6 `by_`;     by_ = binop "by";      infixr 5 `_by`;    _by  x = x <> (nl_ "by")
infixr 6 `for_`;   for_ = binop "for";     infixr 5 `_for`;   _for x = x <> (nl_ "for")
infixr 6 `with_`; with_ = binop "with";    infixr 5 `_with`; _with x = x <> (nl_ "with")
infixr 6 `from_`; from_ = binop "from";    infixr 5 `_from`; _from x = x <> (nl_ "from")

infixr 6 `possessive`
possessive = binop "'s"

someFunc :: IO ()
someFunc = do
  pPrint section34
