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
import Data.List (nub, permutations, sort, sortOn, intercalate, elemIndex)
import Data.Char (toLower)
import qualified Text.PrettyPrint as PP
import Control.Monad (forM_)
import qualified Data.Map.Lazy as Map
import qualified Text.PrettyPrint.Boxes as Bx
import Text.Pretty.Simple (pPrint, pShow)
import Options.Applicative.Simple (simpleOptions, addCommand, addSubCommands)
import Language.Prolog
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

stmToPlain :: Stm -> String
stmToPlain (Section te) = plain te

stmToProlog :: Stm -> PP.Doc
stmToProlog (Section te) = PP.text "%% " <> PP.text (stmToL4 (Section te))
stmToProlog (DRule rn p d a w) =
  let headP = PP.text "drule" PP.<> parencomma [ teToProlog p
                                               , PP.text (toLower <$> dshow d)
                                               , teToProlog a ]
      varargs = teToProlog p : unwrapP a
  in PP.vcat ( [ if null w
                 then headP PP.<> PP.char '.'
                 else headP PP.<+> PP.text ":-" PP.$$ (PP.nest 4 $ PP.text "subRule" PP.<> parencomma (PP.text "SubRuleName" : varargs) PP.<> PP.char '.')
               ] ++ asPrologSubrules w varargs )

asPrologSubrules :: Maybe TermExpr -> [PP.Doc] -> [PP.Doc]
asPrologSubrules Nothing _ = [PP.empty]
asPrologSubrules (Just (Any termexprs)) varargs = asPrologSubrule varargs <$> (zip ['A'..] termexprs)

asPrologSubrule :: [PP.Doc] -> (Char, TermExpr) -> PP.Doc
asPrologSubrule varargs (ruleNum, te) =
  PP.hang
  (PP.text "subRule" PP.<> parencomma ([ PP.text ("subRule_"++[ruleNum]) ] ++ varargs) PP.<+> (PP.text ":-"))
  5
  ((teToProlog te) PP.<> PP.char '.')


unwrapP :: TermExpr -> [PP.Doc]
unwrapP (Compound te termexprs) = teToProlog <$> termexprs
unwrapP (mainex :@ nltags) = [PP.text $ tr " " "_" mainex]
unwrapP (TKey tkey)        = [PP.text $ tr " " "_" tkey]
unwrapP (All termexprs)    = teToProlog <$> termexprs
unwrapP orig@(Any termexprs)     = teToProlog <$> termexprs
unwrapP (And termexprs)          = teToProlog <$> termexprs
unwrapP (Cons te1 te2)           = [teToProlog te1, teToProlog te2]
unwrapP orig@(BinOp te1 bo2 te3) = [teToProlog te1, teToProlog te3]

parencomma docs = PP.parens (PP.hsep $ PP.punctuate (PP.char ',') docs)

teToProlog :: TermExpr -> PP.Doc
teToProlog orig@(mainex :@ nltags) = PP.text $ plain orig
teToProlog orig@(TKey tkey)        = PP.text $ plain orig
teToProlog (All termexprs)    = PP.text "TODO All"
teToProlog orig@(Any termexprs)     = PP.nest 8 $ PP.vcat $ PP.punctuate (PP.text "; ") (teToProlog <$> termexprs)
teToProlog (And termexprs)          = PP.text "TODO And"
teToProlog (Cons te1 te2)           = teToProlog te1 <> PP.char '_' <> teToProlog te2
teToProlog orig@(Compound (Any lhss) termexprs) = teToProlog (Any ((\lhs -> (Compound lhs termexprs)) <$> lhss))
teToProlog orig@(Compound lhs [tk,Any rhss]) = teToProlog (Any ((\rhs -> (Compound lhs [tk,rhs])) <$> rhss))
teToProlog orig@(Compound te1 termexprs)  = teToProlog te1 <> parencomma (teToProlog <$> termexprs)
teToProlog orig@(BinOp (BinOp tk1 ("'s" :@ []) tk2) ("of" :@ []) te3) = teToProlog tk2 <> PP.text "_of" <> parencomma [teToProlog tk1, teToProlog te3]
teToProlog orig@(BinOp te1 ("of" :@ []) te3) = teToProlog te1 <> PP.text "_of" <> parencomma [teToProlog te3]
teToProlog orig@(BinOp te1 ("'s" :@ []) te3) = teToProlog te3 <> parencomma [teToProlog te1]
teToProlog orig@(BinOp te1 bo2 te3)          = teToProlog te1 <> parencomma [teToProlog bo2, teToProlog te3]

plain :: TermExpr -> String
plain (mainex :@ nltags) = tr " " "_" mainex
plain (TKey tkey)        = tr " " "_" tkey
plain (All termexprs)    = "TODO " ++ intercalate ", " (plain <$> termexprs)
plain orig@(Any termexprs)     = "any of: [" ++ intercalate ", " (plain <$> termexprs) ++ "]"
plain (And termexprs)          = "all of: [" ++ intercalate ", " (plain <$> termexprs) ++ "]"
plain (Cons te1 te2)           = unwords [ plain te1, plain te2 ]
plain orig@(Compound (Any lhss) termexprs) = plain (Any ((\lhs -> (Compound lhs termexprs)) <$> lhss))
plain orig@(Compound te1 termexprs)        = plain te1 <> "(" <> intercalate ", " (plain <$> termexprs)  <> ")"
plain orig@(BinOp te1 ("'s" :@ []) te3)    = plain te3 <> "(" <> plain te1           <> ")"
plain orig@(BinOp te1 ("of" :@ []) te3)    = plain te1 <> "(" <> intercalate ", " ["of", plain te3]      <> ")"
plain orig@(BinOp te1 bo2 te3)             = plain te1 <> "(" <> intercalate ", " (plain <$> [bo2, te3]) <> ")"


-- tr "abc" "123" "a cow" -> "1 3ow"
tr x y cs = [ maybe c (y !!) eI | c <- cs, let eI = elemIndex c x ]

stmToL4 :: Stm -> String
stmToL4 (Section te)       = unlines [ unwords [ "SECTION",             teToL4 toplevel te ] ]
stmToL4 (DRule rn p d a w) = unlines [ unwords [ "   RULE",             teToL4 toplevel rn ]
                                     , unwords [ "  PARTY",             teToL4 toplevel p ]
                                     , unwords [ "  "  ++ dshow d,      teToL4 toplevel a ]
                                     , unwords [ "   " ++ maybe "-- EVER"
                                                              (("WHEN " ++) . teToL4 toplevel) w ]

                                     ]
-- rule_34_1(party(LegalPractitioner), shant, accept(ExecA)) :- assocWith(ExecA, Business),
--    detracts_from
toplevel = Ctx []

data Context = Ctx { stack    :: [TermExpr] } deriving (Show, Eq)

ppTE ctx orig@(Any termexprs)     = "Any " ++ (PP.render $ PP.brackets (PP.hcat $ PP.punctuate (PP.char ',') (PP.text . (teToL4' ctx orig) <$> termexprs )))

-- this Context thing really should be a State monad
teToL4 :: Context -> TermExpr -> String
teToL4 ctx (mainex :@ nltags) = mainex
teToL4 ctx (TKey tkey)        = tkey
teToL4 ctx (All termexprs)    = "TODO All"
teToL4 ctx orig@(Any termexprs)     = "TODO Any"
teToL4 ctx (And termexprs)          = "TODO And"
teToL4 ctx (Cons te1 te2)           = "TODO Cons"
teToL4 ctx orig@(Compound te1 termexprs) = teToL4' ctx orig te1 ++ "(" ++ intercalate "," ((teToL4' ctx orig) <$> termexprs) ++ ")"
teToL4 ctx orig@(BinOp te1 bo2 te3)      = teToL4' ctx orig bo2 ++ "(" ++ teToL4' ctx orig te1 ++ " , " ++ teToL4' ctx orig te3 ++ ")"

teToL4' ctx orig = teToL4 (ctx { stack = orig : stack ctx })


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
                              ] $ (en_ "dignity") `of_` ("profession" 💬 "legal profession")  --    detracts(Business,dignity(prof)) :- askUser("does the Business detract from the dignity of the profession?").
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
dshow DShant = "SHANT"
dshow DMust  = "MUST"
dshow DMay   = "MAY"

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

someFunc :: String -> IO ()
someFunc myinput = do
  let stdinAST = case parse parseL4 "parsing L4 toplevel" (pack myinput) of
                   Left  someError  -> error $ errorBundlePretty someError
                   Right rhs -> rhs
  (opts,runCmd) <-
    simpleOptions "v0.01" "lpapcr34" "an early L4 prototype for section 34" (pure ()) $
    do addSubCommands "pretty" "pretty    ast | stdin" (
         do addCommand "ast"   "the manual AST" (const (pPrint section34)) (pure ())
            addCommand "stdin" "parsed STDIN"   (const (pPrint stdinAST))  (pure ())
         )
       -- i think there is a bug in optparse-simple, honestly
       addCommand "prolog" "output to prolog" (const (mapM_ (putStrLn . PP.render . stmToProlog) section34)) (pure ())
       addCommand "plain"  "output to plain"  (const (mapM_ (putStrLn .             stmToPlain)  section34)) (pure ())
  runCmd
