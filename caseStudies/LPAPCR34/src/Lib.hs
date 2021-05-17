{-# LANGUAGE PostfixOperators #-} -- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#postfix-operators
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as TIO
import Prettyprinter hiding (space)
import qualified Prettyprinter as PP
import Data.Void
import Data.List (nub, permutations, sort, sortOn, intercalate, elemIndex, intersperse)
import Data.Char (toLower)
import Control.Monad (forM_)
import qualified Data.Map.Lazy as Map
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
         | DRule { rulename :: TermExpr -- deontic rule
                 , party  :: TermExpr
                 , dmodal :: Deontic
                 , action :: TermExpr
                 , when   :: Maybe TermExpr
                 }
         | CRule { item :: TermExpr
                 , body :: [TermExpr]
                 , when :: Maybe TermExpr
                 }
  deriving (Show, Eq)

data TermExpr = String :@ [NLTag]            -- infix constructor for NL:    "potato" :@ [("en","Solanum Tuberosum")]
              | TKey Key                     -- "potato"
              | All [TermExpr]               -- and -- each element must evaluate true
              | Any [TermExpr]               -- or  -- only one element is needed to be true
              | And [TermExpr]               -- set union! inclusive "and" is not a logical "and"! more of an "or"
              | BinOp TermExpr BinOp TermExpr -- ace "of" bass, useful when either lhs or rhs term is itself an All/Any list
              | UnOp UnOp TermExpr
              | Prim PrimType
              | TE0                          -- nil
  deriving (Show, Eq)

data UnOp = HasAttr
  deriving (Show, Eq)

data BinOp = Cons
           | Compound
           | HasType
           | HasValue
           | TE2 TermExpr
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
  item     <- space *> lexeme "DEFINE"  *> parseNLStr <*                                         some newline
  body     <-     many (lexeme parseTermExpr <* some newline)
  when     <- optional (space *> lexeme "WHEN" *> parseTermExpr)
  return (CRule item body when)

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

stmToProlog :: Stm -> Doc ann
stmToProlog (Section te) = "%% " <> pretty (stmToL4 (Section te))
stmToProlog (DRule rn p d a w) =
  let headP = "drule" <> parencomma [ dquotes (pretty $ plain rn)
                                    , "SubRuleName"
                                    , teToProlog p
                                    , pretty ((toLower <$> dshow d) :: String)
                                    , teToProlog a ]
      varargs = teToProlog p : unwrapP a
  in vsep ( [ if null w
                 then headP <> pretty '.'
                 else hang 4 (headP <+> ":-" <+> ("subRule" <> parencomma ("SubRuleName" : varargs) <> pretty '.') )
               ] ++ (intersperse line' (asPrologSubrules w varargs)) )

asPrologSubrules :: Maybe TermExpr -> [Doc ann] -> [Doc ann]
asPrologSubrules Nothing _ = [emptyDoc]
asPrologSubrules (Just (Any termexprs)) varargs = asPrologSubrule varargs <$> (zip ['A'..] termexprs)

asPrologSubrule :: [Doc ann] -> (Char, TermExpr) -> Doc ann
asPrologSubrule varargs (ruleNum, te) =
  hang 5
  ("subRule" <> parencomma ([ pretty ("subRule_"++[ruleNum]) ] ++ varargs) <+> (":-") <> line <> (teToProlog te) <> pretty '.')


unwrapP :: TermExpr -> [Doc ann]
unwrapP (mainex :@ nltags) = [pretty $ tr_ mainex]
unwrapP (TKey tkey)        = [pretty $ tr_ tkey]
unwrapP (All termexprs)    = teToProlog <$> termexprs
unwrapP orig@(Any termexprs)     = teToProlog <$> termexprs
unwrapP (And termexprs)          = teToProlog <$> termexprs
unwrapP orig@(BinOp te1 Compound te3) = [teToProlog te3]
unwrapP orig@(BinOp te1 bo2 te3) = [teToProlog te1, teToProlog te3]

parencomma docs = parens (hsep $ punctuate (pretty ',') docs)

data NLConj = NLAny | NLAnd | NLAll | NLJust deriving (Show, Eq)
-- we convert the TermExpr AST into a tree of pretty-printable strings -- Natural Language Intermediate Representation
data NLIR = Tree (NLConj, Doc ()) deriving (Show)
-- this is especially useful in the expansion of "any" and "all" subexpressions to disjunctive normal form:
-- input:      [ foo, any [bar, baz], quux ]
-- output: any [ foo bar quux, foo baz quux ]
-- input:      [ foo, any [bar, baz], all [fee, fie], quux]
-- output: any [ foo bar (fee and fie) quux, foo baz (fee and fie) quux ] ]
-- then we compose the NLR into prolog formulas



teToProlog :: TermExpr -> Doc ann
teToProlog orig@(mainex :@ nltags) = pretty $ plain orig
teToProlog orig@(TKey tkey)        = pretty $ plain orig
teToProlog (All termexprs)    = "TODO All"
teToProlog orig@(Any termexprs)     = vcat $ punctuate (semi <> PP.space) (teToProlog <$> termexprs)
teToProlog (And termexprs)          = "TODO And"
teToProlog orig@(BinOp x Cons te2@(y :@ _))
  | isPrep y  = teToProlog x <>          "_" <> teToProlog te2
  | otherwise = teToProlog x <> comma <> " " <> teToProlog te2
teToProlog orig@(BinOp x Cons y)          = teToProlog x <> comma <> " " <> teToProlog y
teToProlog orig@(BinOp (Any lhss) co rhs) = teToProlog (Any ((\lhs -> (BinOp lhs co rhs)) <$> lhss)) -- compound or cons
teToProlog orig@(BinOp lhs co (Any rhss)) = teToProlog (Any ((\rhs -> (BinOp lhs co rhs)) <$> rhss))
teToProlog orig@(BinOp (All lhss) co rhs) = teToProlog (All ((\lhs -> (BinOp lhs co rhs)) <$> lhss)) -- compound or cons
teToProlog orig@(BinOp lhs co (All rhss)) = teToProlog (All ((\rhs -> (BinOp lhs co rhs)) <$> rhss))
teToProlog orig@(BinOp (BinOp tk1 (TE2 ("'s" :@ [])) tk2) (TE2 ("of" :@ [])) te3) = teToProlog tk2 <> "_of" <> parencomma [teToProlog tk1, teToProlog te3]
teToProlog orig@(BinOp te1 Compound te2)     = teToProlog te1 <> parencomma [teToProlog te2]
teToProlog orig@(BinOp te1 (TE2 ("'s" :@ [])) te3@(te3a :@ _)) = teToProlog te3 <> parencomma [teToProlog te1] -- step 1
teToProlog orig@(BinOp te1 (TE2 ("'s" :@ [])) te3@(BinOp te3a Cons te3c)) = teToProlog te3a <> parencomma [teToProlog te1, teToProlog te3c] -- step 3
teToProlog orig@(BinOp te1 (TE2 ("'s" :@ [])) te3) = "unhandled deep BinOp for " <> teToProlog te1
teToProlog orig@(BinOp te1 (TE2 ("of" :@ [])) te3) = teToProlog te1 <> "_of_" <> teToProlog te3
teToProlog orig@(BinOp te1 (TE2 ("as" :@ [])) te3) = teToProlog te1 <> "_as_" <> teToProlog te3
teToProlog orig@(BinOp te1 (TE2 (prep :@ [])) te3) = pretty prep <> parencomma [teToProlog te1, teToProlog te3]

{- INPUT
   , BinOp
       ( TKey "LP" )
       ( TE2 ( "'s" :@ [] ) )
       ( BinOp
           ( "availability to those who may seek" :@ [ ( "en" , "availability to those who may seek" ) ] )
           Cons
           ( BinOp
               ( TKey "LP" )
               ( TE2 ( "'s" :@ [] ) )
               ( "services as a lawyer" :@ [ ( "en" , "services as a lawyer" ) ] )
           )
       )
  OUTPUT
  availability_to_those_who_may_seek(LP, services_as_a_lawyer(LP))

  step 1: BinOp (TKey LP , TE2 's, services as a lawyer
          services_as_a_alwyer(LP)

  step 2: BinOp (availability, Cons, step1)
          \x -> availability(x, step1)
  in practice we bypass this step

  step 3: BinOp (TKey LP, TE2 's, step2)
          availabiilty(LP, step1)

-}

plain :: TermExpr -> String
plain (mainex :@ nltags) = tr_ mainex
plain (TKey tkey)        = tr_ tkey

tr_ = tr " " "_"

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

ppTE ctx orig@(Any termexprs)     = "Any " ++ (show $ brackets (hcat $ punctuate (comma) (pretty . (teToL4' ctx orig) <$> termexprs )))

-- this Context thing really should be a State monad
teToL4 :: Context -> TermExpr -> String
teToL4 ctx (mainex :@ nltags) = mainex
teToL4 ctx (TKey tkey)        = tkey
teToL4 ctx (All termexprs)    = "TODO All"
teToL4 ctx orig@(Any termexprs)     = "TODO Any"
teToL4 ctx (And termexprs)          = "TODO And"
teToL4 ctx orig@(BinOp te1 bo2 te3)      = show (bo2) ++ "(" ++ teToL4' ctx orig te1 ++ " , " ++ teToL4' ctx orig te3 ++ ")"

teToL4' ctx orig = teToL4 (ctx { stack = orig : stack ctx })


section34 :: [Stm]
section34 = [ (§) ("34" :@ [("en","Thirty Four, part One")
                           ,("se","lasdkjklasdjf")]) -- subsequently we speechbubble with English as default
            , section34_1 ]

section34_1 :: Stm
section34_1 =
  (§§) ("34.1" 💬 "Prohibitions")
  ("LP" 💬 "Legal Practitioner")
  mustNot
  (en_ "accept" ⏩ ["execA" 💬 "executive appointment", "Business" 💬 "business"])
  (Any [ "Business" 👉 Any [ (en_ "detracts" `_from`)                        -- [ (1 +),
                              , ("incompat" 💬 "is incompatible" `_with`)    --   (2 *),
                              , (en_ "derogates" `_from`)                    --   (10 -) ] <*> [400] = [401, 800, -390]
                              ] $ (en_ "dignity") `of_` ("profession" 💬 "legal profession")
         , "Business" 👉 (en_ "materially interferes with") $
           Any [ lp's ("occ" 💬 "primary occupation") `of_` (en_ "practising") `as_` (en_ "lawyer")
               , lp's $ en_ "availability to those who may seek" <> lp's (en_ "services as a lawyer")
               , en_ "representation" `of_` lp's ( en_ "clients" ) ]
         , "Business" 👉 (en_ "is likely to") $ (en_ "unfairly attract business") `in_` (en_ "practice of law")
         , "Business" 👉 (en_ "involves") $ ( Any [ ((en_ "sharing") `of_` (lp's (en_ "fees")) `_with`)
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

-- wordnet!
(🔤) :: String -> Integer -> TermExpr
infixr 7 🔤
ex 🔤 tag = ex :@ [("wordnet",show tag)]

(📭) :: TermExpr -> TermExpr -> TermExpr
infix 6 📭
var 📭 typ = BinOp var HasType typ

(📩) :: String -> PrimType -> TermExpr
infix 7 📩
var 📩 val = BinOp (var 💬 var) HasValue (Prim val)

data PrimType
  = L4True | L4False
  | L4S String
  deriving (Show, Eq)

isa = (📭)
infix 6 `isa`

(📪) = typ
typ = "Type" 💬 "Base Type"
bool = "Bool" 💬 "Primitive Type Boolean"
str = "String" 💬 "Primitive Type String"

(§=) = (📬)
(📬) :: TermExpr -> String -> TermExpr
infix 6 📬
obj 📬 t = BinOp obj HasType (t 💬 "User Defined Type")

en_ :: String -> TermExpr
en_ ex = ex :@ [("en",ex)]

nl_ :: String -> TermExpr
nl_ ex = ex :@ []

-- syntactic sugar for a prolog-style Compound term: lhs(r,h,s)
(⏩) :: TermExpr -> [TermExpr] -> TermExpr
infixr 5 ⏩
lhs ⏩ rhs = BinOp lhs Compound (telist2conslist rhs)

telist2conslist :: [TermExpr] -> TermExpr
telist2conslist (te:[]) = te
telist2conslist (t:tes) = BinOp t Cons (telist2conslist tes)

-- StringValue -> BinOp "hasAttr" Compound StringValue
hasAttr :: TermExpr -> TermExpr
hasAttr te = UnOp HasAttr te

-- executive appointment IS associatedWith Something
-- becomes, in prolog, associatedWith(ExecutiveAppointment, Something)
-- which is basically a bit of a flip and rearrangement of the above
(👉) :: Key -> TermExpr -> TermExpr -> TermExpr
infixr 5 👉
lhs 👉 compound = \rhs -> BinOp compound Compound (telist2conslist [TKey lhs, rhs])

-- section marker. this always has to be wrapped in () because it's a section?
(§) :: TermExpr -> Stm
(§) = Section

-- deontic rule -- party x must y
type MkDRule = TermExpr -> TermExpr -> Deontic -> TermExpr -> TermExpr -> Stm
(§§) :: MkDRule
(§§) a b c d e = DRule a b c d (Just e)

-- constitutive rule
{- DEFINE x ISA Type
     WITH attribute ISA String
          attribute2 ISA Boolean
-} 
type MkCRule = TermExpr -> [TermExpr] -> Stm
(§=§) :: MkCRule
(§=§) item body = CRule item body Nothing

-- attribute constructors
type MkDef = TermExpr -> TermExpr -> TermExpr
(§.=§) :: MkDef
(§.=§) a b = BinOp a HasType b

(§.=) :: MkDef
(§.=) a b = BinOp a HasValue b

instance Semigroup TermExpr where (<>) x y = BinOp x Cons y

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

binop op x y = BinOp x (TE2 (op :@ [])) y

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

superSimple1 :: [Stm]
superSimple1 =
  [ (§=§) ("Business" 🔤 12345 📭 typ)
    [ "is_operating" 🔤 23456 📭 bool
    , "bus_name" 💬 "Business Name" 📭 str
    ]
  , (§=§) ("megaCorp" 💬 "Mega Corporation" 📬 "Business")
    [ "is_operating" 📩 L4True
    , "bus_name" 📩 L4S "Mega"
    ]
  ]
data Target = To_TS deriving (Show, Eq)

-- we'll use https://hackage.haskell.org/package/prettyprinter-1.7.0/docs/Prettyprinter.html
-- interface definition
stm2ts :: Stm -> Doc ann
stm2ts (CRule (BinOp (te1 :@ _) HasType ("Type" :@ _)) crb crmw) =
  vsep [ nest 2 (vsep (hsep ["interface", pretty te1, lbrace]
                                :
                                (attrdef To_TS <$> crb)))
          , rbrace ]
-- instance definition
stm2ts (CRule (BinOp (te1 :@ _) HasType (te2 :@ _)) crb crmw) =
  vsep [ nest 2 (vsep (hsep ["let", pretty te1, colon, pretty te2, equals, lbrace]
                                : -- alternatively, use encloseSep, but that does a hanging indent.
                                (punctuate comma (attrdef To_TS <$> crb))))
          , rbrace ]

-- attribute type definition inside interface
attrdef :: Target -> TermExpr -> Doc ann
attrdef To_TS (BinOp (te1 :@ _) HasType (te2 :@_)) =
  pretty te1 <+> colon <+> pretty (typecast To_TS te2) <> semi

-- attribute value definition inside instance
attrdef To_TS (BinOp (te1 :@ _) HasValue te2) =
  pretty te1 <+> colon <+> pretty (showTSval te2)

-- attribute catch-all
attrdef target y = hsep (pretty <$> [ "// attrdef unimplemented: ", show target, show y])

showTSval (Prim L4True) = "true"
showTSval (Prim L4False) = "false"
showTSval (Prim (L4S str)) = "\"" ++ str ++ "\"" -- TODO: need to properly stringify by escaping internal " etc
showTSval x = show x

printStms To_TS stms = mapM_ (putStrLn . show . stm2ts) stms

typecast To_TS "Bool" = "boolean"
typecast To_TS "String" = "string"
typecast target y = y



someFunc :: String -> IO ()
someFunc myinput = do
  let stdinAST = case parse parseL4 "parsing L4 toplevel" (pack myinput) of
                   Left  someError  -> error $ errorBundlePretty someError
                   Right rhs -> rhs
  (opts,runCmd) <-
    simpleOptions "v0.01" "lpapcr34" "an early L4 prototype for section 34" (pure ()) $
    do addSubCommands "pretty" "pretty-print" (
         do addCommand "ast"   "the manual AST" (const (pPrint section34)) (pure ())
            addCommand "baby"  "the baby AST"   (const (pPrint superSimple1)) (pure ())
            addCommand "babyts"  "the baby AST as typescript"   (const (printStms To_TS superSimple1)) (pure ())
            addCommand "stdin" "parsed STDIN"   (const (pPrint stdinAST))  (pure ())
         )
       -- i think there is a bug in optparse-simple, honestly
       addCommand "prolog" "output to prolog" (const (mapM_ (putStrLn . show . stmToProlog) section34)) (pure ())
       addCommand "plain"  "output to plain"  (const (mapM_ (putStrLn .        stmToPlain)  section34)) (pure ())
  runCmd
