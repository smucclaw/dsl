{-# LANGUAGE OverloadedStrings #-}

{-| transpiler from NaturaL4 to Prolog. This module useful as a point of
reference for L4's operational semantics. If you know Prolog, you can
understand the meaning of L4 expressions by seeing how they translate
to Prolog.

For more information see also `RelationalPredicates`.
-}

{-|
TODO: Move the following into a README once the transpiler toolchain
is operational.

The transpiler comes in two versions: for Prolog and for sCasp.
For a generation of valid sCasp to work, some very strong assumptions
about Spreadsheet / csv files have been made, and if these 
are not respected, the compiler produces garbage without mercy and warnings:
* Each argument of a predicate has to be written in a separate CSV cell, 
  and so has the predicate itself
* However, arguments can be composite, e.g. function and arguments.
* Also comparison operators have to be written in a separate cell. 
For available comparison operators, see function showLPspecialSCasp
* Assignment of a numerical value to a variable has to be written with IS:
  var IS val   which is rendered as var #= val in sCasp
* Equating two terms (triggerin unification in sCasp / Prolog) has to be written = .
  Attention, in a spreadsheet, the symbol = has to be preceeded by a simple quote to be accepted.
  term1 = term2 is rendered as term1 = term2 in sCasp
  The tokens == and === are synonymes of = and are also rendered as term1 = term2 in sCasp
-}

module LS.XPile.Prolog where

import AnyAll (BoolStruct (All, Any, Leaf, Not), Dot (xPos))
import Data.List.NonEmpty as NE (NonEmpty (..), toList)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as Text
import Prettyprinter
import Prettyprinter.Render.Text (putDoc)
import LS.Rule as SFL4
  ( Rule (Hornlike, TypeDecl, clauses, enums, has, name, super),
  )
import LS.Types as SFL4
  ( BoolStructP,
    BoolStructR,
    EntityType,
    HornClause (hBody, hHead),
    HornClause2,
    MTExpr (..),
    ParamText,
    ParamType (TList0, TList1, TOne, TOptional, TSet0, TSet1),
    RPRel (RPeq),
    RelationalPredicate (..),
    TypeSig (..),
    mt2text,
    mtexpr2text,
    pt2text,
    rel2txt,
    untypePT,
  )
import Language.Prolog (Clause (Clause), Term (Struct, Var, Wildcard, Cut), Atom, var)
import Data.Functor.Classes (showsBinary1)


-- Document generation for Logic Programs 
-- Currently supported: Prolog and SCasp

data TranslationMode = Prolog | SCasp
class ShowLP x where
    showLP :: TranslationMode -> x -> Doc ann

instance ShowLP Clause where
  -- showLP t c = pretty (show c)
  showLP t (Clause lhs []) =
    showLP t lhs <> pretty ("." :: Text.Text)
  showLP t (Clause lhs rhs) =
    showLP t lhs <>
    pretty (":-" :: Text.Text) <>
    nest 4
      (vsep (punctuate comma (map (showLP t) rhs)) <>
      pretty ("." :: Text.Text))
  showLP t c = pretty (show c)

instance ShowLP Term where
  showLP SCasp trm@(Struct atom terms) =
    if showLPIsSpecial atom
    then showLPspecialSCasp atom terms
    else pretty (show trm)
  showLP t trm = pretty (show trm)

showLPIsSpecial :: Atom -> Bool
showLPIsSpecial "IS" = True
showLPIsSpecial "<" = True
showLPIsSpecial "=<" = True
showLPIsSpecial "<=" = True
showLPIsSpecial ">" = True
showLPIsSpecial ">=" = True
showLPIsSpecial "=" = True
showLPIsSpecial "==" = True
showLPIsSpecial _ = False


showLPspecialSCasp :: Atom -> [Term] -> Doc ann
showLPspecialSCasp "IS" = showBinaryInfixSCasp "#="
showLPspecialSCasp "<"  = showBinaryInfixSCasp "#<"
showLPspecialSCasp "=<" = showBinaryInfixSCasp "#=<"
showLPspecialSCasp "<=" = showBinaryInfixSCasp "#=<"
showLPspecialSCasp ">"  = showBinaryInfixSCasp "#>"
showLPspecialSCasp ">=" = showBinaryInfixSCasp "#>="
showLPspecialSCasp "="  = showBinaryInfixSCasp "=" -- non arithmetic equality
showLPspecialSCasp "==" = showBinaryInfixSCasp "#="

showBinaryInfixSCasp :: Text.Text -> [Term] -> Doc ann
showBinaryInfixSCasp sym (trm1:trm2:trms) =
  pretty (show trm1) <>
  pretty (sym :: Text.Text) <>
  pretty (show trm2)

prologExamples :: [Clause]
prologExamples =
  [ Clause (Struct "foo" [var "Bar", var "Baz"]) [Struct "quux" [var "Bar"], var "Bonk"]
  ]

type Analysis = Map.HashMap Text.Text Text.Text

rulesToProlog :: [SFL4.Rule] -> String
rulesToProlog rs = show (vsep (map (showLP Prolog) (sfl4ToLogProg rs)))

rulesToSCasp :: [SFL4.Rule] -> String
rulesToSCasp rs = show (vsep (map (showLP SCasp) (sfl4ToLogProg rs)))

-- Translation of rules to generic logic programming clauses
sfl4ToLogProg :: [SFL4.Rule] -> [Clause]
sfl4ToLogProg rs =
  let
    analysis = analyze rs :: Analysis
  in
    concatMap (rule2clause analysis) rs

-- TODO: not clear what the "Analysis" is good for. 
-- The corresponding parameter seems to be ignored in all called functions.
-- Also see the comment in the "analyze" function further below.
-- TODO: generation of clauses for type declarations has been disabled completely
-- because the code generated for enums is ill-formed (invalid Prolog)

rule2clause :: Analysis -> SFL4.Rule -> [Clause]
rule2clause st cr@Hornlike {} = hornlike2clauses st (mt2text $ name cr) (clauses cr)
rule2clause st TypeDecl {} = []
-- rule2clause st td@TypeDecl { enums = Just ens }    = clpEnums st (mt2text $ name td) ens


-- rule2clause st td@TypeDecl { has   = rules@(_:_) } = describeDict st (mt2text $ name td) (super td) rules
-- https://www.swi-prolog.org/pldoc/man?section=bidicts
-- TypeDecl
--   { name = "Player"
--   , super = Just
--       ( SimpleType TOne "Entity" )
--   , has = Just
--       [
--           ( "yearBorn"
--           , SimpleType TOne "BirthYear"
--           )
--       ,
--           ( "fullName"
--           , SimpleType TOne "Name"
--           )
--       ]

rule2clause st td@TypeDecl { has   = [], super = Just sup }  = pure $ describeParent st (mt2text $ name td) sup
-- [ TypeDecl
--     { name = "Hand"
--     , super = Just
--         ( SimpleType TOne "Chirality" )

rule2clause _st _ = [ mkComment "clause Not Handled" ]

describeDict :: Analysis -> Text.Text -> Maybe TypeSig -> [Rule] -> [Clause]
describeDict st tname mparent rules =
  maybe [] (\parent -> [describeParent st tname parent]) mparent
  ++
  [ Clause (Struct "l4type" [var "class", vart tname, var "attr", vart (mt2text $ name rule), vart typeDesc]) []
  | rule <- rules
  , let typeDesc = maybe "untyped" showtype (super rule)
  ]

showtype :: TypeSig -> EntityType
showtype (SimpleType TOne      tt) = tt
showtype (SimpleType TOptional tt) = "optional("     <> tt <> ")"
showtype (SimpleType TList0    tt) = "listOf("       <> tt <> ")"
showtype (SimpleType TList1    tt) = "nonEmptyList(" <> tt <> ")"
showtype (SimpleType TSet0     tt) = "setOf("        <> tt <> ")"
showtype (SimpleType TSet1     tt) = "nonEmptySet("  <> tt <> ")"
showtype (InlineEnum pt        tt) = showtype (SimpleType pt (inEnums (fmap mtexpr2text <$> untypePT tt)))

inEnums :: NonEmpty (NonEmpty Text.Text) -> Text.Text
inEnums pt = "enums(" <> Text.unwords [ h | (h :| _) <- NE.toList pt ] <> ")"
             -- we gonna need the same writer magic to append top-level output.
             -- in future, run clpEnums
             -- for now, just blurt it out

describeParent :: Analysis -> Text.Text -> TypeSig -> Clause
describeParent _st tname parent =
  Clause (Struct "l4type" [var "class", vart tname, var "extends", vart (showtype parent)]) []

varmt :: MTExpr -> Term
varmt (MTT t) = var (Text.unpack t)
varmt (MTB b) = var (show b)
varmt (MTF n) = var (show n)
varmt (MTI i) = var (show i)

vart, vartl, vartu :: Text.Text -> Term
vart  = var  . Text.unpack
vartl = vart . Text.toLower
vartu = vart . Text.toTitle

vari :: Int -> Term
vari = var . show

clpEnums :: Analysis -> Text.Text -> ParamText -> [Clause]
clpEnums _st tname ens =
  [ Clause (Struct "l4enum" [vartl tname, vari i, vartl (mtexpr2text v)]) []
  | (v :| _, i) <- Prelude.zip (NE.toList $ untypePT ens) [n..] ]
  where n = 1 :: Int
  -- [TODO]: get n out of Analysis which should become a State monad and then use it as a primary index across all enums

mkComment :: String -> Clause
mkComment str = Clause (Struct "comment" [var (Prelude.filter (/= ' ') str)]) []

hornlike2clauses :: Analysis -> Text.Text -> [HornClause2] -> [Clause]
hornlike2clauses _st _fname hc2s =
  [ clause'
  | hc2 <- hc2s
  , let lhses = rp2goal $ hHead hc2
        rhs'   = mbsr2rhs $ hBody hc2
  , lhs' <- lhses
  , let clause' = Clause lhs' rhs'
  ]

bsp2struct :: BoolStructP -> [Term]
bsp2struct (Leaf pt)     = [vart . pt2text $ pt]
bsp2struct (Not  pt)     = vart "neg" : bsp2struct pt
bsp2struct (All _lbl xs) = concatMap bsp2struct xs
bsp2struct (Any _lbl xs) = vart "or" : concatMap bsp2struct xs

bsr2struct :: BoolStructR -> [Term]
bsr2struct (Leaf rt)     = rp2goal rt
bsr2struct (Not  rt)     = vart "neg" : bsr2struct rt
bsr2struct (All _lbl xs) =    concatMap bsr2struct xs
bsr2struct (Any _lbl xs) = vart "or" : concatMap bsr2struct xs

mbsr2rhs :: Maybe BoolStructR -> [Term]
mbsr2rhs Nothing = []
mbsr2rhs (Just bsr) = bsr2struct bsr

rp2goal :: RelationalPredicate -> [Term]
rp2goal (RPParamText _pt)     = pure $ vart "ERROR: rp2goal: paramtext not supported"
rp2goal (RPMT [])            = pure $ vart ""
rp2goal (RPMT [x])           = pure $ varmt x
rp2goal (RPMT (x:xs))        = pure $ Struct (Text.unpack (mtexpr2text x)) (varmt <$> xs)
rp2goal (RPBoolStructR lhs_ _rel bsr) = Struct (Text.unpack $ mt2text lhs_) <$> [bsr2struct bsr]
rp2goal (RPConstraint mt1 rel mt2) = pure $ Struct (rel2f rel) $ (varmt <$> mt1) ++ (varmt <$> mt2)
rp2goal (RPnary      rprel rps) = pure $ Struct (rel2f rprel) (concatMap rp2goal rps)

-- The equality token RPeq has three external appearances: =, ==, ===
-- whose difference is not clear.
-- Here, they are mapped to =, so that the symbol can be used as 
-- Prolog's "unifiable". TODO: a bad hack.
rel2f :: RPRel -> String
rel2f RPeq = "="
rel2f r = Text.unpack (rel2txt r)

analyze :: [SFL4.Rule] -> Analysis
analyze _rs = Map.fromList [("enumPrimaryKey", "1")] -- sorry, gonna have to read and show this all the time, slightly lame

