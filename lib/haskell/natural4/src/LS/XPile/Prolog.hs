{-# LANGUAGE OverloadedStrings #-}

{-| transpiler from NaturaL4 to Prolog. This module useful as a point of
reference for L4's operational semantics. If you know Prolog, you can
understand the meaning of L4 expressions by seeing how they translate
to Prolog.

For more information see also `RelationalPredicates`.
-}

module LS.XPile.Prolog where

import LS as SFL4
import Language.Prolog
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.List.NonEmpty as NE
import AnyAll
import Data.Tree

prologExamples :: [Clause]
prologExamples =
  [ Clause (Struct "foo" [var "Bar", var "Baz"]) [Struct "quux" [var "Bar"], var "Bonk"]
  ]

type Analysis = Map.Map Text.Text Text.Text

sfl4ToProlog :: [SFL4.Rule] -> [Clause]
sfl4ToProlog rs =
  let
    analysis = analyze rs :: Analysis
  in
    concatMap (rule2clause analysis) rs

rule2clause :: Analysis -> SFL4.Rule -> [Clause]
rule2clause st cr@Hornlike {} = hornlike2clauses st (mt2text $ name cr) (clauses cr)
rule2clause st td@TypeDecl { enums = Just ens }    = clpEnums st (mt2text $ name td) ens


rule2clause st td@TypeDecl { has   = rules }
  | rules /= [] = describeDict st (mt2text $ name td) (super td) rules
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
varmt (MTB b) = error $ "you shouldn't varmt a boolean! " ++ show b
varmt (MTN n) = error $ "you shouldn't varmt a number! "  ++ show n

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
rp2goal (RPnary      rprel rp) = pure $ Struct (rel2f rprel) $ rp2goal rp

rel2f :: RPRel -> String
rel2f = Text.unpack . rel2txt

analyze :: [SFL4.Rule] -> Analysis
analyze _rs = Map.fromList [("enumPrimaryKey", "1")] -- sorry, gonna have to read and show this all the time, slightly lame

