{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.Prolog where

import LS.Types as SFL4
import Language.Prolog
import qualified Data.Text.Lazy as Text
import qualified Data.Map as Map
import Data.List.NonEmpty as NE
import AnyAll

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
rule2clause st cr@Constitutive { keyword = Means } = letbind2clause st (name cr) (cond cr) (letbind cr)
rule2clause st td@TypeDecl { enums = Just ens }    = clpEnums st (name td) ens
-- [ TypeDecl
--     { name = "Chirality"
--     , enums = Just (
--             ( "Left" :| [] ) :|
--             [ "Right" :| [] ]

rule2clause st td@TypeDecl { has   = Just has }    = describeDict st (name td) (super td) has
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

rule2clause st td@TypeDecl { has   = Nothing, super = Just sup }  = pure $ describeParent st (name td) sup
-- [ TypeDecl
--     { name = "Hand"
--     , super = Just
--         ( SimpleType TOne "Chirality" )

rule2clause st _ = [ mkComment "clause Not Handled" ]

describeDict :: Analysis -> Text.Text -> Maybe TypeSig -> [ParamText] -> [Clause]
describeDict st tname mparent hases =
  maybe [] (\parent -> [describeParent st tname parent]) mparent
  ++
  [ Clause (Struct "l4type" [var "class", vart tname, var "attr", vart (pt2text pt), vart typeDesc]) []
  | pt@((_,ts):|_) <- hases
  , let typeDesc = maybe "untyped" showtype ts
  ]

showtype (SimpleType TOne      tt) = tt
showtype (SimpleType TOptional tt) = "optional("     <> tt <> ")"
showtype (SimpleType TList0    tt) = "listOf("       <> tt <> ")"
showtype (SimpleType TList1    tt) = "nonEmptyList(" <> tt <> ")"
showtype (InlineEnum pt        tt) = showtype (SimpleType pt (inEnums (untypePT tt)))
inEnums pt = "enums(" <> Text.unwords [ h | (h :| _) <- NE.toList pt ] <> ")"          
             -- we gonna need the same writer magic to append top-level output.
             -- in future, run clpEnums
             -- for now, just blurt it out

describeParent :: Analysis -> Text.Text -> TypeSig -> Clause
describeParent st tname parent =
  Clause (Struct "l4type" [var "class", vart tname, var "extends", vart (showtype parent)]) []

vart, vartl, vartu :: Text.Text -> Term
vart  = var  . Text.unpack
vartl = vart . Text.toLower
vartu = vart . Text.toTitle

vari :: Int -> Term
vari = var . show

clpEnums :: Analysis -> Text.Text -> ParamText -> [Clause]
clpEnums st tname ens =
  [ Clause (Struct "l4enum" [vartl tname, vari i, vartl v]) []
  | (v :| _, i) <- Prelude.zip (NE.toList $ untypePT ens) [n..] ]
  where n = 1 :: Int
  -- TODO: get n out of Analysis which should become a State monad and then use it as a primary index across all enums

mkComment :: String -> Clause
mkComment str = Clause (Struct "comment" [var (Prelude.filter (/= ' ') str)]) []

-- TODO: convert the upstream of all this stuff to a HornClause
letbind2clause :: Analysis -> Text.Text -> Maybe BoolStructP -> RelationalPredicate -> [Clause]
letbind2clause st fname cond (RPFunction multiterm) =
  let args = Prelude.filter (/= fname) multiterm
  in pure $ Clause (Struct (Text.unpack fname) (vart <$> args))
     (case cond of
         Nothing  -> []
         Just bsp -> bsp2struct bsp
     )

bsp2struct :: BoolStructP -> [Goal]
bsp2struct (Leaf pt)     = pure (vart . pt2text $ pt)
bsp2struct (Not  pt)     = vart "neg" : bsp2struct pt -- how do you say \+ in Language.Prolog?
bsp2struct (All _lbl xs) =    concatMap bsp2struct xs
bsp2struct (Any _lbl xs) = vart "or" : concatMap bsp2struct xs


analyze :: [SFL4.Rule] -> Analysis
analyze rs = Map.fromList [("enumPrimaryKey", "1")] -- sorry, gonna have to read and show this all the time, slightly lame






