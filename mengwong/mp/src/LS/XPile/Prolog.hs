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

describeDict :: Analysis -> Text.Text -> Maybe TypeSig -> [(ConstitutiveName, TypeSig)] -> [Clause]
describeDict st tname mparent hases =
  maybe [] (\parent -> [describeParent st tname parent]) mparent
  ++
  [ Clause (Struct "l4type" [var "class", vart tname, var "attr", vart k, vart typeDesc]) []
  | (k, t) <- hases
  , let typeDesc = showtype t
  ]

showtype (SimpleType TOne      tt) = tt
showtype (SimpleType TOptional tt) = "optional("     <> tt <> ")"
showtype (SimpleType TList0    tt) = "listOf("       <> tt <> ")"
showtype (SimpleType TList1    tt) = "nonEmptyList(" <> tt <> ")"
showtype (InlineEnum pt        tt) = showtype (SimpleType pt (inEnums tt))
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
  | (v :| _, i) <- Prelude.zip (NE.toList ens) [n..] ]
  where n = 1 :: Int
  -- TODO: get n out of Analysis which should become a State monad and then use it as a primary index across all enums

mkComment :: String -> Clause
mkComment str = Clause (Struct "comment" [var (Prelude.filter (/= ' ') str)]) []

letbind2clause :: Analysis -> Text.Text -> Maybe BoolStructP -> Item ParamText -> [Clause]
letbind2clause st fname cond (Leaf pt) = fmap (fundef st fname cond) (NE.toList pt)
letbind2clause st fname cond _         = [ mkComment "cant Handle Nonleaf Items Yet" ]

fundef :: Analysis -> Text.Text -> Maybe BoolStructP -> NonEmpty Text.Text -> Clause
fundef st fname cond pt0
  | NE.length pt0 == 3 = let args = Prelude.filter (/= fname) $ NE.toList pt0
                         in Clause (Struct (Text.unpack fname) (var . Text.unpack <$> args))
                            (case cond of
                              Nothing -> []
                              _       -> [])
  | otherwise = mkComment $ "fundef Unimplemented For Input Lists Of Length " ++ show (NE.length pt0)


analyze :: [SFL4.Rule] -> Analysis
analyze rs = mempty






