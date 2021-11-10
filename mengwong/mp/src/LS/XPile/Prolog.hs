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
rule2clause st cr@Constitutive { keyword = Means, letbind = lb } =
  let fname = name cr
  in letbind2clause st fname (cond cr) lb
rule2clause st _ = [ mkComment "clause Not Handled" ]

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






