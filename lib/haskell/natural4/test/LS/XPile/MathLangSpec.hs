{-# LANGUAGE OverloadedStrings #-}
module LS.XPile.MathLangSpec where

import Data.HashMap.Strict qualified as Map
import Explainable.MathLang
import LS.Rule
import LS.XPile.MathLang (toMathLang)
import Test.Hspec
import LS hiding (RunConfig(..))
import AnyAll

-- spec :: Spec
-- spec = describe "toMathLang" $ do
--   it "should convert Interpreted to MyState" $ do
--     let rule =
--           Hornlike
--             { name = [MTT "foo"],
--               super = Nothing,
--               keyword = Decide,
--               given = Nothing,
--               giveth = Nothing,
--               upon = Nothing,
--               clauses =
--                 [ HC
--                     { hHead = RPMT [MTT "foo"],
--                       hBody = Just (All Nothing [Leaf (RPMT [MTT "bar"]), Leaf (RPMT [MTT "baz"]), Leaf (RPMT [MTT "quux"])])
--                     }
--                 ],
--               rlabel = Nothing,
--               lsource = Nothing,
--               wwhere = [],
--               srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}),
--               defaults = [],
--               symtab = []
--             }
--         interpreted :: Interpreted
--         interpreted = l4interpret LS.defaultInterpreterOptions [rule]
--         output :: Pred Float
--         output = PredBin Nothing PredAnd (PredVar "foo")
--                                   (PredBin Nothing PredAnd (PredVar "bar")
--                                                            (PredBin Nothing PredAnd (PredVar "baz")
--                                                                                     (PredVar "quux")))
--         expected :: MyState
--         expected = emptyState {symtabP = Map.fromList [("must sing", output)]}

--     toMathLang interpreted `shouldBe` expected
