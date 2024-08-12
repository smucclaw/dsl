{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

module LS.RenamerSpec (spec) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State.Strict qualified as State
import Data.Either (fromRight)
import Data.List (intercalate)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import LS.Renamer qualified as Renamer
import LS.Rule
import LS.Types
import System.FilePath ((<.>), (</>))
import Test.Hspec (Example (Arg), Spec, SpecWith, describe, it, shouldBe)
import Test.Hspec.Golden
import Text.Pretty.Simple (pShowNoColor)
import Text.RawString.QQ (r)
import Data.String.Interpolate
import TextuaL4.LexTextuaL (Token)
import TextuaL4.ParTextuaL (myLexer, pListRule, pRule)
import TextuaL4.Transform

goldenGeneric :: (Show a) => String -> a -> Golden TL.Text
goldenGeneric name output_ =
  Golden
    { output = pShowNoColor output_
    , encodePretty = TL.unpack
    , writeToFile = TL.writeFile
    , readFromFile = TL.readFile
    , goldenFile = testPath <.> "expected"
    , actualFile = Just (testPath <.> "actual")
    , failFirstTime = False
    }
 where
  testPath = "test" </> "testdata" </> "golden" </> "renamer" </> name

spec :: Spec
spec = do
  describe "Renamer" do
    test'
      "decide-with-attributes"
      [r|
        GIVEN d DECIDE g d IS y
        WHERE
            y's book IS green IF d > 0;
            y's book IS red OTHERWISE
        |]
    test'
      "id-func"
      [r|
        GIVEN x
        DECIDE id x IS x
        |]
    test'
      "id-func-multi"
      [i|
GIVEN x
DECIDE f x IS x
§
GIVEN x
DECIDE g x IS x
        |]
 where
  test' :: String -> String -> SpecWith (Arg (Golden TL.Text))
  test' fname ruleSource = do
    it fname $
      goldenGeneric fname $
        case runList ruleSource of
          Left err -> Left $ "Failed to parse program:\n" <> ruleSource <> "\n" <> err
          Right rules ->
            let
              parse =
                State.evalState
                  (Except.runExceptT (Renamer.runRenamer $ Renamer.renamer rules))
                  Renamer.emptyScope
            in
              case parse of
                Left err -> Left $ "Failed to rename program: " <> err
                Right rnRules -> Right rnRules

runList :: String -> Either String [Rule]
runList = fmap (fmap transRule) . pListRule . myLexer
