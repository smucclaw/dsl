{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Golden
import Text.Pretty.Simple (pShowNoColor)
import Text.RawString.QQ (r)
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
      "Book Attributes"
      "decide-with-attributes"
      [r|
        GIVEN d DECIDE g d IS y
        WHERE
            y's book IS green IF d > 0;
            y's book IS red OTHERWISE
        |]
    test'
      "Id Function"
      "id-func"
      [r|
        GIVEN x
        DECIDE id x IS x
        |]
 where
  test rule = test' rule rule

  test' desc fname ruleSource = do
    let
      rule :: Rule = fromRight RegBreach $ run ruleSource
    let
      rnRule :: Either String Renamer.RnRule =
        State.evalState (Except.runExceptT (Renamer.runRenamer $ Renamer.renameRule rule)) Renamer.emptyScope
    it desc $ goldenGeneric fname $ rnRule

type Err = Either String
type ParseFun a = [Token] -> Err a
type Verbosity = Int

run :: String -> Either String Rule
run = fmap transRule . pRule . myLexer

runList :: String -> Either String [Rule]
runList = fmap (fmap transRule) . pListRule . myLexer
