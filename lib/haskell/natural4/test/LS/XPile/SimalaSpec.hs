{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LS.XPile.SimalaSpec (spec) where

import Base (runExceptT)
import Control.Monad.Trans.Except (runExcept)
import Data.String.Interpolate
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import LS.Renamer qualified as Renamer
import LS.Rule
import LS.XPile.Logging (pShowNoColorS)
import LS.XPile.Simala.Transpile qualified as Simala
import System.FilePath
import Test.Hspec
import Test.Hspec.Golden
import Text.Pretty.Simple qualified as Pretty
import TextuaL4.ParTextuaL qualified as Parser
import TextuaL4.Transform qualified as Parser

spec :: Spec
spec = do
  describe "rule transpilation" do
    it "id" $
      runSimalaTranspilerForRule
        "id"
        [i|
            GIVEN x
            DECIDE id x IS x
            |]

    it "bookWithAttributes" $
      runSimalaTranspilerForRule
        "bookWithAttributes"
        [i|
            GIVEN d
            DECIDE g d IS y
            WHERE
                y's book IS green IF d > 0;
                y's book IS red OTHERWISE
            |]

runSimalaTranspilerForRule :: String -> String -> Golden TL.Text
runSimalaTranspilerForRule outputName ruleString = goldenGeneric outputName $
  case run ruleString of
    Left err -> "Failed to parse program:\n" <> ruleString
    Right rule -> do
      case Renamer.renameRuleTopLevel' rule of
        (Left err, scope) ->
          unlines
            [ "Renaming failed for program:"
            , ruleString
            , "Because:"
            , err
            , "Scope table:"
            , pShowNoColorS scope
            ]
        (Right rnRule, _) -> do
          case runExcept (Simala.ruleToSimala rnRule) of
            Left err -> "Failed transpilation:\n" <> err
            Right simala -> Text.unpack $ Simala.render simala

goldenGeneric :: String -> String -> Golden TL.Text
goldenGeneric name output_ =
  Golden
    { output = Pretty.pStringNoColor output_
    , encodePretty = TL.unpack
    , writeToFile = TL.writeFile
    , readFromFile = TL.readFile
    , goldenFile = testPath <.> "expected"
    , actualFile = Just (testPath <.> "actual")
    , failFirstTime = False
    }
 where
  testPath = "test" </> "testdata" </> "golden" </> "xpile" </> "simala" </> name

run :: String -> Either String Rule
run = fmap Parser.transRule . Parser.pRule . Parser.myLexer

runList :: String -> Either String [Rule]
runList = fmap (fmap Parser.transRule) . Parser.pListRule . Parser.myLexer
