{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module LS.XPile.SimalaSpec (spec) where

import Control.Monad.Trans.Except (runExcept)
import Data.String.Interpolate
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as TL
import LS.Renamer qualified as Renamer
import LS.Rule
import LS.XPile.Logging (pShowNoColorS)
import LS.XPile.Simala.Transpile qualified as Simala
import System.FilePath
import Test.Hspec
import Test.Hspec.Golden
import TextuaL4.ParTextuaL qualified as Parser
import TextuaL4.Transform qualified as Parser
import LS.Renamer (RenamerResult(..))

spec :: Spec
spec = do
  describe "Simala" do
    describe "basic" do
      basicTests
      multiRuleTests
    describe "real-world" do
      realWorldTests

basicTests :: Spec
basicTests = do
  transpilerTest
    "function-id"
    [i|
      GIVEN x
      DECIDE id x IS x
      |]
  transpilerTest
    "function-record"
    [i|
      GIVEN d
      DECIDE g d IS y
      WHERE
          y's book IS green IF d > 0;
          y's book IS red OTHERWISE
      |]
  transpilerTest
    "function-sum"
    [i|
      GIVEN x
      DECIDE sum3 x IS SUM(x, x, x)
      |]
  transpilerTest
    "function-selector"
    [i|
      GIVEN x
      DECIDE f x IS x's z
      |]
  transpilerTest
    "function-nested-selector"
    [i|
      GIVEN x
      DECIDE f x IS x's y's z
      |]
  transpilerTest
    "function-with-conditionals-1"
    [i|
      GIVEN x
      DECIDE f x IS 1 IF x > 0;
              f x IS 0 OTHERWISE;
              f x IS 2 IF x < 0
      |]
  transpilerTest
    "function-with-conditionals-2"
    [i|
      GIVEN x
      DECIDE f x IS 1 IF x > 0;
              f x IS 0 OTHERWISE
      |]
  transpilerTest
    "function-with-conditionals-3"
    [i|
      GIVEN x
      DECIDE f x IS 1 IF x > 0;
              f x IS 0
      |]
  transpilerTest
    "function-with-attributes-1"
    [i|
      GIVEN x
      DECIDE f x IS y
      WHERE
        y's z IS 0;
        y's p IS SUM(x, x)
      |]
  transpilerTest
    "function-with-attributes-conditionals-1"
    [i|
      GIVEN x
      DECIDE f x IS y
      WHERE
        y's z IS 5 IF x > 3
      |]
  transpilerTest
    "function-with-attributes-conditionals-2"
    [i|
      GIVEN x
      DECIDE f x IS y
      WHERE
        y's z IS 5 IF x > 3;
        y's z IS 0 OTHERWISE;

        y's p IS x IF x > 5;
        y's p IS SUM(x, x) OTHERWISE
      |]
  transpilerTest
    "giveth"
    [i|
      GIVETH x
      DECIDE x IS y
      WHERE
          y IS 5
      |]
  transpilerTest
    "giveth-record"
    [i|
      GIVETH y
      DECIDE y's z IS 5
      |]
  transpilerTest
    "giveth-record-nested"
    [i|
      GIVETH y
      DECIDE y's a's b's c's z IS 5
      |]
  transpilerTest
    "eragon-book"
    [i|
      GIVETH eragon
      DECIDE
        eragon's title IS Eragon;
        eragon's size IS 512;
        eragon's character's main IS "Eragon";
        eragon's character's villain IS "Galbatorix";
        eragon's character's friend IS "Ork"
      |]
  transpilerTest
    "eragon-book-with-attributes"
    [i|
      GIVETH eragon
      DECIDE
        eragon IS localVar
      WHERE
        localVar's title IS "Eragon";
        localVar's size IS 512;
        localVar's character's main IS "Eragon";
        localVar's character's villain IS "Galbatorix";
        localVar's character's friend IS "Ork"
      |]
  transpilerTest
    "no-giveth-adhoc-y"
    [i|
      DECIDE y IS 5
      |]
  transpilerTest
    "no-giveth-adhoc-y-attribute"
    [i|
      DECIDE y's z IS 5
      |]
  transpilerTest
    "function-nested-builtins"
    [i|
      GIVEN x IS A Number ;
            y IS A Number
      DECIDE x `discounted by` y IS SUM(x, MINUS(1, y))
    |]
  transpilerTest "function-with-name-shadowing"
    [i|
      GIVEN x
      DECIDE f x IS g x
      WHERE (
        GIVEN x DECIDE g x IS x
      )
      |]
  transpilerTest
    "function-with-name-shadowing-with-where-rules-1"
    [i|
    GIVEN x
    DECIDE f x IS y
    WHERE (
      GIVETH y DECIDE y IS g x
      §
      GIVEN d DECIDE g d IS y WHERE y IS SUM(d, d)
    )
    |]
  transpilerTest
    "function-with-name-shadowing-with-where-rules-2"
    [i|
    GIVEN x
    DECIDE f x IS y
    WHERE (
      GIVEN d DECIDE g d IS y WHERE y IS SUM(d, d)
      §
      GIVETH y DECIDE y IS g x
    )
    |]

multiRuleTests :: Spec
multiRuleTests = describe "multi-rules" do
  transpilerTest
    "calls-functions"
    [i|
      GIVEN x DECIDE f x IS x
      §
      GIVEN x DECIDE g x IS f x
      |]
  transpilerTest
    "functions"
    [i|
      GIVEN x DECIDE f x IS x
      §
      GIVEN x DECIDE g x IS x
      |]

realWorldTests :: Spec
realWorldTests = do
  transpilerTest
    "rodents-and-vermin"
    [i|
§ "Rodents and vermin"
DECIDE "Not Covered"
IF
  UNLESS ( "Loss or Damage" IS ANY ( "caused by rodents"
                                    , "caused by insects"
                                    , "caused by vermin"
                                    , "caused by birds"
                                    )

          , ANY ( ALL ( "Loss or Damage" IS "to Contents"
                      , "Loss or Damage" IS "caused by birds"
                      )

                , UNLESS ( "Loss or Damage" IS "ensuing covered loss"

                        , ANY ( "any other exclusion applies"
                              , "an animal caused water to escape from"
                                    ANY ( "a household appliance"
                                        , "a swimming pool"
                                        , "a plumbing, heating, or air conditioning system" )
                              )
                        )
                )
        )
        |]

transpilerTest :: String -> String -> SpecWith (Arg (IO (Golden TL.Text)))
transpilerTest outputName ruleString = it outputName $ do
  result <- case runList ruleString of
    Left err -> pure $
      Text.unlines
        [ "Failed to parse program:"
        , Text.pack ruleString
        , "Err:"
        , Text.pack err
        ]
    Right rules -> do
      Renamer.runRenamerFor mempty rules >>= \case
        RenamerFail err scope ->
          pure $ Text.unlines
            [ "Renaming failed for program:"
            , Text.pack ruleString
            , "Because:"
            , Renamer.renderRenamerError err
            , "Scope table:"
            , Text.pack $ pShowNoColorS scope
            ]
        RenamerSuccess rnRules _ -> pure $
          case runExcept (Simala.runTranspiler $ Simala.transpile rnRules) of
            Left err -> "Failed transpilation:\n" <> Simala.renderTranspilerError err
            Right simalaDecls -> Simala.render simalaDecls
  pure $ goldenGeneric outputName result

goldenGeneric :: String -> Text.Text -> Golden Text.Text
goldenGeneric name output_ =
  Golden
    { output = output_
    , encodePretty = Text.unpack
    , writeToFile = Text.writeFile
    , readFromFile = Text.readFile
    , goldenFile = testPath <.> "expected"
    , actualFile = Just (testPath <.> "actual")
    , failFirstTime = False
    }
 where
  testPath = "test" </> "testdata" </> "golden" </> "xpile" </> "simala" </> name

runList :: String -> Either String [Rule]
runList = fmap (fmap Parser.transRule) . Parser.pListRule . Parser.myLexer
