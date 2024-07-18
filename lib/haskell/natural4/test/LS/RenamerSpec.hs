
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LS.RenamerSpec(spec) where


import LS.Rule
import LS.Types
import TextuaL4.Transform
import TextuaL4.LexTextuaL   ( Token )
import TextuaL4.ParTextuaL   ( pRule, pListRule, myLexer )
import Text.Pretty.Simple ( pShowNoColor )
import Text.RawString.QQ ( r )
import Data.List ( intercalate )
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import System.FilePath ( (<.>), (</>) )
import Test.Hspec.Golden
import Test.Hspec (Spec, describe, it, shouldBe)
import Data.Either (fromRight)
import qualified LS.Renamer as Renamer
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Except as Except


goldenGeneric :: Show a => String -> a -> Golden TL.Text
goldenGeneric name output_ = Golden
  { output = pShowNoColor output_
  , encodePretty = TL.unpack
  , writeToFile = TL.writeFile
  , readFromFile = TL.readFile
  , goldenFile =  testPath <.> "expected"
  , actualFile = Just (testPath <.> "actual")
  , failFirstTime = False
  }
  where
    testPath = "test" </> "testdata" </> "golden" </> name

spec :: Spec
spec = do
  describe "Renamer" do
    test' bookWithAttributes "Book Attributes" "decide-with-attributes"
    test' idFunction "Id Function" "id-func"
  where
    test rule = test' rule rule

    test' ruleSource desc fname = do
      let rule :: Rule = fromRight RegBreach $ run ruleSource
      let rnRule :: Either String Renamer.RnRule =
            Except.runExcept (State.evalStateT (Renamer.renameRule rule) Renamer.emptyScope)
      it desc $ goldenGeneric fname $ rnRule

bookWithAttributes = [r|
GIVEN d DECIDE g d IS y
WHERE
    y's book IS green IF d > 0;
    y's book IS red OTHERWISE
|]

idFunction = [r|
GIVEN x
DECIDE id x IS x
|]

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

run :: String -> Either String Rule
run = fmap transRule . pRule . myLexer

runList :: String -> Either String [Rule]
runList = fmap (fmap transRule) . pListRule . myLexer
