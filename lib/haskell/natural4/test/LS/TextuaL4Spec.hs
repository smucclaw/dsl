{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module LS.TextuaL4Spec (spec) where

import LS.Rule
import LS.Types
import TextuaL4.AbsTextuaL qualified as TL4
import TextuaL4.Transform
import TextuaL4.LexTextuaL   ( Token )
import TextuaL4.ParTextuaL   ( pRule, myLexer )
import TextuaL4.PrintTextuaL ( Print, printTree )
import Text.Pretty.Simple (pShowNoColor)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import System.FilePath ( (<.>), (</>) )
import Test.Hspec.Golden
import Test.Hspec (Spec, describe, it, shouldBe)

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
  describe "TextuaL4 output" do
    test "GIVEN foo GIVETH bar DECIDE bar IS 1 > 2" "hornlike-given-giveth-1"
    test "GIVEN foo IS A Number GIVETH bar IS A Boolean DECIDE bar IS foo > 2" "hornlike-given-giveth-2"
    test "EVERY tame ANY (Person, Animal) WHO Qualifies MEANS ALL(walks, ANY(eats, drinks), climbs) MUST ANY (sing, dance)" "regulative-any-tame-person-animal.expected"
  where
    test rule fname =
      it rule $ goldenGeneric fname $ do
        run rule

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

run :: String -> Either String Rule
run = fmap transRule . pRule . myLexer


