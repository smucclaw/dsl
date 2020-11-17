-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Lazy.Encoding (encodeUtf8)
import L4.Executable (prettyPrintParseTree)
import System.FilePath (takeBaseName)
import System.Process (callProcess)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.Program (testProgram)

-- Next goal: Use showbug

main :: IO ()
main = do
  t <- goldenTests
  defaultMain $ testGroup "Main tests" [tests, t]

goldenTests :: IO TestTree
goldenTests = do
  l4Files <- findByExtension [".l4"] "l4/"
  return $
    testGroup
      "Parse l4 golden tests"
      [ goldenVsString
          (takeBaseName l4File) -- Test name
          l4FileGold -- golden file path
          (parseL4File l4File)
        | l4File <- l4Files
        , let l4FileGold = l4File ++ ".gold"
      ]

parseL4File :: FilePath -> IO BL.ByteString
parseL4File f = encodeUtf8 . prettyPrintParseTree <$> readFile f

tests :: TestTree
tests =
  testGroup "SomeName"
    [ runL4ReadmeExample
    ]

runL4ReadmeExample :: TestTree
runL4ReadmeExample =
  testGroup "l4 parses l4 files"
    [ -- testProgram "Run l4 on the readme examples" "l4" ["l4/test.l4"] Nothing
      testProgram "Run l4 on the readme examples" "l4" ["l4/test.l4"] Nothing,
      testProgram "Run l4 on the bike example" "l4" ["l4/deon_bike_meng.l4"] Nothing
    ]

simpleTest :: IO ()
simpleTest = do
  -- _ <- runL4 ["l4/test.l4"] ""
  -- pure ()
  runL4 ["l4/test.l4"]

-- runL4 ["tmp/foo.l4"]

-- runL4 :: [String] -> String -> IO String
-- runL4 = readProcess "l4"
runL4 :: [String] -> IO ()
runL4 = callProcess "l4"

-- stack exec l4 l4/test.l4 > out/test.out 2> out/test.err || stack run showbug l4/test.l4 out/test.out out/test.err

-- TODO:
-- * Run showbug on errors (As library or as executable)
-- * Run L4 as a library (but maybe also keep some integration test)
-- * Use a test framework