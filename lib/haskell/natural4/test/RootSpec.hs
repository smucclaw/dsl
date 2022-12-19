import Test.Hspec
import qualified TestNLG
import qualified MegaparsingSpec
import qualified MegaparsingMeansSpec
import qualified MegaparsingUnlessSpec
import qualified NewParserSpec
import qualified SLParserSpec
import qualified BoolStructParserSpec
import qualified PDPASpec
import qualified CoreL4ParserSpec
import Control.Concurrent.Async
import LS.NLP.NLG (myNLGEnv)
import LS.Types
import System.Environment (lookupEnv)
import Data.Maybe (isJust)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  mpd <- runIO $ lookupEnv "MP_DEBUG"
  mpn <- runIO $ lookupEnv "MP_NLG"
  let runConfig_ = defaultRC {
      debug = isJust mpd,
      runNLGtests = isJust mpn || False
    }

  asyncNlgEnv <- runIO $ async $ putStrLn "Loading env" >> myNLGEnv <* putStrLn "Loaded env"
  nlgEnv <- runIO $ wait asyncNlgEnv

  describe "Megaparsing"           MegaparsingSpec.parserTests
  describe "MegaparsingMeans"      MegaparsingMeansSpec.parserTests
  describe "MegaparsingUnless"     MegaparsingUnlessSpec.parserTests
  describe "NewParserSpec"         NewParserSpec.parserTests
  describe "PDPASpec"              PDPASpec.parserTests
  describe "SLParser"              SLParserSpec.parserTests
  describe "BoolStructParser"      (BoolStructParserSpec.parserTests nlgEnv)
  describe "CoreL4Parser"          CoreL4ParserSpec.parserTests
  describe "TestNLG"               TestNLG.nlgTests2
  describe "NLG tests"             (TestNLG.nlgTests nlgEnv)
