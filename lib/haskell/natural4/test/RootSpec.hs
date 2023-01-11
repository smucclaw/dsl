import Test.Hspec
import qualified TestNLG
import qualified Parsing.MegaparsingSpec
import qualified Parsing.MegaparsingMeansSpec
import qualified Parsing.MegaparsingUnlessSpec
import qualified Parsing.NewParserSpec
import qualified Parsing.SLParserSpec
import qualified Parsing.BoolStructParserSpec
import qualified Parsing.PDPASpec
import qualified Parsing.CoreL4ParserSpec
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

  describe "Megaparsing"           Parsing.MegaparsingSpec.parserTests
  describe "MegaparsingMeans"      Parsing.MegaparsingMeansSpec.parserTests
  describe "MegaparsingUnless"     Parsing.MegaparsingUnlessSpec.parserTests
  describe "NewParserSpec"         Parsing.NewParserSpec.parserTests
  describe "SLParser"              Parsing.SLParserSpec.parserTests
  describe "PDPASpec"              Parsing.PDPASpec.parserTests
  describe "BoolStructParser"      (Parsing.BoolStructParserSpec.parserTests nlgEnv)
  describe "CoreL4Parser"          Parsing.CoreL4ParserSpec.parserTests
  describe "TestNLG"               TestNLG.nlgTests2
  describe "NLG tests"             (TestNLG.nlgTests nlgEnv)
