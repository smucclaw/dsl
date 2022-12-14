import Test.Hspec
import qualified TestNLG
import qualified MegaparsingSpec
import qualified MegaparsingMeansSpec
import qualified ParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser"              ParserSpec.spec
  describe "Megaparsing"         MegaparsingSpec.parserTests
  describe "MegaparsingMeans"    MegaparsingMeansSpec.parserTests
  describe "TestNLG"             TestNLG.nlgTests2