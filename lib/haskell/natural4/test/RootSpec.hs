import Test.Hspec
import qualified TestNLG
import qualified MegaparsingSpec
import qualified ParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser"         ParserSpec.spec
  describe "Megaparsing"    MegaparsingSpec.spec
  describe "TestNLG"        TestNLG.nlgTests2