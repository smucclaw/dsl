import Test.Hspec
import qualified TestNLG
import qualified ParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser"     ParserSpec.spec
  describe "TestNLG"    TestNLG.nlgTests2