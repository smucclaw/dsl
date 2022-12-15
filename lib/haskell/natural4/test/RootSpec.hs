import Test.Hspec
import qualified TestNLG
import qualified MegaparsingSpec
import qualified MegaparsingMeansSpec
import qualified MegaparsingUnlessSpec
import qualified ParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser"                ParserSpec.spec
  describe "Megaparsing"           MegaparsingSpec.parserTests
  describe "MegaparsingMeans"      MegaparsingMeansSpec.parserTests
  describe "MegaparsingUnlessSpec" MegaparsingUnlessSpec.parserTests
  describe "TestNLG"               TestNLG.nlgTests2
