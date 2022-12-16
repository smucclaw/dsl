import Test.Hspec
import qualified TestNLG
import qualified MegaparsingSpec
import qualified MegaparsingMeansSpec
import qualified MegaparsingUnlessSpec
import qualified NewParserSpec
import qualified ParserSpec
import qualified SLParserSpec
import qualified PDPASpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser"                ParserSpec.spec
  describe "Megaparsing"           MegaparsingSpec.parserTests
  describe "MegaparsingMeans"      MegaparsingMeansSpec.parserTests
  describe "MegaparsingUnlessSpec" MegaparsingUnlessSpec.parserTests
  describe "NewParserSpec"         NewParserSpec.parserTests
  describe "PDPASpec"              PDPASpec.parserTests
  describe "SLParser"              SLParserSpec.parserTests
  describe "TestNLG"               TestNLG.nlgTests2
