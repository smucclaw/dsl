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

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Megaparsing"           Parsing.MegaparsingSpec.parserTests
  describe "MegaparsingMeans"      Parsing.MegaparsingMeansSpec.parserTests
  describe "MegaparsingUnless"     Parsing.MegaparsingUnlessSpec.parserTests
  describe "NewParserSpec"         Parsing.NewParserSpec.parserTests
  describe "SLParser"              Parsing.SLParserSpec.parserTests
  describe "PDPASpec"              Parsing.PDPASpec.parserTests
  describe "BoolStructParser"      Parsing.BoolStructParserSpec.parserTests
  describe "CoreL4Parser"          Parsing.CoreL4ParserSpec.parserTests
  describe "NLG tests"             TestNLG.nlgTests
