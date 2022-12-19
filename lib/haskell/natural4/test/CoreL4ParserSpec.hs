{-# LANGUAGE OverloadedStrings #-}
module CoreL4ParserSpec where

-- import qualified Test.Hspec.Megaparsec as THM
import Text.Megaparsec
import LS.Lib
import LS.Interpreter
import AnyAll hiding (asJSON)
import LS.BasicTypes
import LS.Types
import LS.XPile.CoreL4
import Test.Hspec
import qualified Data.ByteString.Lazy as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Hspec.Megaparsec (shouldParse)

filetest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
filetest testfile desc parseFunc expected =
  it (testfile ++ ": " ++ desc ) $ do
  testcsv <- BS.readFile ("test/" <> testfile <> ".csv")
  parseFunc testfile `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]

parserTests :: Spec
parserTests  = do
    let runConfig = defaultRC { sourceURL = "test/Spec" }
        runConfigDebug = runConfig { debug = True }
    let  combine (a,b) = a ++ b
    let _parseWith1 f x y s = f <$> runMyParser combine runConfigDebug x y s
    let  parseR       x y s = runMyParser combine runConfig x y s
    let _parseR1      x y s = runMyParser combine runConfigDebug x y s
    let _parseOther1  x y s = runMyParser id      runConfigDebug x y s
    let  parseOther   x y s = runMyParser id      runConfig x y s

    -- [TODO] it'd be nice to get this working as a filetest rather than the manual way
    describe "transpiler to CoreL4" $ do
      xit "should output a class declaration for seca.csv" $ do
        let testfile = "seca"
        testcsv <- BS.readFile ("test/" <> testfile <> ".csv")
        let rules  = parseR pRules "" `traverse` (exampleStreams testcsv)
        (fmap sfl4ToCorel4 <$> rules) `shouldParse` ["\n#\n# outputted directly from XPile/CoreL4.hs\n#\n\n\n\n-- [SecA_RecoverPassengersVehicleAuthorizedOp]\ndecl s: Situation\n\n--facts\n\nfact <SecA_RecoverPassengersVehicleAuthorizedOp> fromList [([\"s\"],((Just (SimpleType TOne \"Situation\"),[]),[]))]\n\n\n# directToCore\n\n\nrule <SecA_RecoverPassengersVehicleAuthorizedOp>\nfor s: Situation\nif (secA_Applicability && currentSit_s && s == missingKeys)\nthen coverProvided s recoverPassengersVehicleAuthorizedOp SecA_RecoverPassengersVehicleAuthorizedOp\n\n\n"]

      filetest "class-1" "type definitions"
        (parseR pRules)
        [TypeDecl {name = ["Class1"], super = Just (SimpleType TOne "Object"), has = [TypeDecl {name = ["id"], super = Just (SimpleType TOne "Integer"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []}], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), defaults = [], symtab = []},TypeDecl {name = ["Class2"], super = Just (SimpleType TOne "Class1"), has = [TypeDecl {name = ["firstname"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},TypeDecl {name = ["lastname"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},TypeDecl {name = ["office address"], super = Nothing, has = [TypeDecl {name = ["line1"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},TypeDecl {name = ["line2"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []}], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},TypeDecl {name = ["bar address"], super = Just (SimpleType TOne "address"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},TypeDecl {name = ["work address"], super = Just (SimpleType TOne "address"), has = [TypeDecl {name = ["floor"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},TypeDecl {name = ["company name"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []}], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []}], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 4, version = Nothing}), defaults = [], symtab = []},TypeDecl {name = ["address"], super = Nothing, has = [TypeDecl {name = ["line1"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},TypeDecl {name = ["line2"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []}], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 15, version = Nothing}), defaults = [], symtab = []}]

      filetest "class-1" "parent-class identification"
        (parseOther (do
                        rules <- some pTypeDeclaration
                        let classH = classHierarchy rules
                            parent = clsParent classH "Class2"
                        return $ parent))
        (Just "Class1",[])

      filetest "class-1" "attribute enumeration"
        (parseOther (do
                        rules <- some pTypeDeclaration
                        let classH = classHierarchy rules
                            tA = getCTkeys <$> thisAttributes classH "Class2"
                        return $ tA))
        (Just ["bar address","firstname","lastname","office address","work address"], [])

      filetest "class-1" "extended attribute enumeration"
        (parseOther (do
                        rules <- some pTypeDeclaration
                        let classH = classHierarchy rules
                            eA = getCTkeys <$> extendedAttributes classH "Class2"
                        return $ eA))
        (Just ["bar address","firstname","id","lastname","office address","work address"], [])

      filetest "class-fa-1" "financial advisor data modelling"
        (parseR pToplevel) [
          TypeDecl { name = ["FinancialStatus"], super = Just (InlineEnum TOne (("adequate" :| ["inadequate"],Nothing) :| [])), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 1, version = Nothing}), defaults = [], symtab = []},
          TypeDecl {name = ["EarningsStatus"], super = Just (InlineEnum TOne (("steady" :| ["unsteady"],Nothing) :| [])), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 2, version = Nothing}), defaults = [], symtab = []},
          TypeDecl {name = ["InvestmentStrategy"], super = Just (InlineEnum TOne (("savings" :| ["stocks","combination"],Nothing) :| [])), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 3, version = Nothing}), defaults = [], symtab = []},
          TypeDecl {name = ["Person"], super = Nothing, has = [
            TypeDecl {name = ["dependents"], super = Just (SimpleType TOne "Number"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["amountSaved"], super = Just (SimpleType TOne "Number"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["earnings"], super = Just (SimpleType TOne "Number"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["steadiness"], super = Just (SimpleType TOne "EarningsStatus"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["income"], super = Just (SimpleType TOne "FinancialStatus"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["savingsAccount"], super = Just (SimpleType TOne "FinancialStatus"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["isDead"], super = Just (SimpleType TOne "Boolean"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["spendthrift"], super = Just (SimpleType TOne "Boolean"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["investment"], super = Just (SimpleType TOne "InvestmentStrategy"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []}],
            enums = Nothing, given = Nothing, upon = Nothing, rlabel = Just ("\167",2,"person type"), lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 5, version = Nothing}), defaults = [], symtab = []}
          ]

      filetest "class-fa-2" "financial advisor decision modelling"
        (parseR pToplevel)
        [ Hornlike {
          name = ["p", "investment"],
          super = Nothing,
          keyword = Decide,
          given = Just (("p" :| [], Just (SimpleType TOne "Person")) :| []),
          upon = Nothing,
          clauses =
            [ HC {hHead = RPConstraint ["p", "investment"] RPis ["savings"], hBody = Just (mkLeaf (RPConstraint ["p", "savingsAccount"] RPis ["inadequate"]))},
              HC {hHead = RPConstraint ["p", "investment"] RPis ["stocks"], hBody = Just (All Nothing [mkLeaf (RPConstraint ["p", "savingsAccount"] RPis ["adequate"]), mkLeaf (RPConstraint ["p", "income"] RPis ["adequate"])])},
              HC {hHead = RPConstraint ["p", "investment"] RPis ["combination"], hBody = Just (mkLeaf (RPMT ["OTHERWISE"]))},
              HC {hHead = RPConstraint ["p", "minSavings"] RPis ["p's dependents", "*", "5000"], hBody = Nothing},
              HC {hHead = RPConstraint ["p", "savingsAccount"] RPis ["adequate"], hBody = Just (mkLeaf (RPConstraint ["p", "amountSaved"] RPgt ["p", "minSavings"]))},
              HC {hHead = RPConstraint ["p", "savingsAccount"] RPis ["inadequate"], hBody = Just (mkLeaf (RPMT ["OTHERWISE"]))},
              HC {hHead = RPConstraint ["p", "minIncome"] RPis ["15000 + 4000 * p's dependents"], hBody = Nothing},
              HC {hHead = RPConstraint ["p", "income"] RPis ["adequate"], hBody = Just (All Nothing [mkLeaf (RPConstraint ["p", "earnings"] RPgt ["p", "minIncome"]), mkLeaf (RPConstraint ["p", "steadiness"] RPis ["steady"])])},
              HC {hHead = RPConstraint ["p", "blah"] RPis ["42"], hBody = Nothing}
            ],
          rlabel = Nothing,
          lsource = Nothing,
          srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}),
          defaults = [],
          symtab = []
        }
        ]

srcrow_, srcrow1', srcrow1, srcrow2, srccol1, srccol2 :: Rule -> Rule
srcrow', srccol' :: Int -> Rule -> Rule
srcrow_   w = w { srcref = Nothing, hence = srcrow_ <$> (hence w), lest = srcrow_ <$> (lest w) }
srcrow1'  w = w { srcref = (\x -> x  { srcrow = 1 }) <$> srcref defaultReg }
srcrow1     = srcrow' 1
srcrow2     = srcrow' 2
srcrow' n w = w { srcref = (\x -> x  { srcrow = n }) <$> srcref w }
srccol1     = srccol' 1
srccol2     = srccol' 2
srccol' n w = w { srcref = (\x -> x  { srccol = n }) <$> srcref w }
