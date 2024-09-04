{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module LS.XPile.PurescriptSpec (spec) where

import Control.Monad (unless)
import Data.Either (lefts, rights)
import Data.Foldable qualified as DF
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import LS qualified as SFL4
import LS.NLP.NLG (NLGEnv, allLangs, langEng, myNLGEnv, printLangs)
import LS.XPile.Logging (XPileLogW, fmapE, mutter, xpLog)
import LS.XPile.Purescript (translate2PS)
import System.FilePath
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Golden ( Golden(..) )
import Prelude hiding (exp, seq)
import LS.Interpreter (l4interpret)
import PGF (Language)
import LS (Interpreted)
data NLGData
  = MkNLGData
  { env :: NLGEnv,
    allEnv :: [NLGEnv]
  }

loadNLGEnv ::  Either [String] Language -> Interpreted -> IO NLGData
loadNLGEnv engE l4i =
      case engE of
        Left err -> do
          error $ unlines $ "natural4: encountered error when obtaining langEng" : err
        Right eng -> do
          (nlgEnv, _nlgEnvErr) <-  xpLog <$> myNLGEnv l4i eng -- Only load the NLG environment if we need it.
          (allNLGEnv, _) <- do
            nlgLangs <- allLangs
            xps <- traverse (myNLGEnv l4i) nlgLangs
            return (xpLog $ sequenceA xps)
          case nlgEnv of
            Left err -> do
              error $ unlines $ "natural4: encountered error while obtaining myNLGEnv" : err
            Right nlgEnvR -> do
              let allNLGEnvErrors = mconcat $ lefts allNLGEnv
              unless (null allNLGEnvErrors) do
                putStrLn "natural4: encountered error while obtaining allNLGEnv"
                DF.traverse_ putStrLn allNLGEnvErrors

              let allNLGEnvR = rights allNLGEnv

              let nlgData =
                    MkNLGData
                      nlgEnvR
                      allNLGEnvR

              pure nlgData

transpileFile :: String -> IO TL.Text
transpileFile filename = do
    let testPath = "test" </> "testdata" </> "golden" </> "PurescriptSpec" </> filename <.> "csv"
        opts = SFL4.defaultOptions {SFL4.file = [testPath]}
    strLangs <-  printLangs allLangs
    rules <- SFL4.dumpRules opts
    l4i <-  l4interpret rules
    (engE, _) <- xpLog <$> langEng
    nlgData <- loadNLGEnv engE l4i

    let justNLGDate = nlgData
        nlgEnvs = justNLGDate.allEnv
        eng = justNLGDate.env
        (psResult, _) = xpLog do
          fmapE (<> ("\n\n" <> "allLang = [\"" <> strLangs <> "\"]")) (translate2PS nlgEnvs eng rules)
 
    case psResult of
        Left err -> do
          error $ unlines $ "natural4: encountered error while obtaining myNLGEnv" : err
        Right goodResult -> do
          pure $ TL.pack goodResult



goldenGeneric :: String -> TL.Text -> Golden TL.Text
goldenGeneric name myoutput =
  Golden
    { output = myoutput,
      encodePretty = TL.unpack,
      writeToFile = TL.writeFile,
      readFromFile = TL.readFile,
      goldenFile = testPath <.> "purs.expected",
      actualFile = Just (testPath <.> "purs.actual"),
      failFirstTime = False
    }
  where
    testPath = "test" </> "testdata" </> "golden" </> "PurescriptSpec" </> name


spec :: Spec
spec = do
  describe "toMathLang for arithRule3" do
    must_sing_purs <- runIO $ transpileFile "must_sing"
    it "convert must sing to Purescript" $ goldenGeneric "must_sing" must_sing_purs