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
import System.IO.Unsafe (unsafeInterleaveIO)
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Golden ( Golden(..) )
import Prelude hiding (exp, seq)
import LS.Interpreter (l4interpret)

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

data NLGData
  = MkNLGData
  { env :: NLGEnv,
    allEnv :: [NLGEnv],
    allEnvErr :: XPileLogW,
    engErr :: XPileLogW
  }

spec :: Spec
spec = do
  describe "toMathLang for arithRule3" do
    let testPath = "test" </> "testdata" </> "golden" </> "PurescriptSpec" </> "must_sing.csv"
        opts = SFL4.defaultOptions {SFL4.file = [testPath]}
    nlgLangs <- runIO allLangs
    strLangs <- runIO $ printLangs allLangs
    rules <- runIO (SFL4.dumpRules opts)
    l4i <- runIO $ l4interpret rules
    (engE, engErr) <- runIO $ xpLog <$> langEng
    (_, nlgData) <- runIO $
      case engE of
        Left err -> do
          putStrLn $ unlines $ "natural4: encountered error when obtaining langEng" : err
          pure (Nothing, Nothing)
        Right eng -> do
          (nlgEnv, _nlgEnvErr) <- unsafeInterleaveIO $ xpLog <$> myNLGEnv l4i eng -- Only load the NLG environment if we need it.
          (allNLGEnv, allNLGEnvErr) <- unsafeInterleaveIO do
            xps <- traverse (myNLGEnv l4i) nlgLangs
            return (xpLog $ sequenceA xps)

          case nlgEnv of
            Left err -> do
              putStrLn $ unlines $ "natural4: encountered error while obtaining myNLGEnv" : err
              pure (Nothing, Nothing)
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
                      allNLGEnvErr
                      engErr

              pure (Just nlgEnvR, Just nlgData)
    let Just justNLGDate = nlgData
        nlgEnvs = justNLGDate.allEnv
        eng = justNLGDate.env
        (psResult, _) = xpLog do
          mutter "* main calling translate2PS"
          fmapE (<> ("\n\n" <> "allLang = [\"" <> strLangs <> "\"]")) (translate2PS nlgEnvs eng rules)
        (Right justResult) = psResult
        finalResult = TL.pack justResult
    it "convert must sing to Purescript" $ goldenGeneric "must_sing" finalResult