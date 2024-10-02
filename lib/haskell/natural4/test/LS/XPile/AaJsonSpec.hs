{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module LS.XPile.AaJsonSpec (spec) where

import Control.Monad (unless)
import Control.Monad.Trans.Except (runExceptT)
import Data.Either (lefts, rights)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import System.FilePath
import System.IO.Unsafe (unsafeInterleaveIO)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Golden ( Golden(..) )
import Prelude hiding (exp, seq)
import LS.Interpreter (l4interpret)
import PGF (Language)

import LS qualified as SFL4
import LS.Log
import LS.NLP.NLG (NLGEnv, allLangs, langEng, myNLGEnv, NlgLog, printLangs)
import LS.XPile.Logging (xpLog, fmapE)
import LS.XPile.AaJson (translate2AaJson)

data NLGData
  = MkNLGData
  { env :: NLGEnv,
    allEnv :: [NLGEnv]
  }

loadNLGEnv :: IOTracer NlgLog -> Either [Text] Language -> IO NLGData
loadNLGEnv tracer engE =
      case engE of
        Left err -> do
          fail $ Text.unpack $ Text.unlines $ "natural4: encountered error when obtaining langEng" : err
        Right eng -> do
          nlgEnv <- runExceptT $ myNLGEnv tracer eng
          allNLGEnv <- do
            nlgLangs <- allLangs
            traverse (runExceptT . myNLGEnv tracer) nlgLangs
          case nlgEnv of
            Left err -> do
              fail $ Text.unpack $ Text.unlines $ "natural4: encountered error while obtaining myNLGEnv" : err
            Right nlgEnvR -> do
              let allNLGEnvErrors = mconcat $ lefts allNLGEnv
              unless (null allNLGEnvErrors) do
                fail $ Text.unpack $ Text.unlines $ "natural4: encountered error while obtaining allNLGEnv" : allNLGEnvErrors

              let allNLGEnvR = rights allNLGEnv

              let nlgData =
                    MkNLGData
                      nlgEnvR
                      allNLGEnvR

              pure nlgData

transpileFile :: String -> IO TL.Text
transpileFile filename = do
    let tracer =
          -- Use the 'prettyTracer' if you need logs for debugging
          -- prettyTracer
          mempty
    let testPath = "test" </> "testdata" </> "golden" </> "AaJsonSpec" </> filename <.> "csv"
        opts = SFL4.defaultOptions {SFL4.file = [testPath]}
    rules <- SFL4.dumpRules opts
    l4i <-  l4interpret rules
    engE <- runExceptT $ langEng tracer
    nlgData <- loadNLGEnv tracer engE
    strLangs <- unsafeInterleaveIO $ printLangs allLangs
    let justNLGDate = nlgData
        nlgEnvs = justNLGDate.allEnv
        eng = justNLGDate.env
        (psResult, _) = xpLog do
            fmapE (<> ("\n\n" <> "allLang = [\"" <> strLangs <> "\"]")) (translate2AaJson nlgEnvs eng l4i)

    case psResult of
        Left err -> do
          fail $ unlines $ "natural4: encountered error while obtaining myNLGEnv" : err
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
    testPath = "test" </> "testdata" </> "golden" </> "AaJsonSpec" </> name


spec :: Spec
spec = do
  describe "AaJson transpiler" do
    describe "must_sing" do
      it "convert must sing to AaJson" do
        must_sing_purs <- transpileFile "must_sing"
        pure $ goldenGeneric "must_sing" must_sing_purs

    describe "rodents" do
      it "convert must sing to AaJson" do
        rodents_purs <- transpileFile "rodents"
        pure $ goldenGeneric "rodents" rodents_purs
