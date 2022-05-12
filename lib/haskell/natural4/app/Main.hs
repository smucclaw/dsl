{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified LS as SFL4
import Control.Monad.State
import Control.Applicative
import Data.List
import Options.Generic
import Text.Pretty.Simple (pPrint)

import LS.XPile.CoreL4
-- import LS.XPile.Petri

import qualified LS.XPile.Uppaal as Uppaal
import LS.XPile.Prolog ( sfl4ToProlog )
import LS.XPile.SVG
import LS.XPile.VueJSON
import LS.NLP.NLG (nlg,myNLGEnv)
import qualified Data.Text as Text
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Aeson.Encode.Pretty (encodePretty)
import System.IO.Unsafe (unsafeInterleaveIO)

main :: IO ()
main = do
  opts <- unwrapRecord "mp"
  rc <- SFL4.getConfig opts
  nlgEnv <- unsafeInterleaveIO myNLGEnv -- Only load the NLG environment if we need it.

  rules <- SFL4.dumpRules opts

  when (SFL4.only opts == "petri") $ putStrLn $ Text.unpack $ toPetri rules

  when (SFL4.asJSON rc) $
    putStrLn $ toString $ encodePretty rules
  when (SFL4.toNLG rc && null (SFL4.only opts)) $ do
    naturalLangSents <- mapM (nlg nlgEnv) rules
    mapM_ (putStrLn . Text.unpack) naturalLangSents
  when (SFL4.toBabyL4 rc) $ do
    pPrint $ sfl4ToCorel4 rules
  when (SFL4.toUppaal rc) $ do
    pPrint $ Uppaal.toL4TA rules
    putStrLn $ Uppaal.taSysToString $ Uppaal.toL4TA rules

  when (SFL4.toGrounds rc) $ do
    pPrint $ groundrules rc rules

  when (SFL4.toChecklist rc) $ do
    checkls <- checklist nlgEnv rc rules
    pPrint checkls

  when (SFL4.toProlog rc) $ do
    pPrint $ sfl4ToProlog rules

  when (SFL4.toVue rc) $ do
    putStrLn $ toString $ encodePretty $ toVueRules rules
    -- pPrint $ toVueRules rules

  when (SFL4.only opts `elem` ["", "native"]) $ pPrint rules

-- file2rules :: Opts Unwrapped -> [FileName] -> IO [Rule]
-- file2rules opts

type Parser a = StateT String Maybe a

type Parser' a = String -> Maybe (a, String)

newtype T = T [T] deriving Show

ch :: Char -> Parser Char
ch c = mfilter (== c) $ StateT uncons

parens:: Parser T
parens = T <$ ch '(' <*> many parens <* ch ')'

parse :: StateT s m a -> s -> m (a, s)
parse = runStateT


