{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified LS as SFL4
import Control.Monad.State
import Control.Applicative
import Data.List
import Options.Generic
import Text.Pretty.Simple (pPrint)

import LS.XPile.CoreL4
import LS.XPile.Petri

import qualified LS.XPile.Uppaal as Uppaal
import LS.XPile.Prolog
import LS.XPile.SVG
import LS.NLG (nlg)
import qualified Data.Text.Lazy as Text
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Aeson.Encode.Pretty (encodePretty)

main :: IO ()
main = do
  opts <- unwrapRecord "mp"
  rc <- SFL4.getConfig opts

  rules <- SFL4.dumpRules opts

  when (SFL4.only opts == "petri") $ putStrLn $ Text.unpack $ toPetri rules

  when (SFL4.asJSON rc) $
    putStrLn $ toString $ encodePretty rules
  when (SFL4.toNLG rc) $ do
    naturalLangSents <- mapM nlg rules
    mapM_ (putStrLn . Text.unpack) naturalLangSents
  when (SFL4.toBabyL4 rc) $ do
    pPrint $ sfl4ToCorel4 rules
  when (SFL4.toUppaal rc) $ do
    pPrint $ Uppaal.toL4TA rules
    putStrLn $ Uppaal.taSysToString $ Uppaal.toL4TA rules

  when (SFL4.toProlog rc) $ do
    pPrint $ sfl4ToProlog rules

  when (SFL4.only opts == "native") $ pPrint rules

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


