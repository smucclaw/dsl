{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified LS as SFL4
import Control.Monad.State
import Control.Applicative
import Data.List
import Options.Generic
import Text.Pretty.Simple (pPrint)

import LS.XPile.Prolog


main :: IO ()
main = do
  opts <- unwrapRecord "mp"
  rc <- SFL4.getConfig opts

  rules <- SFL4.dumpRules opts

  when (SFL4.toProlog rc) $ do
    pPrint $ sfl4ToProlog rules

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


