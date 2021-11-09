{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified LS as SFL4
import Control.Monad.State
import Control.Applicative
import Data.List
import Options.Generic



main :: IO ()
main = do
  opts <- unwrapRecord "mp"
  SFL4.someFunc opts


type Parser a = StateT String Maybe a

type Parser' a = String -> Maybe (a, String)

newtype T = T [T] deriving Show

ch :: Char -> Parser Char
ch c = mfilter (== c) $ StateT uncons

parens:: Parser T
parens = T <$ ch '(' <*> many parens <* ch ')'

parse :: StateT s m a -> s -> m (a, s)
parse = runStateT


