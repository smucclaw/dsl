module Main where

import qualified LS as SFL4
import Control.Monad.State
import Control.Applicative
import Data.List

type Parser a = StateT String Maybe a

type Parser' a = String -> Maybe (a, String)

newtype T = T [T] deriving Show

ch :: Char -> Parser Char
ch c = mfilter (== c) $ StateT uncons

parens:: Parser T
parens = T <$ ch '(' <*> many parens <* ch ')'

parse :: StateT s m a -> s -> m (a, s)
parse = runStateT

-- >>> parse parens "(()()(()))blerg"
-- Just (T [T [],T [],T [T []]],"blerg")
main :: IO ()
-- main = print $ parse parens "(()()(()))blerg"
main = SFL4.someFunc


