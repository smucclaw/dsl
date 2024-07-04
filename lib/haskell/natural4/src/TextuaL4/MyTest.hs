-- File generated by the BNF Converter (bnfc 2.9.5).

-- | Program to test parser.


module TextuaL4.MyTest where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, mapM_, putStrLn
  )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )
import Data.Text.Lazy qualified as Text
import TextuaL4.AbsTextual qualified as TL4
import TextuaL4.Transform
import TextuaL4.LexTextual   ( Token, mkPosToken )
import TextuaL4.ParTextual   ( pRule, myLexer )
import TextuaL4.PrintTextual ( Print, printTree )
import Text.Pretty.Simple (pShowNoColor)

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

run :: Verbosity -> ParseFun TL4.Rule -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      showTree v tree
      putStrLn "\n\n[Transformed into L4 tree]\n"
      let l4tree = transRule tree
      putStrLn $ Text.unpack $ pShowNoColor l4tree

 where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]

main :: String -> IO ()
main = run 2 pRule

-- Things to try out in ghci
-- main "EVERY Person WHO Qualifies MEANS ALL(walk, ANY(eat, drink), dance) MUST sing"
-- main "Drinks MEANS consumes ANY (alcoholic, non-alcoholic) beverage"

