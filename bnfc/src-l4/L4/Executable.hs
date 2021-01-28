-- Program to test parser, automatically generated by BNF Converter.

module L4.Executable where

import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( when )
import Options.Applicative.Simple

import Text.Pretty.Simple
import qualified Data.Text.Lazy as T
import LexL    ( Token )
import ParL    ( pTops, myLexer )
import SkelL   ()
import PrintL  ( printTree )
import AbsL    ( Tops(..), Rule(..), RuleBody(..), MatchVars(..), Toplevels(..) )
import LayoutL ( resolveLayout )
import ToGraphViz
import L4
import ToGF (bnfc2str)
import PGF (PGF, readPGF)


type Err = Either String
type ParseFun a = [Token] -> Err a

myLLexer = resolveLayout True . myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> PGF -> ParseFun Tops -> InputOpts -> FilePath -> IO ()
runFile v gr p inOpt f = putStrLn f >> readFile f >>= run v gr p inOpt

run :: Verbosity -> PGF -> ParseFun Tops -> InputOpts -> String -> IO ()
run v gr p inOpt s = case p ts of
    Left notTree -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      putStrV v $ show ts
      putStrLn notTree
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      -- output format logic needs to go here
      --case gfOut inOpt of 
        --Just "" -> putStrLn "gf in ENG"
        --Just x  -> putStrLn $ "gf in " <> x
        --Nothing -> pure ()
      if allOutputs inOpt 
        then pure () -- this needs to output everything
        else do
           when (dot inOpt) (showTree "dot" gr v tree) >>
             when (ast inOpt) (showTree "ast" gr v tree) >>
                 when (json inOpt) (showTree "json" gr v tree)


      exitSuccess
  where
  ts = myLLexer s


simpleParseTree :: String  -> Err Tops
simpleParseTree = pTops . myLLexer

prettyPrintParseTree :: String -> Either String T.Text
prettyPrintParseTree = fmap pShowNoColor . simpleParseTree


showTree :: String -> PGF -> Int -> Tops -> IO ()
showTree str gr v tree0
 = let tree = rewriteTree tree0 
       ruleList = getRules tree in do
    case str of 
      "ast" -> do 
        -- ast output
        printMsg "Abstract Syntax" $ T.unpack (pShowNoColor tree)
        printMsg "Linearized tree" $ printTree tree
      -- gf output
      "gf" -> do
        printMsg "In English" $ bnfc2str gr tree
      
      "eh?" -> do 
      -- not quite sure what this is for
        printMsg "Just the Names" $ unlines $ showRuleName <$> ruleList
        printMsg "Dictionary of Name to Rule" $ T.unpack (pShow $ nameList ruleList)
        printMsg "Rule to Exit" $ T.unpack (pShow $ (\r -> (showRuleName r, ruleExits r)) <$> ruleList)
      "png" -> do
      -- i presume this allows png output
        printMsg "As Graph" ""
        printGraph ruleList
      "dot" -> do
      -- dotfile output 
        printMsg "As Dotfile" ""
        putStrLn $ showDot ruleList
        writeFile "graph.dot" (showDot ruleList)
  where
    printMsg msg result = putStrV v $ "\n[" ++ msg ++ "]\n\n" ++ result


rewriteTree :: Tops -> Tops
rewriteTree (Toplevel tops) = Toplevel $ do
  (ToplevelsRule r@(Rule rdef rname asof metalimb rulebody)) <- tops
  ToplevelsRule <$> case rulebody of
    RMatch mvs -> do
      (MatchRule innerRule) <- mvs
      rewrite innerRule
    otherwise -> rewrite r


data InputOpts = InputOpts 
  { allOutputs  :: Bool
  , dot         :: Bool
  , ast         :: Bool
  , json        :: Bool
  , gfOut       :: Maybe String
  , silent      :: Bool
  } deriving Show


optsParse :: Parser InputOpts
optsParse = InputOpts <$>
      switch 
        ( long "all"
       <> short 'a'
       <> help "Generates all possible output formats (natLang defaults to EN)" )
  <*> switch 
        ( long "dot"
       <> help "Enables graphviz DOT language output" )
  <*> switch
        ( long "ast"
       <> help "Enables AST output" )
  <*> switch
        ( long "json"
       <> help "Enables JSON output" )
  <*> optional (strOption 
        ( long "gf" 
       <> help "Generates NLG output in chosen lanugage" 
       <> metavar "<language>" ))
  <*> switch
        ( long "silent"
       <> short 's'
       <> help "Enables silent output" )


main :: IO ()
main = do 
  (opts, ()) <- simpleOptions "VERSION x.xx.x"
                              "l4 - a parser for the l4 language"
                              "\n\nThis is a sample description"
                              optsParse
                              empty
                              
  stdin <- getContents
  let vb = if silent opts then 0 else 2
  gr <- readPGF "src-l4/Top.pgf"
  run vb gr pTops opts stdin



--main :: IO ()
--main = do
  --args <- getArgs
  --gr <- readPGF "src-l4/Top.pgf"
  --case args of
    --["--help"] -> usage
    --[] -> getContents >>= run 2 gr pTops
    --"-s":fs -> mapM_ (runFile 0 gr pTops) fs
    --fs -> mapM_ (runFile 2 gr pTops) fs

--usage :: IO ()
--usage = do
  --putStrLn $ unlines
    --[ "usage: Call with one of the following argument combinations:"
    --, "  --help          Display this help message."
    --, "  (no arguments)  Parse stdin verbosely."
    --, "  (files)         Parse content of files verbosely."
    --, "  -s (files)      Silent mode. Parse content of files silently."
    --]
  --exitFailure
