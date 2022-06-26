{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified LS as SFL4
import Control.Monad.State
import Control.Applicative
import Data.List
import Data.Time.ISO8601
import Options.Generic
import Text.Pretty.Simple (pPrint)

import LS.XPile.CoreL4
import LS.Interpreter
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
import System.Directory (createDirectoryIfMissing, createFileLink, renameFile)
import Data.Time.Clock (getCurrentTime)

main :: IO ()
main = do
  opts     <- unwrapRecord "mp"
  rc       <- SFL4.getConfig opts
  nlgEnv   <- unsafeInterleaveIO myNLGEnv -- Only load the NLG environment if we need it.
  rules    <- SFL4.dumpRules opts
  iso8601  <- now8601
  let toworkdir   = not $ null $ SFL4.workdir opts
      workuuid    = SFL4.workdir opts <> "/" <> SFL4.uuiddir opts
      (toprologFN,  asProlog)  = (workuuid <> "/" <> SFL4.toprolog  opts,  show (sfl4ToProlog rules))
      (topetriFN,   asPetri)   = (workuuid <> "/" <> SFL4.topetri   opts,  Text.unpack $ toPetri rules)
      (toaasvgFN,   asAAsvg)   = (workuuid <> "/" <> SFL4.toaasvg   opts,  SFL4.aaForSVG <$> SFL4.stitchRules rules)
      (tocorel4FN,  asCoreL4)  = (workuuid <> "/" <> SFL4.tocorel4  opts,  sfl4ToCorel4 rules)
      (tojsonFN,    asJSONstr) = (workuuid <> "/" <> SFL4.tojson    opts,  toString $ encodePretty rules)
      (togroundsFN, asGrounds) = (workuuid <> "/" <> SFL4.togrounds opts,  show $ groundrules rc rules)
      tochecklFN               =  workuuid <> "/" <> SFL4.tocheckl  opts
      (tonativeFN,  asNative)  = (workuuid <> "/" <> SFL4.tonative  opts,  show rules
                                                                           <> "\n\n-- class hierarchy:\n" <> show (classHierarchy rules)
                                                                           <> "\n\n-- symbol table:\n" <> show (symbolTable rules))

  when toworkdir $ do
    putStrLn $ "* outputting to workdir " <> workuuid
    unless (null (SFL4.toprolog  opts)) $ mywritefile toprologFN   iso8601 asProlog
    unless (null (SFL4.topetri   opts)) $ mywritefile topetriFN    iso8601 asPetri
    unless (null (SFL4.tocorel4  opts)) $ mywritefile tocorel4FN   iso8601 asCoreL4
    unless (null (SFL4.tojson    opts)) $ mywritefile tojsonFN     iso8601 asJSONstr
    unless (null (SFL4.toaasvg   opts)) $ mapM_ (\(n,s) -> mywritefile toaasvgFN (iso8601 <> "-" <> show n) s) (zip [1 :: Int ..] asAAsvg)
    unless (null (SFL4.tonative  opts)) $ mywritefile tonativeFN   iso8601 asNative
    unless (null (SFL4.togrounds opts)) $ mywritefile togroundsFN  iso8601 asGrounds
    unless (null (SFL4.tocheckl  opts)) $ do -- this is deliberately placed here because the nlg stuff is slow to run, so let's leave it for last
        asCheckl <- show <$> checklist nlgEnv rc rules
        mywritefile tochecklFN   iso8601 asCheckl

  when (SFL4.only opts == "petri") $ putStrLn asPetri

  when (SFL4.asJSON rc) $ putStrLn $ asJSONstr
  when (SFL4.toNLG rc && null (SFL4.only opts)) $ do
    naturalLangSents <- mapM (nlg nlgEnv) rules
    mapM_ (putStrLn . Text.unpack) naturalLangSents

  when (SFL4.toBabyL4 rc) $ putStrLn $ asCoreL4

  when (SFL4.toUppaal rc) $ do
    pPrint $ Uppaal.toL4TA rules
    putStrLn $ Uppaal.taSysToString $ Uppaal.toL4TA rules

  when (SFL4.toGrounds rc) $ do
    pPrint $ groundrules rc rules

  when (SFL4.toChecklist rc) $ do
    checkls <- checklist nlgEnv rc rules
    pPrint checkls

  when (SFL4.toProlog rc) $ pPrint $ asProlog

  when (SFL4.only opts `elem` ["native"]) $ pPrint rules
  when (SFL4.only opts `elem` ["classes"]) $ pPrint (classHierarchy rules)
  when (SFL4.only opts `elem` ["symtab"])  $ pPrint (symbolTable rules)


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

now8601 :: IO String
now8601 = formatISO8601Millis <$> getCurrentTime

mywritefile :: FilePath -> FilePath -> String -> IO ()
mywritefile dirname filename s = do
  createDirectoryIfMissing True dirname
  let mypath = dirname <> "/" <> filename
      mylink_tmp = mypath <> "-" <> "LATEST"
      mylink     = dirname <> "/" <> "LATEST"
  writeFile mypath s
  -- do the symlink more atomically by renaming
  createFileLink filename mylink_tmp
  renameFile mylink_tmp mylink
  putStrLn $ "** output to " <> mypath
