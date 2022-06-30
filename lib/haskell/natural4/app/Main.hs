{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where
import qualified LS as SFL4
import Control.Monad.State
import Control.Applicative
import Data.List
import Data.Time.ISO8601
import Options.Generic
import Text.Pretty.Simple (pPrint, pShowNoColor)

import LS.XPile.CoreL4
import LS.Interpreter

import qualified LS.XPile.Uppaal as Uppaal
import LS.XPile.Prolog ( sfl4ToProlog )
import LS.XPile.Petri
import qualified LS.XPile.SVG as AAS
import LS.XPile.VueJSON
import LS.XPile.Typescript
import LS.NLP.NLG (nlg,myNLGEnv)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Map  as Map
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Aeson.Encode.Pretty (encodePretty)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Directory (createDirectoryIfMissing, createFileLink, renameFile)
import Data.Time.Clock (getCurrentTime)
import AnyAll.SVGLadder (defaultAAVConfig)
import Text.RawString.QQ

main :: IO ()
main = do
  opts     <- unwrapRecord "mp"
  rc       <- SFL4.getConfig opts
  nlgEnv   <- unsafeInterleaveIO myNLGEnv -- Only load the NLG environment if we need it.
  rules    <- SFL4.dumpRules opts
  iso8601  <- now8601
  let toworkdir   = not $ null $ SFL4.workdir opts
      l4i         = l4interpret rules
      workuuid    = SFL4.workdir opts <> "/" <> SFL4.uuiddir opts
      (toprologFN,  asProlog)  = (workuuid <> "/" <> SFL4.toprolog  opts,  show (sfl4ToProlog rules))
      (topetriFN,   asPetri)   = (workuuid <> "/" <> SFL4.topetri   opts,  Text.unpack $ toPetri rules)
      (toaasvgFN,   asaasvg)   = (workuuid <> "/" <> SFL4.toaasvg   opts,  AAS.asAAsvg defaultAAVConfig l4i rules)
      (tocorel4FN,  asCoreL4)  = (workuuid <> "/" <> SFL4.tocorel4  opts,  sfl4ToCorel4 rules)
      (tojsonFN,    asJSONstr) = (workuuid <> "/" <> SFL4.tojson    opts,  toString $ encodePretty             (alwaysLabel $ onlyTheItems rules))
      (topursFN,    asPursstr) = (workuuid <> "/" <> SFL4.topurs    opts,  psPrefix <> TL.unpack (pShowNoColor (alwaysLabel $ onlyTheItems rules)) <> "\n\n")
      (totsFN,      asTSstr)   = (workuuid <> "/" <> SFL4.tots      opts,  show (asTypescript rules))
      (togroundsFN, asGrounds) = (workuuid <> "/" <> SFL4.togrounds opts,  show $ groundrules rc rules)
      tochecklFN               =  workuuid <> "/" <> SFL4.tocheckl  opts  
      (tonativeFN,  asNative)  = (workuuid <> "/" <> SFL4.tonative  opts,  show rules
                                                                           <> "\n\n-- class hierarchy:\n" <> show (classHierarchy rules)
                                                                           <> "\n\n-- symbol table:\n" <> show (symbolTable rules))

  when toworkdir $ do
    putStrLn $ "* outputting to workdir " <> workuuid
    unless (null (SFL4.toprolog  opts)) $ mywritefile True toprologFN   iso8601 "pl"   asProlog
    unless (null (SFL4.topetri   opts)) $ mywritefile True topetriFN    iso8601 "dot"  asPetri
    unless (null (SFL4.tocorel4  opts)) $ mywritefile True tocorel4FN   iso8601 "l4"   asCoreL4
    unless (null (SFL4.tojson    opts)) $ mywritefile True tojsonFN     iso8601 "json" asJSONstr
    unless (null (SFL4.topurs    opts)) $ mywritefile True topursFN     iso8601 "purs" asPursstr
    unless (null (SFL4.tots      opts)) $ mywritefile True totsFN       iso8601 "ts"   asTSstr
    unless (null (SFL4.tonative  opts)) $ mywritefile True tonativeFN   iso8601 "hs"   asNative
    unless (null (SFL4.togrounds opts)) $ mywritefile True togroundsFN  iso8601 "txt"  asGrounds
    unless (null (SFL4.toaasvg   opts)) $ sequence_
      [ do
          mywritefile False dname fname ext outstr
          let fnamext = fname <> "." <> ext
              displayTxt = Text.unpack $ Text.unwords n
          appendFile (dname <> "/index.html") ("<li> " <> "<a href=\"" <> fnamext <> "\">" <> displayTxt <> "</a></li>\n")
          myMkLink iso8601 (workuuid <> "/" <> SFL4.toaasvg   opts <> "/" <> "LATEST")
      | (n,s) <- Map.toList asaasvg
      , let (dname, fname, ext) = (toaasvgFN <> "/" <> iso8601, take 20 (snake_scrub n), "svg")
            outstr = show s
      ]
    unless (null (SFL4.tocheckl  opts)) $ do -- this is deliberately placed here because the nlg stuff is slow to run, so let's leave it for last
        asCheckl <- show <$> checklist nlgEnv rc rules
        mywritefile True tochecklFN   iso8601 "txt" asCheckl

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

  when (SFL4.toTS rc) $ print $ asTypescript rules

  when (SFL4.only opts == "" && SFL4.workdir opts == "") $ pPrint rules
  when (SFL4.only opts `elem` ["native"])  $ pPrint rules
  when (SFL4.only opts `elem` ["classes"]) $ print (classHierarchy rules)
  when (SFL4.only opts `elem` ["symtab"])  $ print (symbolTable rules)

  when (SFL4.toVue rc) $ do
    -- putStrLn $ toString $ encodePretty $ rulesToRuleJSON rules
    putStrLn $ toString $ encodePretty $ itemRPToItemJSON $ toVueRules rules
    -- pPrint $ itemRPToItemJSON  $ toVueRules rules


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

mywritefile :: Bool -> FilePath -> FilePath -> String -> String -> IO ()
mywritefile doLink dirname filename ext s = do
  createDirectoryIfMissing True dirname
  let mypath = dirname <> "/" <> filename     <> "." <> ext
      mylink     = dirname <> "/" <> "LATEST" <> "." <> ext
  writeFile mypath s
  -- do the symlink more atomically by renaming
  when doLink $ myMkLink (filename <> "." <> ext) mylink
  putStrLn $ "** output to " <> mypath

myMkLink :: FilePath -> FilePath -> IO ()
myMkLink filename mylink = do
  let mylink_tmp = mylink <> "-tmp"
  createFileLink filename mylink_tmp
  renameFile mylink_tmp mylink
  putStrLn $ "ln -s " <> filename <> " " <> mylink

snake_scrub :: [Text.Text] -> String
snake_scrub x = fst $ partition (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-") $
                Text.unpack $
                Text.replace " " "_" $
                Text.intercalate "-" x

psPrefix :: String -- the stuff at the top of the purescript output
psPrefix = [r|
module RuleLib.PDPADBNO where

import AnyAll.Types
import Data.Maybe
import Data.Tuple
import Prelude

import Data.Map as Map

schedule1_part1 :: ItemJSONStr
schedule1_part1 =
  |]

