{-# LANGUAGE OverloadedStrings #-}

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
import LS.XPile.Purescript
import LS.XPile.Markdown
import LS.XPile.NaturalLanguage

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
import AnyAll.BoolStruct (alwaysLabeled)
import qualified Data.Foldable as DF
import qualified Text.XML.HXT.Core as HXT
import LS.XPile.DumpRule

myTraceM :: String -> IO ()
myTraceM = SFL4.myTraceM

main :: IO ()
main = do
  opts     <- unwrapRecord "mp"
  rc       <- SFL4.getConfig opts
  nlgEnv   <- unsafeInterleaveIO myNLGEnv
  rules    <- SFL4.dumpRules opts
  iso8601  <- now8601
  let toworkdir   = not $ null $ SFL4.workdir opts
      l4i         = l4interpret SFL4.defaultInterpreterOptions rules
      workuuid    = SFL4.workdir opts <> "/" <> SFL4.uuiddir opts
      (toprologFN,  asProlog)  = (workuuid <> "/" <> "prolog",   show (sfl4ToProlog rules))
      (topetriFN,   asPetri)   = (workuuid <> "/" <> "petri",    Text.unpack $ toPetri rules)
      (toaasvgFN,   asaasvg)   = (workuuid <> "/" <> "aasvg",    AAS.asAAsvg defaultAAVConfig l4i rules)
      (tocorel4FN,  asCoreL4)  = (workuuid <> "/" <> "corel4",   sfl4ToCorel4 rules)
      (tobabyl4FN,  asBabyL4)  = (workuuid <> "/" <> "babyl4",   sfl4ToBabyl4 l4i)
      (toaspFN,     asASP)     = (workuuid <> "/" <> "asp",      sfl4ToASP rules)
      (todmnFN,     asDMN)     = (workuuid <> "/" <> "dmn",      sfl4ToDMN rules)
      (tojsonFN,    asJSONstr) = (workuuid <> "/" <> "json",     toString $ encodePretty             (alwaysLabeled $ onlyTheItems l4i))
      (topursFN,    asPursstr) = (workuuid <> "/" <> "purs",     translate2PS nlgEnv rules)
      (totsFN,      asTSstr)   = (workuuid <> "/" <> "ts",       show (asTypescript rules))
      (togroundsFN, asGrounds) = (workuuid <> "/" <> "grounds",  show $ groundrules rc rules)
      (tomarkdownFN, asMD)     = (workuuid <> "/" <> "md",  markdown nlgEnv rules)
      tochecklFN               =  workuuid <> "/" <> "checkl"
      (toOrgFN,     asOrg)     = (workuuid <> "/" <> "org",      Text.unpack (SFL4.myrender (musings l4i rules)))
      (toNL_FN,     asNatLang) = (workuuid <> "/" <> "natlang",  toNatLang l4i)
      (tonativeFN,  asNative)  = (workuuid <> "/" <> "native",   rules2String l4i rules)

  -- if --workdir is specified, and there are no --only, then we run all the things
  -- however, we can flag specific exclusions by adding the --tomd option which, counterintuitively, disables tomd
  when (toworkdir && not (null $ SFL4.uuiddir opts) && (null $ SFL4.only opts)) $ do

    when (SFL4.tonative  opts) $ mywritefile True toOrgFN      iso8601 "org"  asOrg
    when (SFL4.tonative  opts) $ mywritefile True tonativeFN   iso8601 "hs"   asNative
    when (SFL4.tocorel4  opts) $ mywritefile True tocorel4FN   iso8601 "l4"   asCoreL4
    when (not $ SFL4.tocorel4  opts) $ putStrLn "skipping corel4"
    when (SFL4.tobabyl4  opts) $ mywritefile True tobabyl4FN   iso8601 "l4"   asBabyL4
    when (not $ SFL4.tobabyl4  opts) $ putStrLn "skipping babyl4"
    when (SFL4.toasp     opts) $ mywritefile True toaspFN      iso8601 "lp"   asASP
    when (SFL4.todmn     opts) $ mywritefileDMN True todmnFN   iso8601 "dmn"  asDMN
    when (SFL4.tojson    opts) $ mywritefile True tojsonFN     iso8601 "json" asJSONstr
    when (SFL4.topurs    opts) $ mywritefile True topursFN     iso8601 "purs" asPursstr
    when (SFL4.toprolog  opts) $ mywritefile True toprologFN   iso8601 "pl"   asProlog
    when (SFL4.topetri   opts) $ mywritefile True topetriFN    iso8601 "dot"  asPetri
    when (SFL4.tots      opts) $ mywritefile True totsFN       iso8601 "ts"   asTSstr
    when (SFL4.tonl      opts) $ mywritefile True toNL_FN      iso8601 "txt"  asNatLang
    when (SFL4.togrounds opts) $ mywritefile True togroundsFN  iso8601 "txt"  asGrounds
    when (SFL4.tomd      opts) $ mywritefile True tomarkdownFN iso8601 "md" =<< asMD
    when (SFL4.toaasvg   opts) $ do
      let dname = toaasvgFN <> "/" <> iso8601
      if null asaasvg
        then do
        createDirectoryIfMissing True dname
        appendFile (dname <> "/index.html") "<!-- this file intentionally left blank -->"
        else sequence_
             [ do
               mywritefile False dname (fname<>"-tiny")   ext (show svgtiny)
               mywritefile False dname (fname<>"-full")   ext (show svgfull)
               mywritefile False dname (fname<>"-anyall") "hs"   (TL.unpack $ pShowNoColor hsAnyAllTree)
               mywritefile False dname (fname<>"-anyall") "json" (toString $ encodePretty hsAnyAllTree)
               mywritefile False dname (fname<>"-qtree")  "hs"   (TL.unpack $ pShowNoColor hsQtree)
               mywritefile False dname (fname<>"-qjson")  "json" (toString $ encodePretty hsQtree)
               let fnamext = fname <> "." <> ext
                   displayTxt = Text.unpack $ SFL4.mt2text n
               appendFile (dname <> "/index.html") ("<li> " <> "<a target=\"aasvg\" href=\"" <> fnamext <> "\">" <> displayTxt
                                                    <> "</a></li>\n")
           | (n,(svgtiny,svgfull,hsAnyAllTree,hsQtree)) <- Map.toList asaasvg
           , let (fname, ext) = (take 20 (snakeScrub (SFL4.mtexpr2text <$> n)), "svg")
           ]
      myMkLink iso8601 (toaasvgFN <> "/" <> "LATEST")


    when (SFL4.tocheckl  opts) $ do -- this is deliberately placed here because the nlg stuff is slow to run, so let's leave it for last -- [TODO] move this to below, or eliminate this entirely
        asCheckl <- show <$> checklist nlgEnv rc rules
        mywritefile True tochecklFN   iso8601 "txt" asCheckl
    putStrLn "natural4: output to workdir done"

  -- some transpiler targets are a bit slow to run so we offer a way to call them specifically
  -- natural4-exe --workdir workdir --only md inputfile.csv
  -- will produce only the workdir output file
  when (toworkdir && not (null $ SFL4.uuiddir opts) && (not $ null $ SFL4.only opts)) $ do
    when (SFL4.only opts `elem` ["md", "tomd"]) $ mywritefile True tomarkdownFN iso8601 "md" =<< asMD

  -- when workdir is not specified, --only will dump to STDOUT
  when (not toworkdir) $ do
    when (SFL4.only opts == "petri")  $ putStrLn asPetri
    when (SFL4.only opts == "aatree") $ mapM_ pPrint (getAndOrTree l4i 1 <$> rules)

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

    when (SFL4.toProlog rc) $ pPrint asProlog

    when (SFL4.toTS rc) $ print $ asTypescript rules

    when (SFL4.only opts == "" && SFL4.workdir opts == "") $ pPrint rules
    when (SFL4.only opts == "native")  $ pPrint rules
    when (SFL4.only opts == "classes") $ pPrint (SFL4.classtable l4i)
    when (SFL4.only opts == "symtab")  $ pPrint (SFL4.scopetable l4i)

    when (SFL4.toVue rc) $ do
      putStrLn $ toString $ encodePretty $ itemRPToItemJSON $ toVueRules rules

now8601 :: IO String
now8601 = formatISO8601Millis <$> getCurrentTime

mywritefile :: Bool -> FilePath -> FilePath -> String -> String -> IO ()
mywritefile doLink dirname filename ext s = do
  createDirectoryIfMissing True dirname
  let mypath = dirname <> "/" <> filename     <> "." <> ext
      mylink     = dirname <> "/" <> "LATEST" <> "." <> ext
  writeFile mypath s
  when doLink $ myMkLink (filename <> "." <> ext) mylink

mywritefileDMN :: Bool -> FilePath -> FilePath -> String -> HXT.IOSLA (HXT.XIOState ()) HXT.XmlTree HXT.XmlTree -> IO ()
mywritefileDMN doLink dirname filename ext xmltree = do
  createDirectoryIfMissing True dirname
  let mypath = dirname <> "/" <> filename     <> "." <> ext
      mylink = dirname <> "/" <> "LATEST" <> "." <> ext
  _ <- HXT.runX ( xmltree HXT.>>> HXT.writeDocument [ HXT.withIndent HXT.yes ] mypath )
  when doLink $ myMkLink (filename <> "." <> ext) mylink

myMkLink :: FilePath -> FilePath -> IO ()
myMkLink filename mylink = do
  let mylink_tmp = mylink <> "-tmp"
  createFileLink filename mylink_tmp
  renameFile mylink_tmp mylink

snakeScrub :: [Text.Text] -> String
snakeScrub x = fst $ partition (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-") $
                Text.unpack $
                Text.replace " " "_" $
                Text.intercalate "-" x
