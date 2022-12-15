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
import LS.XPile.Purescript
import LS.XPile.Markdown
import LS.XPile.NaturalLanguage

import LS.NLP.NLG (nlg,myNLGEnv)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Map  as Map
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy.Char8 as Byte (ByteString, writeFile)
import Data.Aeson.Encode.Pretty (encodePretty)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Directory (createDirectoryIfMissing, createFileLink, renameFile)
import Data.Time.Clock (getCurrentTime)
import AnyAll.SVGLadder (defaultAAVConfig)
import AnyAll.BoolStruct (alwaysLabeled)
import qualified Text.RawString.QQ as QQ
import qualified Data.Foldable as DF
-- import qualified Data.Traversable as DT

myTraceM :: String -> IO ()
myTraceM = SFL4.myTraceM

main :: IO ()
main = do
  opts     <- unwrapRecord "mp"
  rc       <- SFL4.getConfig opts
  nlgEnv   <- unsafeInterleaveIO myNLGEnv -- Only load the NLG environment if we need it.
--  putStrLn "main: doing dumpRules"
  rules    <- SFL4.dumpRules opts
--  putStrLn "main: done with dumpRules"
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
      (tojsonFN,    asJSONstr) = (workuuid <> "/" <> "json",     toString $ encodePretty             (alwaysLabeled $ onlyTheItems l4i))
      (topursFN,    asPursstr) = (workuuid <> "/" <> "purs",     psPrefix <> TL.unpack (maybe "-- nothing" (pShowNoColor . alwaysLabeled) (biggestItem l4i rules)) <> "\n\n" <>
                                                                 psSuffix <> "\n\n" <>
                                                                 asPurescript l4i)
      (totsFN,      asTSstr)   = (workuuid <> "/" <> "ts",       show (asTypescript rules))
      (togroundsFN, asGrounds) = (workuuid <> "/" <> "grounds",  show $ groundrules rc rules)
      (tomarkdownFN, asMD) = (workuuid <> "/" <> "md",  markdown nlgEnv rules)
      tochecklFN               =  workuuid <> "/" <> "checkl"
      (toOrgFN,     asOrg)     = (workuuid <> "/" <> "org",      Text.unpack (SFL4.myrender (musings l4i rules)))
      (toNL_FN,     asNatLang) = (workuuid <> "/" <> "natlang",  toNatLang l4i)
      (tonativeFN,  asNative)  = (workuuid <> "/" <> "native",   unlines
                                   [ "-- original rules:\n"
                                   , TL.unpack (pShowNoColor rules)

                                   , "-- variable-substitution expanded AnyAll rules\n"
                                   , TL.unpack (pShowNoColor $ [ r { SFL4.clauses = expandClauses l4i 1 (SFL4.clauses r) }
                                                               | r@SFL4.Hornlike{} <- rules
                                                               ])

                                   , "\n\n-- class hierarchy:\n"
                                   , TL.unpack (pShowNoColor (SFL4.classtable l4i))

                                   , "\n\n-- symbol table:\n"
                                   , TL.unpack (pShowNoColor (SFL4.scopetable l4i))

                                   , "-- getAndOrTrees"
                                   , unlines $ (\r -> "\n-- " <> show (SFL4.ruleLabelName r) <> "\n" <>
                                                 TL.unpack (pShowNoColor $ getAndOrTree l4i 1 r)) <$> rules

                                   , "-- traverse toList of the getAndOrTrees"
                                   , unlines $ TL.unpack . pShowNoColor . traverse DF.toList . getAndOrTree l4i 1 <$> rules

                                   , "-- onlyTheItems"
                                   , TL.unpack $ pShowNoColor (onlyTheItems l4i)

                                   , "-- ItemsByRule"
                                   , TL.unpack $ pShowNoColor (SFL4.itemsByRule l4i rules)

                                   ])



  
  -- if --workdir is specified, and there are no --only, then we run all the things
  -- however, we can flag specific exclusions by adding the --tomd option which, counterintuitively, disables tomd
  -- putStrLn $ "natural4: only = " <> SFL4.only opts
  when (toworkdir && not (null $ SFL4.uuiddir opts) && (null $ SFL4.only opts)) $ do

    when (SFL4.tonative  opts) $ mywritefile True toOrgFN      iso8601 "org"  asOrg
    when (SFL4.tonative  opts) $ mywritefile True tonativeFN   iso8601 "hs"   asNative
    when (SFL4.tocorel4  opts) $ mywritefile True tocorel4FN   iso8601 "l4"   asCoreL4
    when (SFL4.tobabyl4  opts) $ mywritefile True tobabyl4FN   iso8601 "l4"   asBabyL4
    when (SFL4.toasp     opts) $ mywritefile True toaspFN      iso8601 "lp"   asASP
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
                   displayTxt = Text.unpack $ Text.unwords n
               appendFile (dname <> "/index.html") ("<li> " <> "<a target=\"aasvg\" href=\"" <> fnamext <> "\">" <> displayTxt
                                                    <> "</a></li>\n")
           | (n,(svgtiny,svgfull,hsAnyAllTree,hsQtree)) <- Map.toList asaasvg
           , let (fname, ext) = (take 20 (snake_scrub n), "svg")
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

    when (SFL4.toProlog rc) $ pPrint $ asProlog

    when (SFL4.toTS rc) $ print $ asTypescript rules

    when (SFL4.only opts == "" && SFL4.workdir opts == "") $ pPrint rules
    when (SFL4.only opts `elem` ["native"])  $ pPrint rules
    when (SFL4.only opts `elem` ["classes"]) $ print (SFL4.classtable l4i)
    when (SFL4.only opts `elem` ["symtab"])  $ print (SFL4.scopetable l4i)

    when (SFL4.toVue rc) $ do
      -- putStrLn $ toString $ encodePretty $ rulesToRuleJSON rules
      putStrLn $ toString $ encodePretty $ itemRPToItemJSON $ toVueRules rules
      -- pPrint $ itemRPToItemJSON  $ toVueRules rules

    -- when (SFL4.toHTML rc) $ do
    --   mkdn <- mapM (toMarkdown nlgEnv) rules
    --   let htm = concatMap toHTML mkdn
    --   writeFile "output.html" htm
    --   pPrint htm

    -- when (SFL4.toPDF rc) $ do
    --   mkdn <- mapM (toMarkdown nlgEnv) rules
    --   pdf <- toPDF (Text.concat mkdn)
    --   Byte.writeFile "output.pdf" pdf

    when (SFL4.only opts `elem` ["native"]) $ pPrint rules

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
  -- putStrLn ("mywritefile: outputting to " <> mypath)
  writeFile mypath s
  -- do the symlink more atomically by renaming
  when doLink $ myMkLink (filename <> "." <> ext) mylink

myMkLink :: FilePath -> FilePath -> IO ()
myMkLink filename mylink = do
  let mylink_tmp = mylink <> "-tmp"
  createFileLink filename mylink_tmp
  renameFile mylink_tmp mylink

snake_scrub :: [Text.Text] -> String
snake_scrub x = fst $ partition (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-") $
                Text.unpack $
                Text.replace " " "_" $
                Text.intercalate "-" x

psPrefix :: String -- the stuff at the top of the purescript output
psPrefix = [QQ.r|

-- This file was automatically generated by natural4.
-- Do not edit by hand.
-- Instead, revise the toolchain starting at smucclaw/dsl/lib/haskell/natural4/app/Main.hs

module RuleLib.PDPADBNO where

import Prelude
import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Map as Map

import AnyAll.Types

schedule1_part1 :: Item String
schedule1_part1 =
  |]

psSuffix :: String -- at the bottom of the purescript output
psSuffix = [QQ.r|
schedule1_part1_nl :: NLDict
schedule1_part1_nl =
  Map.fromFoldable
    [ ]
    |]

-- Tuple "en" $ Map.fromFoldable
--         [ Tuple "1" "The amount of any wages, salary, fee, commission, bonus, gratuity, allowance or other remuneration paid or payable to the individual by any person, whether under a contract of service or a contract for services."
--         , Tuple "2" "The income of the individual from the sale of any goods or property."
--         , Tuple "3" "The number of any credit card, charge card or debit card issued to or in the name of the individual."
--         , Tuple "4" "The number assigned to any account the individual has with any organisation that is a bank or finance company."
--         , Tuple "5" "Any information that identifies, or is likely to lead to the identification of, the individual as a child or young person who —"
--         , Tuple "5.a" "is or had been the subject of any investigation under the CYPA;"
--         , Tuple "5.b" "is or had been arrested, on or after 1 July 2020, for an offence committed under any written law;"
--         , Tuple "5.c" "is or had been taken into care or custody by the Director-General of Social Welfare, a protector, any officer generally or specially authorised in that behalf in writing by the Director-General or protector or a police officer under the CYPA;"
--         , Tuple "5.d" "is attending or had attended a family programme in relation to an application to be made under section 50 of the CYPA;"
--         , Tuple "5.e" "is or was the subject of an order made by a court under the CYPA; or"
--         , Tuple "5.f" "is or had been concerned in any proceedings in any court or on appeal from any court, whether the individual is the person against or in respect of whom the proceedings are taken or a witness in those proceedings."
--         , Tuple "6" "Any information that identifies, or is likely to lead to the identification of —"
--         , Tuple "6.a" "the individual who has been or is the subject of any investigation, examination, assessment or treatment under the VAA relating to whether the individual is a vulnerable adult experiencing or at risk of abuse, neglect or self-neglect;"
--         , Tuple "6.b" "the individual as a vulnerable adult who has been committed to a place of temporary care and protection or place of safety or to the care of a fit person under the VAA;"
--         , Tuple "6.c" "the individual as a vulnerable adult who is the subject of an order made by a court under the VAA;"
--         , Tuple "6.d" "a place of temporary care and protection or place of safety in which an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is committed, or the location of such a place of temporary care and protection or place of safety; or"
--         , Tuple "6.e" "a fit person under whose care an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is placed, or the location of the premises of such a fit person."
--         , Tuple "7" "Any private key of or relating to the individual that is used or may be used —"
--         , Tuple "7.a" "to create a secure electronic record or secure electronic signature;"
--         , Tuple "7.b" "to verify the integrity of a secure electronic record; or"
--         , Tuple "7.c" "to verify the authenticity or integrity of a secure electronic signature."
--         , Tuple "8" "The net worth of the individual."
--         , Tuple "9" "The deposit of moneys by the individual with any organisation."
--         , Tuple "10" "The withdrawal by the individual of moneys deposited with any organisation."
--         , Tuple "11" "The granting by an organisation of advances, loans and other facilities by which the individual, being a customer of the organisation, has access to funds or financial guarantees."
--         , Tuple "12" "The incurring by the organisation of any liabilities other than those mentioned in paragraph 11 on behalf of the individual."
--         , Tuple "13" "The payment of any moneys, or transfer of any property, by any person to the individual, including the amount of the moneys paid or the value of the property transferred, as the case may be."
--         , Tuple "14" "The creditworthiness of the individual."
--         , Tuple "15" "The individual’s investment in any capital markets products."
--         , Tuple "16" "The existence, and amount due or outstanding, of any debt —"
--         , Tuple "16.a" "owed by the individual to an organisation; or"
--         , Tuple "16.b" "owed by an organisation to the individual."
--         , Tuple "17" "Any of the following:"
--         , Tuple "17.a" "the terms and conditions of any accident and health policy or life policy (called in this item the applicable policy) of which the individual is the policy owner or under which the individual is a beneficiary;"
--         , Tuple "17.b" "the premium payable by the policy owner under the applicable policy;"
--         , Tuple "17.c" "the benefits payable to any beneficiary under the applicable policy;"
--         , Tuple "17.d" "any information relating to any claim on, or payment under, the applicable policy, including the condition of the health of the individual and the diagnosis, treatment, prevention or alleviation of any ailment, condition, disability, disease, disorder or injury that the individual has suffered or is suffering from;"
--         , Tuple "17.e" "any other information that the individual is the policy owner of, or a beneficiary under, an applicable policy."
--         , Tuple "18" "The assessment, diagnosis, treatment, prevention or alleviation by a health professional of any of the following affecting the individual:"
--         , Tuple "18.a" "any sexually-transmitted disease such as Chlamydial Genital Infection, Gonorrhoea and Syphilis;"
--         , Tuple "18.b" "Human Immunodeficiency Virus Infection;"
--         , Tuple "18.c" "schizophrenia or delusional disorder;"
--         , Tuple "18.d" "substance abuse and addiction, including drug addiction and alcoholism."
--         , Tuple "19" "The provision of treatment to the individual for or in respect of —"
--         , Tuple "19.a" "the donation or receipt of a human egg or human sperm; or"
--         , Tuple "19.b" "any contraceptive operation or procedure or abortion."
--         , Tuple "20" "Any of the following:"
--         , Tuple "20.a" "subject to section 4(4)(b) of the Act, the donation and removal of any organ from the body of the deceased individual for the purpose of its transplantation into the body of another individual;"
--         , Tuple "20.b" "the donation and removal of any specified organ from the individual, being a living organ donor, for the purpose of its transplantation into the body of another individual;"
--         , Tuple "20.c" "the transplantation of any organ mentioned in sub-paragraph (a) or (b) into the body of the individual."
--         , Tuple "21" "Subject to section 4(4)(b) of the Act, the suicide or attempted suicide of the individual."
--         , Tuple "22" "Domestic abuse, child abuse or sexual abuse involving or alleged to involve the individual."
--         , Tuple "23" "Any of the following:"
--         , Tuple "23.a" "information that the individual is or had been adopted pursuant to an adoption order made under the Adoption of Children Act (Cap. 4), or is or had been the subject of an application for an adoption order;"
--         , Tuple "23.b" "the identity of the natural father or mother of the individual;"
--         , Tuple "23.c" "the identity of the adoptive father or mother of the individual;"
--         , Tuple "23.d" "the identity of any applicant for an adoption order;"
--         , Tuple "23.e" "the identity of any person whose consent is necessary under that Act for an adoption order to be made, whether or not the court has dispensed with the consent of that person in accordance with that Act;"
--         , Tuple "23.f" "any other information that the individual is or had been an adopted child or relating to the adoption of the individual."
--         ]
--     ]
-- |]


