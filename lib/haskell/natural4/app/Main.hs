{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import AnyAll.BoolStruct (alwaysLabeled)
import AnyAll.SVGLadder (defaultAAVConfig)
import Control.Arrow ((>>>))
import Control.Monad (unless, void, when)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (first)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Either (lefts, rights)
import Data.Foldable qualified as DF
import Data.HashMap.Strict qualified as HashMap
import Data.List (intercalate, isPrefixOf, sortOn, (\\))
import Data.Maybe (mapMaybe)
import Data.String (IsString(..))
import Data.String.Interpolate (i, __i)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Time.Clock (getCurrentTime)
import Data.Time.ISO8601 (formatISO8601Millis)
import LS qualified as SFL4
import LS.DataFlow (dataFlowAsDot)
import LS.Interpreter
  ( expandClauses,
    getAndOrTree,
    l4interpret,
    onlyTheItems,
  )
import LS.Lib (Mode(..), ModeName(..), Options(..))
import LS.NLP.NLG
  ( NLGEnv,
    allLangs,
    expandRulesForNLG,
    langEng,
    myNLGEnv,
    nlg,
    printLangs,
  )
import LS.XPile.CoreL4
  ( sfl4ToASP,
    sfl4ToBabyl4,
    sfl4ToCorel4,
    sfl4ToDMN,
    sfl4ToEpilog,
  )
import LS.XPile.Edn.L4ToEdn qualified as Edn
import LS.XPile.ExportTypes
  ( rulesToHaskellTp,
    rulesToJsonSchema,
    rulesToPrologTp,
    rulesToUISchema,
  )
import LS.XPile.GFTrees (gftrees)
import LS.XPile.IntroBase (toBase)
import LS.XPile.IntroBasic (toBasic)
import LS.XPile.IntroLog (toLog)
import LS.XPile.IntroReader (defaultReaderEnv, toReader)
import LS.XPile.IntroShoehorn (toShoehorn)
import LS.XPile.IntroTrivial (toTrivial)
import LS.XPile.JSONRanges (asJSONRanges)
import LS.XPile.Logging
  ( XPileLogW,
    fmapE,
    mutter,
    pShowNoColorS,
    xpLog,
  )
import LS.XPile.LogicalEnglish (toLE)
import LS.XPile.Markdown (bsMarkdown)
import LS.XPile.MathLang (toMathLangGen, toMathLangMw)
import LS.XPile.Maude qualified as Maude
import LS.XPile.NaturalLanguage (toNatLang)
import LS.XPile.Org (toOrg)
import LS.XPile.Petri (toPetri)
import LS.XPile.Prolog (rulesToProlog, rulesToSCasp)
import LS.XPile.Purescript (translate2PS)
import LS.XPile.SVG qualified as AAS
import LS.XPile.Typescript (asTypescript)
import LS.XPile.Uppaal qualified as Uppaal
import LS.XPile.VueJSON
  ( checklist,
    groundrules,
    itemRPToItemJSON,
    toVueRules,
  )
import Optics ( over, set, Optic', Is, A_Setter )
import Options.Applicative
  ( (<**>),
    Alternative(..),
    flag',
    help,
    info,
    long,
    metavar,
    strArgument,
    strOption,
    internal,
    execParser,
    helper,
    FlagFields,
    Mod,
    OptionFields,
    Parser,
  )
import Options.Applicative.Help (tabulate, Chunk (..))
import Options.Applicative.Help.Pretty (prettyString)
import System.Directory
  ( createDirectoryIfMissing,
    createFileLink,
    renameFile,
  )
import System.Exit (exitSuccess)
import System.FilePath ((-<.>), (</>))
import System.IO.Unsafe (unsafeInterleaveIO)
import Text.Pretty.Simple (pPrint, pShowNoColor)
import Text.Regex.PCRE.Heavy qualified as PCRE
import Text.XML.HXT.Core qualified as HXT

--
-- Command-line options parsing
--

-- | Utility function that creates an option parser accepting many
-- occurrences of the option and chaining them so that the last one
-- wins.
--
composeMany :: Parser (a -> a) -> Parser (a -> a)
composeMany p =
  foldr (flip (.)) id <$> many p

switch' :: Is k A_Setter => Optic' k is a Bool -> String -> Mod FlagFields (a -> a) -> Parser (a -> a)
switch' optic txt mods =
  composeMany
    (   flag' (set optic True)  (long txt <> mods)
    <|> flag' (set optic False) (long ("no-" <> txt) <> internal)
    )

demoOption :: Parser (Options -> Options)
demoOption =
  switch' #demo "demo" (help "demo mode")

dbugOption :: Parser (Options -> Options)
dbugOption =
  switch' #dbug "dbug" internal

debugOption :: Parser (Options -> Options)
debugOption =
  switch' #dbug "debug" (help "debug mode")

extdOption :: Parser (Options -> Options)
extdOption =
  switch' #extd "extd" (help "unhide grounds carrying typical values")

dstreamOption :: Parser (Options -> Options)
dstreamOption =
  switch' #dstream "dstream" mempty

filesArgs :: Parser (Options -> Options)
filesArgs =
  composeMany
    ((\ f -> over #file (++ [f])) <$> strArgument (metavar "FILES" <> help "files to process"))

workdirOption :: Parser (Options -> Options)
workdirOption =
  dirOption #workdir "workdir" (help "workdir to save all the output files to")

uuiddirOption :: Parser (Options -> Options)
uuiddirOption =
  dirOption #workdir "uuiddir" (help "uuid prefix to follow the workdir")

modeOption :: Parser (Options -> Options)
modeOption =
  composeMany
    (   ((over #mode . goExclude) <$> strOption (long "exclude" <> help "exclude a transpiler"))
    <|> ((over #mode . goOnly   ) <$> strOption (long "only" <> help "only run this transpiler"))
    )
  where
    goExclude :: String -> Mode -> Mode
    goExclude str (Exclude xs) = Exclude (xs ++ [MkModeName str])
    goExclude str (Only    _ ) = Exclude [MkModeName str]
    goExclude _   ListModes    = ListModes

    goOnly :: String -> Mode -> Mode
    goOnly    str (Exclude _ ) = Only [MkModeName str]
    goOnly    str (Only    xs) = Only (xs ++ [MkModeName str])
    goOnly    _   ListModes    = ListModes

listModesOption :: Parser (Options -> Options)
listModesOption =
  composeMany
    (flag' (set #mode ListModes) (long "list-modes" <> help "list available modes / transpilers"))

dirOption ::
     Is k A_Setter
  => Optic' k is a (Maybe FilePath)
  -> String
  -> Mod OptionFields FilePath
  -> Parser (a -> a)
dirOption optic txt mods =
  composeMany
    (   (set optic . Just)
    <$> strOption
      (  long txt
      <> metavar "DIR"
      <> mods
      )
    )

allOptions :: Parser Options
allOptions =
       pure SFL4.defaultOptions
  <**> demoOption
  <**> dbugOption
  <**> debugOption
  <**> extdOption
  <**> dstreamOption
  <**> workdirOption
  <**> uuiddirOption
  <**> modeOption
  <**> listModesOption
  <**> filesArgs
  <**> helper

-- | Captures the configuration of the main pipeline driver
-- after options processing, parsing and interpreting.
--
-- Serves as input to all the backends of the pipeline.
--
data DriverState =
  MkDriverState
    { options     :: SFL4.Options
    , runConfig   :: SFL4.RunConfig
    , parsed      :: [SFL4.Rule]
    , interpreted :: SFL4.Interpreted
    , timestamp   :: String
    , nlgEnv      :: Maybe NLGEnv
    , nlgData     :: Maybe NLGData
    }

-- | Computes the unique output path we potentially want to use,
-- and execute the continuation with it, or do nothing if the
-- output path is not set.
--
workuuid :: DriverState -> Maybe FilePath
workuuid MkDriverState { options } =
  let
    uuid :: FilePath
    uuid = maybe "no-uuid" id (SFL4.uuiddir options)
  in
    (</> uuid) <$> SFL4.workdir options

-- | The main entry point proceeds as follows:
--
-- 1. Parse options
-- 2. Prepare for most transpilers by running the interpreter.
-- 3. Do NLG setup for NLG transpilers.
-- 4. Handle all selected transpilers according to options.
--
main :: IO ()
main = do

  -- Part 1: Parse options
  opts     <- execParser (info allOptions mempty)

  handleListModes opts

  rc       <- SFL4.getConfig opts
--  putStrLn "main: doing dumpRules"
  rules    <- SFL4.dumpRules opts
  let l4i  = l4interpret SFL4.defaultInterpreterOptions rules
  iso8601  <- now8601

  -- NLG stuff
  --
  -- TODO: unsafeInterleaveIO should probably be replaced by "once",
  -- i.e., code that runs the IO actions at most once, but only if needed
  -- by one of the selected transpilers

  nlgLangs <- unsafeInterleaveIO allLangs
  (engE, engErr) <- xpLog <$> langEng
  (nlgEnv, nlgData) <-
    case engE of
      Left err -> do
        putStrLn $ unlines $ "natural4: encountered error when obtaining langEng" : err
        pure (Nothing, Nothing)
      Right eng -> do
        (nlgEnv, _nlgEnvErr)  <- unsafeInterleaveIO $ xpLog <$> myNLGEnv l4i eng -- Only load the NLG environment if we need it.
        (allNLGEnv, allNLGEnvErr) <- unsafeInterleaveIO do
          xps <- traverse (myNLGEnv l4i) nlgLangs
          return (xpLog $ sequenceA xps)

        case nlgEnv of
          Left  err     -> do
            putStrLn $ unlines $ "natural4: encountered error while obtaining myNLGEnv" : err
            pure (Nothing, Nothing)
          Right nlgEnvR -> do
            let allNLGEnvErrors = mconcat $ lefts allNLGEnv
            unless (null allNLGEnvErrors) do
              putStrLn "natural4: encountered error while obtaining allNLGEnv"
              DF.traverse_ putStrLn allNLGEnvErrors

            let allNLGEnvR = rights allNLGEnv

            let
              nlgData =
                MkNLGData
                  nlgEnvR
                  allNLGEnvR
                  allNLGEnvErr
                  engErr

            pure (Just nlgEnvR, Just nlgData)

  let
    driverState =
      MkDriverState
        { options     = opts
        , runConfig   = rc
        , parsed      = rules
        , interpreted = l4i
        , timestamp   = iso8601
        , nlgEnv      = nlgEnv
        , nlgData     = nlgData
        }

  handleMode driverState

-- | Show available transpilers / modes.
handleListModes :: Options -> IO ()
handleListModes o = do
  case mode o of
    SFL4.ListModes   -> do
      putStrLn "Available modes:"
      let chunk = tabulate 24 [ (fromString n, fromString txt) | (MkModeName n, txt, _) <- transpilersMap ]
      putStrLn (prettyString 1 80 (maybe mempty id (unChunk chunk)))
      exitSuccess
    _ -> pure ()

handleMode :: DriverState -> IO ()
handleMode ds = do
  let o = ds.options

  case mode o of
    SFL4.ListModes   -> pure () -- should not occur, should already have been handled
    SFL4.Only mns    -> doModes ds mns
    SFL4.Exclude mns -> doModes ds (SFL4.defaultModes \\ mns)

doModes :: DriverState -> [ModeName] -> IO ()
doModes ds modesToRun = do
  let transpilersToRun :: [Transpiler]
      transpilersToRun =
        concat (mapMaybe (\ (mn, _, ts) -> if mn `elem` modesToRun then Just ts else Nothing) transpilersMap)

  putStrLn $ "Running: " <> show ((.subdir) <$> transpilersToRun)

  mapM_ (writeUsingTranspiler ds) transpilersToRun

-- | All transpilers known to the system.
--
-- The order of transpilers in this list affects the order in
-- which they are being processed. It might be better to use the
-- order of command line flags instead, but when refactoring the
-- driver, I kept the original order which was independent of
-- flags.
--
-- The new system cannot be completely faithful to the old one
-- because the on-disk order and the on-screen order have to be
-- the same.
--
-- As the on-screen order is more visible, I give preference to
-- that.
--
transpilersMap ::
  [(ModeName, String, [Transpiler])]
transpilersMap =
  -- petriMode
  -- aatree (simplified form of native)
  -- json
  -- coreL4 / babyL4(this is slightly unclear)
  -- uppaal
  -- grounds
  -- prolog
  -- scasp
  -- native
  -- classes
  -- symtab
  -- maude

  [ -- order follows old on-screen output:
    ( SFL4.nlgMode
    , "... (only screen)"
    , [nlgTranspiler]
    )
  , ( SFL4.petriMode
    , "a petri-net Dot file of the state graph"
    , [petriTranspiler]
    )
  , ( SFL4.aatreeMode
    , "aatree (only screen)"
    , [aatreeTranspiler]
    )
  , ( SFL4.jsonMode
    , "anyall representation dumped as raw JSON"
    , [jsonTranspiler]
    )
  , ( SFL4.corel4Mode
    , "in core-l4 syntax"
    , [coreL4Transpiler]
    )
  , ( SFL4.babyl4Mode
    , "in baby-l4 syntax (directly via AST)"
    , [babyL4Transpiler]
    )
  , ( SFL4.uppaalMode
    , "uppaal (only screen)"
    , [uppaalTranspiler]
    )
  , ( SFL4.groundsMode
    , "ground terms"
    , [groundsTranspiler]
    )
  , ( SFL4.prologMode
    , "prolog-like syntax representing the predicate logic"
    , [prologTranspiler]
    )
  , ( SFL4.scaspMode
    , "sCasp-like syntax for representing the predicate logic"
    , [sCaspTranspiler]
    )
  , ( SFL4.nativeMode
    , "native Haskell data structure of the AST"
    , (onlyDisk <$> [dataFlowTranspiler, orgTranspiler, nativeTranspiler]) <> [rulesTranspiler]
    )
  , ( SFL4.classesMode
    , "class table (screen only)"
    , [classesTranspiler]
    )
  , ( SFL4.symtabMode
    , "symbol table (screen only)"
    , [symtabTranspiler]
    )
  , ( SFL4.maudeMode
    , "maude"
    , [maudeTranspiler]
    )
    -- from here on, the order follows the old on-disk order
  , ( SFL4.checklistMode
    , "ground terms phrased in checklist syntax"
    , [checklistTranspiler]
    )
  , ( SFL4.gftreesMode
    , "nlg trees"
    , [gftreesTranspiler]
    )
  , ( SFL4.mdMode
    , "nlg markdown"
    , [bsMarkdownTranspiler]
    )
  , ( SFL4.pursMode
    , "anyall representation dumped as Purescript source code for mv'ing into RuleLib/*.purs"
    , [purescriptTranspiler]
    )
  -- old position of nativeMode in on-disk output
  , ( SFL4.logicalEnglishMode
    , "logical english"
    , [logicalEnglishTranspiler]
    )
    -- old position of corel4Mode / babyl4Mode in on-disk output
  , ( SFL4.aspMode
    , "in ASP syntax"
    , [aspTranspiler]
    )
  , ( SFL4.epilogMode
    , "in Epilog syntax"
    , [epilogTranspiler]
    )
  , ( SFL4.dmnMode
    , "in DMN syntax"
    , [dmnTranspiler]
    )
    -- old position of jsonMode in to-disk output
  , ( SFL4.introMode
    , "introduction to transpilation"
    , [ intro1Transpiler
      , intro2Transpiler
      , intro3Transpiler
      , intro4Transpiler
      , intro5Transpiler
      , intro6Transpiler
      ]
    )
  , ( SFL4.mathlangmwMode
    , "Meng's typed lambda calculus with arithmetic ops and convenience hofs, TS"
    , [mathLangMwTSTranspiler]
    )
  , ( SFL4.genmathlangMode
    , "generic version of untyped lambda calculus with arithmetic ops and convenience hofs"
    , [mathLangGenTranspiler]
    )
  , ( SFL4.vueMode -- TODO: "vue" vs "vuejson" name
    , "anyall representation dumped as JSON for the web app (currently Vue) to pick up"
    , [vueTranspiler]
    )
    -- old position of prologMode in on-disk output
  , ( SFL4.prologTpMode
    , "prolog-like syntax from type declarations"
    , [prologTpTranspiler]
    )
  , ( SFL4.haskellTpMode
    , "haskell-like syntax from type declarations"
    , [haskellTpTranspiler]
    )
  , ( SFL4.jsonTpMode
    , "json-like syntax from type declarations"
    , [jsonSchemaTpTranspiler]
    )
  , ( SFL4.jsonUIMode
    , "json-like syntax from type declarations for web form"
    , [jsonUISchemaTranspiler]
    )
    -- old position of scaspMode in to-disk output
    -- old position of petriMode in to-disk output
  , ( SFL4.jsrMode
    , "JSON ranges for QuickCheck-style testing"
    , [jsonRangesTranspiler]
    )
  , ( SFL4.tsMode
    , "typescript"
    , [typescriptTranspiler]
    )
  , ( SFL4.nlMode
    , "natural language"
    , [natLangTranspiler]
    )
    -- old position of groundsMode in on-disk output
    -- old position of maudeMode in on-disk output
  , ( SFL4.ednMode
    , "EDN"
    , [ednTranspiler]
    )
  , ( SFL4.aasvgMode
    , "an anyall SVG of the decision trees"
    , [aasvgTranspiler]
    )
  , ( SFL4.simalaMode
    , "Simala expressions for hornlike"
    , [simalaTranspiler]
    )
  ]

toNative :: [SFL4.Rule] -> SFL4.Interpreted -> String
toNative rules l4i =
  unlines
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

    ]

--
-- Transpilers and their metadata
--

data TranspilationResult a =
    Skipped String -- ^ reason
  | Success a (Maybe [String])

mkResultWithoutErrors :: a -> TranspilationResult a
mkResultWithoutErrors r = Success r Nothing

mkResultWithErrors :: (a, [String]) -> TranspilationResult a
mkResultWithErrors (r, e) = Success r (Just e)

mapResult :: (a -> b) -> TranspilationResult a -> TranspilationResult b
mapResult _ (Skipped r)     = Skipped r
mapResult f (Success a err) = Success (f a) err

-- | Configuration of a transpiler, parameterised by the input needed for the
-- transpilation entry point.
--
-- NOTE: Most actual transpilers are pure. As far as I (Andres) can see, the
-- main reason why some are not is some form of tracing / logging? This should
-- probably just be permitted, but general IO should not be.
--
data Transpiler =
  forall a.
  MkTranspiler
    { subdir     :: FilePath
    , extension  :: String
    , entryPoint :: DriverState -> IO (TranspilationResult a)
    , output     :: TranspilerOutput a -- ^ describes how the transpiler outputs results to screen / files
    }

data TranspilerOutput a =
    DefaultTranspilerOutput (TranspilerOutputConfig a)
  | CustomTranspilerOutput (DriverState -> FilePath -> String -> TranspilationResult a -> IO ())

data TranspilerOutputConfig a =
  forall b.
  MkTranspilerOutputConfig
    { ppFile     :: TranspilationResult a -> Maybe (TranspilationResult b) -- ^ post-process result when writing to file
    , writer     :: FilePath -> b -> IO () -- ^ how to write a file to disk
    , ppStdout   :: a -> IO () -- ^ post-process result when writing to screen
    }

data NLGData =
  MkNLGData
    { env       :: NLGEnv
    , allEnv    :: [NLGEnv]
    , allEnvErr :: XPileLogW
    , engErr    :: XPileLogW
    }

withNLGEnv ::
  (NLGEnv -> DriverState -> TranspilationResult a) -> DriverState -> TranspilationResult a
withNLGEnv k ds =
  case ds.nlgEnv of
    Nothing  -> Skipped "skipping transpiler due to lacking NLG environment"
    Just env -> k env ds

withNLGData ::
  (NLGData -> DriverState -> IO (TranspilationResult a)) -> DriverState -> IO (TranspilationResult a)
withNLGData k ds =
  case ds.nlgData of
    Nothing  -> pure (Skipped "skipping transpiler due to lacking NLG environment")
    Just env -> k env ds

simpleTranspiler ::
     FilePath
  -> String
  -> (DriverState -> TranspilationResult String)
  -> Transpiler
simpleTranspiler subdir ext entry =
  MkTranspiler
    { subdir     = subdir
    , extension  = ext
    , entryPoint = \ ds -> pure (entry ds)
    , output     = DefaultTranspilerOutput (MkTranspilerOutputConfig Just writeFile putStrLn)
    }

pprintTranspiler ::
     Show b
  => FilePath
  -> String
  -> (DriverState -> TranspilationResult b)
  -> Transpiler
pprintTranspiler subdir ext entry =
  MkTranspiler
    { subdir     = subdir
    , extension  = ext
    , entryPoint = \ ds -> pure (entry ds)
    , output     = DefaultTranspilerOutput
                     (MkTranspilerOutputConfig
                        { ppFile   = Just . mapResult show
                        , writer   = writeFile
                        , ppStdout = pPrint
                        }
                     )
    }

screenTranspiler ::
     Show a
  => FilePath -- ^ subdir unused as directory, but doubles as "name" / "handle", so still set
  -> (DriverState -> TranspilationResult a)
  -> Transpiler
screenTranspiler subdir entry =
  MkTranspiler
    { subdir     = subdir
    , extension  = ""
    , entryPoint = pure . entry
    , output     = DefaultTranspilerOutput
                     (MkTranspilerOutputConfig
                        { ppFile   = const Nothing
                        , writer   = writeFile
                        , ppStdout = pPrint
                        }
                     )
    }

withoutErrors :: (DriverState -> b) -> DriverState -> TranspilationResult b
withoutErrors entry ds = mkResultWithoutErrors (entry ds)

withErrors :: (DriverState -> (b, [String])) -> DriverState -> TranspilationResult b
withErrors entry ds = mkResultWithErrors (entry ds)

onlyRules :: ([SFL4.Rule] -> a) -> DriverState -> a
onlyRules entry ds = entry ds.parsed

onlyInterpreted :: (SFL4.Interpreted -> a) -> DriverState -> a
onlyInterpreted entry ds = entry ds.interpreted

-- | Modify a transpiler so that it does not do anything for on-screen output.
onlyDisk :: Transpiler -> Transpiler
onlyDisk t@MkTranspiler { entryPoint, output } =
  t { entryPoint = entryPoint, output = toDiskOutput output }

toDiskOutput :: TranspilerOutput a -> TranspilerOutput a
toDiskOutput (DefaultTranspilerOutput (MkTranspilerOutputConfig { ppFile, writer })) =
  DefaultTranspilerOutput (MkTranspilerOutputConfig ppFile writer (const (pure ())))
toDiskOutput o = o

now8601 :: IO String
now8601 = formatISO8601Millis <$> getCurrentTime

-- | Runs a transpiler. Depending on the workdir setting and post-processing options,
-- will write either to files, to screen, or neither.
--
writeUsingTranspiler ::
     DriverState
  -> Transpiler
  -> IO ()
writeUsingTranspiler ds MkTranspiler { subdir, extension, entryPoint, output } = do
  result <- entryPoint ds
  case output of
    DefaultTranspilerOutput (MkTranspilerOutputConfig { ppFile, writer, ppStdout }) -> do
      case workuuid ds of
        Just dir ->
          case ppFile result of
            Nothing      -> pure () -- post-processing indicates we don't want to write to file
            Just result' ->
              writeTranspilationResultWith
                writer
                True
                (dir </> subdir)
                ds.timestamp
                extension
                result'
        Nothing  -> do
          putStrLn $ "=== output of: " <> subdir <> " ==="
          possiblySkip (\ x _ -> ppStdout x) result
    CustomTranspilerOutput handler -> handler ds subdir extension result

possiblySkip :: (a -> Maybe [String] -> IO ()) -> TranspilationResult a -> IO ()
possiblySkip _k (Skipped msg)            = putStrLn msg
possiblySkip  k (Success output merrors) = k output merrors

-- | output both "stdout" to outfile and "stderr" to outfile.err.
writeTranspilationResultWith ::
     (FilePath -> a -> IO ())   -- ^ how to actually write the file
  -> Bool                       -- ^ whether to link LATEST to the output
  -> FilePath                   -- ^ output directory
  -> FilePath                   -- ^ basename of output files
  -> String                     -- ^ extension of main output file
  -> TranspilationResult a      -- ^ contents / results to write to files
  -> IO ()
writeTranspilationResultWith write doLink dirname filename ext result =
  flip possiblySkip result $ \ output merrors -> do
  -- putStrLn $ "=== writing: " <> dirname <> " ==="
  -- ensure the output directory exists (including parents)
  createDirectoryIfMissing True dirname
  let outFile  = dirname </> filename -<.> ext
      errFile  = dirname </> filename -<.> if ext == "org" then "err" else "org"
      linkFile = dirname </> "LATEST" -<.> ext
  -- there is a difference between the errors being 'Nothing' (then no
  -- error file gets written) and the errors being an empty list (then
  -- an empty file gets written)
  maybe (pure ()) (\ errors -> writeFile errFile (intercalate "\n" errors)) merrors
  write outFile output
  when doLink $ myMkLink (filename -<.> ext) linkFile

--
-- Specific transpilers
--

checklistTranspiler :: Transpiler
checklistTranspiler =
  simpleTranspiler "checkl" "txt"
    (withNLGEnv (\ nlge -> withErrors (\ ds ->
      first show (xpLog (checklist nlge ds.runConfig ds.parsed))
    )))

gftreesTranspiler :: Transpiler
gftreesTranspiler =
  simpleTranspiler "gftrees" "gftrees"
    (withNLGEnv (\ nlge -> withErrors (\ ds ->
      first pShowNoColorS (xpLog (gftrees nlge ds.parsed))
    )))

-- The entry point here has IO. TODO: Why?
bsMarkdownTranspiler :: Transpiler
bsMarkdownTranspiler =
  MkTranspiler
    { subdir     = "md"
    , extension  = "md"
    , entryPoint = withNLGData (\ nlgd ds -> mkResultWithoutErrors <$> bsMarkdown nlgd.allEnv ds.parsed)
    , output     = DefaultTranspilerOutput (MkTranspilerOutputConfig Just writeFile putStrLn)
    }

purescriptTranspiler :: Transpiler
purescriptTranspiler =
  MkTranspiler
    { subdir     = "purs"
    , extension  = "purs"
    , entryPoint = go
    , output     = DefaultTranspilerOutput (MkTranspilerOutputConfig Just writeFile putStrLn)
    }
  where
    go = withNLGData (\ nlgd ds -> do
      strLangs <- unsafeInterleaveIO $ printLangs allLangs
      let (psResult, psErrors) = xpLog do
            mutter "* main calling translate2PS"
            fmapE (<> ("\n\n" <> "allLang = [\"" <> strLangs <> "\"]")) (translate2PS nlgd.allEnv nlgd.env ds.parsed)
      pure (Success (commentIfError "-- ! -- " psResult) (Just (nlgd.engErr <> nlgd.allEnvErr <> psErrors)))
      )

prologTranspiler :: Transpiler
prologTranspiler =
  pprintTranspiler "prolog" "pl" -- unclear if this should use pprint
    (withoutErrors (onlyRules rulesToProlog))

prologTpTranspiler :: Transpiler
prologTpTranspiler =
  simpleTranspiler "prologTp" "pl" -- unclear if this should use pprint
    (withoutErrors (onlyRules rulesToPrologTp))

haskellTpTranspiler :: Transpiler
haskellTpTranspiler =
  simpleTranspiler "haskellTp" "hs"
    (withoutErrors (onlyRules rulesToHaskellTp))

jsonSchemaTpTranspiler :: Transpiler
jsonSchemaTpTranspiler =
  simpleTranspiler "jsonTp" "json"
    (withoutErrors (onlyRules rulesToJsonSchema))

jsonUISchemaTranspiler :: Transpiler
jsonUISchemaTranspiler =
  simpleTranspiler "jsonUI" "json"
    (withoutErrors (onlyRules rulesToUISchema))

sCaspTranspiler :: Transpiler
sCaspTranspiler =
  pprintTranspiler "scasp" "pl" -- unclear if this should use pprintTranspiler
    (withoutErrors (onlyRules rulesToSCasp))

petriTranspiler :: Transpiler
petriTranspiler =
  simpleTranspiler "petri" "dot"
    (withErrors (onlyRules (first (commentIfError "//") . xpLog . toPetri)))

jsonRangesTranspiler :: Transpiler
jsonRangesTranspiler =
  simpleTranspiler "jsonranges" "json" (withErrors (onlyInterpreted (first show . xpLog . asJSONRanges)))

typescriptTranspiler :: Transpiler
typescriptTranspiler =
  simpleTranspiler "ts" "ts" (withErrors (onlyInterpreted (first show . xpLog . asTypescript)))

natLangTranspiler :: Transpiler
natLangTranspiler =
  simpleTranspiler "natlang" "txt" (withoutErrors (onlyInterpreted toNatLang))

groundsTranspiler :: Transpiler
groundsTranspiler =
  pprintTranspiler "grounds" "txt" (withoutErrors (\ ds -> groundrules ds.runConfig ds.parsed))

maudeTranspiler :: Transpiler
maudeTranspiler =
  simpleTranspiler "maude" "natural4" (withoutErrors (onlyRules Maude.rules2maudeStr))

ednTranspiler :: Transpiler
ednTranspiler =
  simpleTranspiler "edn" "edn" (withoutErrors (onlyRules Edn.l4rulesToEdnStr))

intro1Transpiler :: Transpiler
intro1Transpiler =
  simpleTranspiler "intro1" "txt" (withoutErrors (onlyInterpreted toTrivial))

intro2Transpiler :: Transpiler
intro2Transpiler =
  simpleTranspiler "intro2" "txt" (withoutErrors (onlyInterpreted toBasic))

intro3Transpiler :: Transpiler
intro3Transpiler =
  simpleTranspiler "intro3" "txt" (withoutErrors (onlyInterpreted (flip toReader defaultReaderEnv)))

intro4Transpiler :: Transpiler
intro4Transpiler =
  simpleTranspiler "intro4" "txt" (withErrors (\ ds -> xpLog (toLog ds.interpreted defaultReaderEnv)))

intro5Transpiler :: Transpiler
intro5Transpiler =
  simpleTranspiler "intro5" "txt" (withErrors (\ ds -> toShoehorn ds.interpreted defaultReaderEnv))

intro6Transpiler :: Transpiler
intro6Transpiler =
  simpleTranspiler "intro6" "txt" (withErrors (\ ds -> toBase ds.interpreted defaultReaderEnv))

-- temporarily use .txt
mathLangGenTranspiler :: Transpiler
mathLangGenTranspiler =
  simpleTranspiler "mathlangGen" "txt" (withErrors (onlyInterpreted toMathLangGen))
-- will upstream a generic xpiler + app env in the future,
-- and have `toMathLangGen` take that as one of its inputs

mathLangMwTSTranspiler :: Transpiler
mathLangMwTSTranspiler =
  simpleTranspiler "mathlangTS" "ts" (withErrors (onlyInterpreted (flip toMathLangMw defaultReaderEnv)))

orgTranspiler :: Transpiler
orgTranspiler =
  simpleTranspiler "org" "org" (withoutErrors (\ ds -> toOrg ds.interpreted ds.parsed))

nativeTranspiler :: Transpiler
nativeTranspiler =
  simpleTranspiler "native" "hs" (withoutErrors (\ ds -> toNative ds.parsed ds.interpreted))

-- This is a weird special case of nativeTranspiler.
aatreeTranspiler :: Transpiler
aatreeTranspiler =
  MkTranspiler
    { subdir     = "aatree"
    , extension  = "" -- screen
    , entryPoint = \ ds -> pure (mkResultWithoutErrors ds)
    , output     = DefaultTranspilerOutput
                     (MkTranspilerOutputConfig
                       { ppFile   = const Nothing
                       , writer   = writeFile
                       , ppStdout = \ ds -> DF.for_ ds.parsed $ pPrint . getAndOrTree ds.interpreted 1
                       }
                     )
    }

-- This is another weird special case.
nlgTranspiler :: Transpiler
nlgTranspiler =
  MkTranspiler
    { subdir     = "nlg"
    , extension  = "" -- screen
    , entryPoint = withNLGData $ \ nlgd ds -> pure (mkResultWithoutErrors (nlgd, ds))
    , output     = DefaultTranspilerOutput
                     (MkTranspilerOutputConfig
                       { ppFile   = const Nothing
                       , writer   = writeFile
                       , ppStdout = go
                       }
                     )
    }
  where
    go = \ (nlgd, ds) ->
      DF.for_ nlgd.allEnv $ \ env -> do
        naturalLangSents <- nlg env `traverse` expandRulesForNLG env ds.parsed
        DF.for_ naturalLangSents $ putStrLn . Text.unpack


dataFlowTranspiler :: Transpiler
dataFlowTranspiler =
  simpleTranspiler "dataflow" "dot" (withErrors (onlyInterpreted (xpLog . dataFlowAsDot)))

aspTranspiler :: Transpiler
aspTranspiler =
  simpleTranspiler "asp" "lp" (withErrors (onlyRules (first (commentIfError "%%") . xpLog . sfl4ToASP)))

epilogTranspiler :: Transpiler
epilogTranspiler =
  simpleTranspiler "epilog" "lp" (withErrors (onlyRules (first (commentIfError "%%") . xpLog . sfl4ToEpilog)))

dmnTranspiler :: Transpiler
dmnTranspiler =
  MkTranspiler
    { subdir     = "dmn"
    , extension  = "dmn"
    , entryPoint = \ ds -> pure (mkResultWithoutErrors (sfl4ToDMN ds.parsed))
    , output     = DefaultTranspilerOutput
                     (MkTranspilerOutputConfig
                       { ppFile   = Just
                       , writer   = xmlWriter
                       , ppStdout = const (pure ())
                       }
                     )
    }
  where
    xmlWriter f x =
      void (HXT.runX (x HXT.>>> HXT.writeDocument [ HXT.withIndent HXT.yes ] f))

jsonTranspiler :: Transpiler
jsonTranspiler =
  simpleTranspiler "json" "json" (withoutErrors (onlyInterpreted (toString . encodePretty . alwaysLabeled . onlyTheItems)))

coreL4Transpiler :: Transpiler
coreL4Transpiler =
  simpleTranspiler "corel4" "l4" (withErrors (onlyRules (first (commentIfError "--") . xpLog . sfl4ToCorel4)))

babyL4Transpiler :: Transpiler
babyL4Transpiler =
  simpleTranspiler "babyl4" "l4" (withoutErrors (onlyInterpreted sfl4ToBabyl4))

logicalEnglishTranspiler :: Transpiler
logicalEnglishTranspiler =
  simpleTranspiler "logical_english" "le" (withoutErrors (onlyRules toLE))

vueTranspiler :: Transpiler
vueTranspiler =
  simpleTranspiler "vuejson" "vuejson" $ \ ds ->
    let
      asVueJSONrules :: [(SFL4.RuleName, (Either XPileLogW SFL4.BoolStructR, XPileLogW))]
      asVueJSONrules = fmap xpLog <$> toVueRules ds.parsed
      -- [TODO] this is terrible. we should have a way to represent this inside of a data structure that gets prettyprinted. We should not be outputting raw JSON fragments.
      toWriteVue :: [(String, XPileLogW)]
      toWriteVue =  [ ( case out' of
                          Right _ -> [__i|
                            SFL4.mt2text #{rname}:
                          |]
                          Left  _ -> "" -- this little section is inelegant
                          -- If   error, dump // "!! error"
                          -- Else dump out' <> ', \n"
                        <> commentIfError "// !! error" out' <> ", \n"
                      , err)
                    | (rname, (out, err)) <- asVueJSONrules
                    , let out' = toString . encodePretty . itemRPToItemJSON <$> out
                    ]
      vuePrefix :: String
      vuePrefix = -- "# this is vuePrefix from natural4/app/Main.hs\n\n" <>
                  "{"
      vueSuffix :: String
      vueSuffix = "}"
                  -- <> "\n\n# this is vueSuffix from natural4/app/Main.hs"

      jsonProhibitsComments :: String -> String
      jsonProhibitsComments = unlines . filter (not . ("//" `isPrefixOf`)) . lines

      -- [TODO] Terrible hack to make it a legal json, to remove the last trailing comma
      removeLastComma :: String -> String
      removeLastComma unlined
        | length lined > 3 = -- only if there's a valid json in there
          unlines $ take (length lined - 3) lined <> ["}"] <> drop (length lined - 2) lined
        | otherwise = unlined
        where lined = lines unlined

      finalResult :: String
      finalResult =
        removeLastComma $ jsonProhibitsComments $
          intercalate "\n" [vuePrefix, foldMap fst toWriteVue, vueSuffix]

      finalErrors :: [String]
      finalErrors = foldMap snd toWriteVue
    in
      mkResultWithErrors (finalResult, finalErrors)

aasvgTranspiler :: Transpiler
aasvgTranspiler =
  MkTranspiler
    { subdir     = "aasvg"
    , extension  = "" -- mixed
    , entryPoint = go
    , output     = CustomTranspilerOutput out
    }
  where
    go ds = pure (mkResultWithoutErrors (AAS.asAAsvg defaultAAVConfig ds.interpreted ds.parsed))
    out ds subdir _ext r =
      flip possiblySkip r $ \ asaasvg _errors ->
        case workuuid ds of
          Just dir -> do
            let dname = dir </> subdir </> ds.timestamp
            if HashMap.null asaasvg
              then do
                createDirectoryIfMissing True dname
                appendFile (dname </> "index" -<.> "html") "<!-- this file intentionally left blank -->"
              else DF.sequenceA_
                     [ do
                       mywritefile False dname (fname<>"-tiny")   ext (show svgtiny)
                       mywritefile False dname (fname<>"-full")   ext (show svgfull)
                       mywritefile False dname (fname<>"-anyall") "hs"   [i|#{pShowNoColor hsAnyAllTree}|]
                       mywritefile False dname (fname<>"-anyall") "json" [i|#{encodePretty hsAnyAllTree}|]
                       mywritefile False dname (fname<>"-qtree")  "hs"   [i|#{pShowNoColor hsQtree}|]
                       mywritefile False dname (fname<>"-qjson")  "json" [i|#{encodePretty hsQtree}|]
                       let fnamext = fname -<.> ext
                           displayTxt = Text.unpack $ SFL4.mt2text n
                       appendFile (dname </> "index" -<.> "html")
                          [__i|
                            <li>
                              <a target="aasvg" href="#{fnamext}">
                                #{displayTxt}
                              </a>
                            </li>
                          |]
                        -- "<li> " <> "<a target=\"aasvg\" href=\"" <> fnamext <> "\">" <> displayTxt <> "</a></li>\n"
                   | (n,(svgtiny,svgfull,hsAnyAllTree,hsQtree)) <- sortOn (fmap SFL4.mtexpr2text . fst) $ HashMap.toList asaasvg
                   , let (fname, ext) = (take 127 (snakeScrub (SFL4.mtexpr2text <$> n)), "svg")
                   ]
            myMkLink ds.timestamp $ dir </> subdir </> "LATEST"
          Nothing ->
            pure () -- no on-screen output

simalaTranspiler :: Transpiler
simalaTranspiler = undefined
  -- simpleTranspiler "simala" "simala" (withErrors (onlyRules id))

rulesTranspiler :: Transpiler
rulesTranspiler =
  screenTranspiler "rules" (withoutErrors (onlyRules id))

classesTranspiler :: Transpiler
classesTranspiler =
  screenTranspiler "classes" (withoutErrors (onlyInterpreted SFL4.classtable))

symtabTranspiler :: Transpiler
symtabTranspiler =
  screenTranspiler "symtab" (withoutErrors (onlyInterpreted SFL4.scopetable))

uppaalTranspiler :: Transpiler
uppaalTranspiler =
  MkTranspiler
    { subdir     = "uppaal"
    , extension  = "" -- screen
    , entryPoint = \ ds -> pure (mkResultWithoutErrors (Uppaal.toL4TA ds.parsed))
    , output     = DefaultTranspilerOutput
                     (MkTranspilerOutputConfig
                       { ppFile   = const Nothing
                       , writer   = writeFile
                       , ppStdout = print2
                       }
                     )
    }
  where
    print2 x = do
      pPrint x
      putStrLn $ Uppaal.taSysToString x

--
-- Utilities
--

-- | output only "stdout" to outfile
mywritefile :: Bool -> FilePath -> FilePath -> String -> String -> IO ()
mywritefile doLink dirname filename ext s = do
  createDirectoryIfMissing True dirname
  let mypath = dirname </> filename -<.> ext
      mylink = dirname </> "LATEST" -<.> ext
  writeFile mypath s
  when doLink $ myMkLink (filename -<.> ext) mylink

myMkLink :: FilePath -> FilePath -> IO ()
myMkLink filename mylink = do
  let mylink_tmp = [i|#{mylink}-tmp|]
  createFileLink filename mylink_tmp
  renameFile mylink_tmp mylink

snakeScrub :: [Text.Text] -> String
snakeScrub =
  Text.intercalate "-"
    >>> PCRE.gsub [PCRE.re|[^a-zA-Z0-9_\-]|\s+|] ("" :: Text.Text)
    >>> Text.unpack
    -- >>> partition (`elem` ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_-")
    -- >>> fst

-- | if the return value of an xpLog is a Left, dump to output file with the error message commented; otherwise dump the regular output.
commentIfError :: String -> Either XPileLogW String -> String
commentIfError commentPrefix = either (foldMap prependCommentPrefix) id
  -- Disjunction elimination, prepending the comment prefix to each line before
  -- concating when eliminating the error log (ie XPileLogW) to String.
  where
    prependCommentPrefix str = [i|#{commentPrefix} #{str}|]

