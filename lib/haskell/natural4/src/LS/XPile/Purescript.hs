{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-| transpiler to SVG visualization of the AnyAll and/or trees.

Largely a wrapper. Most of the functionality is in the anyall lib.

-}

module LS.XPile.Purescript where

import AnyAll qualified as AA
import AnyAll.BoolStruct (alwaysLabeled)
import Control.Applicative (liftA2)
import Control.Monad (guard, join, liftM, unless, when, forM_)
import Data.Bifunctor (second)
import Data.Char qualified as Char
import Data.Either (lefts, rights)
import Data.HashMap.Strict ((!))
import Data.HashMap.Strict qualified as Map
import Data.List (sortOn)
import Data.List qualified as DL
import Data.List.Split (chunk)
import Data.Maybe (listToMaybe)
import Data.Ord qualified
import Data.String.Interpolate (i, __i)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Flow ((|>))
import LS.Interpreter ( qaHornsT, getMarkings )
import LS.Rule ( Rule(DefNameAlias), ruleLabelName, Interpreted(..) )
import LS.Types
    ( RuleName, BoolStructT, mt2text, defaultInterpreterOptions )
import LS.NLP.NL4Transformations ()
import LS.NLP.NLG
    ( NLGEnv(gfLang, interpreted), ruleQuestions, expandRulesForNLG )
import LS.Utils ((|$>))
import LS.XPile.Logging
    ( xpReturn, mutter, XPileLogE, XPileLog, mutters, mutterd, mutterd1, mutterd2, mutterdhs, mutterdhsf )
import PGF ( showLanguage )
import Text.Pretty.Simple (pShowNoColor)

-- | extract the tree-structured rules from Interpreter
-- currently: construct a Data.Map of rulenames to exposed decision root expanded BSR
-- in future: also ship out a Marking which represents the TYPICALLY values
-- far future: construct a JSON with everything in it, and get the Purescript to read the JSON, so we are more interoperable with non-FP languages


-- | shim for Purescript tuples which use slightly different syntax
data Tuple a b = Tuple a b
  deriving (Show, Eq, Ord)

-- | output Haskell tuples to Purescript
toTuple :: (a,b) -> Tuple a b
toTuple (x,y) = Tuple x y

-- | RuleName to text multiterm
textMT :: [RuleName] -> [T.Text]
textMT = map mt2text


mutterRuleNameAndBS ::          [([RuleName], [BoolStructT])]
                    -> XPileLog [([RuleName], [BoolStructT])]
mutterRuleNameAndBS rnbss = do
  mutterd 3 "rulename, bs pairs:"
  forM_ rnbss $ \(names, bs) -> do
    mutterdhsf 4 (T.unpack (T.intercalate " / " (mt2text <$> names)))
      pShowNoColorS bs
  return rnbss

-- two boolstructT: one question and one phrase
namesAndStruct :: Interpreted -> [Rule] -> XPileLog [([RuleName], [BoolStructT])]
namesAndStruct l4i rl = do
  mutter $ "*** namesAndStruct: running on " ++ show (length rl) ++ " rules"
  mutter "calling qaHornsT against l4i"
  mutterRuleNameAndBS [ (names, [bs]) | (names, bs) <- qaHornsT l4i]

-- | for each rule, construct the questions for that rule;
-- and then jam them together with all the names for all the rules???
namesAndQ :: NLGEnv -> [Rule] -> XPileLog [([RuleName], [BoolStructT])]
namesAndQ env rl = do
  mutterdhsf 3 "namesAndQ: name" show name
  questStruct <- traverse (ruleQuestions env alias) (expandRulesForNLG env rl)
  let wut = concat [ [ (name, q)
                     | q' <- q ]
                   | q <- questStruct ]
  mutter $ "*** wut the heck are we returning? like, " ++ show (length w) ++ " things."
  sequence_ [ mutterdhsf 4 (show n) pShowNoColorS w | (n,w) <- zip [1..] wut ]
  return wut
  where
    name = map ruleLabelName rl
    alias = listToMaybe [ (you,org) | DefNameAlias you org _ _ <- rl]
    -- [AA.OptionallyLabeledBoolStruct Text.Text]

-- | not sure why this is throwing away information
combine :: [([RuleName], [BoolStructT])]
        -> [([RuleName], [BoolStructT])]
        -> XPileLog [([RuleName], [BoolStructT])]
combine x y = combine' 3 x y

combine' :: Int -- ^ depth
         -> [([RuleName], [BoolStructT])]
         -> [([RuleName], [BoolStructT])]
         -> XPileLog [([RuleName], [BoolStructT])]

combine' d [] []     = mutter "*** combine: case 1, nil" >> pure []
combine' d (b:bs) [] = mutter "*** combine: case 2, nil" >> pure []
combine' d [] (q:qs) = mutter "*** combine: case 3, nil" >> pure []
combine' d (b:bs) (q:qs) = do
  mutterd  d "combine: case 4, non-nil"
  mutterd1 d "input"
  mutterdhsf (d+2) "fst b"    pShowNoColorS (fst b)
  mutterdhsf (d+2) "snd b ++" pShowNoColorS (snd b)
  mutterdhsf (d+2) "snd q"    pShowNoColorS (snd q)
  (:) <$> pure (fst b, snd b <> snd q) <*> combine' (d+1) bs qs

-- | helper function; basically a better show, from the pretty-simple package
pShowNoColorS :: (Show a) => a -> String
pShowNoColorS = TL.unpack . pShowNoColor


-- [TODO] shouldn't this recurse down into the All and Any structures?
-- something like fixNot AA.Any k xs = AA.Any k (fixNot <$> xs)
fixNot :: BoolStructT -> BoolStructT
fixNot (AA.Leaf x) = AA.Leaf x
fixNot (AA.Not (AA.Leaf x)) = AA.Leaf x
fixNot y = y

-- | this throws away the first argument, in favour of the second. Not sure about this ...
justQuestions :: BoolStructT -> [BoolStructT] -> BoolStructT
justQuestions (AA.All Nothing a) q = AA.All Nothing q
justQuestions (AA.Any Nothing a) q = AA.Any Nothing q
justQuestions xs y = xs

justStatements :: BoolStructT -> [BoolStructT] -> BoolStructT
justStatements (AA.All Nothing a) q = AA.All Nothing a
justStatements (AA.Any Nothing a) q = AA.Any Nothing a
justStatements xs y = xs

labelQs :: [AA.OptionallyLabeledBoolStruct T.Text] -> [AA.BoolStruct (AA.Label T.Text) T.Text]
labelQs = map alwaysLabeled

biggestQ :: NLGEnv -> [Rule] -> XPileLog [BoolStructT]
biggestQ env rl = do
  mutter $ "*** biggestQ: running"
  q <- join $ combine <$> namesAndStruct (interpreted env) rl <*> namesAndQ env rl
  let flattened = (\(x,ys) ->
        (x, [ AA.extractLeaves y | y <- ys])) <$> q

      onlyqs = [ (x, justQuestions yh (map fixNot yt))
               | (x, y) <- q
               , Just (yh, yt) <- [DL.uncons y] ]

      sorted = sortOn (Data.Ord.Down . DL.length) flattened
  if not (null sorted)
    then case fst (DL.head sorted) `Map.lookup` Map.fromList onlyqs of
           Nothing -> mutter ("biggestQ didn't work, couldn't find " ++ show (fst (DL.head sorted)) ++ " in dict") >> return []
           Just x  -> return [x]
    else return []

biggestS :: NLGEnv -> [Rule] -> XPileLog [BoolStructT]
biggestS env rl = do
  mutter $ "*** biggestS running"
  q <- join $ combine <$> namesAndStruct (interpreted env) rl <*> namesAndQ env rl
  let flattened = (\(x,ys) ->
        (x, [ AA.extractLeaves y | y <- ys])) <$> q
      onlys = [ (x, justStatements yh (map fixNot yt))
              | (x,y) <- q
              , Just (yh, yt) <- [DL.uncons y] ]
      sorted = sortOn (Data.Ord.Down . DL.length) flattened
  return $
    if not (null sorted)
    then pure $ Map.fromList onlys ! fst (DL.head sorted)
    else []

asPurescript :: NLGEnv -> [Rule] -> XPileLogE String
asPurescript env rl = do
  let nlgEnvStr = env |> gfLang |> showLanguage
  let l4i       = env |> interpreted
  mutter [i|** asPurescript running for gfLang=#{nlgEnvStr}|]

  mutterd 3 "building namesAndStruct"
  nAS <- namesAndStruct l4i rl
  mutterd 3 "building namesAndQ"
  nAQ <- namesAndQ      env rl
  c'  <- combine nAS nAQ
  mutterdhsf 3 "c'" pShowNoColorS c'

  guts <- sequence [
    do
      mutterdhsf 3 "names: " show ( mt2text <$> names )
      mutterdhsf 4 "hbs = head boolstruct" show hbs
      mutterdhsf 4 "tbs = tail boolstruct" show tbs
      mutterdhsf 4 "fixedNot" show fixedNot
      mutterdhsf 4 "jq" show jq
      mutterdhsf 4 "labeled" show labeled
      xpReturn $ toTuple ( T.intercalate " / " (mt2text <$> names) , labeled)

    | (names,bs) <- c'
    , Just (hbs, tbs) <- [DL.uncons bs]
    , let fixedNot = map fixNot tbs
          jq       = justQuestions hbs fixedNot
          labeled  = alwaysLabeled jq
    ]
  let nlgEnvStrLower = Char.toLower <$> nlgEnvStr
      listOfMarkings = Map.toList . AA.getMarking $ getMarkings l4i

  mutter "*** Markings"
  mutters [ "**** " ++ T.unpack (fst m) ++ "\n" ++ show (snd m) | m <- listOfMarkings]

  xpReturn
    [__i|
      #{nlgEnvStrLower} :: Object.Object (Item String)
      #{nlgEnvStrLower} = Object.fromFoldable
        #{pShowNoColor guts}
      #{nlgEnvStrLower}Marking :: Marking
      #{nlgEnvStrLower}Marking = Marking $ Map.fromFoldable
        #{TL.replace "False" "false"
          . TL.replace "True" "true"
          . pShowNoColor $
              fmap toTuple listOfMarkings}
    |]
          -- #{pretty $ showLanguage $ gfLang env}Statements :: Object.Object (Item String)
          -- , (pretty $ showLanguage $ gfLang env) <> "Statements = Object.fromFoldable " <>
          --   (pretty $ TL.unpack (
          --       pShowNoColor
          --         [ toTuple ( T.intercalate " / " (mt2text <$> names)
          --                 , alwaysLabeled (justStatements (head bs) (map fixNot (tail bs))))
          --         | (names,bs) <- (combine (namesAndStruct env rl) (namesAndQ env rl))
          --         ]
          --       )
          --   )

translate2PS :: [NLGEnv] -> NLGEnv -> [Rule] -> XPileLogE String
translate2PS nlgEnv eng rules = do
  mutter $ "** translate2PS: running against " ++ show (length rules) ++ " rules"
  mutter $ "*** nlgEnv has " ++ show (length nlgEnv) ++ " elements"
  mutter $ "*** eng.gfLang = " ++ show (gfLang eng)
  mutter $ "** calling biggestQ"
  bigQ <- biggestQ eng rules
  mutter $ "** got back bigQ"
  mutter $ show bigQ
  let topBit =
        bigQ
          |$> alwaysLabeled
          |> pShowNoColor
          |> TL.unpack
          |> init
          |> tail
          |> interviewRulesRHS2topBit
  bottomBit <- traverse (`asPurescript` rules) nlgEnv
  mutterdhsf 2 "topBit" pShowNoColorS topBit
  mutterdhsf 2 "bottomBit without running rights" pShowNoColorS bottomBit
  -- [TODO] make this work
  -- mutters (concat $ lefts bottomBit) >>
  mutterdhsf 2 "actual bottomBit output" pShowNoColorS (rights bottomBit)
  xpReturn [__i|
    #{topBit}

    #{unlines $ rights bottomBit}
  |]

interviewRulesRHS2topBit :: String -> String
interviewRulesRHS2topBit interviewRulesRHS =
  let interviewRulesRHS' = case interviewRulesRHS of
        (null -> True) -> [i|Leaf ""|]
        _ -> interviewRulesRHS
  in [__i|
    -- This file was automatically generated by natural4.
    -- Do not edit by hand.
    -- Instead, revise the toolchain starting at smucclaw/dsl/lib/haskell/natural4/app/Main.hs

    module RuleLib.Interview where

    import Prelude
    import Data.Either
    import Data.Maybe
    import Data.Tuple
    import Data.Map as Map
    import Foreign.Object as Object

    import AnyAll.Types

    interviewRules :: Item String
    interviewRules = #{interviewRulesRHS'}

    interviewRules_nl :: NLDict
    interviewRules_nl =
      Map.fromFoldable
        [ ]

  |]

