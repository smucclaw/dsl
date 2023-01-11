{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| transpiler to SVG visualization of the AnyAll and/or trees.

Largely a wrapper. Most of the functionality is in the anyall lib.

-}

module LS.XPile.Purescript where

import LS
import AnyAll.BoolStruct (alwaysLabeled)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Prettyprinter
import Text.Pretty.Simple (pShowNoColor)
import qualified AnyAll as AA
import qualified Data.Map as Map
import LS.NLP.NLG
import LS.Interpreter
import Control.Monad (guard)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.List as DL
import Data.Map ((!))
import Data.Bifunctor (second)


-- | extract the tree-structured rules from Interpreter
-- currently: construct a Data.Map of rulenames to exposed decision root expanded BSR
-- in future: also ship out a Marking which represents the TYPICALLY values
-- far future: construct a JSON with everything in it, and get the Purescript to read the JSON, so we are more interoperable with non-FP languages

data Tuple a b = Tuple a b
  deriving (Show, Eq, Ord)

toTuple :: (a,b) -> Tuple a b
toTuple (x,y) = Tuple x y

-- startWithRule :: InterpreterOptions -> NLGEnv -> RunConfig -> [Rule] ->


-- qaHornsR :: Interpreted -> [([RuleName], BoolStructR)]
-- qaHornsR l4i =
--      [ ( ruleLabelName <$> uniqrs
--        , expanded)
--      | (grpval, uniqrs) <- groupedByAOTree l4i $ -- NUBBED
--                            exposedRoots l4i      -- EXPOSED
--      , not $ null grpval
--      , expanded <- expandBSR l4i 1 <$> maybeToList (getBSR (DL.head uniqrs))
--      ]


-- two boolstructT: one question and one phrase
namesAndStruct :: NLGEnv -> [Rule] -> [([RuleName], [BoolStructT])]
-- namesAndStruct env rl =
--   [(names, (fakeStruct)) | (names, bs) <- qaHornsT (l4interpret defaultInterpreterOptions rl)]
--   where
--     fakeStruct = map (ruleQuestions env) rl
namesAndStruct env rl =
  [ (names, ((bs) : (unsafePerformIO q))) | (names, bs) <- qaHornsT interp, q <- questStruct]
  where
    questStruct = map (ruleQuestions env) rl -- [AA.OptionallyLabeledBoolStruct Text.Text]
    interp = l4interpret defaultInterpreterOptions rl

biggestQ :: NLGEnv -> [Rule] -> Maybe BoolStructT
biggestQ env rl = do
  let q = namesAndStruct env rl
      flattened = (\(x,y) -> (x, AA.extractLeaves (last y))) <$> q
      onlyqs = Data.Bifunctor.second last <$> q
      -- onlyqs = (\(x,y) -> (x, (last y))) <$> q
      sorted = DL.reverse $ DL.sortOn (DL.length . snd) flattened
  guard (not $ null sorted)
  return ((Map.fromList onlyqs) ! (fst $ DL.head sorted))

asPurescript :: NLGEnv -> [Rule] -> String
asPurescript env rl =
     show (vsep
           [ "toplevelDecisions :: Map.Map (String) (Item String)"
           , "toplevelDecisions = Map.fromFoldable " <>
             (pretty $ TL.unpack (
                 pShowNoColor
                   [ toTuple ( T.intercalate " / " (T.unwords <$> names)
                            --  , (T.pack $ unsafePerformIO $ boolStructQuestion env bs))
                            , ((last bs)))

                   | (names,bs) <- namesAndStruct env rl
                   ]
                 )
             )
           , "toplevelDefaultMarking :: Marking"
           , "toplevelDefaultMarking = Marking $ Map.fromFoldable " <>
             (pretty . TL.unpack
              . TL.replace "False" "false"
              . TL.replace "True" "true"
              . pShowNoColor $
              fmap toTuple . Map.toList . AA.getMarking $
              getMarkings (l4interpret defaultInterpreterOptions rl)
             )
           ]
          )
