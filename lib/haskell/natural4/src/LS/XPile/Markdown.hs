{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LS.XPile.Markdown where

import LS
import LS.Types
import LS.NLP.NLG
import qualified AnyAll as AA
import qualified Data.Map as Map
import qualified Data.Text as Text
-- import Debug.Trace (trace)

markdown :: NLGEnv -> [Rule] -> IO String
markdown env rl = do
  nl <- mapM (nlg env) rl
  print $ concatMap Text.unpack nl
  print "---"
  return $ concatMap Text.unpack nl

bsMarkdown :: [Rule] -> IO String
bsMarkdown rl = do
  let txt = Text.unwords $ bs rl
  return $ Text.unpack txt

rpFilter :: RelationalPredicate -> MultiTerm
rpFilter (RPConstraint mt1 rel mt2) = mt1 ++ mt2
rpFilter (RPBoolStructR mt1 rel bsr) = mt1 ++ bs2mt bsr
rpFilter (RPnary rel rp) = rpFilter rp
rpFilter x = rp2mt x

rl2bs :: [Rule] -> [BoolStructR]
rl2bs rl = map snd $ qaHornsR interp
  where
    interp = l4interpret defaultInterpreterOptions rl

bs :: [Rule] -> [Text.Text]
bs rl = map (mt2text . bs2mt) $ rl2bs rl

bs2mt :: BoolStructR -> MultiTerm
bs2mt (AA.All _ x)  = concatMap bs2mt x
bs2mt (AA.Any _ x)  = concatMap bs2mt x
bs2mt (AA.Not x)    =           bs2mt x
bs2mt (AA.Leaf rp)  =           rpFilter rp

-- Leaf (RPConstraint [MTT "Loss or Damage"] RPis [MTT "caused by",MTT "birds"])]