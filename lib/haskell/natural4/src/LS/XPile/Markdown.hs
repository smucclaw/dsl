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
rpFilter (RPParamText pt) = pt2multiterm pt
rpFilter (RPConstraint mt1 rel mt2) = mt1 ++ mt2
rpFilter (RPBoolStructR mt1 rel bsr) = mt1++ bs2mt bsr
rpFilter (RPnary rel rp) = rpFilter rp
rpFilter (RPMT mt) = mt

rl2bs :: [Rule] -> [BoolStructR]
-- rl2bs rl = map snd $ qaHornsR interp
--   where
--     interp = l4interpret defaultInterpreterOptions rl
rl2bs rl = concatMap r2b rl

r2b :: Rule -> [BoolStructR]
r2b rl = case getBSR rl of
    Nothing  -> []
    Just x -> [x]


bs :: [Rule] -> [Text.Text]
bs rl = map (mt2text . bs2mt) $ rl2bs rl


bs2mt :: BoolStructR -> MultiTerm
bs2mt (AA.All _ x)  = concatMap bs2mt x
bs2mt (AA.Any _ x)  = concatMap bs2mt x
bs2mt (AA.Not x)    =           bs2mt x
bs2mt (AA.Leaf rp)  =           rpFilter rp

-- Leaf (RPConstraint [MTT "Loss or Damage"] RPis [MTT "caused by",MTT "birds"])]

-- All Nothing [Any (Just (Pre "Loss or Damage caused by")) [Leaf (RPMT [MTT "rodents"]),Leaf (RPMT [MTT "insects"]),Leaf (RPMT [MTT "vermin"]),Leaf (RPMT [MTT "birds"])],
-- Not (Any Nothing [All Nothing [Leaf (RPConstraint [MTT "Loss or Damage"] RPis [MTT "to Contents"]),Leaf (RPConstraint [MTT "Loss or Damage"] RPis [MTT "caused by",MTT "birds"])]