{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LS.XPile.Markdown
  ( bsMarkdown
  )
where

import LS.RelationalPredicates ( getBSR )
import LS.Rule ( Rule )
import LS.Types
    ( mt2text,
      pt2multiterm,
      BoolStructR,
      MultiTerm,
      RelationalPredicate(..) )
import LS.NLP.NLG ( nlg, NLGEnv )
import qualified AnyAll as AA
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Monad (liftM2)

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Paths_natural4 ()

-- import Debug.Trace (trace)

-- doc :: [NLGEnv] -> [Rule] -> IO ByteString.ByteString
-- doc env rl = do
--   txt <- bsnlg env rl
--   runIO (writeDocx def =<< readMarkdown def txt) >>= handleError

-- pdf :: [NLGEnv] -> [Rule] -> IO ByteString.ByteString
-- pdf env rl = do
--     txt <- bsnlg env rl
--     templ <- getDataFileName "src/LS/XPile/templates/default.latex"
--     template <- runIOorExplode $ getTemplate templ
--     Right pandTemplate <- compileTemplate "" template :: IO (Either String (Template Text.Text))
--     pdf <- runIOorExplode (makePDF "xelatex" [] writeLaTeX (def {writerTemplate = Just pandTemplate}) =<< readMarkdown def txt)
--     case pdf of
--         Right p -> return p
--         Left err -> do
--             return err

bsMarkdown :: [NLGEnv] -> [Rule] -> IO String
bsMarkdown envs rl = fmap (Text.unpack . Text.intercalate "  ") (sequence [eachnlg env rl | env <- envs])

eachnlg :: NLGEnv -> [Rule] -> IO Text.Text
eachnlg env rl = do
  txt <- mapM (nlg env) rl
  return $ Text.unwords txt

-- bsMarkdown :: [Rule] -> String
-- bsMarkdown rl = Text.unpack $ Text.unwords $ bs rl

rpFilter :: RelationalPredicate -> MultiTerm
rpFilter (RPParamText pt) = pt2multiterm pt
rpFilter (RPConstraint mt1 rel mt2) = mt1 ++ mt2
rpFilter (RPBoolStructR mt1 rel bsr) = mt1++ bs2mt bsr
rpFilter (RPnary rel rp) = rpFilter rp
rpFilter (RPMT mt) = mt

rl2bs :: [Rule] -> [BoolStructR]
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