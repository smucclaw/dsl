{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Markdown
  ( bsMarkdown )
where

import AnyAll qualified as AA
import Data.Text qualified as Text
import LS.NLP.NLG (NLGEnv, nlg)
import LS.RelationalPredicates (getBSR)
import LS.Rule (Rule)
import LS.Types
  ( BoolStructR,
    MultiTerm,
    RelationalPredicate (..),
    mt2text,
    pt2multiterm,
  )

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
bsMarkdown envs rl = Text.unpack . Text.intercalate "  " <$> flip eachnlg rl `traverse` envs -- (sequence [eachnlg env rl | env <- envs])

eachnlg :: NLGEnv -> [Rule] -> IO Text.Text
eachnlg env = (Text.unwords <$>) . traverse (nlg env)

-- bsMarkdown :: [Rule] -> String
-- bsMarkdown rl = Text.unpack $ Text.unwords $ bs rl

rpFilter :: RelationalPredicate -> MultiTerm
rpFilter (RPParamText pt) = pt2multiterm pt
rpFilter (RPConstraint mt1 rel mt2) = mt1 <> mt2
rpFilter (RPBoolStructR mt1 rel bsr) = mt1 <> bs2mt bsr
rpFilter (RPnary rel rps) = foldMap rpFilter rps
rpFilter (RPMT mt) = mt

rl2bs :: [Rule] -> [BoolStructR]
rl2bs = foldMap r2b

r2b :: Rule -> [BoolStructR]
r2b (getBSR -> Just x) = [x]
r2b _ = []

bs :: [Rule] -> [Text.Text]
bs rl = mt2text . bs2mt <$> rl2bs rl

bs2mt :: BoolStructR -> MultiTerm
bs2mt (AA.All _ x)  = foldMap bs2mt x
bs2mt (AA.Any _ x)  = foldMap bs2mt x
bs2mt (AA.Not x)    =           bs2mt x
bs2mt (AA.Leaf rp)  =           rpFilter rp
