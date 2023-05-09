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
import Text.Pandoc.Writers.Docx
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Writers.LaTeX (writeLaTeX)

import Text.Pandoc (Format(..), handleError, runIO, runIOorExplode, Extension (..), ReaderOptions(..), Pandoc, def, renderError)
import qualified Data.ByteString.Lazy.Char8 as ByteString

-- import Debug.Trace (trace)

doc :: [Rule] -> IO ByteString.ByteString
doc rl = do runIO (writeDocx def =<< readMarkdown def (Text.pack $ bsMarkdown rl)) >>= handleError

pdf :: [Rule] -> IO ByteString.ByteString
pdf rl = do
    pdf <- runIOorExplode (makePDF "xelatex" [] writeLaTeX def =<< readMarkdown def (Text.pack $ bsMarkdown rl))
    case pdf of
        Right p -> return p
        Left err -> do
            return err

bsMarkdown :: [Rule] -> String
bsMarkdown rl = Text.unpack $ Text.unwords $ bs rl

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

-- Leaf (RPConstraint [MTT "Loss or Damage"] RPis [MTT "caused by",MTT "birds"])]

-- All Nothing [Any (Just (Pre "Loss or Damage caused by")) [Leaf (RPMT [MTT "rodents"]),Leaf (RPMT [MTT "insects"]),Leaf (RPMT [MTT "vermin"]),Leaf (RPMT [MTT "birds"])],
-- Not (Any Nothing [All Nothing [Leaf (RPConstraint [MTT "Loss or Damage"] RPis [MTT "to Contents"]),Leaf (RPConstraint [MTT "Loss or Damage"] RPis [MTT "caused by",MTT "birds"])]