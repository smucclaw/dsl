{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LS.XPile.LogicalEnglish.ReplaceTxt
  ( replaceTxt
  )
where

import Control.Category ((>>>))
import Data.Sequences (LazySequence (..))
import Data.String.Interpolate (i)
import Data.Text qualified as T
import LS.XPile.LogicalEnglish.Types
  ( TemplateVar (..),
    VCell (..),
  )
import Text.Regex.PCRE.Heavy qualified as PCRE

{- | 
TODO: Would be better to read in a dictionary of what/how to replace from some config file,
a config file that is kept in sync with the downstream stuff 
(since have to do this kind of replacement in the converse direction when generating justification)
-}
replaceTxt :: T.Text -> T.Text
replaceTxt =
  T.strip
    >>> replaceInf
    >>> replacePeriod
    >>> replaceComma
    >>> replacePercent
    >>> trimWhitespaces

trimWhitespaces :: T.Text -> T.Text
trimWhitespaces = T.strip >>> PCRE.gsub [PCRE.re|\s+|] (" " :: T.Text)

replaceComma :: T.Text -> T.Text
replaceComma = PCRE.gsub [PCRE.re|,|] (" COMMA " :: T.Text)

replacePercent :: T.Text -> T.Text
replacePercent = PCRE.gsub [PCRE.re|%|] (" PERCENT " :: T.Text)

  {- ^ it's cleaner not to put a space after `percent`
    because it's usually something like "100% blah blah" in the encoding
    So if you add a space after, you end up getting "100 percent  blah blah", which doesn't look as nice.
    And similarly with `comma`.

    Couldn't figure out quickly how to get doc tests to work for this function, so not bothering with that for now. (TODO)
    >>> replaceTxt ""
    ""
    >>> replaceTxt "100% guarantee"
    "100 PERCENT guarantee"

    >>> replaceTxt "rocks, stones, and trees"
    "rocks COMMA stones COMMA and trees"
  -}

-- LE, as with Prolog, uses "inf" to denote positive infinity.
replaceInf :: T.Text -> T.Text
replaceInf =
  PCRE.sub
    [PCRE.re|^infinity$|^Infinity$|^Inf$|]
    ("inf" :: T.Text)

-- LE has no trouble parsing dots that appear in numbers, ie things like
-- "clause 2.1 applies" is fine.
-- However, dots used as a full-stop, as in "The car is blue." is not ok
-- and so that "." needs to be turned into "PERIOD".
-- Also, references to clause numbers of the form "14.1.3" are not ok and so
-- must be replaced with "14.1 PERIOD 3".
replacePeriod :: T.Text -> T.Text
replacePeriod = replaceFullStop >>> replaceClauseNums
  where
    replaceFullStop =
      PCRE.gsub
        -- https://stackoverflow.com/a/45616898 
        [PCRE.re|[a-zA-z] + [^0-9\s.]+|\.(?!\d)|]
        (" PERIOD " :: T.Text)

    replaceClauseNums =
      PCRE.gsub
        [PCRE.re|(\d+\.\d+)\.(\d+)|]
        \(x:y:_ :: [T.Text]) -> [i|#{x} PERIOD #{y}|] :: T.Text

-- replaceHyphen =
--   PCRE.gsub
--     -- https://stackoverflow.com/a/31911114
--     [PCRE.re|(?=\S*[-])([a-zA-Z]+)\-([a-zA-Z]+)|]
--     \(s0:s1:_) -> mconcat [s0, " HYPHEN ", s1] :: T.Text