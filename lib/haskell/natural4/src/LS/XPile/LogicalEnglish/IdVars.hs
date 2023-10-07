{-# OPTIONS_GHC -W #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.LogicalEnglish.IdVars (
    idVarsInHC
  -- , idVarsInAP
  -- , idVarsInBody
) where

import Data.Coerce (coerce)
import Data.HashSet qualified as HS
import Data.Sequences (fromStrict, toStrict)
import Data.Text qualified as T
import Text.Regex.PCRE.Heavy qualified as PCRE
import Text.Replace (Replace (Replace), listToTrie, replaceWithTrie)

import LS.XPile.LogicalEnglish.Types
  (
      BoolPropn(..)
    -- L4-related types
    , GVar(..)
    , GVarSet
    , Cell(..)
    
    , SimpleL4HC(MkL4FactHc, fgiven, fhead,
                 MkL4RuleHc, rgiven, rhead, rbody)

    , AtomicBPropn(..)
    , L4AtomicP

    -- Intermediate representation types, prisms, and constants
    , TemplateVar(..)
    -- , _TempVar
    --, _AposAtom, _NonVarOrNonAposAtom
    -- , aposSuffix
    , VarsHC(MkVarsFact,
             MkVarsRule, 
             vfhead,
             vrhead, vrbody)    
    , AtomicPWithVars
    , VCell(..)
  )
import Data.String.Interpolate (i)

-- $setup
-- >>> import Data.Text qualified as T
-- >>> :seti -XOverloadedStrings
-- >>> import Text.Replace (Replace (Replace), listToTrie, replaceWithTrie)
-- >>> import Data.Sequences (fromStrict, toStrict)

idVarsInHC :: SimpleL4HC -> VarsHC
idVarsInHC = \case
  MkL4FactHc{..} -> MkVarsFact { vfhead =  idVarsInAP fgiven fhead }
  MkL4RuleHc{..} -> MkVarsRule { vrhead =  idVarsInAP rgiven rhead
                               , vrbody = idVarsInBody rgiven rbody }

-- TODO: Refactor with a Reader when time permits to de-emphasize the gvars threading
{- | Identifies vars in L4AtomicP:
* non-vars (according to the spec) stay as text
* things that should be vars (according to the spec) get converted to TemplateVars
-}
idVarsInAP :: GVarSet -> L4AtomicP -> AtomicPWithVars
idVarsInAP gvars = postprocAP . fmap makeVCell
  where
    makeVCell = cell2vcell gvars

-- | Replace "." with "dot" and "," with "comma", in the Pred txts of ABPatomics
postprocAP :: AtomicPWithVars -> AtomicPWithVars
postprocAP = \case
  ABPatomic cells -> ABPatomic $ fmap replaceTxtVCell cells
  others          -> others

idVarsInBody :: GVarSet -> BoolPropn L4AtomicP -> BoolPropn AtomicPWithVars
idVarsInBody gvars = fmap $ idVarsInAP gvars


---- helpers

-- | Replace text in VCells
replaceTxtVCell :: VCell -> VCell
replaceTxtVCell = \case
  TempVar (MatchGVar txt)  -> TempVar $ MatchGVar $ replaceTxt txt
  TempVar (EndsInApos txt) -> TempVar $ EndsInApos $ replaceTxt txt
  TempVar (IsNum txt)      -> TempVar $ IsNum $ replaceTxt txt
  -- tv@(TempVar _) -> tv
  AposAtom txt             -> AposAtom $ replaceTxt txt
  -- apAtm@(AposAtom _) -> apAtm
  NonVarOrNonAposAtom txt  -> NonVarOrNonAposAtom $ replaceTxt txt

{- | 
TODO: Would be better to read in a dictionary of what/how to replace from some config file,
a config file that is kept in sync with the downstream stuff 
(since have to do this kind of replacement in the converse direction when generating justification)
-}
replaceTxt :: T.Text -> T.Text
replaceTxt = replacePeriod . replaceTxtPlain . replaceInf

replaceTxtPlain :: T.Text -> T.Text
replaceTxtPlain = toStrict . replaceWithTrie replacements . fromStrict
  where
    replacements = listToTrie replaceCommaPercent

    replaceCommaPercent =
      [ Replace "," " COMMA",
        Replace "%" " PERCENT"
      ]
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
-- TODO: Add test cases for this replacement.`
replaceInf :: T.Text -> T.Text
replaceInf =
  PCRE.sub
    [PCRE.re|^\s*infinity\s*$|^\s*∞\s*$|]
    ("inf" :: T.Text)

-- LE has no trouble parsing dots that appear in numbers, ie things like
-- "clause 2.1 applies" is fine.
-- However, dots used as a full-stop, as in "The car is blue." is not ok
-- and so that "." needs to be turned into "PERIOD".
-- Also, references to clause numbers of the form "14.1.3" are not ok and so
-- must be replaced with "14.1 PERIOD 3".
replacePeriod :: T.Text -> T.Text
replacePeriod = replaceClauseNums . replaceFullStop
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
        
{- | Convert a SimplifiedL4 Cell to a VCell
The code for simplifying L4 AST has established these invariants:  
  * every IS NUM has had the IS removed, with the number converted to T.Text and wrapped in a MkCellIsNum
  * every IS tt was NOT an IS NUM has been marked as belonging to the ABPBaseIs variant of AtomicBP

So the only time we need to think about IS-es, going forward, is when we have a MkCellIsNum. 
In other words, we can convert an arbitrary Cell to a VCell as long as we know the set of given vars, without having to check what other cells are / are not around it.
-}
cell2vcell :: GVarSet -> Cell -> VCell
cell2vcell gvars = \case
  MkCellT celltxt    -> celltxt2vcell gvars celltxt
  MkCellIsNum numtxt -> TempVar (IsNum numtxt)

celltxt2vcell :: GVarSet -> T.Text -> VCell
celltxt2vcell gvars (T.stripSuffix "'s" -> Just prefix) = 
-- NOTE / TODO: this matching on "'s" is a bit brittle cos unicode
    if txtIsAGivenVar gvars prefix 
    then TempVar (EndsInApos prefix)
    else AposAtom prefix 
celltxt2vcell gvars celltxt
  | txtIsAGivenVar gvars celltxt = TempVar (MatchGVar celltxt)
  | otherwise = NonVarOrNonAposAtom celltxt

txtIsAGivenVar :: GVarSet -> T.Text -> Bool
txtIsAGivenVar gvars txt = HS.member (coerce txt) gvars


-- {-  Deprecating this and the next fn b/c the encoding suggests terms other than the args for op of might not just be either MatchGVar or EndsInApos --- they can also be atoms / non-variables
-- -}
-- term2tvar :: GVarSet -> Term -> TemplateVar
-- term2tvar gvars = \case
--   MkCellT trm -> whichTVar trm
--   MkCellIsNum trm -> whichTVar trm
--   where 
--     whichTVar :: T.Text -> TemplateVar
--     whichTVar trm
--       | txtIsAGivenVar gvars trm = MatchGVar trm
--       | checkApos gvars trm = EndsInApos trm
--       | otherwise = error "shouldn't be anything else"
--         -- TODO: add a check upfront for this 
-- optOfArg :: Cell -> TemplateVar
-- optOfArg = \case
--   MkCellT t -> OpOfVarArg t
--   MkCellIsNum t -> OpOfVarArg t
