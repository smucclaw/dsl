{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, RecordWildCards, NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia, DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}


module LS.XPile.LogicalEnglish.GenNLAs (
      nlasFromVarsHC
    , NLATxt(..)
    , _MkNLATxt

    , NLA       -- opaque
    , mkNLA     -- smart constructor
    , getNLAtxt

    , RegexTrav
    , FilterResult(..)
    , removeInternallySubsumed
    , removeRegexMatches
    , removeDisprefdInEquivUpToVarNames
    , regextravifyNLASection
    , regextravifyLENLA
  )
where

import LS.Utils ((<||>))
import Data.Text qualified as T
import Data.Ord (Down(..))
import GHC.Exts (sortWith)
import Data.HashSet qualified as HS
import Data.Containers (difference)
import Data.Hashable (Hashable, hashWithSalt, hashUsing)
import Data.Foldable (fold, foldl', toList)
import Data.Maybe (catMaybes)
-- import Debug.Trace (trace)
import Data.Coerce (coerce)

import LS.XPile.LogicalEnglish.Types
import LS.XPile.LogicalEnglish.Utils (setInsert)
import Data.String (IsString)
import Data.String.Interpolate ( i )

import Data.List.Split (splitOn)
import Data.String.Conversions (cs)
-- import           Data.String.Conversions.Monomorphic
import Text.RawString.QQ
import qualified Text.Regex.PCRE.Heavy as PCRE
-- import Text.Regex.PCRE.Heavy()
import Control.Lens.Regex.Text

import Optics
-- import Data.Text.Optics (unpacked)
import Data.HashSet.Optics (setOf)
import Data.Sequence.Optics (seqOf)
import Data.Containers.NonEmpty (NE, HasNonEmpty, nonEmpty, fromNonEmpty)
-- onNonEmpty, fromNonEmpty, 
import Data.Sequence (Seq)
-- import qualified Data.Sequence as Seq
import Data.Sequences (SemiSequence, intersperse) --groupAllOn
import Data.List qualified as L
import Data.MonoTraversable (Element)
import Prettyprinter(Pretty)


newtype NLATxt = MkNLATxt T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Semigroup, Monoid, Hashable, Pretty)
makePrisms ''NLATxt

type RegexTrav = Traversal T.Text T.Text Match Match
type RawRegexStr = String
data NLA =
  MkNLA { getBase    :: NE (Seq VCell)
        , numVars    :: !Int
        , getNLATxt' :: NLATxt
        , regex      :: RegexTrav }

instance Eq NLA where
  a == b = a.getNLATxt' == b.getNLATxt'
instance Ord NLA where
  a `compare` b = a.getNLATxt' `compare` b.getNLATxt'
instance Show NLA where
  show :: NLA -> String
  show nla = show nla.getBase <> "\n" <> show nla.numVars <> "\n" <> show nla.getNLATxt'

instance Hashable NLA where
  hashWithSalt = hashUsing getNLAtxt
  -- prob the easiest way to filter out overlapping NLAs is to use a separate function, rather than trying to shoehorn it into Eq and Hashable and Eq somehow

{- | Think of this as the specialized-to-the-Txt-to-NLATxt-direction form of `coerce`
Using this allows you to get the nice no-run-time-overhead of `coerce`, 
/without/ having to add the type annotations you'd need if you were using `coerce`
-}
mkNLATxt :: T.Text -> NLATxt
mkNLATxt = view (re _MkNLATxt)

nlaAsTxt :: NLA -> T.Text
nlaAsTxt = view _MkNLATxt . getNLAtxt

{- | public getter to view the NLAtxt
Don't need to export a lens for this field cos not going to change / set it -}
getNLAtxt :: NLA -> NLATxt
getNLAtxt nla = nla.getNLATxt'

-- | Smart constructor for making NLA
mkNLA :: forall f. (Foldable f, HasNonEmpty (f VCell)) => f VCell -> Maybe NLA
mkNLA (seqOf folded -> vcells) = do
  nmtVcells <- nonEmpty vcells
  regex     <- regexifyVCells nmtVcells ^? _Right
  return $ MkNLA { getBase    = nmtVcells
                  , numVars    = lengthOf (folded % filteredBy _TempVar) vcells
                  , getNLATxt' = annotxtify vcells
                  , regex      = traversify regex}

--- helpers for making NLAs

-- | Private function for making NLATxt for NLA
annotxtify :: Seq VCell -> NLATxt
annotxtify = textify spaceDelimtr vcell2NLAtxt
  where
    spaceDelimtr :: NLATxt = coerce (" " :: T.Text)

{- | Replace each variable indicator with a regex pattern 
      that matches either a word or another variable indicator.
-}
regexifyVCells :: NE (Seq VCell) -> Either String Regex
regexifyVCells = makeRegex . textify strdelimitr regexf . fromNonEmpty
  where
    strdelimitr :: String = " "
    regexf = \case
        TempVar tvar   -> tvar2WordOrVIregex tvar
        Pred nonvartxt -> (PCRE.escape . T.unpack $ nonvartxt)
          --TODO: Add tests to check if have to escape metachars in Pred
            -- T.unpack nonvartxt
            -- PCRE.escape . T.unpack $ nonvartxt

textify :: (Foldable t, Monoid c, SemiSequence (t c), Functor t) => Element (t c) -> (a -> c) -> t a -> c
textify spaceDelimtr mappingfn = fold . intersperse spaceDelimtr . fmap mappingfn

--- helpers for working with regex

{- | a regex that matches either a word or another variable indicator -}
wordOrVI :: RawRegexStr
wordOrVI = [r|(\w+|\*[\w\s]+\*)|]

tvar2WordOrVIregex :: TemplateVar -> RawRegexStr
tvar2WordOrVIregex = \case
    MatchGVar _    -> wordOrVI
    EndsInApos _   -> wordOrVI <> [r|'s|]
    IsNum _        -> [r|is |] <> wordOrVI

makeRegex :: RawRegexStr -> Either String Regex
makeRegex rawregex = PCRE.compileM (cs rawregex) []

traversify :: Regex -> RegexTrav
traversify regex = traversalVL (regexing regex)

regextravify :: RawRegexStr -> Maybe RegexTrav
regextravify rawregex =
  rawregex ^? (to makeRegex % _Right % to traversify)

matchesTxtOf :: RegexTrav -> NLATxt -> Bool
regexTrav `matchesTxtOf` nlatxt = has regexTrav (view _MkNLATxt nlatxt)

------------------- Filtering out subsumed NLAs

{- | Returns a set of non-subsumed NLAs
Currently implemented in a somewhat naive way -}
removeInternallySubsumed :: HS.HashSet NLA -> HS.HashSet NLA
removeInternallySubsumed nlaset = foldl' addIfNotSubsumed HS.empty nlasByNumVs
  where
    nlasByNumVs = sortWith (Down . (.numVars)) (toList nlaset)
                  -- See https://ro-che.info/articles/2016-04-02-descending-sort-haskell for why not sortBy (compare `on`
    addIfNotSubsumed :: HS.HashSet NLA -> NLA -> HS.HashSet NLA
    addIfNotSubsumed acc nla =
      if any (nla `isSubsumedBy`) acc
      then acc
      else nla `setInsert` acc

-- filtering out subsumed by lib templates

{- | filter out NLAs that are matched by any of the regex travs
Use this for filtering out NLATxts that are subsumed by lib template NLAs
-}
removeRegexMatches :: (Foldable f, Foldable g) => f RegexTrav -> g NLA -> HS.HashSet NLA
removeRegexMatches regtravs = foldr addIfNotMatched HS.empty
  where
    addIfNotMatched :: NLA -> HS.HashSet NLA -> HS.HashSet NLA
    addIfNotMatched nla acc =
      if any (`matchesTxtOf` nla.getNLATxt') regtravs
        --- i.e., if any of the regextravs matches the nlatext of the nla under consideration
      then acc
      else nla `setInsert` acc

{- | For parsing lib templates, as well as templates from, e.g., unit tests -}
regextravifyNLASection :: T.Text -> [RegexTrav]
regextravifyNLASection nlasectn =
  nlasectn
    & view (to T.lines)
    & toListOf (traversed % to T.unsnoc
                % folded % _1
                % to regextravifyLENLA % _Right)

-- filtering out dispreferred among the equivalent up to var names
-- TODO: first pass; haven't fully thought thru the API yet
data FilterResult a = MkFResult { subsumed :: a, kept :: a }
  deriving (Eq, Show, Functor)

instance Semigroup a => Semigroup (FilterResult a) where
  MkFResult s1 k1 <> MkFResult s2 k2 =  MkFResult (s1 <> s2) (k1 <> k2)
instance Monoid a => Monoid (FilterResult a) where
  mempty = MkFResult mempty mempty

{- | TODO: Add doctests/examples
-}
removeDisprefdInEquivUpToVarNames :: Foldable t => t NLA -> FilterResult [NLA]
removeDisprefdInEquivUpToVarNames nlas =
  let eqclasses :: [[NLA]] = L.groupBy isEquivUpToVarNames . L.sort . toList $ nlas
                                                             -- Re sorting NLAs: recall that the Ord for an NLA delegates to the Ord for its NLATxt
      makeFResult :: [NLA] -> FilterResult [NLA]
      makeFResult eqclass = if length eqclass == 1
                            then MkFResult {subsumed = mempty, kept = eqclass}
                            else (fmap toList . removeDisprefdInEqClass) eqclass
  in mconcat . fmap makeFResult $ eqclasses

--- helpers

{- | x `subsumes` y <=> x overlaps with y and x's arg places >= y's

Examples of NLAs that overlap:
  NLA Orig: @*a person*'s blahed *a person* blah2@

  `NLA Orig` overlaps with each of:
  @
      *a number*'s blahed *a star* blah2
      sdsd7's blahed sfsi23mkm blah2
  @
  but does NOT overlap with
  @
      Alice's blahed *a person* *hohoho* blah2
      Alice's2 blahed *a person* blah2
  @                                            
  TODO: add / make the above doctests unit tests -}
subsumes :: NLA -> NLA -> Bool
x `subsumes` y =
  x.numVars > y.numVars && x.regex `matchesTxtOf` y.getNLATxt'
  -- TODO: Look into the is-num vs "is payout" duplication

isSubsumedBy :: NLA -> NLA -> Bool
isSubsumedBy = flip subsumes

isEquivUpToVarNames :: NLA -> NLA -> Bool
x `isEquivUpToVarNames` y = x.numVars == y.numVars && x.regex `matchesTxtOf` y.getNLATxt'
                            {- ^ the above two conditions imply also that y.regex `matchesTxtOf` x.getNLATxt'
                                 This could be made into a property test
                            -}

{- Given an equiv class of (two or more) NLAs, remove the dispreferred in that class
   Assumes (without checking!) that the class has > 1 NLA
-}
removeDisprefdInEqClass :: Foldable f => f NLA -> FilterResult (HS.HashSet NLA)
removeDisprefdInEqClass nlas =
  let
    maybeMaxNumChars :: Maybe Int = nlas & maximumOf (folded % to nlaAsTxt % to T.length)
    fewerThanMaxNumChars :: T.Text -> Bool
    fewerThanMaxNumChars txt = case maybeMaxNumChars of
                                Just maxNumChars -> T.length txt < maxNumChars
                                Nothing          -> False
    isLessInformative :: T.Text -> Bool = T.isInfixOf "a number" <||> fewerThanMaxNumChars

    subsumed :: HS.HashSet NLA = nlas & setOf (folded
                                              % filteredBy (to nlaAsTxt % filtered isLessInformative))
    kept :: HS.HashSet NLA = difference (setOf folded nlas) subsumed
  in MkFResult {subsumed=subsumed, kept=kept}


-- | Takes as input a T.Text NLA that has already had the final char (either comma or period) removed
regextravifyLENLA :: T.Text -> Either String RegexTrav
regextravifyLENLA = fmap traversify . makeRegex . rawregexifyLENLA

rawregexifyLENLA :: T.Text -> RawRegexStr
rawregexifyLENLA (T.unpack -> nlastr) =
  let
    splitted = splitOn "*" nlastr
    isVarIdx = if splitted ^? ix 0 == Just "" then odd else even
               {- first elt of `splitted` will be a "" if the template begins with a variable indicator
                   >>> splitOn "*" "*a blah*'s nested *a blah list* is"
                   ["","a blah","'s nested ","a blah list"," is"]
                   >>> splitOn "*" "a class's *a list*"
                   ["a class's ","a list",""]
               -}
  in splitted
    & itraversed %& indices isVarIdx         .~ wordOrVI
    & itraversed %& indices (not . isVarIdx) %~ PCRE.escape
                    -- escape metachars in text that's not part of any var indicator
    & toListOf (folded % folded)

------------------- Building NLAs from VarsHCs

nlasFromVarsHC :: VarsHC -> HS.HashSet NLA
nlasFromVarsHC = \case
  VhcF vfact ->
    (maybe HS.empty HS.singleton (nlaFromVFact vfact))
  VhcR vrule ->
    nlasFromVarsRule vrule

--- helpers

nlaFromVFact :: VarsFact -> Maybe NLA
nlaFromVFact VFact{..} = nlaLoneFromVAtomicP varsfhead

nlasFromVarsRule :: VarsRule -> HS.HashSet NLA
nlasFromVarsRule MkBaseRule{..} =
  let bodyNLAs = nlasFromBody rbody
  in case nlaLoneFromVAtomicP rhead of
    Nothing -> bodyNLAs
    Just headNLA -> HS.insert headNLA bodyNLAs

nlasFromBody :: BoolPropn AtomicPWithVars -> HS.HashSet NLA
nlasFromBody varsABP =
  let lstNLAs = fmap nlaLoneFromVAtomicP varsABP
  in HS.fromList . catMaybes . toList $ lstNLAs

nlaLoneFromVAtomicP :: AtomicPWithVars -> Maybe NLA
nlaLoneFromVAtomicP =  \case
  ABPatomic vcells         -> mkNLA vcells
  ABPIsOpSuchTt _ _ vcells -> mkNLA vcells
  -- the other two cases are accounted for by lib NLAs/templates
  ABPIsDiffFr{} -> Nothing
  ABPIsOpOf{}   -> Nothing

vcell2NLAtxt :: VCell -> NLATxt
vcell2NLAtxt = \case
  TempVar tvar    -> tvar2NLAtxt tvar
  Pred nonvartxt  -> coerce nonvartxt

tvar2NLAtxt :: TemplateVar -> NLATxt
tvar2NLAtxt = \case
  EndsInApos prefix  -> mkNLATxt [i|*a #{prefix}*'s|]
  IsNum _numtxt      -> mkNLATxt "is *a number*"
  MatchGVar gvar     -> mkNLATxt [i|*a #{gvar}*|]

{- ^
From the LE handbook:
  An instance of a template is obtained from the template by replacing every parameter of the template by a list of words separated by spaces. 
  **There need not be any relationship between the words in a parameter and the words in the instance of the parameter. Different parameters in the same template can be replaced by different or identical instances.** (emphasis mine)
-}