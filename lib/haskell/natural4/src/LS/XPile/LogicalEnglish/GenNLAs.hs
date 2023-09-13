{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, RecordWildCards, NoFieldSelectors #-}
-- {-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies, DerivingVia, DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}


module LS.XPile.LogicalEnglish.GenNLAs (
      nlasFromVarsHC
    , NLATxt(..)

    , NLA' (NLA) -- opaque; exporting only pattern for matching on the NLATxt
    , mkNLA      -- smart constructor
    , getNLAtxt
  )
where

import Data.Text qualified as T
import Data.HashSet qualified as HS
import Data.Hashable (Hashable, hashWithSalt, hashUsing)
import Data.Foldable (fold, toList)
import Data.Maybe (catMaybes)
import qualified Data.List as L hiding (head, tail)
-- import Debug.Trace (trace)
import Data.Coerce (coerce)

import LS.XPile.LogicalEnglish.Types

import Data.String (IsString)
import Data.String.Interpolate ( i )

import Data.String.Conversions
import           Data.String.Conversions.Monomorphic
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as BS
import Text.RawString.QQ
import qualified Text.Regex.PCRE.Heavy as PCRE
import Text.Regex.PCRE.Heavy (re)

import Optics hiding (re)
import Data.Text.Optics 
import Data.Sequence.Optics (seqOf)
import Control.Lens.Regex.Text
import Data.Containers.NonEmpty (NE, HasNonEmpty, nonEmpty)
-- onNonEmpty, fromNonEmpty, 
import Data.Sequence (Seq)
import Data.Sequences (intersperse)

import Prettyprinter(Pretty)


newtype NLATxt = MkNLATxt T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Semigroup, Monoid, Hashable, Pretty)

-- Traversal T.Text T.Text T.Text T.Text
data NLA' =  
  MkNLA' { getBase'    :: NE (Seq VCell) 
         , getNumVars' :: !Int
         , getNLATxt'  :: NLATxt
         , getRegex'   :: Regex }
    deriving stock (Show, Eq, Ord) 

instance Hashable NLA' where
  hashWithSalt = hashUsing (\nla -> nla.getNLATxt')
  -- TODO temp placeholder

    
{-| public getter to view the NLAtxt
Don't need to export a lens for this field cos not going to change / set it -}
getNLAtxt :: NLA' -> NLATxt
getNLAtxt nla' = nla'.getNLATxt'

-- | public pattern to match on the NLAtxt
pattern NLA :: NLATxt -> NLA'
pattern NLA nlatxt <- (getNLAtxt -> nlatxt)

-- | Smart constructor for making NLA'
mkNLA :: forall f. (Foldable f, HasNonEmpty (f VCell)) => f VCell -> Maybe NLA'
mkNLA (seqOf folded -> vcells) = do
  nmtVcells <- nonEmpty vcells 
  regex     <- regexify nmtVcells ^? _Right
  return $ MkNLA' { getBase'    = nmtVcells
                  , getNumVars' = lengthOf (folded % filteredBy _TempVar) vcells
                  , getNLATxt'  = annotxtify vcells
                  , getRegex'   = regex}


-- | Private function for making NLATxt for NLA'
annotxtify :: Seq VCell -> NLATxt              
annotxtify = fold . intersperseWithSpace . fmap vcell2NLAtxt 
  where 
    spaceDelimtr = coerce (" " :: T.Text)
    intersperseWithSpace :: Seq NLATxt -> Seq NLATxt
    intersperseWithSpace = intersperse spaceDelimtr 


{- | Replace each variable indicator with a regex pattern 
      that matches either a word or another variable indicator.
-}
regexify :: NE (Seq VCell) -> Either String Regex
regexify = makeRegex . foldMap (\case
  TempVar tvar   -> tvar2WordOrVIregex tvar
  Pred nonvartxt -> PCRE.escape . T.unpack $ nonvartxt)
                    --TODO: Add tests to check if have to escape metachars in Pred

type RawRegexStr = String
makeRegex :: RawRegexStr -> Either String Regex
makeRegex rawregex = PCRE.compileM (cs rawregex) []


{- | a regex pattern that matches either a word or another variable indicator -}
tvar2WordOrVIregex :: TemplateVar -> RawRegexStr
tvar2WordOrVIregex = 
  let wordOrVI :: RawRegexStr
      wordOrVI = [r|(\w+|\*[\w\s]+\*)|]
  in \case
    MatchGVar _    -> wordOrVI
    EndsInApos _   -> wordOrVI <> [r|'s|]
    IsNum _        -> [r|is |] <> wordOrVI
     



-------------------


nlasFromVarsHC :: VarsHC -> HS.HashSet NLA'
nlasFromVarsHC = \case
  VhcF vfact ->
    case nlaFromVFact vfact of
      Nothing -> HS.empty
      Just nla -> HS.singleton nla
  VhcR vrule ->
    nlasFromVarsRule vrule

-- TODO: When have more time, write smtg tt checks if it is indeed in fixed lib, and add it if not.
nlaFromVFact :: VarsFact -> Maybe NLA'
nlaFromVFact VFact{..} = nlaLoneFromVAtomicP varsfhead

nlasFromVarsRule :: VarsRule -> HS.HashSet NLA'
nlasFromVarsRule MkBaseRule{..} =
  let bodyNLAs = nlasFromBody rbody
  in case nlaLoneFromVAtomicP rhead of
    Nothing -> bodyNLAs
    Just headNLA -> HS.insert headNLA bodyNLAs

nlasFromBody :: BoolPropn AtomicPWithVars -> HS.HashSet NLA'
nlasFromBody varsABP =
  let lstNLAs = fmap nlaLoneFromVAtomicP varsABP
  in HS.fromList . catMaybes . toList $ lstNLAs

-- TODO: Check if this really does conform to the spec --- went a bit fast here
nlaLoneFromVAtomicP :: AtomicPWithVars -> Maybe NLA'
nlaLoneFromVAtomicP =  \case
  ABPatomic vcells         -> mkNLA vcells
  ABPIsOpSuchTt _ _ vcells -> mkNLA vcells
  ABPIsDiffFr{} -> Nothing
  ABPIsOpOf{}   -> Nothing
  -- where
  --   annotFromVCells :: [VCell] -> Maybe NLATxt
  --   annotFromVCells = annotFromNLAcells . nlacellsFromVCells

  --   nlacellsFromVCells :: [VCell] -> [NLACell]
  --   nlacellsFromVCells = fmap vcell2NLAcell

-- annotFromNLAcells :: [NLACell] -> Maybe NLATxt
-- annotFromNLAcells = \case
--   (mconcat . intersperseWithSpace -> MkNonParam concatted) -> 
--         Just $ coerce concatted
--   _ ->  Nothing
--   where 
--     spaceDelimtr = MkNonParam " "
--     intersperseWithSpace = L.intersperse spaceDelimtr 

-- vcell2NLAcell :: VCell -> NLACell
-- vcell2NLAcell = \case
--   TempVar tvar -> tvar2NLAcell tvar
--   Pred nonparamtxt -> MkNonParam nonparamtxt



vcell2NLAtxt :: VCell -> NLATxt
vcell2NLAtxt = \case
  TempVar tvar -> tvar2NLAtxt tvar
  Pred nonparamtxt -> coerce nonparamtxt

tvar2NLAtxt :: TemplateVar -> NLATxt
tvar2NLAtxt = \case
  EndsInApos prefix  -> coerce $ ([i|*a #{prefix}*'s|] :: T.Text)
  IsNum _numtxt      -> coerce $ ("is *a number*" :: T.Text)
  -- handling this case explicitly to remind ourselves tt we've handled it, and cos we might want to use "*a number*" instead
  MatchGVar gvar     -> coerce $ ([i|*a #{gvar}*|] :: T.Text)



{- | 
Invariant: all NLAParams take one of the following two forms:
  *a var*
  *a var*'s
-}
-- tvar2NLAcell :: TemplateVar -> NLACell
-- tvar2NLAcell = \case
--   EndsInApos _   -> MkParam "*a var*'s"
--   IsNum _numtxt  -> MkParam "is *a var*"
--   -- handling this case explicitly to remind ourselves tt we've handled it, and cos we might want to use "*a number*" instead
--   MatchGVar _    -> MkParam "*a var*"
  {- ^
  From the LE handbook:
    An instance of a template is obtained from the template by replacing every parameter of the template by a list of words separated by spaces. 
    **There need not be any relationship between the words in a parameter and the words in the instance of the parameter. Different parameters in the same template can be replaced by different or identical instances.** (emphasis mine)

  Right now I'm making all of them "a var" or "a var's", as opposed to "a <orig text>" / "a <orig text>'s", so tt it'll be easy to remove duplicates
  -}