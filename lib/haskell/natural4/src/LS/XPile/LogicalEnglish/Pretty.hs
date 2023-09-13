{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

{-# LANGUAGE DataKinds, KindSignatures, AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module LS.XPile.LogicalEnglish.Pretty (LEProg(..)) where

-- import Text.Pretty.Simple   ( pShowNoColor )
-- import Data.Text qualified as T
-- import Data.HashSet qualified as HS
-- import Data.Hashable (Hashable)
-- import Data.Coerce (coerce)
-- import Data.Maybe (fromMaybe, listToMaybe)
import Data.String()

import Prettyprinter
  ( Doc,
    Pretty (pretty),
    comma,
    hsep,
    line,
    -- parens,
    punctuate,
    list,
    indent,
    nest,
    vsep,
    -- (<+>),
    -- viaShow,
    -- encloseSep,
    concatWith,
    dot)
import LS.PrettyPrinter
    ( vvsep, (<//>) )
import Prettyprinter.Interpolate (__di)

import LS.XPile.LogicalEnglish.Types
import LS.XPile.LogicalEnglish.GenNLAs (NLATxt)
-- import LS.XPile.LogicalEnglish.ValidateL4Input
--       (L4Rules, ValidHornls, Unvalidated,
--       check, refine, loadRawL4AsUnvalid)

-- import LS.XPile.LogicalEnglish.UtilsLEReplDev -- for prototyping

{-------------------------------------------------------------------------------
   L4 rules -> SimpleL4HCs -> VRules
-------------------------------------------------------------------------------}


data LEProg = MkLEProg {  nlatxts :: [NLATxt]
                        , leHCs   :: [LEhcPrint] 
                        }


-- | config record for pretty printing
data PrintCfg = MkPrintCfg { numIndentSpcs :: !Int}
printcfg :: PrintCfg
printcfg = MkPrintCfg { numIndentSpcs = 2 }

nestLE :: Doc ann -> Doc ann
nestLE = nest printcfg.numIndentSpcs

indentLE :: Doc ann -> Doc ann
indentLE = indent printcfg.numIndentSpcs

nestVsepSeq :: [Doc ann] -> Doc ann
nestVsepSeq seq =  nestLE (vsep seq)

instance Pretty OpOf where
  pretty :: OpOf -> Doc ann
  pretty = \case
    MaxOf -> "is the maximum of"
    MinOf -> "is the minimum of"
    SumOf -> "is the sum of"
    ProductOf -> "is the product of"

instance Pretty OpSuchTt where
  pretty :: OpSuchTt -> Doc ann
  pretty = \case
    MaxXSuchThat -> "is the max x such that"
    MinXSuchThat -> "is the min x such that"
    SumEachXSuchThat -> "is the sum of each x such that"


instance Pretty LEhcPrint where
  pretty :: LEhcPrint -> Doc ann
  pretty = \case
    LEHcF fact -> pretty fact; LEHcR rule -> pretty rule

instance Pretty LERuleForPrint where
  pretty :: LERuleForPrint -> Doc ann
  pretty MkBaseRule{..} =
    [__di|
      #{pretty rhead}
      if #{pretty rbody}
    |]

instance Pretty a => Pretty (BoolPropn a) where
  pretty :: Pretty a => BoolPropn a -> Doc ann
  pretty = \case
      AtomicBP bp ->
        pretty bp
      And bps     ->
        concatBoolOp "and" (map pretty bps)
      Or bps      ->
        concatBoolOp "or" (map pretty bps)
      Not bp      ->
        "it is not the case that" <//> indentLE (pretty bp)
    where
      concatBoolOp boolop = concatWith (\x y -> x <> line <> boolop <> " " <> y)

instance Pretty TxtAtomicBP where
  pretty :: TxtAtomicBP -> Doc ann
  pretty = \case
    ABPatomic prop ->
      prettyprop prop
    ABPIsDiffFr t1 t2 ->
      [__di|#{pretty t1} is different from #{pretty t2}|]
    ABPIsOpOf t1 opof targs ->
      [__di|#{pretty t1} #{pretty opof} #{list $ map pretty targs}|]
    ABPIsOpSuchTt term ostt prop ->
      [__di|#{pretty term} #{pretty ostt}|] <//> indentLE (prettyprop prop)
    where
      prettyprop = hsep . map pretty

endWithDot txt = [__di|#{ txt }.|]
 
instance Pretty LEProg where
  pretty :: LEProg -> Doc ann
  pretty MkLEProg{..} =
    
    let indentedNLAs = endWithDot . nestVsepSeq . punctuate comma 
                      . map pretty $ nlatxts
                      -- assume list of NLAs is pre-sorted
        prettyLEhcs   = vvsep $ map ((<> dot) . pretty) leHCs
        {- ^ Assume commas and dots already replaced in NLAs and LEHcs
           (can't replace here b/c we sometimes do want the dot, e.g. for numbers) -}
    in
      [__di|
        the target language is: prolog.

        the templates are:
          #{indentedNLAs}
          #{nestLE joeLibTemplates}

        % Predefined stdlib for translating natural4 -> LE.
        the knowledge base prelude includes:
          #{nestLE joeLibHCs}

        the knowledge base encoding includes:
          #{nestLE prettyLEhcs}
      
        query q is:
          0 < 1.
      |]

joeLibTemplates :: Doc ann
joeLibTemplates =
  [__di|
  *a class*'s *a field* is *a value*,
  *a class*'s nested *a list of fields* is *a value*,
  *a class*'s *a field0*'s *a field1* is *a value*,
  *a class*'s *a field0*'s *a field1*'s *a field2* is *a value*,
  *a class*'s *a field0*'s *a field1*'s *a field2*'s *a field3* is *a value*,
  *a class*'s *a field0*'s *a field1*'s *a field2*'s *a field3*'s *a field4* is *a value*.

  *a number* is a lower bound of *a list*,
  *a number* is an upper bound of *a list*,
  *a number* is the minimum of *a number* and the maximum of *a number* and *a number*,
  the sum of *a list* does not exceed the minimum of *a list*,
  *a number* does not exceed the minimum of *a list*.
  |]

joeLibHCs :: Doc ann
joeLibHCs =
  [__di|
  % Note: LE's parsing of [H | T] is broken atm because it transforms that
  % into [H, T] rather than the Prolog term [H | T].

  % a class's nested [] is a value.

  % a class's nested [a field | a fields] is a value
  % if the class's the field is an other class
  % and the other class's nested the fields is the value.

  % Nested accessor predicates.
  a class's a field0's a field1 is a value
  if class's field0 is a class0
  and class0's field1 is value.

  a class's a field0's a field1's a field2 is a value
  if class's field0 is a class0
  and class0's field1 is a class1
  and class1's field2 is value.

  a class's a field0's a field1's a field2's a field3 is a value
  if class's field0 is a class0
  and class0's field1 is a class1
  and class1's field2 is a class2
  and class2's field3 is value.

  a class's a field0's a field1's a field2's a field3's a field4 is a value
  if the class's field0 is a class0
  and class0's field1 is a class1
  and class1's field2 is a class2
  and class2's field3 is a class3
  and class3's field4 is value.

  % Arithmetic predicates.
  a number is an upper bound of a list
  if for all cases in which
     a X is in list
     it is the case that
      X is [a class, a field]
          and class's field is a value
          and number >= value
        or number >= X.

  a number is a lower bound of a list
  if for all cases in which
     a X is in list
     it is the case that
      X is [a class, a field]
          and class's field is a value
          and number =< value
        or number =< X.

  % number = min(x, max(y, z))
  a number is the minimum of a x and the maximum of a y and a z
  if a m is the maximum of [y, z]
  and number is the minimum of [x, m].

  a number does not exceed the minimum of a list of numbers
  if a min is the minimum of list of numbers
  and number =< min.

  the sum of a list does not exceed the minimum of a other list
  if a x is the sum of list 
  and x does not exceed the minimum of other list.|]