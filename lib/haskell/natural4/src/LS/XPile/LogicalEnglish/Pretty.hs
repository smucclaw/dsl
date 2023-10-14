{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

{-# LANGUAGE DataKinds, AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module LS.XPile.LogicalEnglish.Pretty (LEProg(..), libAndBuiltinTemplates) where

-- import Text.Pretty.Simple   ( pShowNoColor )
import Data.Text qualified as T
-- import Data.HashSet qualified as HS
import Data.String()

import Prettyprinter
  ( Doc,
    Pretty (pretty),
    comma,
    hsep,
    line,
    -- parens,
    list,
    indent,
    nest,
    vsep,
    (<+>),
    -- viaShow,
    -- encloseSep,
    concatWith,
    dot)
import LS.PrettyPrinter
    ( vvsep, (<//>), myrender )
import Prettyprinter.Interpolate (__di, di)
-- import Optics
-- import Optics.State.Operators
-- import Optics.TH
-- import Data.Set.Optics (setOf)
import Data.List ( sort )

import LS.XPile.LogicalEnglish.Types
import LS.XPile.LogicalEnglish.GenNLAs (NLATxt)
-- import LS.XPile.LogicalEnglish.ValidateL4Input
--       (L4Rules, ValidHornls, Unvalidated,
--       check, refine, loadRawL4AsUnvalid)

-- import LS.XPile.LogicalEnglish.UtilsLEReplDev -- for prototyping

{-------------------------------------------------------------------------------
   L4 rules -> SimpleL4HCs -> VRules
-------------------------------------------------------------------------------}

data LEProg = MkLEProg {  keptnlats :: [NLATxt]
                        , subsumednlats :: [NLATxt]
                          -- ^ this wouldn't be *all* of the filtered-out NLATxts -- just those that are equiv up to var names (and have the same number of vars)
                        , leHCs   :: [LEhcPrint]
                        , commentSym :: T.Text
                        }



-- | config record for pretty printing
data PrintCfg = MkPrintCfg { numIndentSpcs :: !Int} deriving stock (Show)
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
  pretty =
    \case
      AtomicBP bp ->
        pretty bp
      And bps     ->
        boolOp "and" bps
      Or bps      ->
        boolOp "or" bps
      Not bp      ->
        "it is not the case that" <//> indentLE (pretty bp)
    where
      -- | Nest iff it's an AND or OR, so that won't get extra indentation for "it is not the case that...". 
      -- TODO: This could be done more elegantly
      prettnestIfAndOr :: Pretty a => BoolPropn a -> Doc ann
      prettnestIfAndOr = \case
        atom@(AtomicBP _) -> pretty atom
        notbp@(Not _)     -> pretty notbp
        andbp@(And _)     -> nestLE . pretty $ andbp
        orbp@(Or _)       -> nestLE . pretty $ orbp

      boolOp opstr bps = concatBoolOp opstr (map prettnestIfAndOr bps)
      concatBoolOp boolopstr = concatWith (\x y -> x <> line <> boolopstr <+> y)

instance Pretty TxtAtomicBP where
  pretty :: TxtAtomicBP -> Doc ann
  pretty = \case
    ABPatomic prop ->
      prettyprop prop
    ABPBaseIs lefts rights -> 
      [__di|#{prettyprop lefts} is #{prettyprop rights}|]
    ABPIsIn t1 t2 ->
      [__di|#{pretty t1} is in #{pretty t2}|]
    ABPIsDiffFr t1 t2 ->
      [__di|#{pretty t1} is different from #{pretty t2}|]
    ABPIsOpOf t1 opof targs ->
      [__di|#{pretty t1} #{pretty opof} #{list $ map pretty targs}|]
    ABPIsOpSuchTt term ostt prop ->
      [__di|#{pretty term} #{pretty ostt}|] <//> indentLE (prettyprop prop)
    where
      prettyprop = hsep . map pretty

-- endWithDot txt = [__di|#{ txt }.|]

-- | Like punctuate from the pretty printer lib, except that this puts `p` at the end of every doc 
punctuate'
    :: Doc ann -- ^ Punctuation, e.g. 'comma'
    -> [Doc ann]
    -> [Doc ann]
punctuate' p = map (<> p)

instance Pretty LEProg where

  {-
  Preconditions: 
    * The Pretty-ing code will not do any 'substantive' filtering: 
        it expects that any required filtering of any of the constituent parts of LEProg (either the NLATxts or the LEhcs) will already have been done, prior to being passed into `pretty`
  -}
  pretty :: forall ann. LEProg -> Doc ann
  pretty MkLEProg{..} =
    let
      indentedNLAs :: Doc ann = nestVsepSeq . punctuate' comma . map pretty . sort $ keptnlats
      prettyLEhcs  :: Doc ann = vvsep $ map ((<> dot) . pretty) leHCs
                        {- ^ Assume commas and dots already replaced in NLAs and LEHcs
                          (can't replace here b/c we sometimes do want the dot, e.g. for numbers) -}

      prependWithCommentOp :: Doc ann -> Doc ann = (pretty commentSym <+>)
      removedNLAs          ::            Doc ann = vsep . map (prependWithCommentOp . pretty) $ subsumednlats
      removedNLAsection    ::            Doc ann = if not (null subsumednlats)
                                                   then
                                                      line <> [__di|%% Some of the removed templates (just the equiv-up-to-var-names-with-same-num-vars ones):
                                                        #{indentLE removedNLAs}|]
                                                   else ""
    in
      [__di|
        the target language is: prolog.

        the templates are:
          #{indentedNLAs}
          #{nestLE libTemplates}
        #{removedNLAsection}

        % Predefined stdlib for translating natural4 -> LE.
        the knowledge base lib includes:
          #{nestLE libHCs}

        the knowledge base rules includes:
          #{nestLE prettyLEhcs}
      |]

{- | Templates which are predefined in LE itself, and hence should not be
      included in the LE output.
      Note: we don't strictly speaking need *all* of the following, 
            because we don't make NLAs out of every construct,
            but having all the built-in templates here does make for good documentation 
-}
builtinTemplates :: Doc ann
builtinTemplates =
  [__di|
  *a thing* is in *a thing*,
  #{nlas}.|]
  where
    nlas = concatNlaList $ mconcat [mathNlaList, dateNlaList]

    concatNlaList = concatWith \x y -> mconcat [x, ",\n", y]

    mathNlaList =
      [ [di|*a number* #{binOp} *a number*|]
        | binOp :: Doc ann <- ["<", ">", "=<", ">=", "="]
      ]

    dateNlaList =
      [ [di|*a date* is *a n* #{timeUnit}#{s} #{comparison} *a date*|]
        | timeUnit :: Doc ann <- ["day", "week", "month", "year"],
          s ::Doc ann <- ["", "s"],
          comparison :: Doc ann <- ["before", "after", "within"]
      ]

libTemplates :: Doc ann
libTemplates =
  [__di|
  *a number* <= *a number*,
  *a date* is before *a date*,
  *a date* is after *a date*,
  *a date* is strictly before *a date*,
  *a date* is strictly after *a date*,
  *a class*'s *a field* is *a value*,
  *a class*'s nested *a list of fields* is *a value*,
  *a class*'s *a field0*'s *a field1* is *a value*,
  *a class*'s *a field0*'s *a field1*'s *a field2* is *a value*,
  *a class*'s *a field0*'s *a field1*'s *a field2*'s *a field3* is *a value*,
  *a class*'s *a field0*'s *a field1*'s *a field2*'s *a field3*'s *a field4* is *a value*,
  *a number* is a lower bound of *a list*,
  *a number* is an upper bound of *a list*,
  *a number* is the minimum of *a number* and the maximum of *a number* and *a number*,
  the sum of *a list* does not exceed the minimum of *a list*,
  *a number* does not exceed the minimum of *a list*.|]

libAndBuiltinTemplates :: T.Text
libAndBuiltinTemplates =
  T.strip . myrender $ vsep [libTemplates, builtinTemplates]
{- ^
>>> libAndBuiltinTemplates
"*a number* <= *a number*,\n*a date* is before *a date*,\n*a date* is after *a date*,\n*a date* is strictly before *a date*,\n*a date* is strictly after *a date*,\n*a class*'s *a field* is *a value*,\n*a class*'s nested *a list of fields* is *a value*,\n*a class*'s *a field0*'s *a field1* is *a value*,\n*a class*'s *a field0*'s *a field1*'s *a field2* is *a value*,\n*a class*'s *a field0*'s *a field1*'s *a field2*'s *a field3* is *a value*,\n*a class*'s *a field0*'s *a field1*'s *a field2*'s *a field3*'s *a field4* is *a value*,\n*a number* is a lower bound of *a list*,\n*a number* is an upper bound of *a list*,\n*a number* is the minimum of *a number* and the maximum of *a number* and *a number*,\nthe sum of *a list* does not exceed the minimum of *a list*,\n*a number* does not exceed the minimum of *a list*.\n*a thing* is in *a thing*,\n*a number* < *a number*,\n*a number* > *a number*,\n*a number* =< *a number*,\n*a number* >= *a number*,\n*a number* = *a number*,\n*a date* is *a n* day before *a date*,\n*a date* is *a n* day after *a date*,\n*a date* is *a n* day within *a date*,\n*a date* is *a n* days before *a date*,\n*a date* is *a n* days after *a date*,\n*a date* is *a n* days within *a date*,\n*a date* is *a n* week before *a date*,\n*a date* is *a n* week after *a date*,\n*a date* is *a n* week within *a date*,\n*a date* is *a n* weeks before *a date*,\n*a date* is *a n* weeks after *a date*,\n*a date* is *a n* weeks within *a date*,\n*a date* is *a n* month before *a date*,\n*a date* is *a n* month after *a date*,\n*a date* is *a n* month within *a date*,\n*a date* is *a n* months before *a date*,\n*a date* is *a n* months after *a date*,\n*a date* is *a n* months within *a date*,\n*a date* is *a n* year before *a date*,\n*a date* is *a n* year after *a date*,\n*a date* is *a n* year within *a date*,\n*a date* is *a n* years before *a date*,\n*a date* is *a n* years after *a date*,\n*a date* is *a n* years within *a date*."

The T.strip isn't currently necessary, 
but it seems like a good thing to include to pre-empt any future issues from accidentally adding whitespace.
-}

libHCs :: Doc ann
libHCs =
  [__di|
  a number <= an other number
  if number =< other number.

  % Note: LE's parsing of [H | T] is broken atm because it transforms that
  % into [H, T] rather than the Prolog term [H | T].

  % a class's nested [] is a value.

  % a class's nested [a field | a fields] is a value
  % if the class's the field is an other class
  % and the other class's nested the fields is the value.

  a d0 is before a d1
  if d0 is a n days before d1
  and n >= 0.

  a d0 is strictly before a d1
  if d0 is a n days before d1
  and n > 0.

  a d0 is after a d1
  if d1 is before d0.

  a d0 is strictly after a d1
  if d1 is strictly before d0.

  % Nested accessor predicates.
  a class's a field is a value
  if field is different from name
  and field is different from id
  and a class0's name is class
    or class0's id is class
  and class0's field is value.

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
