{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-
  Work-in-progress transpiler to Maude.
  Note that since we do all the parsing and transpilation within Maude itself,
  all we do here is convert the list of rules to a textual, string
  representation that Maude can parse.

  Note that we use NoMonomorphism restriction and ScopedTypeVariables because
  we use a _lot_ of polymorphic functions, like those from pretty printer, and
  GHC's type checker can be very, very annoying otherwise.
-}

module LS.XPile.Maude where

import AnyAll (BoolStruct (Leaf))
import Data.Bifunctor (Bifunctor(bimap))
import Data.Either (rights)
import Control.Monad.Except (MonadError (throwError))
import Data.Foldable (Foldable (toList,  elem))
import Data.Functor ((<&>))
import Data.Kind (Type, Constraint)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (IsString)
import Data.Text qualified as T
import Debug.Trace
import Data.Traversable (mapAccumL)
import Flow ((.>), (|>))
import LS.Rule
  ( Rule (..),
  )
import LS.Types
  ( MTExpr (MTT),
    RegKeywords (RParty),
    TComparison (..),
    TemporalConstraint (TemporalConstraint),
  )
import Prettyprinter
  ( Doc,
    Pretty,
    concatWith,
    hsep,
    line,
    pretty,
    vcat,
    viaShow,
    vsep,
    (<+>),
  )

{-
  Based on experiments being run here:
  https://docs.google.com/spreadsheets/d/1leBCZhgDsn-Abg2H_OINGGv-8Gpf9mzuX1RR56v0Sss/edit#gid=929226277
-}
-- testRule :: String
-- testRule = rules2maudeStr [Regulative {..}]
--   where
--     rlabel = Just ("§", 1, "START")
--     rkeyword = RParty
--     subj = Leaf ((MTT "actor" :| [], Nothing) :| [])
--     deontic = DMust
--     action = Leaf ((MTT "action" :| [], Nothing) :| [])
--     temporal = Just (TemporalConstraint TBefore (Just 5) "day")
--     hence = Just (RuleAlias [MTT "rule0", MTT "and", MTT "rule1"])
--     lest = Nothing

--     -- The remaining fields aren't used and hence don't matter.
--     given = Nothing
--     having = Nothing
--     who = Nothing
--     cond = Nothing
--     lsource = Nothing
--     srcref = Nothing
--     upon = Nothing
--     wwhere = []
--     defaults = []
--     symtab = []

-- Main function to transpile rules to plaintext natural4 for Maude.
rules2maudeStr :: Foldable t => t Rule -> String
rules2maudeStr rules = rules |> rules2doc |> show

{-
  Auxiliary functions that help with the transpilation.
  Note that we annotate some of these functions with explicit foralls because
  the pretty printer functions are all polymorphic over some (ann :: Type) and
  we sometimes need to explicitly pass these around as type params to keep GHC
  happy.
-}

{- 
  This function happily swallows up rules that don't transpile properly and
  only outputs those that do to plaintext.
-}
rules2doc ::
  forall ann s (t :: Type -> Type).
  Foldable t => t Rule -> Doc ann
rules2doc (null -> True) = mempty
rules2doc rules =
  rules
    |> toList
    |> renameFirstRegRule
    |$> rule2doc
    |> rights
    |> concatWith (<.>)
  where
    renameFirstRegRule (rule@(Regulative {}) : rules) =
      rule { rlabel = Just ("§", 1, "START") } : rules
    renameFirstRegRule (rule : rules) = rule : renameFirstRegRule rules
    renameFirstRegRule rules = rules
    x <.> y = [x, ",", line, line, y] |> mconcat

-- Main function that transpiles individual rules.
rule2doc ::
  forall ann s (m :: Type -> Type).
  MonadErrorIsString s m => Rule -> m (Doc ann)
rule2doc
  Regulative
    { rlabel = Just (_, _, ruleName),
      rkeyword,
      subj = Leaf ((MTT actorName :| [], Nothing) :| []),
      deontic,
      action = Leaf ((MTT actionName :| [], Nothing) :| []),
      temporal =
        Just
          ( TemporalConstraint
              tComparison@((`elem` [TOn, TBefore]) -> True)
              (Just n)
              (T.toUpper .> (`elem` ["DAY", "DAYS"]) -> True)
            ),
      hence, -- @(isValidHenceLest -> True),
      lest, -- @(isValidHenceLest -> True),
      srcref, -- May want to use this for better error reporting.
      given,
      having,
      who,
      cond,
      lsource,
      upon,
      wwhere,
      defaults,
      symtab
    } =
    {-
      Here we first process separately:
      - the part of the rule without the HENCE/LEST clauses.
      - the HENCE/LEST clauses.
      We then combine these together via vcat.
    -}
    henceLestClauses |$> (ruleNoHenceLest :) |$> vcat
    where
      ruleNoHenceLest =
        [ ["RULE", text2qidDoc ruleName],
          ["PARTY", text2qidDoc actorName],
          [deontic2doc deontic, text2qidDoc actionName],
          [tComparison2doc tComparison, pretty n, "DAY"]
        ]
          |$> hsep
          |> vcat
      henceLestClauses =
        [hence, lest]
          |> traverseWith henceLest2doc [Hence, Lest]
          |$> filter isNonEmptyDoc
      deontic2doc deon =
        deon |> show2text |> T.tail |> T.toUpper |> pretty
      tComparison2doc TOn = "ON"
      tComparison2doc TBefore = "WITHIN"
      isNonEmptyDoc doc = doc |> show |> not . null
rule2doc _ = errMsg

-- Auxiliary stuff for handling HENCE/LEST clauses.

{-
  A valid HENCE/LEST clause has the form
  rule0 [ AND rule1 AND ... AND ruleN ]
-}
-- isValidHenceLest :: Maybe Rule -> Bool
-- isValidHenceLest Nothing = True
-- isValidHenceLest (Just (RuleAlias xs)) =
--   xs |> zipWith isValidMTExpr [0 ..] |> and
--   where
--     isValidMTExpr (even -> True) (MTT _) = True
--     isValidMTExpr (odd -> True) (MTT (T.toUpper -> "AND")) = True
--     isValidMTExpr _ _ = False

data HenceOrLest = Hence | Lest
  deriving (Eq, Ord, Read, Show)

instance Pretty HenceOrLest where
  pretty henceOrLest = henceOrLest |> show2text |> T.toUpper |> pretty

{-
  This function can handle invalid HENCE/LEST clauses.
  A left with an error message is returned in such cases.
-}
henceLest2doc ::
  forall ann s (m :: Type -> Type).
  MonadErrorIsString s m => HenceOrLest -> Maybe Rule -> m (Doc ann)
henceLest2doc _ Nothing = pure mempty
henceLest2doc henceOrLest (Just (RuleAlias henceLest)) =
  henceLest
    |> traverseIndexed quotOrUpper
    |$> hsep
    |$> parenthesizeIf (length henceLest > 1)
    |$> (pretty henceOrLest <+>)
  where
    quotOrUpper (odd -> True) (MTT (T.toUpper -> "AND")) = pure "AND"
    quotOrUpper (even -> True) (MTT ruleName) = ruleName |> text2qidDoc |> pure
    quotOrUpper _ _ = errMsg
    parenthesizeIf True doc = ["(", doc, ")"] |> mconcat
    parenthesizeIf False doc = doc
henceLest2doc _ _ = errMsg

-- Common utilities

{-
  Error monad, polymorphic over:
  - a type variable (s :: Type) such that IsString s.
  - a type constructor (m :: Type -> Type) such that (m a) is a monad for all
    (a :: Type).
    (which could be (Either s) or ExceptT)
-}
type MonadErrorIsString s (m :: Type -> Type) = (IsString s, MonadError s m)

mapIndexed :: (Traversable t, Num s) => (s -> a -> b) -> t a -> t b
mapIndexed f xs = xs |> mapAccumL g 0 |> snd
  where
    g index val = (index + 1, f index val)

traverseIndexed ::
  (Traversable t, Num s, Applicative f) =>
  (s -> a -> f b) -> t a -> f (t b)
traverseIndexed f xs = xs |> mapIndexed f |> sequenceA

traverseWith ::
  (Foldable t1, Foldable t2, Applicative f) =>
  (a1 -> a2 -> f b) -> t1 a1 -> t2 a2 -> f [b]
traverseWith f xs ys =
    (xs, ys) |> bimap toList toList |> uncurry (zipWith f) |> sequenceA

infixl 0 |$>

(|$>) :: Functor f => f a -> (a -> b) -> f b
(|$>) = (<&>)

show2text :: Show a => a -> T.Text
show2text x = x |> show |> T.pack

text2qidDoc :: T.Text -> Doc ann
text2qidDoc x = ["qid(\"", x, "\")"] |> mconcat |> pretty

errMsg :: MonadErrorIsString s m => m a
errMsg = throwError "Not supported."