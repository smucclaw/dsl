{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-
  Work-in-progress transpiler to Maude.
  Note that since we do all the parsing and transpilation within Maude itself,
  all we do here is convert the list of rules to a textual, string
  representation that Maude can parse.
-}
module LS.XPile.Maude where

import AnyAll (BoolStruct (Leaf))
-- import Debug.Trace

import Control.Applicative (liftA2)
import Control.Lens ((<&>))
import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Char (toUpper)
import Data.Coerce (Coercible, coerce)
import Data.Foldable (Foldable (foldMap', toList))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as T
import Flow ((|>))
import LS.Rule
  ( Rule (..),
  )
import LS.Types
  ( MTExpr (MTT),
    RegKeywords (RParty),
    TComparison (TBefore),
    TemporalConstraint (TemporalConstraint),
  )
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    hsep,
    line,
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

rule2doc :: Rule -> Either String (Doc ann)
rule2doc
  Regulative
    { rlabel = Just ("§", 1, ruleName),
      rkeyword = RParty,
      subj = Leaf ((MTT actorName :| [], Nothing) :| []),
      deontic,
      action = Leaf ((MTT actionName :| [], Nothing) :| []),
      temporal =
        Just (TemporalConstraint TBefore (Just n) (T.toLower -> "day")),
      hence,
      lest,

      srcref, -- May want to use this for better error reporting.
      given = Nothing, having = Nothing, who = Nothing, cond = Nothing,
      lsource = Nothing, upon = Nothing, wwhere = [],
      defaults = [], symtab = []
    }
    | all isValidHenceLest [hence, lest] =
        liftA2 (<+>) rule_without_henceLest henceLest
    where
      rule_without_henceLest =
        [ ["RULE", pretty2Qid ruleName],
          ["PARTY", pretty2Qid actorName],
          [deontic2str deontic, pretty2Qid actionName],
          ["WITHIN", pretty n, "DAY"]
        ]
          |> map hsep
          |> vcat
          |> pure
      henceLest =
        [(HENCE, hence), (LEST, lest)]
          |> map (uncurry henceLest2maudeStr)
          |> sequence
          |> fmap vsep
      deontic2str deon =
        deon |> show |> tail |> map toUpper |> pretty

rule2doc _ = Left "Not supported."

rules2doc :: Foldable t => t Rule -> Either String (Doc ann)
rules2doc rules =
  rules
    |> toList
    |> map rule2doc
    |> sequence
    |> fmap (mapButLast (<> line))
    |> fmap vcat

pretty2Qid :: T.Text -> Doc ann
pretty2Qid x = x |> T.strip |> pretty |> ("'" <>)

rules2maudeStr :: Foldable t => t Rule -> String
rules2maudeStr rules = rules |> rules2doc |> show

data HenceOrLest = HENCE | LEST
  deriving (Eq, Ord, Read, Show)

isValidHenceLest :: Maybe Rule -> Bool
isValidHenceLest Nothing = True
isValidHenceLest (Just (RuleAlias xs)) =
  isValidMTTs xs
  where
    isValidMTTs [MTT _] = True
    isValidMTTs (MTT _ : MTT (T.toUpper -> "AND") : xs) = isValidMTTs xs

henceLest2maudeStr :: HenceOrLest -> Maybe Rule -> Either String (Doc ann)
henceLest2maudeStr henceOrLest hence =
  hence |> maybe (pure "") f
  where
    f (RuleAlias hence') =
      hence'
        |> map quotOrUpper
        |> sequence
        |> fmap hsep
        |> fmap (parenthesizeIf (length hence' > 1))
        |> fmap (viaShow henceOrLest <+>)
    f _ = errMsg
    quotOrUpper (MTT (T.toLower -> "and")) = pure "AND"
    quotOrUpper (MTT x) = x |> pretty2Qid |> pure
    quotOrUpper _ = errMsg
    parenthesizeIf True x = mconcat ["(", x, ")"]
    parenthesizeIf False x = x

errMsg :: Either String a
errMsg = Left "Not supported."

mapButLast :: (a -> b) -> [a] -> [b]
mapButLast _ [] = []
mapButLast _ [_] = []
mapButLast f (x : xs) = f x : mapButLast f xs