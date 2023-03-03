{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
import Control.Monad.Except (MonadError (throwError))
import Data.Foldable (Foldable (toList))
import Data.Functor ((<&>))
import Data.Kind (Type, Constraint)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (IsString)
import Data.Text qualified as T
import Debug.Trace
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
rules2maudeStr rules = rules |> rules2doc |> either show show

{-
  Auxiliary functions that help with the transpilation.
  Note that we annotate some of these functions with explicit foralls because
  the pretty printer functions are all polymorphic over some (ann :: Type) and
  we sometimes need to explicitly pass these around as type params to keep GHC
  happy.
-}

rules2doc ::
  forall ann s (m :: Type -> Type) t.
  (MonadErrorIsString s m, Foldable t) => t Rule -> m (Doc ann)
rules2doc (null -> True) = pure mempty
rules2doc rules =
  rules |> toList |$> rule2doc |> sequence |$> concatWith (<.>)
  where
    x <.> y = [x, ",", line, line, y] |> mconcat

-- Main function that transpiles individual rules.
rule2doc ::
  forall ann s (m :: Type -> Type).
  MonadErrorIsString s m => Rule -> m (Doc ann)
rule2doc
  Regulative
    { rlabel = Just ("§", 1, ruleName),
      rkeyword = RParty,
      subj = Leaf ((MTT actorName :| [], Nothing) :| []),
      deontic,
      action = Leaf ((MTT actionName :| [], Nothing) :| []),
      temporal =
        Just
          ( TemporalConstraint
              tComparison@((`elem` [TOn, TBefore]) -> True)
              (Just n)
              (T.toUpper -> "DAY")
            ),
      hence = hence@(isValidHenceLest -> True),
      lest = lest@(isValidHenceLest -> True),
      srcref, -- May want to use this for better error reporting.
      given = Nothing,
      having = Nothing,
      who = Nothing,
      cond = Nothing,
      lsource = Nothing,
      upon = Nothing,
      wwhere = [],
      defaults = [],
      symtab = []
    } =
    {-
      Here we first process separately:
      - the part of the rule without the HENCE/LEST clauses.
      - the HENCE/LEST clauses.
      We then combine these together via vcat, using sequence to collect all
      the errors which occured while processing each part.
    -}
    [ruleNoHenceLest, henceLestClauses] |> sequence |$> vcat
    where
      ruleNoHenceLest =
        [ ["RULE", text2qidDoc ruleName],
          ["PARTY", text2qidDoc actorName],
          [deontic2doc deontic, text2qidDoc actionName],
          [tComparison2doc tComparison, pretty n, "DAY"]
        ]
          |$> hsep
          |> vcat
          |> pure
      henceLestClauses =
        [(Hence, hence), (Lest, lest)]
          |$> uncurry henceLest2maudeStr
          |> sequence
          |$> filter isNonEmptyDoc
          |$> vcat
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
isValidHenceLest :: Maybe Rule -> Bool
isValidHenceLest maybeRule = maybeRule |> maybe True isValidRuleAlias
  where
    isValidRuleAlias (RuleAlias xs) = go xs
    go [MTT _] = True
    go (MTT _ : MTT (T.toUpper -> "AND") : xs) = go xs
    go _ = False

data HenceOrLest = Hence | Lest
  deriving (Eq, Ord, Read, Show)

instance Pretty HenceOrLest where
  pretty henceOrLest = henceOrLest |> show2text |> T.toUpper |> pretty

{-
  This function can handle invalid HENCE/LEST clauses.
  A left with an error message is returned in such cases.
-}
henceLest2maudeStr ::
  forall ann s (m :: Type -> Type).
  MonadErrorIsString s m => HenceOrLest -> Maybe Rule -> m (Doc ann)
henceLest2maudeStr henceOrLest henceLest =
  henceLest |> maybe (pure mempty) henceLest2doc
  where
    henceLest2doc (RuleAlias henceLest') =
      henceLest'
        |$> quotOrUpper
        |> sequence
        |$> hsep
        |$> parenthesizeIf (length henceLest' > 1)
        |$> (pretty henceOrLest <+>)
    henceLest2doc _ = errMsg
    quotOrUpper (MTT (T.toUpper -> "AND")) = pure "AND"
    quotOrUpper (MTT x) = x |> text2qidDoc |> pure
    quotOrUpper _ = errMsg
    parenthesizeIf True x = ["(", x, ")"] |> mconcat
    parenthesizeIf False x = x

-- Common utilities

{-
  Error monad, polymorphic over:
  - a type variable (s :: Type) such that IsString s.
  - a type constructor (m :: Type -> Type) such that (m a) is a monad for all
    (a :: Type).
    (which could be (Either s) or ExceptT)
-}
type MonadErrorIsString :: Type -> (Type -> Type) -> Constraint
type MonadErrorIsString s m = (IsString s, MonadError s m)

infixl 0 |$>

(|$>) :: Functor f => f a -> (a -> b) -> f b
(|$>) = (<&>)

show2text :: Show a => a -> T.Text
show2text x = x |> show |> T.pack

text2qidDoc :: T.Text -> Doc ann
text2qidDoc x = ["qid(\"", x, "\")"] |> mconcat |> pretty

errMsg :: MonadErrorIsString s m => m a
errMsg = throwError "Not supported."