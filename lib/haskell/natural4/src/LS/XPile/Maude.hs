{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import AnyAll (BoolStruct (All, Leaf))
import Control.Applicative (Applicative (liftA2))
import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Either (rights)
import Data.Foldable (Foldable (elem, toList))
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (mapMaybe)
import Data.String (IsString)
import Data.Text qualified as T
import Data.Traversable (mapAccumL)
-- import Debug.Trace
import Flow ((.>), (|>))
import LS.Rule
  ( Rule (..),
  )
import LS.Types
  ( HasToken (tokenOf),
    HornClause (HC, hBody, hHead),
    MTExpr (MTT),
    MultiTerm,
    MyToken (Means),
    ParamText,
    RPRel (RPis),
    RegKeywords (REvery, RParty),
    RelationalPredicate (RPBoolStructR, RPMT),
    TComparison (TBefore, TOn),
    TemporalConstraint (TemporalConstraint),
    mt2text,
    mtexpr2text,
    pt2text,
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
  forall ann s t.
  Foldable t =>
  t Rule ->
  Doc ann
rules2doc (null -> True) = mempty

rules2doc rules =
  (startRule, transpiledRules) |> uncurry (:) |> concatWith (<.>)
  where
    startRule =
      rules'
        |> mapMaybe rule2RegRuleName
        |> safeHead
        |$> text2qid
        |> maybe mempty ("START" <+>)
    transpiledRules = rules' |$> rule2doc |> rights
    rules' = toList rules
    rule2RegRuleName Regulative {rlabel = Just (_, _, ruleName)} =
      Just ruleName
    rule2RegRuleName _ = Nothing
    x <.> y = [x, ",", line, line, y] |> mconcat

-- Main function that transpiles individual rules.
rule2doc ::
  forall ann s m.
  MonadErrorIsString s m =>
  Rule ->
  m (Doc ann)
rule2doc
  Regulative
    { rlabel = Just (_, _, ruleName),
      rkeyword, subj = Leaf actor,
      deontic, action = Leaf action,
      temporal,
      hence, lest
      -- srcref, -- May want to use this for better error reporting.
    } =
    {-
      Here we first process separately:
      - RULE ruleName
      - rkeyword actor
      - deontic action
      - deadline
      - HENCE/LEST clauses
      We then combine these together via vcat.
    -}
    ([ruleName', rkeywordActor, deonticAction, deadline], henceLestClauses)
      |> first pure
      |> uncurry (liftA2 (<>))
      |$> vcat
    where
      ruleName' = ruleName |> text2qid |> ("RULE" <+>)
      rkeywordActor = rkeywordDeonParamText2doc rkeyword actor
      deonticAction = rkeywordDeonParamText2doc deontic action
      deadline = maybeTempConstr2doc temporal
      henceLestClauses =
        [hence, lest]
          |> traverseWith henceLest2doc [HENCE, LEST]
          |$> filter isNonEmptyDoc
      isNonEmptyDoc doc = doc |> show |> not . null

rule2doc DefNameAlias {name, detail} =
  nameDetails2means name [detail] |> pure

{-
  clauses =
  [ Leaf ( RPMT [MTT "Notify PDPC"] ),
    Leaf ( RPMT [MTT "Notify Individuals"] ) ]
-}
rule2doc
  Hornlike
    { keyword = Means,
      clauses = [HC {hHead = RPBoolStructR mtExpr RPis (All _ leaves)}]
    } =
    leaves |> traverse leaf2mtt |$> nameDetails2means mtExpr
    where
      leaf2mtt (Leaf (RPMT mtt)) = pure mtt
      leaf2mtt _ = errMsg

rule2doc _ = errMsg

text2qid :: forall ann a. (IsString a, Monoid a, Pretty a) => a -> Doc ann
text2qid x = ["qid(\"", x, "\")"] |> mconcat |> pretty

rkeywordDeonParamText2doc :: forall ann a. Show a => a -> ParamText -> Doc ann
rkeywordDeonParamText2doc rkeywordDeon paramText =
  rkeywordDeon' <+> paramText'
  where
    rkeywordDeon' = rkeyword2doc rkeywordDeon
    paramText' = paramText2qid paramText
    rkeyword2doc rkeyword =
      rkeyword |> show2text |> T.tail |> T.toUpper |> pretty
    paramText2qid ((mtExpr, _) :| _) = mtExpr |> toList |> multiTerm2qid

maybeTempConstr2doc :: Maybe (TemporalConstraint T.Text) -> Doc ann
maybeTempConstr2doc
  ( Just
      ( TemporalConstraint
          tComparison@((`elem` [TOn, TBefore]) -> True)
          (Just n)
          (T.toUpper .> (`elem` ["DAY", "DAYS"]) -> True)
        )
    ) =
    [tComparison', n', "DAY"] |> hsep
    where
      n' = pretty n
      tComparison' = tComparison2doc tComparison
      tComparison2doc TOn = "ON"
      tComparison2doc TBefore = "WITHIN"

maybeTempConstr2doc _ = "WITHIN 7 DAY"

multiTerm2qid :: MultiTerm -> Doc ann
multiTerm2qid multiTerm = multiTerm |> mt2text |> text2qid

nameDetails2means ::
  forall ann t. Foldable t => MultiTerm -> t MultiTerm -> Doc ann
nameDetails2means name details =
  [name', "MEANS", details'] |> hsep
  where
    name' = multiTerm2qid name
    details' =
      details
        |> details2qids
        |> intersperse "AND"
        |> hsep
        |> parenthesizeIf (length details > 1)
    details2qids details = details |> toList |$> multiTerm2qid
    parenthesizeIf True x = ["(", x, ")"] |> mconcat
    parenthesizeIf _ x = x

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

data HenceOrLest = HENCE | LEST
  deriving (Eq, Ord, Read, Show)

instance Pretty HenceOrLest where
  pretty = viaShow

{-
  This function can handle invalid HENCE/LEST clauses.
  A left with an error message is returned in such cases.
-}
henceLest2doc ::
  forall ann s m.
  MonadErrorIsString s m =>
  HenceOrLest ->
  Maybe Rule ->
  m (Doc ann)
henceLest2doc _ Nothing = pure mempty

henceLest2doc henceOrLest (Just (RuleAlias henceLest)) =
  (henceOrLest, henceLest)
    |> bimap pretty multiTerm2qid
    |> uncurry (<+>)
    |> pure

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

-- map where the function is also passed the index of the current element.
-- mapIndexed :: (Traversable t, Num s) => (s -> a -> b) -> t a -> t b
-- mapIndexed f xs = xs |> mapAccumL g 0 |> snd
--   where
--     g index val = (index + 1, f index val)

-- mapFirst pred f xs applies f to the first element of xs that satisfies pred.
-- mapFirst :: Traversable t => (b -> Bool) -> (b -> b) -> t b -> t b
-- mapFirst pred f xs = xs |> mapAccumL g False |> snd
--   where
--     g False elt@(pred -> True) = (True, f elt)
--     g seen elt = (seen, elt)

--- Like mapIndexed, but uses traverse/sequenceA for short-circuiting.
-- traverseIndexed ::
--   (Traversable t, Num s, Applicative f) =>
--   (s -> a -> f b) -> t a -> f (t b)
-- traverseIndexed f xs = xs |> mapIndexed f |> sequenceA

--- Like zipWith, but uses traverse/sequenceA for short-circuiting.
traverseWith ::
  (Foldable t1, Foldable t2, Applicative f) =>
  (a1 -> a2 -> f b) ->
  t1 a1 ->
  t2 a2 ->
  f [b]
traverseWith f xs ys =
  zipWith f xs' ys' |> sequenceA
  where
    xs' = toList xs
    ys' = toList ys

infixl 0 |$>

(|$>) :: Functor f => f a -> (a -> b) -> f b
(|$>) = (<&>)

show2text :: Show a => a -> T.Text
show2text x = x |> show |> T.pack

safeHead :: (Applicative f, Monoid (f a)) => [a] -> f a
safeHead (x : _) = pure x
safeHead _ = mempty

errMsg :: MonadErrorIsString s m => m a
errMsg = throwError "Not supported."