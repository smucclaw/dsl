{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-
  Work-in-progress transpiler to Maude.
  Note that since we do all the parsing and transpilation within Maude itself,
  all we do here is convert the list of rules to a textual, string
  representation that Maude can parse.
-}

module LS.XPile.Maude where

import AnyAll (BoolStruct (All, Leaf))
import Control.Monad.Except (MonadError (throwError), catchError)
import Data.Coerce (coerce, Coercible)
import Data.Either (rights)
import Data.Foldable ( Foldable(elem, toList), find )
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid (Ap(Ap))
import Data.String (IsString)
import Data.Text qualified as T
-- import Data.Traversable (mapAccumL)
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
rules2maudeStr rules =
  rules
    |> rules2doc
    |> (coerce :: Ap (Either (Doc ann)) (Doc ann) -> Either (Doc ann) (Doc ann))
    |> either id id
    |> show

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
  forall ann t m s.
  (MonadErrorIsString s m, Foldable t) =>
  t Rule ->
  Ap m (Doc ann)
rules2doc rules =
  (startRule : transpiledRules)
    -- TODO:
    -- Don't just swallow up errors and turn them into mempty.
    -- Actually output a comment indicating what went wrong while transpiling
    -- those erraneous rules.
    |> traverseAndremoveEmptyDocs swallowErrs
    |$> concatWith (<.>)
  where
    -- Find the first regulative rule and extracts its rule name.
    -- This returns a Maybe because there may not be any regulative rule.
    -- In such cases, we simply return mempty, the empty doc.
    -- Otherwise, we turn it into a quoted symbol and prepend START.
    startRule =
      rules'
        |> findWithErrMsg isRegRule "No regulative rule found."
        |$> regRule2startRule

    -- Transpile the rules to docs and collect all those that transpiled
    -- correctly, while ignoring erraneous ones.
    transpiledRules = rules' |$> rule2doc

    swallowErrs docs = docs `catchError` const mempty

    x <.> y = mconcat [x, ",", line, line, y]

    rules' = toList rules

    isRegRule Regulative {} = True
    isRegRule _ = False

    findWithErrMsg pred errMsg xs =
      xs |> find pred |> maybe (throwError errMsg) pure

    regRule2startRule Regulative {rlabel = Just (_, _, ruleName)} =
      "START" <+> text2qid ruleName

-- Main function that transpiles individual rules.
rule2doc ::
  forall ann s m.
  MonadErrorIsString s m =>
  Rule ->
  Ap m (Doc ann)

rule2doc
  Regulative
    { rlabel = Just (_, _, ruleName),
      rkeyword,
      subj = Leaf actor,
      deontic,
      action = Leaf action,
      temporal,
      hence = maybeHence,
      lest = maybeLest
      -- srcref, -- May want to use this for better error reporting.
    } =
    {-
      Here we first process separately:
      - RULE ruleName
      - rkeyword actor
      - deontic action
      - deadline
      - HENCE/LEST clauses
      If an error occurs, traverse short-circuits the unhappy path.
      We continue along the happy path by removing empty docs and vcat'ing
      everything together.
    -}
    [ruleActorDeonticAction, [deadline], henceLestClauses]
      |> mconcat
      |> traverseAndremoveEmptyDocs id
      |$> vcat
    where
      ruleActorDeonticAction =
        [ruleName', rkeywordActor, deonticAction] |$> pure
      ruleName' = "RULE" <+> text2qid ruleName
      rkeywordActor = rkeywordDeonParamText2doc rkeyword actor
      deonticAction = rkeywordDeonParamText2doc deontic action

      deadline = maybeEmpty tempConstr2doc temporal

      henceLestClauses =
        zipWith maybeHenceLest2doc [HENCE, LEST] [maybeHence, maybeLest]

      {-
        Note that GHC's type inference breaks down if we don't type annotate
        this and for that, we need ScopedTypeVariables with m and ann
        quantified in the outer scope.
        In any case, a type annotations is provided here for readability.
      -}
      maybeHenceLest2doc :: HenceOrLest -> Maybe Rule -> Ap m (Doc ann)
      maybeHenceLest2doc = henceLest2doc .> maybeEmpty

      isNonEmptyDoc doc = doc |> show |> not . null

rule2doc DefNameAlias {name, detail} =
  pure $ nameDetails2means name [detail]

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

traverseAndremoveEmptyDocs ::
  (Applicative m, Show a) => (Ap m a -> Ap m a) -> [Ap m a] -> Ap m [a]
traverseAndremoveEmptyDocs f docs =
  docs |> traverse f |$> filter (not . null . show)

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
    -- Note that mtExprs is a (NonEmpty MTExpr) but MultiTerm = [MTExpr] so
    -- that we have to use toList to convert it to a multi term before passing
    -- it to multiTerm2qid.
    paramText2qid ((mtExprs, _) :| _) = mtExprs |> toList |> multiTerm2qid

tempConstr2doc ::
  forall ann s m.
  MonadErrorIsString s m => TemporalConstraint T.Text -> Ap m (Doc ann)
tempConstr2doc
  ( TemporalConstraint
      tComparison@((`elem` [TOn, TBefore]) -> True)
      (Just n)
      (T.toUpper .> (`elem` ["DAY", "DAYS"]) -> True)
    ) =
    [tComparison', n', "DAY"] |> hsep |> pure
    where
      n' = pretty n
      tComparison' = tComparison2doc tComparison
      tComparison2doc TOn = "ON"
      tComparison2doc TBefore = "WITHIN"

tempConstr2doc _ = errMsg

multiTerm2qid :: MultiTerm -> Doc ann
multiTerm2qid multiTerm = multiTerm |> mt2text |> text2qid

nameDetails2means ::
  forall ann t. Foldable t => MultiTerm -> t MultiTerm -> Doc ann
nameDetails2means name details =
  hsep [name', "MEANS", details']
  where
    name' = multiTerm2qid name
    details' =
      details
        |> details2qids
        |> intersperse "AND"
        |> hsep
        |> parenthesizeIf (length details > 1)

    details2qids details = details |> toList |$> multiTerm2qid

    parenthesizeIf True x = mconcat ["(", x, ")"]
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

data HenceOrLest where
  HENCE :: HenceOrLest
  LEST :: HenceOrLest
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
  Rule ->
  Ap m (Doc ann)
henceLest2doc henceOrLest (RuleAlias henceLest) =
  pure $ henceOrLest' <+> henceLest'
  where
    henceOrLest' = pretty henceOrLest
    henceLest' = multiTerm2qid henceLest

henceLest2doc _ _ = errMsg

-- Common utilities

{-
  Error monad, polymorphic over:
  - a type variable (s :: Type) such that IsString s.
  - a type constructor (m :: Type -> Type) with the structure of a monad.
-}
type MonadErrorIsString s m = (IsString s, MonadError s m)

{-
  The idea is that given a (MonadError s m) and a monoid (a :: Type), we want
  to operate on (m a :: Type) as if it were also a monoid.
  Here, we often take (m = Either a) and (a = Doc ann) and we want to
  utilize the (<>) and mempty of (Doc ann), lifted up into (Either a).

  Suppose (m :: Type -> Type) is a type constructor and (a :: Type) is a monoid.
  Then Data.Monoid defines (newtype Ap m a) which lifts the monoid structure of
  a up into the applicative m.
  More concretely, (Ap m :: Type -> Type) inherits both the applicative
  structure of m and the monoid structure of its input type argument, with:
  - (<>) = liftA2 (<>)
  - mempty = pure mempty
  Moreover, if m is also a monad, then (Ap m) also inherits this monad
  structure.
  The standalone deriving via thing below enables (Ap m) to inherit the
  MonadError instance of m should m also be a MonadError.
-}
deriving via m :: Type -> Type instance
  MonadError s m => MonadError s (Ap m)

infixl 0 |$>

(|$>) :: Functor f => f a -> (a -> b) -> f b
(|$>) = (<&>)

maybeEmpty :: Monoid b => (a -> b) -> Maybe a -> b
maybeEmpty = maybe mempty

show2text :: Show a => a -> T.Text
show2text x = x |> show |> T.pack

-- safeHead :: (Applicative f, Monoid (f a)) => [a] -> f a
-- safeHead (x : _) = pure x
-- safeHead _ = mempty

errMsg :: MonadErrorIsString s m => m a
errMsg = throwError "Not supported."

-- traverseWith ::
--   (Foldable t1, Foldable t2, Applicative f) =>
--   (a1 -> a2 -> f b) ->
--   t1 a1 ->
--   t2 a2 ->
--   f [b]
-- traverseWith f xs ys =
--   zipWith f xs' ys' |> sequenceA
--   where
--     xs' = toList xs
--     ys' = toList ys

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