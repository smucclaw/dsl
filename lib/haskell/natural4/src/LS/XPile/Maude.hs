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
import Data.Coerce (coerce)
import Data.Either (rights)
import Data.Foldable qualified as Fold
  ( Foldable (elem, toList),
    find,
    fold,
    toList,
  )
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Monoid (Ap (Ap))
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
import Witherable (wither)

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
    |> wither swallowErrs
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

    swallowErrs :: Ap m (Doc ann) -> Ap m (Maybe (Doc ann))
    swallowErrs doc = doc |$> pure |$> (`catchError` const mempty)

    x <.> y = mconcat [x, ",", line, line, y]

    rules' = Fold.toList rules

    isRegRule Regulative {} = True
    isRegRule _ = False

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
      hence,
      lest
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
      |> wither id
      |$> vcat
    where
      ruleActorDeonticAction :: [Ap m (Maybe (Doc ann))]
      ruleActorDeonticAction =
        [ruleName', rkeywordActor, deonticAction] |$> Just |$> pure
      ruleName' = "RULE" <+> text2qid ruleName
      rkeywordActor = rkeywordDeonParamText2doc rkeyword actor
      deonticAction = rkeywordDeonParamText2doc deontic action

      deadline = tempConstr2doc temporal

      henceLestClauses =
        [(HENCE, hence), (LEST, lest)]
          |$> uncurry HenceLestClause
          |$> henceLest2doc

rule2doc DefNameAlias {name, detail} =
  pure $ nameDetails2means name [detail]

{-
  clauses =
  [ RPBoolStructR ["Notification"] RPis
    (All _
      Leaf ( RPMT [MTT "Notify PDPC"] ),
      Leaf ( RPMT [MTT "Notify Individuals"] )) ]
-}
rule2doc
  Hornlike
    { keyword = Means,
      clauses = [HC {hHead = RPBoolStructR mtExpr RPis (All _ leaves)}]
    } =
    leaves |> traverse leaf2mtt |$> nameDetails2means mtExpr
    where
      leaf2mtt (Leaf (RPMT mtt)) = pure mtt
      leaf2mtt _ = throwDefaultErr

rule2doc _ = throwDefaultErr

-- traverseAndremoveEmptyDocs ::
--   (Traversable t, Applicative f, Wither.Filterable t, Show b) =>
--   (a -> f b) -> t a -> f (t b)
-- traverseAndremoveEmptyDocs f docs =
--   docs |> traverse f |$> Wither.filter (not . null . show)

text2qid :: forall ann a. (IsString a, Monoid a, Pretty a) => a -> Doc ann
text2qid x = ["qid(\"", x, "\")"] |> mconcat |> pretty

{-
  This function handles things like:
  - PARTY/EVERY (some paramText denoting the actor)
  - MUST/MAY/SHANT (some paramText denoting the action)
-}
rkeywordDeonParamText2doc :: forall ann a. Show a => a -> ParamText -> Doc ann
rkeywordDeonParamText2doc rkeywordDeon ((mtExprs, _) :| _) =
  rkeywordDeon' <+> paramText'
  where
    rkeywordDeon' =
      rkeywordDeon |> show2text |> T.tail |> T.toUpper |> pretty
    -- Note that mtExprs is a (NonEmpty MTExpr) but MultiTerm = [MTExpr] so
    -- that we have to use toList to convert it to a multi term before passing
    -- it to multiExprs2qid.
    paramText' = multiExprs2qid mtExprs

tempConstr2doc ::
  forall ann s m. 
  MonadErrorIsString s m =>
  Maybe (TemporalConstraint T.Text) ->
  Ap m (Maybe (Doc ann))
tempConstr2doc = traverse go
  {-
    Note that traverse is effectively an effectful fmap, meaning that the
    function used for traversal can throw exceptions, which traverse will lift
    up to the outer layer.
    This lets us short-circuit evaluation if we are traversing over something
    like a list.
  -}
  where
    go :: TemporalConstraint T.Text -> Ap m (Doc ann)
    go
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

    go _ = throwDefaultErr

multiExprs2qid :: Foldable t => t MTExpr -> Doc ann
multiExprs2qid multiExprs = multiExprs |> Fold.toList |> mt2text |> text2qid

nameDetails2means ::
  forall ann t. Foldable t => MultiTerm -> t MultiTerm -> Doc ann
nameDetails2means name details =
  hsep [name', "MEANS", details']
  where
    name' = multiExprs2qid name
    details' =
      details
        |> Fold.toList
        |$> multiExprs2qid
        |> intersperse "AND"
        |> hsep
        |> parenthesizeIf (length details > 1)

    parenthesizeIf True x = mconcat ["(", x, ")"]
    parenthesizeIf False x = x

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

data HenceLest where
  HENCE :: HenceLest
  LEST :: HenceLest
  deriving (Eq, Ord, Read, Show)

-- instance Pretty HenceLest where
--   pretty = viaShow

data HenceLestClause where
  HenceLestClause ::
    { henceLest :: HenceLest,
      clause :: Maybe Rule
    } ->
    HenceLestClause
  deriving (Eq, Ord, Show)

henceLest2doc ::
  forall ann s m.
  MonadErrorIsString s m =>
  HenceLestClause ->
  Ap m (Maybe (Doc ann))
henceLest2doc HenceLestClause {henceLest, clause} =
  traverse clause2doc clause
  where
    clause2doc :: Rule -> Ap m (Doc ann)
    clause2doc (RuleAlias clause) =
      pure $ viaShow henceLest <+> multiExprs2qid clause
    clause2doc _ = throwDefaultErr

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
  Here, we often take (m = Either (Doc ann)) and (a = Maybe (Doc ann)) and we
  want to utilize the (<>) and mempty of (Maybe (Doc ann)), lifted up into
  the Either.

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

  TODO: 
  Note that for (m = Either s), the monoid structure of (Ap m a) is
  a bit stupid in that Lefts are left-absorbing under (<>), that is
    Ap (Left x) <> Ap _ = Ap (Left x)
  Thus it may be better to use (m a) directly and manually define a monoid
  structure for that rather than use (Ap m a).

  See http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf
-}
deriving via
  m :: Type -> Type
  instance
    MonadError s m => MonadError s (Ap m)

findWithErrMsg ::
  (Foldable t, MonadError e m) => (a -> Bool) -> e -> t a -> m a
findWithErrMsg pred err xs =
  xs |> Fold.find pred |> maybe (throwError err) pure

throwDefaultErr :: MonadErrorIsString s m => m a
throwDefaultErr = throwError "Not supported."

infixl 0 |$>

(|$>) :: Functor f => f a -> (a -> b) -> f b
(|$>) = (<&>)

show2text :: Show a => a -> T.Text
show2text x = x |> show |> T.pack

-- safeHead :: (Applicative f, Monoid (f a)) => [a] -> f a
-- safeHead (x : _) = pure x
-- safeHead _ = mempty

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