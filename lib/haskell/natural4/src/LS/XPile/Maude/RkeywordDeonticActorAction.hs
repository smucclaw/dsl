{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Maude.RkeywordDeonticActorAction
  ( RkeywordActor (..),
    DeonticAction (..),
    rkeywordActor2doc,
    deonticAction2doc,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid (Ap)
import Data.Text qualified as T
import Flow ((|>))
import LS.Types
  ( Deontic (DMay, DMust, DShant),
    ParamText,
    RegKeywords (REvery, RParty), MTExpr,
  )
import LS.XPile.Maude.Utils (multiExprs2qid, throwDefaultErr)
import Prettyprinter (Doc, Pretty (pretty), (<+>))

data RkeywordActor where
  RkeywordActor ::
    { rkeyword :: RegKeywords,
      actor :: ParamText
    } ->
    RkeywordActor

{-
  This function handles things like:
  - PARTY/EVERY (some paramText denoting the actor)
  - MUST/MAY/SHANT (some paramText denoting the action)
-}
rkeywordActor2doc ::
  RkeywordActor -> Ap (Either (Doc ann1)) (Doc ann2)
rkeywordActor2doc
  RkeywordActor
    { rkeyword = rkeyword@((`elem` [REvery, RParty]) -> True),
      actor
    } =
    rkeywordDeonticActorAction2doc rkeyword actor

rkeywordActor2doc _ = throwDefaultErr

data DeonticAction where
  DeonticAction ::
    { deontic :: Deontic,
      action :: ParamText
    } ->
    DeonticAction
  deriving (Eq, Ord, Show)

deonticAction2doc ::
  DeonticAction -> Ap (Either (Doc ann1)) (Doc ann2)
deonticAction2doc
  DeonticAction
    { deontic = deontic@((`elem` [DMust, DMay, DShant]) -> True),
      action
    } =
    rkeywordDeonticActorAction2doc deontic action

deonticAction2doc _ = throwDefaultErr

rkeywordDeonticActorAction2doc ::
  (Foldable t, Show a, Applicative f) =>
  a ->
  NonEmpty (t MTExpr, b) ->
  f (Doc ann)
rkeywordDeonticActorAction2doc rkeywordDeontic ((actorAction, _) :| _) =
  pure $ rkeywordDeontic' <+> actorAction'
  where
    rkeywordDeontic' =
      rkeywordDeontic |> show |> T.pack |> T.tail |> T.toUpper |> pretty
    actorAction' = multiExprs2qid actorAction