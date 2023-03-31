{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Maude.RkeywordDeonticActorAction
  ( RkeywordActor (..),
    DeonticAction (..),
    rkeywordActor2doc,
    deonticAction2doc,
  )
where

import Control.Monad.Validate (Validate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid (Ap)
import Data.Text qualified as T
import Data.Type.Equality (type (==))
import Flow ((|>))
import LS.Types
  ( Deontic (DMay, DMust, DShant),
    MTExpr,
    ParamText,
    RegKeywords (REvery, RParty),
  )
import LS.XPile.Maude.Utils (IsList, multiExprs2qid, throwDefaultErr)
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import Data.Type.Bool (type (||))

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
  RkeywordActor -> Ap (Validate (Doc ann1)) (Doc ann2)
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
  DeonticAction -> Ap (Validate (Doc ann1)) (Doc ann2)
deonticAction2doc
  DeonticAction
    { deontic = deontic@((`elem` [DMust, DMay, DShant]) -> True),
      action
    } =
    rkeywordDeonticActorAction2doc deontic action

deonticAction2doc _ = throwDefaultErr

rkeywordDeonticActorAction2doc ::
  (Show a, (a == RegKeywords || a == Deontic) ~ True) =>
  a ->
  NonEmpty (NonEmpty MTExpr, b) ->
  Ap (Validate (Doc ann1)) (Doc ann2)
rkeywordDeonticActorAction2doc rkeywordDeontic ((actorAction, _) :| _) =
  pure $ rkeywordDeontic' <+> actorAction'
  where
    rkeywordDeontic' =
      rkeywordDeontic |> show |> T.pack |> T.tail |> T.toUpper |> pretty
    actorAction' = multiExprs2qid actorAction