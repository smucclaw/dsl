{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Maude.RkeywordDeonticActorAction
  ( RKeywordActorDeonticAction (..),
    rkeywordDeonticActorAction2doc,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid (Ap)
import Data.Text qualified as T
import Flow ((|>))
import LS.Types
  ( Deontic (DMay, DMust, DShant),
    ParamText,
    RegKeywords (REvery, RParty),
  )
import LS.XPile.Maude.Utils (multiExprs2qid, throwDefaultErr)
import Prettyprinter (Doc, Pretty (pretty), (<+>))

data RKeywordActorDeonticAction where
  RKeywordActor ::
    { rkeyword :: RegKeywords,
      actor :: ParamText
    } ->
    RKeywordActorDeonticAction
  DeonticAction ::
    { deontic :: Deontic,
      action :: ParamText
    } ->
    RKeywordActorDeonticAction
  deriving (Eq, Ord, Show)

{-
  This function handles things like:
  - PARTY/EVERY (some paramText denoting the actor)
  - MUST/MAY/SHANT (some paramText denoting the action)
-}
rkeywordDeonticActorAction2doc ::
  RKeywordActorDeonticAction -> Ap (Either (Doc ann1)) (Doc ann2)
rkeywordDeonticActorAction2doc = \case
  RKeywordActor
    { rkeyword = rkeyword@((`elem` [REvery, RParty]) -> True),
      actor
    } -> go rkeyword actor

  DeonticAction
    { deontic = deontic@((`elem` [DMust, DMay, DShant]) -> True),
      action
    } -> go deontic action

  _ -> throwDefaultErr
  where
    go rkeywordDeontic ((actorAction, _) :| _) =
      pure $ rkeywordDeontic' <+> actorAction'
      where
        rkeywordDeontic' =
          rkeywordDeontic |> show |> T.pack |> T.tail |> T.toUpper |> pretty
        actorAction' = multiExprs2qid actorAction