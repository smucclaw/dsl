{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Maude.Regulative.RkeywordDeonticActorAction
  ( RkeywordActor (..),
    DeonticAction (..),
    rkeywordActor2doc,
    deonticAction2doc,
  )
where

import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as T
import Data.Type.Bool (type (||))
import Data.Type.Equality (type (==))
import Flow ((|>))
import GHC.Generics (Generic)
import LS.Types
  ( Deontic (DMay, DMust, DShant),
    MTExpr,
    ParamText,
    RegKeywords (REvery, RParty),
  )
import LS.Utils (MonoidValidate)
import LS.XPile.Maude.Utils (multiExprs2qid, throwDefaultErr)
import Prettyprinter (Doc)
import Prettyprinter.Interpolate (di)

data RkeywordActor = RkeywordActor
  { rkeyword :: RegKeywords,
    actor :: ParamText
  }
  deriving (Eq, Generic, Ord, Show)

instance Hashable RkeywordActor

{-
  This function handles things like:
  - PARTY/EVERY (some paramText denoting the actor)
  - MUST/MAY/SHANT (some paramText denoting the action)
-}
rkeywordActor2doc :: RkeywordActor -> MonoidValidate (Doc ann1) (Doc ann2)
rkeywordActor2doc
  RkeywordActor
    { rkeyword = rkeyword@((`elem` [REvery, RParty]) -> True),
      actor
    } =
    rkeywordDeonticActorAction2doc rkeyword actor

rkeywordActor2doc _ = throwDefaultErr

data DeonticAction = DeonticAction
  { deontic :: Deontic,
    action :: ParamText
  }
  deriving (Eq, Generic, Ord, Show)

instance Hashable DeonticAction

deonticAction2doc :: DeonticAction -> MonoidValidate (Doc ann1) (Doc ann2)
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
  MonoidValidate (Doc ann1) (Doc ann2)
rkeywordDeonticActorAction2doc rkeywordDeontic ((actorAction, _) :| _) =
  pure [di|#{rkeywordDeontic'} #{actorAction'}|]
  where
    rkeywordDeontic' =
      rkeywordDeontic |> show |> T.pack |> T.tail |> T.toUpper
    actorAction' = multiExprs2qid actorAction