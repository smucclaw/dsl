{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Maude.Rule
  ( rule2doc,
  )
where

import AnyAll (BoolStruct (All, Leaf))
import Data.List (intersperse)
import Data.Maybe (maybeToList)
import Data.MonoTraversable (Element, MonoFoldable (otoList, ocompareLength))
import Data.Sequences as Seq (IsSequence)
import Flow ((|>), (.>))
import LS.Rule (Rule (..), rkeyword)
import LS.Types
  ( HornClause (..),
    MTExpr,
    MultiTerm,
    MyToken (Means),
    RPRel (RPis),
    RelationalPredicate (RPBoolStructR, RPMT),
  )
import LS.XPile.Maude.Regulative.HenceLest
  ( HenceLest (..),
    HenceLestClause (..),
    henceLest2doc,
  )
import LS.XPile.Maude.Regulative.RkeywordDeonticActorAction
  ( DeonticAction (..),
    RkeywordActor (..),
    deonticAction2doc,
    rkeywordActor2doc,
  )
import LS.XPile.Maude.Regulative.TempConstr (tempConstr2doc)
import LS.XPile.Maude.Utils
  ( multiExprs2qid,
    text2qid,
    throwDefaultErr,
  )
import LS.Utils ( (|$>), MonoidValidate )
import Prettyprinter (Doc, vcat, hsep)
import Witherable (wither)
import Prettyprinter.Interpolate (di)

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

-- Main function that transpiles individual rules.
rule2doc :: Rule -> MonoidValidate (Doc ann1) (Doc ann2)
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
      If an error occurs, seaquenceA short-circuits the unhappy path.
      We continue along the happy path by removing empty docs and vcat'ing
      everything together.
    -}
      vcat <$> ruleActorAction <> deadline <> henceLestClauses
    where
      ruleActorAction = sequenceA [ruleName', rkeywordActor, deonticAction]
      ruleName' = pure [di|RULE #{text2qid ruleName}|]
      rkeywordActor =
        RkeywordActor {rkeyword, actor} |> rkeywordActor2doc -- |$> pure
      deonticAction =
        DeonticAction {deontic, action} |> deonticAction2doc -- |$> pure

      deadline = temporal |> tempConstr2doc |$> maybeToList

      henceLestClauses =
        -- wither is an effectful mapMaybes, so that this maps henceLest2doc
        -- which returns (Either s (Maybe (Doc ann)) over the list,
        -- throwing out all the (Right Nothing).
        -- Note that this is effectful in that we short-circuit when we
        --- encounter a Left.
        [(HENCE, hence), (LEST, lest)] 
          |$> uncurry HenceLestClause
          |> wither henceLest2doc

rule2doc DefNameAlias {name, detail} =
  pure $ mkMeans name [detail]

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
    leaves
      |> traverse
        -- Convert each leaf into a mtt
        ( \case
            Leaf (RPMT mtt) -> pure mtt
            _ -> throwDefaultErr
        )
      |$> mkMeans mtExpr

rule2doc _ = throwDefaultErr

{-
  mkMeans "A" ["B", "C", "D"] = "A MEANS (B AND C AND D)"
-}
mkMeans ::
  (IsSequence t, Element t ~ MultiTerm) => MultiTerm -> t -> Doc ann
mkMeans name (otoList -> details :: [MultiTerm]) =
  [di|#{name'} MEANS #{details'}|]
  where
    name' = multiExprs2qid name
    details' =
      details
        |$> multiExprs2qid
        |> \case
          [qid] -> qid
          qids -> [di|(#{qids |> intersperse "AND" |> hsep})|]
        -- |> parenthesizeIf (lengthMoreThanOne details)

    -- parenthesizeIf True x = [di|(#{x})|]
    -- parenthesizeIf False x = x

    -- lengthMoreThanOne ((`ocompareLength` 1) -> GT) = True
    -- lengthMoreThanOne _ = False