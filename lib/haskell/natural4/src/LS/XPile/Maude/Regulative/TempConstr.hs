{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Maude.Regulative.TempConstr
  ( tempConstr2doc,
  )
where

import Data.String.Interpolate (i)
import Data.Text qualified as T
import Flow ((.>))
import LS.Types
  ( TComparison (TBefore, TOn),
    TemporalConstraint (TemporalConstraint),
  )
import LS.Utils (MonoidValidate)
import LS.XPile.Maude.Utils (throwDefaultErr)
import Prettyprinter (Doc)
import Prettyprinter.Interpolate (di)

tempConstr2doc ::
  forall ann1 ann2.
  Maybe (TemporalConstraint T.Text) ->
  MonoidValidate (Doc ann1) (Maybe (Doc ann2))
tempConstr2doc = traverse \case
  ( TemporalConstraint
      tComparison@((`elem` [TOn, TBefore]) -> True)
      (Just n)
      (T.toUpper .> (`elem` ["DAY", "DAYS"]) -> True)
    ) ->
      pure [di|#{tComparison'} #{n} DAY|]
      where
        tComparison' :: Doc ann2 = case tComparison of
          TOn -> "ON"
          TBefore -> "WITHIN"

  _ -> throwDefaultErr