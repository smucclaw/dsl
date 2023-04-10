{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Maude.Regulative.TempConstr
  ( tempConstr2doc,
  )
where

import Control.Monad.Validate (Validate)
import Data.Monoid (Ap)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Flow ((.>))
import LS.Types
  ( TComparison (TBefore, TOn),
    TemporalConstraint (TemporalConstraint),
  )
import LS.XPile.Maude.Utils (throwDefaultErr)
import Prettyprinter (Doc, Pretty (pretty), hsep)
import Prettyprinter.Interpolate (di)

tempConstr2doc ::
  Maybe (TemporalConstraint T.Text) ->
  Ap (Validate (Doc ann1)) (Maybe (Doc ann2))
tempConstr2doc = traverse $ \case
  ( TemporalConstraint
      tComparison@((`elem` [TOn, TBefore]) -> True)
      (Just n)
      (T.toUpper .> (`elem` [[i|DAY|], [i|DAYS|]]) -> True)
    ) ->
      pure [di|#{tComparison'} #{n} DAY|]
      where
        tComparison' = case tComparison of
          TOn -> [di|ON|]
          TBefore -> [di|WITHIN|]

  _ -> throwDefaultErr