{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Maude.Regulative.TempConstr
  ( tempConstr2doc,
  )
where

import Control.Monad.Validate (Validate)
import Data.Monoid (Ap)
import Data.Text qualified as T
import Flow ((.>))
import LS.Types
  ( TComparison (TBefore, TOn),
    TemporalConstraint (TemporalConstraint),
  )
import LS.XPile.Maude.Utils (throwDefaultErr)
import Prettyprinter (Doc, Pretty (pretty), hsep)

tempConstr2doc ::
  Maybe (TemporalConstraint T.Text) ->
  Ap (Validate (Doc ann1)) (Maybe (Doc ann2))
tempConstr2doc = traverse $ \case
  {-
    Note that traverse is effectively an effectful fmap, meaning that the
    function used for traversal can throw exceptions, which will short-circuit
    traverse.
  -}
  -- _ :: TemporalConstraint T.Text -> Ap (Either s) (Doc ann)
  ( TemporalConstraint
      tComparison@((`elem` [TOn, TBefore]) -> True)
      (Just n)
      (T.toUpper .> (`elem` ["DAY", "DAYS"]) -> True)
    ) ->
      pure $ hsep [tComparison', n', "DAY"]
      where
        n' = pretty n
        tComparison' = case tComparison of
          TOn -> "ON"
          TBefore -> "WITHIN"

  _ -> throwDefaultErr