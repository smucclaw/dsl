{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LS.XPile.Maude.HenceLest
  ( HenceLest(..),
    HenceLestClause(..),
    henceLest2doc,
  )
where

import LS.Rule ( Rule(RuleAlias) )
import Data.Monoid (Ap)
import Prettyprinter (Doc, viaShow, (<+>))
import Data.Traversable (for)
import LS.XPile.Maude.Utils (multiExprs2qid, throwDefaultErr)

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
  HenceLestClause ->
  Ap (Either (Doc ann1)) (Maybe (Doc ann2))
henceLest2doc HenceLestClause {henceLest, clause} =
   for clause $ \case
    (RuleAlias clause) ->
      pure $ henceLest' <+> clause'
      where
        henceLest' = viaShow henceLest
        clause' = multiExprs2qid clause
    _  -> throwDefaultErr

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