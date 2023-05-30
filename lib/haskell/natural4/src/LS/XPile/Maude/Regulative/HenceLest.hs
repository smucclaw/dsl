{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module LS.XPile.Maude.Regulative.HenceLest
  ( HenceLest (..),
    HenceLestClause (..),
    henceLest2doc,
  )
where

import Data.Hashable (Hashable)
import Data.Traversable (for)
import GHC.Generics (Generic)
import LS.Rule (Rule (RuleAlias))
import LS.Utils (MonoidValidate)
import LS.XPile.Maude.Utils (multiExprs2qid, throwDefaultErr)
import Prettyprinter (Doc)
import Prettyprinter.Interpolate (di)

data HenceLest where
  HENCE :: HenceLest
  LEST :: HenceLest
  deriving (Eq, Generic, Ord, Read, Show)

instance Hashable HenceLest

-- instance Pretty HenceLest where
--   pretty = viaShow

data HenceLestClause where
  HenceLestClause ::
    { henceLest :: HenceLest,
      clause :: Maybe Rule
    } ->
    HenceLestClause
  deriving (Eq, Generic, Ord, Show)

henceLest2doc ::
  HenceLestClause ->
  MonoidValidate (Doc ann1) (Maybe (Doc ann2))
henceLest2doc HenceLestClause {henceLest, clause} =
   for clause $ \case
    (RuleAlias clause) ->
      pure [di|#{henceLest} #{multiExprs2qid clause}|]

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