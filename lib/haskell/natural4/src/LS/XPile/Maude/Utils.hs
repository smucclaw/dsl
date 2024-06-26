{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Maude.Utils
  ( throwDefaultErr,
    multiExprs2qid,
    text2qid
  )
where

import Control.Monad.Validate (MonadValidate (refute))
import Data.MonoTraversable (Element, MonoFoldable (otoList))
import Data.Sequences (SemiSequence)
import Data.Text (Text)
import Flow ((|>))
import LS.Types (MTExpr, mt2text)
import LS.Utils (MonoidValidate)
import Prettyprinter (Doc)
import Prettyprinter.Interpolate (di)

throwDefaultErr :: MonoidValidate (Doc ann) a
throwDefaultErr = refute [di|Not supported.|]

-- type IsList :: (Type -> Type) -> Constraint
-- type IsList t =
--   ( Foldable t,
--     forall a. Show a => Show (t a),
--     forall a. Ord a => Ord (t a),
--     (t == NonEmpty || t == []) ~ True
--   )

{-
  Note that (mt2text :: MultiTerm -> Text) and that Multiterm = [MTExpr], but
  sometimes we want to apply this function not just to MultiTerm but to
  NonEmpty MTExpr.
-}
multiExprs2qid :: (SemiSequence t, Element t ~ MTExpr) => t -> Doc ann
multiExprs2qid (otoList -> multiExprs :: [MTExpr]) =
  multiExprs |> mt2text |> text2qid

text2qid :: Text -> Doc ann
text2qid x = [di|qid("#{x}")|]
