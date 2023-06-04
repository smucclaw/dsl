{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LS.XPile.Maude.Utils
  ( throwDefaultErr,
    multiExprs2qid,
    text2qid
  )
where

import Control.Monad.Validate (MonadValidate (refute), Validate, runValidate)
import Data.Coerce (coerce)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import Data.MonoTraversable (Element, MonoFoldable (otoList))
import Data.Monoid (Ap (Ap))
import Data.Sequences as Seq (SemiSequence)
import Data.Text qualified as T
import Data.Type.Bool (type (||))
import Data.Type.Equality (type (==))
import Flow ((|>))
import LS.Types (MTExpr, mt2text)
import LS.Utils (MonoidValidate)
import Prettyprinter (Doc, Pretty (pretty))
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
multiExprs2qid multiExprs = multiExprs |> otoList |> mt2text |> text2qid

text2qid :: T.Text -> Doc ann
text2qid x = [di|qid("#{x}")|]