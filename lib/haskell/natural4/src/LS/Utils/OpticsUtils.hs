{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, RecordWildCards, NoFieldSelectors #-}
-- {-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module LS.Utils.OpticsUtils (setInsert) where

-- import Data.Text qualified as T
-- import Data.HashSet qualified as HS
-- import Data.Hashable (Hashable, hashWithSalt, hashUsing)
import Optics

{- Optics provide a nice interface over concrete data structures,
but they can sometimes be hard to read / understand.
The following functions provide more readable variants of those functions
-}

-- $setup
-- >>> import Data.HashSet qualified as HS

{- | Insert into __any__ kind of set

Examples:

>>> setInsert 5 (HS.fromList [1..3])
fromList [1,2,3,5]
-}
setInsert :: forall a. (IxValue a ~ (), At a) => Index a -> a -> a
elt `setInsert` set = set & at' elt ?~()
