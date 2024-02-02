module LS.Utils.TextUtils
  ( float2Text,
    int2Text
  )
where

import Data.Text qualified as T
import Data.Text.Lazy qualified as T (toStrict)
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Lazy.Builder.Int qualified as B
import Data.Text.Lazy.Builder.RealFloat qualified as B
  ( FPFormat (..),
    formatRealFloat,
  )


{-| From https://github.com/haskell/text/issues/218
Thanks to Jo Hsi for finding these!
-}

float2Text :: RealFloat a => a -> T.Text
float2Text = T.toStrict . B.toLazyText . decFloat

{- | Differs from B.realFloat only in that we use standard decimal notation (i.e., in the choice of FPFormat)
See https://hackage.haskell.org/package/text-2.1/docs/src/Data.Text.Lazy.Builder.RealFloat.html
-}
decFloat :: RealFloat a => a -> B.Builder
decFloat = B.formatRealFloat B.Fixed Nothing

{-# SPECIALIZE decFloat :: Float -> B.Builder #-}
{-# SPECIALIZE decFloat :: Double -> B.Builder #-}

int2Text :: Integral a => a -> T.Text
int2Text = T.toStrict . B.toLazyText . B.decimal
