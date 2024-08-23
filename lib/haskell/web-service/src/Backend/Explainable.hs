{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Backend.Explainable where

import qualified Data.Text as Text
import Data.Aeson
import GHC.Generics (Generic)

{- | A MathLangException is some form of panic thrown by 'MathLang'.
The execution of a function had to be interrupted for /some/ reason.
Such an exception is unrecoverable.
The error message may contain hints of what might have gone wrong.
-}
newtype MathLangException = MathLangException
  { getMathLangException :: Text.Text
  -- ^ Error message of a fatal math lang execution exception.
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
