{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Backend.Error where

import Data.Aeson
import Data.Text qualified as Text
import GHC.Generics (Generic)

-- | An 'EvaluatorError' is some form of panic thrown by an evaluator.
-- The execution of a function had to be interrupted for /some/ reason.
-- Such an exception is unrecoverable.
-- The error message may contain hints of what might have gone wrong.
newtype EvaluatorError = EvaluatorError
  { getEvaluatorError :: Text.Text
  -- ^ Error message of a fatal evaluation error.
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
