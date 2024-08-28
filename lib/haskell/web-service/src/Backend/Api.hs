{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Api (
  module Backend.Api,
) where

import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Scientific qualified as Scientific
import Data.Text qualified as Text
import Data.Text.Read qualified as TextReader
import GHC.Generics (Generic)
import Optics.Cons
import Servant.API

data FunctionName
  = ComputeQualifies
  | RodentsAndVermin
  deriving (Show, Read, Ord, Eq)

data FnLiteral
  = FnLitInt !Integer
  | FnLitDouble !Double
  | FnLitBool !Bool
  | FnLitString !Text.Text
  deriving (Show, Read, Ord, Eq, Generic)

instance ToJSON FnLiteral where
  toJSON = \case
    FnLitInt val -> Aeson.Number $ fromIntegral val
    FnLitDouble val -> Aeson.Number $ Scientific.fromFloatDigits val
    FnLitBool val -> Aeson.String $ tshow val
    FnLitString val -> Aeson.String val
   where
    tshow :: forall a. (Show a) => a -> Text.Text
    tshow = Text.pack . show

instance FromJSON FnLiteral where
  parseJSON = \case
    Aeson.String val -> pure $ parseTextAsFnLiteral val
    Aeson.Number val
      | Just (i :: Int) <- Scientific.toBoundedInteger val -> pure $ FnLitInt $ fromIntegral i
      | Right d <- Scientific.toBoundedRealFloat val -> pure $ FnLitDouble d
      | otherwise -> Aeson.typeMismatch "Failed to parse number into bounded real or integer" (Aeson.Number val)
    obj -> Aeson.typeMismatch "Failed to parse FnLiteral" obj

data ResponseWithReason = ResponseWithReason
  { responseValue :: FnLiteral
  , responseReasoning :: Reasoning
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Wrap our reasoning into a top-level field.
data Reasoning = Reasoning
  { getReasoning :: ReasoningTree
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Basically a rose tree, but serialisable to json and specialised to our purposes.
data ReasoningTree = ReasoningTree
  { treeNode :: ReasonNode
  , treeChildren :: [ReasoningTree]
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ReasonNode = ReasonNode
  { reasoningNodeExampleCode :: [Text.Text]
  , reasoningNodeExplanation :: [Text.Text]
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | An 'EvaluatorError' is some form of panic thrown by an evaluator.
-- The execution of a function had to be interrupted for /some/ reason.
-- Such an exception is unrecoverable.
-- The error message may contain hints of what might have gone wrong.
data EvaluatorError
  = InterpreterError !Text.Text
  | RequiredParameterMissing
      { expectedParameters :: !Int
      , actualParameters :: !Int
      }
  | CannotHandleUnknownVars
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance FromHttpApiData FnLiteral where
  parseQueryParam t = Right $ parseTextAsFnLiteral t

parseTextAsFnLiteral :: Text.Text -> FnLiteral
parseTextAsFnLiteral t
  | Right (d, "") <- TextReader.signed TextReader.decimal t = FnLitInt d
  | Right (d, "") <- TextReader.double t = FnLitDouble d
  | Just b <- parseAsBool = FnLitBool b
  | Just st <- stripQuotes = FnLitString st
  | otherwise = FnLitString t
 where
  parseAsBool = case Text.toLower t of
    "true" -> Just True
    "false" -> Just False
    "yes" -> Just True
    "no" -> Just False
    _ -> Nothing

  stripQuotes = do
    ('\"', t') <- uncons t
    (t'', '\"') <- unsnoc t'
    pure t''

data Evaluator = Evaluator
  { runEvaluatorForFunction :: FunctionName -> [Maybe FnLiteral] -> ExceptT EvaluatorError IO ResponseWithReason
  }
