{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Api (
  module Backend.Api,
) where

import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types as Aeson
import Data.Bifunctor (bimap)
import Data.Foldable qualified as Foldable
import Data.Map.Strict (Map)
import Data.Scientific qualified as Scientific
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as TextReader
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Optics.Cons
import Servant.API

type FunctionName = Text

data FnLiteral
  = FnLitInt !Integer
  | FnLitDouble !Double
  | FnLitBool !Bool
  | FnLitString !Text
  | FnArray [FnLiteral]
  | FnObject [(Text, FnLiteral)]
  | FnUncertain
  | FnUnknown
  deriving (Show, Read, Ord, Eq, Generic)

instance ToJSON FnLiteral where
  toJSON = \case
    FnLitInt val -> String $ tshow val
    FnLitDouble val -> String $ tshow val
    FnLitBool val -> String $ tshow val
    FnLitString val -> String val
    FnArray vals -> Array $ V.fromList $ fmap toJSON vals
    FnObject ps -> Object $ Aeson.fromList $ fmap (bimap Aeson.fromText toJSON) ps
    FnUncertain -> Object $ Aeson.fromList []
    FnUnknown -> Null
   where
    tshow :: forall a. (Show a) => a -> Text
    tshow = Text.pack . show

instance FromJSON FnLiteral where
  parseJSON = \case
    String val -> pure $ parseTextAsFnLiteral val
    Bool val -> pure $ FnLitBool val
    Number val
      | Just (i :: Int) <- Scientific.toBoundedInteger val -> pure $ FnLitInt $ fromIntegral i
      | Right d <- Scientific.toBoundedRealFloat val -> pure $ FnLitDouble d
      | otherwise -> Aeson.typeMismatch "Failed to parse number into bounded real or integer" (Number val)
    Null -> pure FnUnknown
    Array vals -> FnArray <$> traverse parseJSON (Foldable.toList vals)
    Object o
      | [] <- Aeson.toList o -> pure FnUncertain
      | otherwise -> do
          ps <- traverse (\(k, v) -> fmap (Aeson.toText k,) (parseJSON v)) (Aeson.toList o)
          pure $ FnObject ps

data RunFunction = RunFunction
  { runFunction ::
      -- \^ Parameters to the function
      [(Text, Maybe FnLiteral)] ->
      -- \^ Output filter, as the function may return a record of
      -- outputs.
      -- If this filter is 'Nothing', we do not filter anything.
      Maybe (Set Text) ->
      ExceptT EvaluatorError IO ResponseWithReason
  -- ^ Run a function with parameters
  }

data FunctionDeclaration = FunctionDeclaration
  { name :: !Text
  , description :: !Text
  , parametersLongNames :: !(Set Text)
  , parametersMapping :: !(Map Text Text)
  }

data ResponseWithReason = ResponseWithReason
  { responseValue :: [(Text, FnLiteral)]
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

emptyTree :: Reasoning
emptyTree =
  Reasoning
    { getReasoning =
        ReasoningTree
          { treeNode =
              ReasonNode
                { reasoningNodeExampleCode = []
                , reasoningNodeExplanation = []
                }
          , treeChildren = []
          }
    }

-- | Basically a rose tree, but serialisable to json and specialised to our purposes.
data ReasoningTree = ReasoningTree
  { treeNode :: ReasonNode
  , treeChildren :: [ReasoningTree]
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ReasonNode = ReasonNode
  { reasoningNodeExampleCode :: [Text]
  , reasoningNodeExplanation :: [Text]
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | An 'EvaluatorError' is some form of panic thrown by an evaluator.
-- The execution of a function had to be interrupted for /some/ reason.
-- Such an exception is unrecoverable.
-- The error message may contain hints of what might have gone wrong.
data EvaluatorError
  = InterpreterError !Text
  | RequiredParameterMissing !ParameterMismatch
  | UnknownArguments ![Text]
  | CannotHandleUnknownVars
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ParameterMismatch = ParameterMismatch
  { expectedParameters :: !Int
  , actualParameters :: !Int
  }
  deriving stock (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance FromHttpApiData FnLiteral where
  parseQueryParam t = Right $ parseTextAsFnLiteral t

parseTextAsFnLiteral :: Text -> FnLiteral
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
