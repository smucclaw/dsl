{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Explainable (genericMathLangEvaluator) where

import Backend.Api
import Control.Exception (try)
import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as HashMap
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tree qualified as Tree
import Explainable (XP)
import Explainable.MathLang

genericMathLangEvaluator :: FunctionDeclaration -> Expr Double -> Evaluator
genericMathLangEvaluator fnDecl expr =
  Evaluator
    { runEvaluatorForFunction = functionHandler fnDecl expr
    }

functionHandler :: (MonadIO m) => FunctionDeclaration -> Expr Double -> [(Text, Maybe FnLiteral)] -> ExceptT EvaluatorError m ResponseWithReason
functionHandler decl impl args
  | length decl.parameters /= length args =
      throwError $
        RequiredParameterMissing $
          ParameterMismatch
            { expectedParameters = length decl.parameters
            , actualParameters = length args
            }
  | unknowns@(_ : _) <- filter (\(k, _) -> Set.notMember k decl.parameters) args =
      throwError $
        UnknownArguments $
          fmap fst unknowns
  | otherwise = do
      evaluatorState <- transformParameters args
      runExplainableInterpreter evaluatorState impl

runExplainableInterpreter :: (MonadIO m) => MyState -> Expr Double -> ExceptT EvaluatorError m ResponseWithReason
runExplainableInterpreter s scenario = do
  executionResult <- liftIO $ try (xplainF () s scenario)
  case executionResult of
    Left (e :: IOError) -> do
      throwError $ InterpreterError $ Text.pack $ show e
    Right (res, xp, _, _) -> do
      pure $ ResponseWithReason (FnLitDouble res) (Reasoning $ reasoningFromXp xp)

transformParameters :: (MonadIO m) => [(Text, Maybe FnLiteral)] -> ExceptT EvaluatorError m MyState
transformParameters attrs = do
  let
    explainableState = emptyState

    splitParameters _ Nothing _ = throwError $ CannotHandleUnknownVars
    splitParameters key (Just arg) state = case arg of
      FnLitInt integer ->
        pure $
          state
            { symtabF = HashMap.insert (Text.unpack key) (Val Nothing $ fromIntegral integer) (symtabF state)
            }
      FnLitDouble d ->
        pure $
          state
            { symtabF = HashMap.insert (Text.unpack key) (Val Nothing d) (symtabF state)
            }
      FnLitBool b ->
        pure $
          state
            { symtabP = HashMap.insert (Text.unpack key) (PredVal Nothing b) (symtabP state)
            }
      FnLitString t ->
        pure $
          state
            { symtabS = HashMap.insert (Text.unpack key) (Text.unpack t) (symtabS state)
            }
  foldM (\s (k, v) -> splitParameters k v s) explainableState attrs

-- | Translate a Tree of explanations into a reasoning tree that can be sent over
-- the wire.
-- For now, this is essentially just a 1:1 translation, but might prune the tree in the future.
reasoningFromXp :: XP -> ReasoningTree
reasoningFromXp (Tree.Node (xpExampleCode, xpJustification) children) =
  ReasoningTree
    (ReasonNode (fmap Text.pack xpExampleCode) (fmap Text.pack xpJustification))
    (fmap reasoningFromXp children)
