{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Backend.Simala (simalaEvaluator) where

import Backend.Api
import Control.Monad (foldM)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Simala.Eval.Monad qualified as Simala
import Simala.Eval.Type qualified as Simala
import Simala.Expr.Evaluator qualified as Simala
import Simala.Expr.Parser qualified as Simala
import Simala.Expr.Render qualified as Simala
import Simala.Expr.Type qualified as Simala

simalaEvaluator ::
  (Monad m) =>
  FunctionDeclaration ->
  Text ->
  ExceptT EvaluatorError m Evaluator
simalaEvaluator fnDecl fnImpl =
  case Simala.parseExpr "" fnImpl of
    Left err -> throwError $ InterpreterError $ "Failed to parse Simala program: " <> Text.pack err
    Right expr ->
      pure $
        Evaluator
          { runEvaluatorForFunction = functionHandler fnDecl expr
          }

functionHandler ::
  FunctionDeclaration ->
  Simala.Expr ->
  [(Text, Maybe FnLiteral)] ->
  ExceptT EvaluatorError IO ResponseWithReason
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
      evaluator evaluatorState impl

evaluator :: (MonadIO m) => Simala.Env -> Simala.Expr -> ExceptT EvaluatorError m ResponseWithReason
evaluator env expr = do
  let
    (result, evalTrace) = Simala.runEval (Simala.withEnv env (Simala.eval expr))
  case result of
    Left err -> throwError $ InterpreterError $ "Failed to evaluate expression: " <> Simala.render err
    Right val -> do
      r <- simalaValToFnLiteral val
      pure $
        ResponseWithReason
          { responseValue = r
          , responseReasoning = Reasoning $ reasoningFromEvalTrace evalTrace
          }

reasoningFromEvalTrace :: Simala.EvalTrace -> ReasoningTree
reasoningFromEvalTrace = go
 where
  go (Simala.Trace (Just n) e subs v) =
    ReasoningTree
      { treeNode =
          ReasonNode
            { reasoningNodeExampleCode = [Simala.render n <> " = " <> Simala.render e]
            , reasoningNodeExplanation =
                [ renderResult (Just n) v
                ]
            }
      , treeChildren = map go subs
      }
  go (Simala.Trace Nothing e subs v) =
    ReasoningTree
      { treeNode =
          ReasonNode
            { reasoningNodeExampleCode = [Simala.render e]
            , reasoningNodeExplanation =
                [ renderResult Nothing v
                ]
            }
      , treeChildren = map go subs
      }
  renderResult :: Maybe Simala.Name -> Either Simala.EvalError Simala.Val -> Text
  renderResult (Just n) (Right x) = (Simala.render n <> " = " <> Simala.render x)
  renderResult (Just n) (Left x) = (Simala.render n <> " aborted with " <> Simala.render x)
  renderResult Nothing (Right x) = (Simala.render x)
  renderResult Nothing (Left x) = (Simala.render x)

transformParameters :: (MonadIO m) => [(Text, Maybe FnLiteral)] -> ExceptT EvaluatorError m Simala.Env
transformParameters attrs = do
  let
    initialState = Map.empty

    splitParameters _ Nothing _ = throwError $ CannotHandleUnknownVars
    splitParameters key (Just arg) env = do
      simalaVal <- fnLiteralToSimalaVar arg
      pure $ Map.insert key simalaVal env
  foldM (\s (k, v) -> splitParameters k v s) initialState attrs

fnLiteralToSimalaVar :: (MonadIO m) => FnLiteral -> ExceptT EvaluatorError m Simala.Val
fnLiteralToSimalaVar = \case
  FnLitInt integer -> pure $ Simala.VInt $ fromIntegral integer
  FnLitDouble _ -> throwError undefined
  FnLitBool b -> pure $ Simala.VBool b
  FnLitString atom -> pure $ Simala.VAtom atom

simalaValToFnLiteral :: (MonadIO m) => Simala.Val -> ExceptT EvaluatorError m FnLiteral
simalaValToFnLiteral = \case
  Simala.VInt integer -> pure $ FnLitInt $ fromIntegral integer
  Simala.VBool b -> pure $ FnLitBool b
  Simala.VAtom atom -> pure $ FnLitString atom
  val -> throwError $ InterpreterError $ "Cannot translate " <> Simala.render val
