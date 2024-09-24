{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Backend.Simala (simalaEvaluator) where

import Backend.Api (
  Evaluator (..),
  EvaluatorError (InterpreterError, UnknownArguments),
  FnLiteral (..),
  FunctionDeclaration (parametersLongNames, parametersMapping),
  ReasonNode (
    ReasonNode,
    reasoningNodeExampleCode,
    reasoningNodeExplanation
  ),
  Reasoning (Reasoning),
  ReasoningTree (..),
  ResponseWithReason (..),
 )
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Simala.Eval.Monad qualified as Simala
import Simala.Eval.Type qualified as Simala
import Simala.Expr.Evaluator qualified as Simala
import Simala.Expr.Parser qualified as Simala
import Simala.Expr.Render qualified as Simala
import Simala.Expr.Type as Simala
import qualified Data.List as List
import Control.Applicative (asum)
import qualified Data.Maybe as Maybe

simalaEvaluator ::
  (Monad m) =>
  FunctionDeclaration ->
  Text ->
  ExceptT EvaluatorError m Evaluator
simalaEvaluator fnDecl fnImpl =
  case Simala.parseDecls "" fnImpl of
    Left err -> throwError $ InterpreterError $ "Failed to parse Simala program: " <> Text.pack err
    Right expr ->
      pure $
        Evaluator
          { runEvaluatorForFunction = functionHandler fnDecl expr
          }

functionHandler ::
  FunctionDeclaration ->
  [Decl] ->
  [(Text, Maybe FnLiteral)] ->
  Maybe (Set Text) ->
  ExceptT EvaluatorError IO ResponseWithReason
functionHandler decl impl args outputs = do
  input <- transformParameters decl args
  evaluator input outputs impl

evaluator ::
  (MonadIO m) =>
  Row Expr ->
  Maybe (Set Text) ->
  [Decl] ->
  ExceptT EvaluatorError m ResponseWithReason
evaluator inputs outputVars decls = do
  let
    rulesName = asum $ fmap (\case
        NonRec _ name _
          | Text.isPrefixOf "rules" name -> Just name
        Rec _ name _
          | Text.isPrefixOf "rules" name -> Just name
        _ -> Nothing) decls

  case rulesName of
    Nothing -> throwError $ InterpreterError $ "No \"rules\" function found"
    Just ruleName -> do
      let
        evalCall = Eval $ App (Var ruleName) [Record inputs]
        declsWithInput = decls <> [evalCall]
        (result, evalTrace) = Simala.runEval' (Simala.evalDecls declsWithInput)
      case result of
        Left err -> throwError $ InterpreterError $ "Failed to evaluate expression: " <> Simala.render err
        Right () -> do
          case evalTrace of
            [(Right (VRecord outputs), trace)] -> do
              outputsFn' <- traverse (\(k, v) -> fmap (k,) (simalaValToFnLiteral v)) outputs
              -- Only keep the fields in the output that were actually requested.
              -- If nothing was explicitly requested, we keep all outputs.
              let outputsFn = maybe outputsFn' (\keys -> filter (\(k, _) -> Set.member k keys) outputsFn') outputVars
              pure $
                ResponseWithReason
                  { responseValue = outputsFn
                  , responseReasoning = Reasoning $ reasoningFromEvalTrace trace
                  }
            (Left err, trace):_ -> do
              liftIO $ Text.putStrLn $ Simala.renderFullTrace trace
              throwError $ InterpreterError $ "Unexpected output format: " <> Simala.render err
            _ -> throwError $ InterpreterError $ "Unexpected output format"

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
  renderResult :: Maybe Name -> Either Simala.EvalError Val -> Text
  renderResult (Just n) (Right x) = Simala.render n <> " = " <> Simala.render x
  renderResult (Just n) (Left x) = Simala.render n <> " aborted with " <> Simala.render x
  renderResult Nothing (Right x) = Simala.render x
  renderResult Nothing (Left x) = Simala.render x

transformParameters :: (MonadIO m) => FunctionDeclaration -> [(Text, Maybe FnLiteral)] -> ExceptT EvaluatorError m (Row Expr)
transformParameters decl attrs = do
  let
    splitParameters key mValue = do
      -- We support long and short names for the same parameter name.
      -- This makes us compatible with OIA.
      keyName <- case key `Set.member` decl.parametersLongNames of
        False -> case key `Map.lookup` decl.parametersMapping of
          Nothing -> throwError $ UnknownArguments [key]
          Just longName -> pure longName
        True -> pure key
      val <- case mValue of
        Nothing ->
          -- null is resolved to 'uncertain per OIA convention
          pure uncertain
        Just arg -> do
          fnLiteralToSimalaVar arg

      pure (keyName, val)

  env <- traverse (\(k, v) -> splitParameters k v) attrs
  -- Unknown parameters are added to the input as 'uncertain
  let
    parametersNotGiven = Set.toList $ Set.difference decl.parametersLongNames (Set.fromList $ fmap fst env)

  let
    allInputs =
      env <> fmap (,uncertain) parametersNotGiven
  pure allInputs
 where
  uncertain = Atom "uncertain"

fnLiteralToSimalaVar :: (MonadIO m) => FnLiteral -> ExceptT EvaluatorError m Expr
fnLiteralToSimalaVar = \case
  FnLitInt integer -> pure $ Lit $ FracLit $ fromIntegral integer
  FnLitDouble d -> pure $ Lit $ FracLit d
  FnLitBool b -> pure $ Lit $ BoolLit b
  FnLitString atom -> pure $ Atom atom

simalaValToFnLiteral :: (MonadIO m) => Val -> ExceptT EvaluatorError m FnLiteral
simalaValToFnLiteral = \case
  Simala.VInt integer -> pure $ FnLitInt $ fromIntegral integer
  Simala.VBool b -> pure $ FnLitBool b
  Simala.VAtom atom -> pure $ FnLitString atom
  Simala.VFrac f -> pure $ FnLitDouble f
  val -> throwError $ InterpreterError $ "Cannot translate " <> Simala.render val
