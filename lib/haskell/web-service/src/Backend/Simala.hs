{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Backend.Simala (createSimalaFunction) where

import Control.Applicative (asum)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Backend.Api
import Simala.Eval.Monad qualified as Simala
import Simala.Eval.Type qualified as Simala
import Simala.Expr.Evaluator qualified as Simala
import Simala.Expr.Parser qualified as Simala
import Simala.Expr.Render qualified as Simala
import Simala.Expr.Type as Simala
import Data.Tuple.Extra (secondM)

createSimalaFunction ::
  (Monad m) =>
  FunctionDeclaration ->
  Text ->
  ExceptT EvaluatorError m RunFunction
createSimalaFunction fnDecl fnImpl =
  case Simala.parseDecls "" fnImpl of
    Left err -> throwError $ InterpreterError $ "Failed to parse Simala program: " <> Text.pack err
    Right expr ->
      pure $
        RunFunction
          { runFunction = runSimalaFunction fnDecl expr
          }

runSimalaFunction ::
  FunctionDeclaration ->
  [Decl] ->
  [(Text, Maybe FnLiteral)] ->
  Maybe (Set Text) ->
  ExceptT EvaluatorError IO ResponseWithReason
runSimalaFunction decl impl args outputFilter = do
  input <- transformParameters decl args
  evalSimala input outputFilter impl

-- | Evaluate the simala program using the given parameters.
--
-- We assume each program defines a function named 'rules' which we think as the
-- the main entry point of a program.
-- We additionally allow a function named 'rules*' (e.g. 'rules_1234') to be the
-- main entry point to simala.
evalSimala ::
  (Monad m) =>
  -- | A record of inputs which are assumed to be validated and complete.
  -- Complete means that each parameter specified in a 'FunctionDeclaration'
  -- is included in this row.
  Row Expr ->
  -- | Optional output variable filter. If 'Nothing', no filter is applied.
  Maybe (Set Text) ->
  -- | The parsed program to evaluate.
  [Decl] ->
  ExceptT EvaluatorError m ResponseWithReason
evalSimala inputs outputFilter decls = do
  let
    isRulesName = Text.isPrefixOf "rules"

    mMainRuleName =
      asum $
        fmap
          ( \case
              NonRec _ name _
                | isRulesName name -> Just name
              Rec _ name _
                | isRulesName name -> Just name
              _ -> Nothing
          )
          decls

  case mMainRuleName of
    Nothing -> throwError $ InterpreterError $ "No \"rules\" main function found"
    Just mainRuleName -> do
      let
        -- To get anything evaluated, we programmatically add a so-called '#eval'
        -- declaration.
        -- In simala, this looks like '#eval rules({...})'.
        -- We expect each program has exactly one such '#eval' declaration.
        -- TODO: As we have parsed the program, we could reject a program if it contains a '#eval' decl.
        evalCall = Eval $ App (Var mainRuleName) [Record inputs]
        declsWithInput = decls <> [evalCall]
        (result, evalTrace) = Simala.runEval' (Simala.evalDecls declsWithInput)
      case result of
        Left err -> throwError $ InterpreterError $ "Failed to evaluate expression: " <> Simala.render err
        Right () -> do
          case evalTrace of
            [(Right (VRecord outputRecord), trace)] -> do
              -- Only keep the fields in the output that were actually requested.
              -- If nothing was explicitly requested, we keep all outputs.
              let
                filteredOutputs = maybe outputRecord (\keys -> filter (\(k, _) -> Set.member k keys) outputRecord) outputFilter

              transformedOutputs <- traverse (\(k, v) -> fmap (k,) (simalaValToFnLiteral v)) filteredOutputs
              pure $
                ResponseWithReason
                  { responseValue = transformedOutputs
                  , responseReasoning = Reasoning $ reasoningFromEvalTrace trace
                  }
            (Left err, _trace) : _ -> do
              -- TODO: add logging, this is a program that crashed during evaluation
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

-- | Given a list of parameters with values, transform them to the 'simala'
-- equivalent if possible.
-- We map short names to their long forms, as we expect the program to
-- exclusively use the long form.
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
          -- null is resolved to 'unknown per OIA convention
          pure unknown
        Just arg -> do
          fnLiteralToSimalaVar arg

      pure (keyName, val)

  env <- traverse (\(k, v) -> splitParameters k v) attrs
  -- Unknown parameters are added to the input as 'uncertain
  let
    parametersNotGiven = Set.toList $ Set.difference decl.parametersLongNames (Set.fromList $ fmap fst env)

  let
    allInputs =
      env <> fmap (,unknown) parametersNotGiven
  pure allInputs

fnLiteralToSimalaVar :: (MonadIO m) => FnLiteral -> ExceptT EvaluatorError m Expr
fnLiteralToSimalaVar = \case
  FnLitInt integer -> pure $ Lit $ FracLit $ fromIntegral integer
  FnLitDouble d -> pure $ Lit $ FracLit d
  FnLitBool b -> pure $ Lit $ BoolLit b
  FnLitString atom -> pure $ Atom atom
  FnArray elems -> do
    simalaExprs <- traverse fnLiteralToSimalaVar elems
    pure $ List simalaExprs
  FnObject ps -> do
    simalaExprs <- traverse (secondM fnLiteralToSimalaVar) ps
    pure $ Record simalaExprs
  FnUncertain -> pure uncertain
  FnUnknown -> pure unknown

simalaValToFnLiteral :: (Monad m) => Val -> ExceptT EvaluatorError m FnLiteral
simalaValToFnLiteral = \case
  VInt integer -> pure $ FnLitInt $ fromIntegral integer
  VBool b -> pure $ FnLitBool b
  VAtom atom
    | uncertainName == atom -> pure FnUncertain
    | unknownName == atom -> pure FnUnknown
    | otherwise -> pure $ FnLitString atom
  VFrac f -> pure $ FnLitDouble f
  VList vals -> do
    fnVals <- traverse simalaValToFnLiteral vals
    pure $ FnArray fnVals
  VRecord ps -> do
    fnPairs <- traverse (secondM simalaValToFnLiteral) ps
    pure $ FnObject fnPairs
  val -> throwError $ InterpreterError $ "Cannot translate \"" <> Simala.render val <> "\""

-- ----------------------------------------------------------------------------
-- Constants for function evaluation
-- ----------------------------------------------------------------------------

uncertain :: Expr
uncertain = Atom uncertainName

uncertainName :: Name
uncertainName = "uncertain"

unknown :: Expr
unknown = Atom unknownName

unknownName :: Name
unknownName = "uncertain"
