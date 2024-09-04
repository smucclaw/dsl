{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Explainable (genericMathLangEvaluator) where

import Backend.Api
import Control.Exception (try)
import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Data.Tree qualified as Tree
import Explainable (XP)
import Explainable.MathLang

genericMathLangEvaluator :: Evaluator
genericMathLangEvaluator =
  Evaluator
    { runEvaluatorForFunction = \name params -> case name of
        ComputeQualifies -> personQualifiesImpl params
        RodentsAndVermin -> rodentsAndVerminImpl params
    }

evaluator :: (MonadIO m) => MyState -> Expr Double -> ExceptT EvaluatorError m ResponseWithReason
evaluator s scenario = do
  executionResult <- liftIO $ try (xplainF () s scenario)
  case executionResult of
    Left (e :: IOError) -> do
      throwError $ InterpreterError $ Text.pack $ show e
    Right (res, xp, _, _) -> do
      pure $ ResponseWithReason (FnLitDouble res) (Reasoning $ reasoningFromXp xp)

transformParameters :: (MonadIO m) => [(Text.Text, Maybe FnLiteral)] -> ExceptT EvaluatorError m MyState
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

-- ----------------------------------------------------------------------------
-- Example Rules
-- ----------------------------------------------------------------------------

functionHandler :: (MonadIO m) => [Text.Text] -> [Maybe FnLiteral] -> Expr Double -> ExceptT EvaluatorError m ResponseWithReason
functionHandler labels arguments func
  | length labels /= length arguments =
      throwError $
        RequiredParameterMissing $
          ParameterMismatch
            { expectedParameters = length labels
            , actualParameters = length arguments
            }
  | otherwise = do
      let
        labelledArguments = zip labels arguments
      evaluatorState <- transformParameters labelledArguments
      evaluator evaluatorState func

rodentsAndVerminImpl :: (MonadIO m) => [Maybe FnLiteral] -> ExceptT EvaluatorError m ResponseWithReason
rodentsAndVerminImpl args = functionHandler argument_labels args rodentsAndVermin
 where
  argument_labels :: [Text.Text]
  argument_labels =
    [ "Loss or Damage.caused by insects"
    , "Loss or Damage.caused by birds"
    , "Loss or Damage.caused by vermin"
    , "Loss or Damage.caused by rodents"
    , "Loss or Damage.to Contents"
    , "Loss or Damage.ensuing covered loss"
    , "any other exclusion applies"
    , "a household appliance"
    , "a swimming pool"
    , "a plumbing, heating, or air conditioning system"
    ]

personQualifiesImpl :: (MonadIO m) => [Maybe FnLiteral] -> ExceptT EvaluatorError m ResponseWithReason
personQualifiesImpl args = functionHandler argument_labels args personQualifies
 where
  argument_labels :: [Text.Text]
  argument_labels =
    [ "drinks"
    , "walks"
    , "eats"
    ]

-- | Example function which computes whether a person qualifies for *something*.
personQualifies :: Expr Double
personQualifies =
  "qualifies"
    @|= MathPred
      ( getvar "walks" |&& (getvar "drinks" ||| getvar "eats")
      )

rodentsAndVermin :: Expr Double
rodentsAndVermin =
  "not covered"
    @|= ( MathITE
            (Just "Not Covered If \8230")
            ( PredFold
                Nothing
                PLAnd
                [ PredFold
                    Nothing
                    PLOr
                    [ PredVar "Loss or Damage.caused by rodents"
                    , PredVar "Loss or Damage.caused by insects"
                    , PredVar "Loss or Damage.caused by vermin"
                    , PredVar "Loss or Damage.caused by birds"
                    ]
                , PredFold
                    Nothing
                    PLAnd
                    [ PredNot
                        Nothing
                        ( PredFold
                            Nothing
                            PLOr
                            [ PredFold
                                Nothing
                                PLAnd
                                [ PredVar "Loss or Damage.to Contents"
                                , PredFold
                                    Nothing
                                    PLAnd
                                    [PredVar "Loss or Damage.caused by birds"]
                                ]
                            , PredFold
                                Nothing
                                PLAnd
                                [ PredVar "Loss or Damage.ensuing covered loss"
                                , PredFold
                                    Nothing
                                    PLAnd
                                    [ PredNot
                                        Nothing
                                        ( PredFold
                                            Nothing
                                            PLOr
                                            [ PredVar "any other exclusion applies"
                                            , PredFold
                                                Nothing
                                                PLOr
                                                [ PredVar "a household appliance"
                                                , PredVar "a swimming pool"
                                                , PredVar
                                                    "a plumbing, heating, or air conditioning system"
                                                ]
                                            ]
                                        )
                                    ]
                                ]
                            ]
                        )
                    ]
                ]
            )
            -- (MathSet "Loss or Damage" (MathVar "Not Covered"))
            --
            (Val Nothing 1.0)
            (Val Nothing 0.0)
        )
