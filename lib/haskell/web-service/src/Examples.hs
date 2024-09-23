{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples (functionSpecs) where

import Backend.Explainable
import Backend.Simala qualified as Simala
import Control.Monad.Trans.Except
import Data.Map.Strict qualified as Map
import Data.String.Interpolate
import Data.Text (Text)
import Explainable.MathLang
import Explainable.MathLang qualified as Gml
import Server

-- ----------------------------------------------------------------------------
-- Example data
-- ----------------------------------------------------------------------------

functionSpecs :: Map.Map Text ValidatedFunction
functionSpecs =
  Map.fromList
    [ (f.fnImpl.name, f)
    | f <-
        [ builtinProgram personQualifiesFunction
        , builtinProgram rodentsAndVerminFunction
        ]
    ]

-- | Metadata about the function that the user might want to know.
-- Further, an LLM could use this info to ask specific questions to the user.
personQualifiesFunction :: Except EvaluatorError ValidatedFunction
personQualifiesFunction = do
  let
    fnDecl =
      Function
        { name = "compute_qualifies"
        , description =
            [__i|Determines if a person qualifies for the purposes of the rule.
                  The input object describes the person's properties in the primary parameters: walks, eats, drinks.
                  Secondary parameters can be given which are sufficient to determine some of the primary parameters.
                  A person drinks whether or not they consume an alcoholic or a non-alcoholic beverage, in part or in whole;
                  those specific details don't really matter.
                  The output of the function can be either a request for required information;
                  a restatement of the user input requesting confirmation prior to function calling;
                  or a Boolean answer with optional explanation summary.
                |]
        , parameters =
            Parameters $
              Map.fromList
                [ ("walks", Parameter "string"  Nothing ["true", "false"] "Did the person walk?")
                , ("eats", Parameter "string"   Nothing ["true", "false"] "Did the person eat?")
                , ("drinks", Parameter "string" Nothing ["true", "false"] "Did the person drink?")
                ]
        , supportedEvalBackend = [GenericMathLang, Simala]
        }
  simalaEval <- Simala.simalaEvaluator (toDecl fnDecl) computeQualifiesSimala
  pure $
    ValidatedFunction
      { fnImpl = fnDecl
      , fnEvaluator =
          Map.fromList
            [
              ( Simala
              , simalaEval
              )
            ,
              ( GenericMathLang
              , genericMathLangEvaluator (toDecl fnDecl) personQualifiesGml
              )
            ]
      }

-- | Metadata about the function that the user might want to know.
-- Further, an LLM could use this info to ask specific questions to the user.
rodentsAndVerminFunction :: Except EvaluatorError ValidatedFunction
rodentsAndVerminFunction = do
  let
    fnDecl =
      Function
        { name = "vermin_and_rodent"
        , description =
            [__i|We do not cover any loss or damage caused by rodents, insects, vermin, or birds.
                  However, this exclusion does not apply to:
                  a) loss or damage to your contents caused by birds; or
                  b) ensuing covered loss unless any other exclusion applies or where an animal causes water to escape from
                    a household appliance, swimming pool or plumbing, heating or air conditioning system
                  |]
        , parameters =
            Parameters $
              Map.fromList
                [ ("Loss or Damage.caused by insects", Parameter "string" Nothing ["true", "false"]  "Was the damage caused by insects?")
                , ("Loss or Damage.caused by birds", Parameter "string" Nothing ["true", "false"]     "Was the damage caused by birds?")
                , ("Loss or Damage.caused by vermin", Parameter "string" Nothing ["true", "false"]    "Was the damage caused by vermin?")
                , ("Loss or Damage.caused by rodents", Parameter "string" Nothing ["true", "false"]   "Was the damage caused by rodents?")
                , ("Loss or Damage.to Contents", Parameter "string" Nothing ["true", "false"] "Is the damage to your contents?")
                , ("Loss or Damage.ensuing covered loss", Parameter "string" Nothing ["true", "false"] "Is the damage ensuing covered loss")
                , ("any other exclusion applies", Parameter "string" Nothing ["true", "false"] "Are any other exclusions besides mentioned ones?")
                , ("a household appliance", Parameter "string" Nothing ["true", "false"] "Did water escape from a household appliance due to an animal?")
                , ("a swimming pool", Parameter "string" Nothing ["true", "false"] "Did water escape from a swimming pool due to an animal?")
                , ("a plumbing, heating, or air conditioning system", Parameter "string" Nothing ["true", "false"] "Did water escape from a plumbing, heating or conditioning system due to an animal?")
                ]
        , supportedEvalBackend = [GenericMathLang, Simala]
        }
  simalaEval <- Simala.simalaEvaluator (toDecl fnDecl) rodentsAndVerminSimala
  pure $
    ValidatedFunction
      { fnImpl = fnDecl
      , fnEvaluator =
          Map.fromList
            [
              ( Simala
              , simalaEval
              )
            ,
              ( GenericMathLang
              , genericMathLangEvaluator (toDecl fnDecl) rodentsAndVerminGml
              )
            ]
      }

computeQualifiesSimala :: Text
computeQualifiesSimala =
  [i|
  rules = fun(i) =>
    { qualifies = i.walks && (i.drinks || i.eats)
    }
|]

rodentsAndVerminSimala :: Text
rodentsAndVerminSimala =
  [i|
  rules = fun(i) =>
    let
      notCoveredIf = fun (b) => if b then true else false ;

      lossOrDamagedByAnimals =
           i.`Loss or Damage.caused by rodents`
        || i.`Loss or Damage.caused by insects`
        || i.`Loss or Damage.caused by vermin`
        || i.`Loss or Damage.caused by birds` ;

      damageToContentsAndCausedByBirds =
           i.`Loss or Damage.to Contents`
        && i.`Loss or Damage.caused by birds` ;

      ensuingCoveredLoss = i.`Loss or Damage.ensuing covered loss` ;

      exclusionsApply =
           i.`any other exclusion applies`
        || i.`a household appliance`
        || i.`a swimming pool`
        || i.`a plumbing, heating, or air conditioning system` ;

    in
      { covered = notCoveredIf
        ( lossOrDamagedByAnimals
          && not
          (  damageToContentsAndCausedByBirds
          || (  ensuingCoveredLoss
              && not ( exclusionsApply )
              )
          )
        )
      }
|]

-- | Example function which computes whether a person qualifies for *something*.
personQualifiesGml :: Gml.Expr Double
personQualifiesGml =
  "qualifies"
    @|= MathPred
      ( getvar "walks" |&& (getvar "drinks" ||| getvar "eats")
      )

rodentsAndVerminGml :: Gml.Expr Double
rodentsAndVerminGml =
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

builtinProgram :: Except EvaluatorError a -> a
builtinProgram m = case runExcept m of
  Left err -> error $ "Builtin failed to load " <> show err
  Right e -> e
