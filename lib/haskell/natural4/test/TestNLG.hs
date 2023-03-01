{-# LANGUAGE OverloadedStrings #-}

module TestNLG where

import Test.Hspec
import LS.NLP.NLG
import LS.NLP.NL4
import Parsing.PDPASpec (expected_pdpadbno1)
import qualified AnyAll as AA
import LS.Types
import PGF (mkCId)

spec :: Spec
spec = do
  env <- runIO $ myNLGEnv (mkCId "NL4Eng")
  describe "test rodents" $ do
    it "Should return questions about rodent damage" $ do
        let questions = linBStext env $ mkConstraintText env GqPREPOST GqCONSTR rodentsBSR
        questions `shouldBe` AA.All Nothing [AA.Any (Just (AA.Pre "Is the Loss or Damage caused by")) [AA.Leaf "rodents ?",AA.Leaf "insects ?",AA.Leaf "vermin ?",AA.Leaf "birds ?"],AA.Not (AA.Any Nothing [AA.All Nothing [AA.Leaf "is Loss or Damage to contents ?",AA.Leaf "is Loss or Damage caused by birds ?"],AA.All Nothing [AA.Leaf "is Loss or Damage ensuing loss ?",AA.Leaf "is Loss or Damage covered ?",AA.Not (AA.Any Nothing [AA.Leaf "does any other exclusion apply ?",AA.Any (Just (AA.Pre "did an animal cause water to escape from")) [AA.Leaf "a household appliance ?",AA.Leaf "a swimming pool ?",AA.Leaf "a plumbing, heating, or air conditioning system ?"]])]])]

  describe "test not bird" $ do
    it "Should return questions about not bird damage" $ do
        let questions = linBStext env $ mkConstraintText env GqPREPOST GqCONSTR notRodentsBSR
        questions `shouldBe` AA.All Nothing [AA.Any (Just (AA.Pre "Is the Loss or Damage caused by")) [AA.Leaf "rodents ?",AA.Leaf "insects ?",AA.Leaf "vermin ?",AA.Leaf "birds ?"],AA.Not (AA.Any Nothing [AA.All Nothing [AA.Leaf "is Loss or Damage to contents ?",AA.Leaf "isn't Loss or Damage caused by birds ?"],AA.All Nothing [AA.Leaf "is Loss or Damage ensuing loss ?",AA.Leaf "is Loss or Damage covered ?",AA.Not (AA.Any Nothing [AA.Leaf "does any other exclusion apply ?",AA.Any (Just (AA.Pre "did an animal cause water to escape from")) [AA.Leaf "a household appliance ?",AA.Leaf "a swimming pool ?",AA.Leaf "a plumbing, heating, or air conditioning system ?"]])]])]

  describe "test PDPA" $ do
    it "Should return questions about PDPA" $ do
      questions <- ruleQuestions env Nothing (head expected_pdpadbno1)
      questions `shouldBe` [AA.Not (AA.Leaf "is the organisation a public agency ?"),AA.Leaf "has the organisation become aware that a data breach may have occurred ?",AA.Leaf "does the data breach occur on or after 1 Feb 2022 ?"]

rodentsBSR :: BoolStructR
rodentsBSR = AA.All Nothing
  [ AA.Any
      ( Just
          ( AA.Pre "Loss or Damage caused by" )
      )
      [ AA.Leaf
          ( RPMT
              [ MTT "rodents" ]
          )
      , AA.Leaf
          ( RPMT
              [ MTT "insects" ]
          )
      , AA.Leaf
          ( RPMT
              [ MTT "vermin" ]
          )
      , AA.Leaf
          ( RPMT
              [ MTT "birds" ]
          )
      ]
  , AA.Not
      ( AA.Any Nothing
          [ AA.All Nothing
              [ AA.Leaf
                  ( RPConstraint
                      [ MTT "Loss or Damage" ] RPis
                      [ MTT "to Contents" ]
                  )
              , AA.Leaf
                  ( RPConstraint
                      [ MTT "Loss or Damage" ] RPis
                      [ MTT "caused by"
                      , MTT "birds"
                      ]
                  )
              ]
          , AA.All Nothing
              [ AA.Leaf
                  ( RPConstraint
                      [ MTT "Loss or Damage" ] RPis
                      [ MTT "ensuing loss" ]
                  )
              , AA.Leaf
                  ( RPConstraint
                      [ MTT "Loss or Damage" ] RPis
                      [ MTT "Covered" ]
                  )
              , AA.Not
                  ( AA.Any Nothing
                      [ AA.Leaf
                          ( RPMT
                              [ MTT "any other exclusion applies" ]
                          )
                      , AA.Any
                          ( Just
                              ( AA.Pre "an animal caused water to escape from" )
                          )
                          [ AA.Leaf
                              ( RPMT
                                  [ MTT "a household appliance" ]
                              )
                          , AA.Leaf
                              ( RPMT
                                  [ MTT "a swimming pool" ]
                              )
                          , AA.Leaf
                              ( RPMT
                                  [ MTT "a plumbing, heating, or air conditioning system" ]
                              )
                          ]
                      ]
                  )
              ]
          ]
      )
  ]



notRodentsBSR :: BoolStructR
notRodentsBSR = AA.All Nothing
  [ AA.Any
      ( Just
          ( AA.Pre "Loss or Damage caused by" )
      )
      [ AA.Leaf
          ( RPMT
              [ MTT "rodents" ]
          )
      , AA.Leaf
          ( RPMT
              [ MTT "insects" ]
          )
      , AA.Leaf
          ( RPMT
              [ MTT "vermin" ]
          )
      , AA.Leaf
          ( RPMT
              [ MTT "birds" ]
          )
      ]
  , AA.Not
      ( AA.Any Nothing
          [ AA.All Nothing
              [ AA.Leaf
                  ( RPConstraint
                      [ MTT "Loss or Damage" ] RPis
                      [ MTT "to Contents" ]
                  )
              , AA.Leaf
                  ( RPBoolStructR
                      [ MTT "Loss or Damage" ] RPis
                      (AA.Not
                        (AA.Leaf
                          (RPMT
                            [ MTT "caused by"
                            , MTT "birds"
                            ])
                        )
                      )
                  )
                ]
          , AA.All Nothing
              [ AA.Leaf
                  ( RPConstraint
                      [ MTT "Loss or Damage" ] RPis
                      [ MTT "ensuing loss" ]
                  )
              , AA.Leaf
                  ( RPConstraint
                      [ MTT "Loss or Damage" ] RPis
                      [ MTT "Covered" ]
                  )
              , AA.Not
                  ( AA.Any Nothing
                      [ AA.Leaf
                          ( RPMT
                              [ MTT "any other exclusion applies" ]
                          )
                      , AA.Any
                          ( Just
                              ( AA.Pre "an animal caused water to escape from" )
                          )
                          [ AA.Leaf
                              ( RPMT
                                  [ MTT "a household appliance" ]
                              )
                          , AA.Leaf
                              ( RPMT
                                  [ MTT "a swimming pool" ]
                              )
                          , AA.Leaf
                              ( RPMT
                                  [ MTT "a plumbing, heating, or air conditioning system" ]
                              )
                          ]
                      ]
                  )
              ]
          ]
      )
  ]
