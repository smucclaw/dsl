{-# LANGUAGE OverloadedStrings #-}

module LS.RelationalPredicates where

import qualified Data.Set           as Set
import qualified Data.Text.Lazy as Text
import Text.Megaparsec
import Control.Monad.Reader (asks, local)
import Control.Monad.Writer.Lazy

import LS.Types
import LS.Tokens
import LS.ParamText

pRelationalPredicate :: Parser RelationalPredicate
pRelationalPredicate = debugName "pRelationalPredicate" $ do
  try pConstraint <|> try (RPParamText <$> pParamText)

pIsRelation :: Parser RelationalPredicate
pIsRelation = pToken Is *> pConstraint

pConstraint :: Parser RelationalPredicate
pConstraint = debugName "pConstraint" $ do
  RPConstraint
    <$> pMultiTerm
    <*> tok2rel
    <*> pMultiTerm

-- can we rephrase this as Either or Maybe so we only accept certain tokens as RPRels?
tok2rel :: Parser RPRel
tok2rel = choice
    [ RPis      <$ pToken Is      
    , RPeq      <$ pToken TokEQ   
    , RPlt      <$ pToken TokLT   
    , RPlte     <$ pToken TokLTE  
    , RPgt      <$ pToken TokGT   
    , RPgte     <$ pToken TokGTE  
    , RPelem    <$ pToken TokIn   
    , RPnotElem <$ pToken TokNotIn
    ]

