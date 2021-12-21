{-# LANGUAGE OverloadedStrings #-}

module LS.ParamText where

import qualified Data.Text.Lazy as Text
import Text.Megaparsec

import LS.Types
import LS.Tokens
import Data.List.NonEmpty

pParamText :: Parser ParamText
pParamText = debugName "pParamText" $ do
  (:|) <$> (pKeyValues <?> "paramText head") `indented0` (optional dnl *> pParams)


  -- === flex for
  --     (myhead, therest) <- (pKeyValues <* dnl) `indented0` pParams
  --     return $ myhead :| therest

type KVsPair = (NonEmpty Text.Text, Maybe TypeSig) -- so really there are multiple Values

pParams :: Parser [KVsPair]
pParams = many $ pKeyValues <* dnl    -- head (name+,)*

pKeyValues :: Parser KVsPair
pKeyValues = debugName "pKeyValues" $
  (,) <$> ((:|) <$> pAnyText `indented1` many pAnyText)
      <*> optional pTypeSig

pTypeSig :: Parser TypeSig
pTypeSig = debugName "pTypeSig" $ do
  _           <- pToken TypeSeparator <|> pToken Is
  simpletype <|> inlineenum
  where
    simpletype = do
      cardinality <- choice [ TOne      <$ pToken One
                            , TOne      <$ pToken A_An
                            , TOptional <$ pToken Optional
                            , TList0    <$ pToken List0
                            , TList1    <$ pToken List1 ]
      base        <- pOtherVal
      return $ SimpleType cardinality base
    inlineenum = do
      InlineEnum TOne <$> pOneOf

pOneOf :: Parser ParamText
pOneOf = id <$ pToken OneOf `indented0` pParamText

