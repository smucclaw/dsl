{-# LANGUAGE OverloadedStrings #-}

module LS.ParamText where

import qualified Data.Text.Lazy as Text
import Text.Megaparsec

import LS.Types
import LS.Tokens
import Data.List.NonEmpty

--- | MUST | startParamText        | optionalRestOfLine    | OptionalType   |
--- |      | optional nextline key | nextline value        |                |
pParamText :: Parser ParamText
pParamText = debugName "pParamText" $
  (:|) <$> (pKeyValues <?> "paramText head") <*> pParams


  -- === flex for
  --     (myhead, therest) <- (pKeyValues <* dnl) `indented0` pParams
  --     return $ myhead :| therest

pParams :: Parser [KVsPair]
pParams = debugName "pParams: calling manyDeep pKeyValues" $ manyIndentation $ many pKeyValues    -- head (name+,)*

pKeyValues :: Parser KVsPair
pKeyValues = debugName "pKeyValues"
             (debugName "pAny :| pAny*" $ (:|)
              <$> debugName "first pAny" pAnyText
              <*> debugName "subsequent manyDeep pAny" (manyDeep pAnyText))
             `optIndentedTuple` pTypeSig

pTypeSig :: Parser TypeSig
pTypeSig = debugName "pTypeSig" $ do
  _           <- pToken TypeSeparator <|> pToken Is
  someIndentation (simpletype <|> inlineenum)
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
pOneOf = pToken OneOf *> someIndentation pParamText

