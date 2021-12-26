{-# LANGUAGE OverloadedStrings #-}

module LS.ParamText where

import Text.Megaparsec

import LS.Types
import LS.Tokens
import Data.List.NonEmpty

-- there are two possible styles: flat, and tree.
--- in the flat style, succeeding lines start at the same indentation level as the startParamText:
--- | MUST | startParamText         | optionalRestOfLine    | OptionalType   |
--- |      | optional nextline key  | nextline values       |                |

--- in the tree style, succeeding lines could start at a deeper indentation level; if so, they become nested records.
--- | MUST | startParamText         | optionalRestOfLine    | OptionalType   |
--- |      | optional nextline key1 | nextline values       |                |
--- |      | optional nextline key2 | nextline values       |                |
--- |      |                        | child key 2a          | child values   |
--- |      |                        | child key 2b          | child values   |

--- for this initial implementation of SFL4 we will allow only the flat style, but in the future we will want to move to the tree style.
--- the tree style corresponds more naturally to the idea of, e.g., a JSON object that has multi-level objects nested.

pParamText :: Parser ParamText
pParamText = debugName "pParamText" $
  (:|) <$> (pKeyValues <?> "paramText first line") <*> (sameDepth pKeyValues <?> "paramText subsequent lines")

pKeyValues :: Parser KVsPair
pKeyValues = debugName "pKeyValues"
             (debugName "pAny :| pAny*" $ (:|)
              <$> debugName "first pAny" pAnyText
              <*> debugName "subsequent someDeep pAny" (someDeep pAnyText))
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

