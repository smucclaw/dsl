{-# LANGUAGE OverloadedStrings #-}

module LS.ParamText where

import Text.Megaparsec
import Control.Monad.Writer.Lazy

import LS.Types
import LS.Tokens
-- import LS.Parser
import Data.List.NonEmpty

-- there are two possible styles: flat, and tree.
--- in the flat style, succeeding lines start at the same indentation level as the startParamText:
--- | MUST | startParamText         | optionalRestOfLine    | OptionalType   |
--- |      | optional nextline key  | nextline values       |                |

--- the list is flat; there are no nested sublists. This produces a ParamText.

--- in the tree style, succeeding lines could start at a deeper indentation level; if so, they become nested records.
--- | MUST | startParamText         | optionalRestOfLine    | OptionalType   |
--- |      | optional nextline key1 | nextline values       |                |
--- |      | optional nextline key2 | nextline values       |                |
--- |      |                        | child key 2a          | child values   |
--- |      |                        | child key 2b          | child values   |

--- this produces a PTree.

--- the initial implementation of SFL4 will process only the flat style; so the Legal Spreadsheets MUST all be phrased flat;
--- but in the future we will want to add support for the tree style.
--- the tree style corresponds more naturally to the idea of, e.g., a JSON object that has multi-level objects nested.

pParamText :: Parser ParamText
pParamText = debugName "pParamText" $
  (:|)
  <$> debugName "pParamText(flat) first line: pKeyValues" pKeyValues
  <*> debugName "pParamText(flat) subsequent lines: sameMany pKeyValues" (sameMany pKeyValues)

pPTree :: Parser PTree
pPTree = debugName "pPTtree tree" $ do
  try pTreeOneWord <|> pTreeSomeWords

pTreeOneWord, pTreeSomeWords :: Parser PTree
-- single word on the first line, followed by many indenteds on subsequent lines.
pTreeOneWord = debugName "pTreeOneWord" $ do
  firstWord  <- debugName "pTreeOneWord: the word" (pSingleTermAka <* debugName "EOL" dnl)
  inners     <- debugName "pTreeOneWord: inners" (sameDepth pPTree)
  return $ mkPTree firstWord inners

-- multiple words on the first line, followed by many subsequent lines which are indented relative to the first word.
pTreeSomeWords = debugName "pTreeSomeWords" $ do
  (firstLine, inners) <- debugName "pTreeSomeWords: first line, first word" $ do
    firstLine  <- debugName "pTreeSomeWords: lookahead pMultiTermAka" (lookAhead pKeyValuesAka)
    _firstWord <- debugName "pTreeSomeWords: first line, first word" pAnyText
    (_nextWords, nextLines) <- someIndentation $ (,)
                              <$> debugName "pTreeSomeWords: first line, subsequent words, no save of AKA"
                               (local (\rc -> rc {saveAKA=False}) pMultiTermAka)
                              -- it should be possible to merge the pTree*Words functions into a single function that just matches   pMultiTermAka <|> dnl
                              <*> debugName "pTreeSomeWords: subsequent lines at the same indented level, recursing"
                               (sameDepth pPTree)
    return (firstLine, nextLines)
  return $ mkPTree firstLine inners

pTypeSig :: Parser TypeSig
pTypeSig = debugName "pTypeSig" $ do
  _           <- pToken TypeSeparator <|> pToken Is
  manyIndentation (simpletype <|> inlineenum) -- sometimes there is no GoDeeper between the TypeSeparator and the A_An due to toToken "IS A"
  where
    simpletype = do
      cardinality <- choice [ TOne      <$ pToken One
                            , TOne      <$ pToken A_An
                            , TOptional <$ pToken Optional
                            , TList0    <$ pToken List0
                            , TList1    <$ pToken List1 ]
      base        <- someIndentation pOtherVal
      return $ SimpleType cardinality base
    inlineenum = do
      InlineEnum TOne <$> pOneOf

pOneOf :: Parser ParamText
pOneOf = pToken OneOf *> someIndentation pParamText

-- sometimes we want a multiterm, just a list of text
pMultiTermAka :: Parser MultiTerm
pMultiTermAka = debugName "pMultiTermAka" $ pAKA pMultiTerm id

-- head of nonempty list
pSingleTermAka :: Parser KVsPair
pSingleTermAka = debugName "pSingleTermAka" $ pAKA pSingleTerm (toList . fst)

pSingleTerm :: Parser KVsPair
pSingleTerm = debugName "pSingleTerm" $ ((:|[]) <$> pAnyText) `optIndentedTuple` pTypeSig

-- a nonempty list, with an optional type signature
pKeyValuesAka :: Parser KVsPair
pKeyValuesAka = debugName "pKeyValuesAka" $ pAKA pKeyValues (toList . fst)

pKeyValues :: Parser KVsPair
pKeyValues = debugName "pKeyValues" $ do
             (lhs, typesig) <- pNumOrText `manyDeepThenMaybe` pTypeSig
             return (fromList lhs, typesig)

-- utility function for the above
pAKA :: (Show a) => Parser a -> (a -> MultiTerm) -> Parser a
pAKA baseParser toMultiTerm = debugName "pAKA" $ do
  base <- debugName "pAKA base" baseParser
  let detail' = toMultiTerm base
  leftY       <- lookAhead pYLocation
  leftX       <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  entityalias <- optional $ try $ manyIndentation (debugName "Aka Token" (pToken Aka) *>
                                                   debugName "someDeep pOtherVal" (someDeep pOtherVal)) -- ("MegaCorp")
  -- myTraceM $ "pAKA: entityalias = " ++ show entityalias
  srcurl <- asks sourceURL
  let srcref' = SrcRef srcurl srcurl leftX leftY Nothing
  let defalias = maybe mempty (\t -> singeltonDL (DefNameAlias t detail' Nothing (Just srcref'))) entityalias
  tell defalias
  return base
-- a BoolStructR is the new ombibus type for the WHO and COND keywords,
-- being an AnyAll tree of RelationalPredicates.

