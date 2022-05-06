{-# LANGUAGE OverloadedStrings #-}

module LS.ParamText where

import Text.Megaparsec
import Control.Monad.Writer.Lazy
import qualified Data.Text.Lazy as Text

import LS.Types
import LS.Parser
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

-- let's use the Parser library to do this stuff
pBoolStructPT :: Parser BoolStructP
pBoolStructPT = prePostParse pParamText

pParamText :: Parser ParamText
pParamText = debugName "pParamText" $ (<* pToken EOL) $
  (:|)
  <$> ((,)
      <$> ((:|) <$> pOtherVal <*> pure [])
      <*> pure Nothing)
  <*> pure []
  -- <$> debugName "pParamText(flat) first line: pKeyValues" pKeyValuesAka
  -- <*> debugName "pParamText(flat) subsequent lines: sameMany pKeyValues" (manyIndentation (sameMany pKeyValuesAka))

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
pMultiTermAka = debugName "pMultiTermAka" $ pAKA (liftSL pMultiTerm) id <* dnl

-- head of nonempty list
pSingleTermAka :: Parser KVsPair
pSingleTermAka = debugName "pSingleTermAka" $ pAKA slTypedMulti (toList . fst)

pSingleTerm :: Parser KVsPair
pSingleTerm = debugName "pSingleTerm" $ ((:|[]) <$> pAnyText) `optIndentedTuple` pTypeSig

slParamText :: SLParser ParamText
slParamText = debugNameSL "slParamText" $ pure <$> slTypedMulti

slTypedMulti :: SLParser KVsPair
slTypedMulti = debugNameSL "slTypedMulti" $ do
  (l,ts,typicalval) <- (,,)
    $*| slMultiTerm
    |*| (|?|) slTypeSig
    |*| (|?|) typically
  liftSL $ writeTypically l typicalval
  return (fromList l, ts)

writeTypically :: MultiTerm -> Maybe MultiTerm -> Parser ()
writeTypically somekey someval = do
  srcref' <- getSrcRef
  tell $ maybe mempty (\t -> singeltonDL (DefTypically somekey [RPConstraint somekey RPis t] (Just srcref'))) someval
  return ()

slTypeSig :: SLParser TypeSig
slTypeSig = debugNameSL "slTypeSig" $ do
  (_typesep, typesig) <- (,)
       $>| (pToken TypeSeparator <|> pToken Is)
       |*| (simpletype <|> inlineenum)
  return typesig
  where
    simpletype = SimpleType
                 $>| choice [ TOne      <$ pToken One
                            , TOne      <$ pToken A_An
                            , TOptional <$ pToken Optional
                            , TList0    <$ pToken List0
                            , TList1    <$ pToken List1 ]
                 |>| pOtherVal
    inlineenum = InlineEnum TOne $*| slOneOf

slOneOf :: SLParser ParamText
slOneOf = do
  (flip const)
    $>| pToken OneOf
    |>| pParamText

-- a nonempty list, with an optional type signature and an optional AKA; single line. for multiline see pParamText above
pKeyValuesAka :: Parser KVsPair
pKeyValuesAka = debugName "pKeyValuesAka" $ pAKA (liftSL pKeyValues) (toList . fst)

pKeyValues :: Parser KVsPair
pKeyValues = debugName "pKeyValues" $ do
             (lhs, typesig)   <- (,)
                                 <$> some pNumOrText
                                 <*> optional pTypeSig
             return (fromList lhs, typesig)

slKeyValues :: SLParser KVsPair
slKeyValues = debugNameSL "slKeyValues" $ do
             (lhs, typesig)   <- (,)
                                 $*| someLiftSL pNumOrText
                                 |*| (|?|) slTypeSig
             return (fromList lhs, typesig)

getSrcRef :: Parser SrcRef
getSrcRef = do
  leftY  <- lookAhead pYLocation
  leftX  <- lookAhead pXLocation
  srcurl <- asks sourceURL
  return SrcRef {url = srcurl, short = srcurl, srcrow = leftY, srccol = leftX, version = Nothing}


-- utility function for the above
pAKA :: (Show a) => SLParser a -> (a -> MultiTerm) -> Parser a
pAKA baseParser toMultiTerm = debugName "pAKA" $ do
  slAKA baseParser toMultiTerm |<$ undeepers

slAKA :: (Show a) => SLParser a -> (a -> MultiTerm) -> SLParser a
slAKA baseParser toMultiTerm = debugNameSL "slAKA" $ do
  (base, entityalias, typicalval) <- (,,)
                         $*| debugName "slAKA base" baseParser
                         |*| debugName "slAKA optional akapart"   ((|?|) akapart)
                         |*| debugName "slAKA optional typically" ((|?|) typically)

  debugPrint "slAKA: proceeding after base and entityalias are retrieved ..."
  let detail' = toMultiTerm base

  debugPrint $ "pAKA: entityalias = " ++ show entityalias
  srcref' <- liftSL getSrcRef
  let defalias = maybe mempty (\t -> singeltonDL (DefNameAlias t detail' Nothing (Just srcref'))) entityalias
  liftSL $ tell defalias
  liftSL $ writeTypically detail' typicalval
  return base
-- a BoolStructR is the new ombibus type for the WHO and COND keywords,
-- being an AnyAll tree of RelationalPredicates.

  where
    akapart :: SLParser RuleName
    akapart = debugName "PAKA/akapart" $ do
      (_akatoken, akaval) <- (,)
                                $>| debugName "Aka Token" (pToken Aka)
                                |*| someLiftSL pOtherVal
      return akaval

typically :: SLParser MultiTerm
typically = debugName "typically" $ do
  (_typically, someterm) <- (,)
                                $>| pToken Typically
                                |*| slMultiTerm
  return someterm
