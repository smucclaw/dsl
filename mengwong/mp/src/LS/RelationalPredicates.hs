{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module LS.RelationalPredicates where

import Text.Megaparsec
import Control.Monad.Writer.Lazy
import Text.Parser.Permutation
import Debug.Trace

import qualified AnyAll as AA
import Data.List.NonEmpty ( NonEmpty((:|)), nonEmpty, toList )
import Data.Maybe (fromMaybe, fromJust, maybeToList)

import LS.Types
import LS.Tokens
import LS.ParamText
import LS.Parser

pRelationalPredicate :: Parser RelationalPredicate
pRelationalPredicate = pRelPred

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


-- TODO: FIXME: this is a hack, because we don't have a good way to parse the thing
unLeaf :: BoolStructR -> RelationalPredicate
unLeaf (AA.Leaf x) = x
unLeaf _ = error "unLeaf: not a leaf"


rpConstitutiveAsElement :: Rule -> BoolStructR
rpConstitutiveAsElement = multiterm2bsr

rpHornlikeAsElement :: Rule -> BoolStructR
rpHornlikeAsElement =  multiterm2bsr

rpLeafVal :: Parser BoolStructR
rpLeafVal = debugName "rpLeafVal" $ do
  leafVal <- pRelationalPredicate
  myTraceM $ "rpLeafVal returning " ++ show leafVal
  return $ AA.Leaf leafVal


  
-- this is probably going to need cleanup
addneg :: Maybe BoolStructR -> Maybe BoolStructR -> Maybe BoolStructR
addneg Nothing  Nothing   = Nothing
addneg Nothing  (Just n)  = Just $ AA.Not n
addneg (Just p) (Just n)  = Just $ AA.All Nothing [p, AA.Not n]
addneg (Just p) Nothing   = Just p

-- combine all the boolrules under the first preamble keyword
mergePBRS :: [(Preamble, BoolStructR)] -> Maybe (Preamble, BoolStructR)
mergePBRS [] = Nothing
mergePBRS [x] = Just x
mergePBRS xs         = Just (fst . head $ xs, AA.All Nothing (snd <$> xs))

c2hornlike :: Rule -> Rule
c2hornlike Constitutive { name, keyword, letbind, cond, given, rlabel, lsource, srcref } =
  let clauses = pure $ HC2 (RPBoolStructR name RPis letbind) cond
      upon = Nothing
  in Hornlike { name, keyword, given, upon, clauses, rlabel, lsource, srcref   }
c2hornlike r = r

pConstitutiveRule :: Parser Rule
pConstitutiveRule = debugName "pConstitutiveRule" $ do
  leftY              <- lookAhead pYLocation
  namep              <- debugName "calling myindented pNameParens" $ manyIndentation pNameParens
  leftX              <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.

  ( (copula, mletbind), whenifs, unlesses, givens ) <-
    permutationsCon [Means,Includes] [When,If] [Unless] [Given]
  srcurl <- asks sourceURL
  let srcref' = SrcRef srcurl srcurl leftX leftY Nothing

  return $ Constitutive
    { name = namep
    , keyword = copula
    , letbind = mletbind
    , cond = addneg
             (snd <$> mergePBRS whenifs)
             (snd <$> mergePBRS unlesses)
    , given = nonEmpty $ foldMap toList (snd <$> givens)
    , rlabel = noLabel
    , lsource = noLSource
    , srcref = Just srcref'
    }

-- bob's your uncle
-- MEANS
--    bob's your mother's brother
-- OR bob's your father's mother

permutationsCon :: [MyToken] -> [MyToken] -> [MyToken] -> [MyToken]
                           -- preamble = copula   (means,deem,decide)
                -> Parser (  (Preamble, BoolStructR)  -- body of horn clause
                          , [(Preamble, BoolStructR)] -- positive conditions (when,if)
                          , [(Preamble, BoolStructR)] -- negative conditions (unless)
                          , [(Preamble, ParamText)] -- given    (given params)
                          )
permutationsCon copula ifwhen l4unless l4given =
  debugName ("permutationsCon"
             <> ": copula="   <> show copula
             <> ", positive=" <> show ifwhen
             <> ", negative=" <> show l4unless
             <> ", given="    <> show l4given
            ) $ do
  permute $ (,,,)
    <$$>             preambleBoolStructR copula
    <|?> ([], some $ preambleBoolStructR ifwhen)
    <|?> ([], some $ preambleBoolStructR l4unless)
    <|?> ([], some $ preambleParamText l4given)

-- degustates
--     MEANS eats
--        OR drinks
--      WHEN weekend

pBoolConnector :: Parser MyToken
pBoolConnector = debugName "pBoolConnector" $ do
  pToken And <|> pToken Or <|> pToken Unless <|> pToken MPNot

-- support name-like expressions tagged with AKA, which means "also known as"
-- sometimes we want a plain Text.Text
pNameParens :: Parser RuleName
pNameParens = pMultiTermAka

-- sometimes we want a ParamText
pPTParens :: Parser ParamText
pPTParens = debugName "pPTAka" $ pAKA pParamText pt2multiterm


preambleBoolStructR :: [MyToken] -> Parser (Preamble, BoolStructR)
preambleBoolStructR wanted = debugName ("preambleBoolStructR " <> show wanted)  $ do
  -- leftX     <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  condWord <- choice (try . pToken <$> wanted)
  -- myTraceM ("preambleBoolStructR: found: " ++ show condWord ++ " at depth " ++ show leftX)
  ands <- manyIndentation pBSR -- (foo AND (bar OR baz), [constitutive and regulative sub-rules])
  return (condWord, ands)






preambleParamText :: [MyToken] -> Parser (Preamble, ParamText)
preambleParamText preambles = do
  preamble <- choice (try . pToken <$> preambles)
  paramtext <- pPTParens -- pPTParens is a bit awkward here because of the multiline possibility of a paramtext
  return (preamble, paramtext)

pHornlike :: Parser Rule
pHornlike = debugName "pHornlike" $ do
  (rlabel, srcref) <- pSrcRef
  ((keyword, name, clauses), given, upon, topwhen) <- debugName "pHornlike / permute" $ permute $ (,,,)
    <$$> someStructure
    <|?> (Nothing, fmap snd <$> optional givenLimb)
    <|?> (Nothing, fmap snd <$> optional uponLimb)
    <|?> (Nothing, whenCase)
  return $ Hornlike { name
                    , keyword = fromMaybe Means keyword
                    , given
                    , clauses = addWhen topwhen clauses
                    , upon, rlabel, srcref
                    , lsource = noLSource }
  where
    addWhen :: Maybe BoolStructR -> [HornClause2] -> [HornClause2]
    addWhen mbsr hcs = [ hc2 { hBody = hBody hc2 <> mbsr }
                       | hc2 <- hcs ]

    -- this is actually kind of a meta-rule, because it really means
    -- assert(X :- (Y1, Y2)) :- body.
    whenMeansIf = choice [ pToken When, pToken Means, pToken If ]
    whenCase = debugName "whenCase" $ whenMeansIf *> (Just <$> pBSR) <|> Nothing <$ pToken Otherwise

    -- DECIDE x IS y WHEN Z IS Q

    someStructure = debugName "pHornlike/someStructure" $ do
      keyword <- optional $ choice [ pToken Define, pToken Decide ]
      (relPred, whenpart) <- manyIndentation (pRelPred `optIndentedTuple` whenCase)
      return (keyword, getFirstWord relPred, [HC2 relPred (fromMaybe Nothing whenpart)])

    givenLimb = debugName "pHornlike/givenLimb" $ preambleParamText [Given]
    uponLimb  = debugName "pHornlike/uponLimb"  $ preambleParamText [Upon]

    getFirstWord :: RelationalPredicate -> RuleName
    getFirstWord (RPParamText pt) = pt2multiterm pt
    getFirstWord (RPMT mt)        = mt
    getFirstWord (RPConstraint mt _ _) = mt
    getFirstWord (RPBoolStructR mt _ _) = mt

pRelPred :: Parser RelationalPredicate
pRelPred = debugName "pRelPred" $ do
  try (debugName "RPConstraint" $ RPConstraint $|| dMultiTerm |>| tok2rel ||< dMultiTerm)
    <|> try (debugName "RPBoolStructR" $ indent3 RPBoolStructR pMultiTerm tok2rel pBSR)
    <|> try (debugName "RPMT" $ RPMT <$> pMultiTerm)
  where dMultiTerm = debugName "dMultiTerm" $ (.:|) (debugName "pNumOrText" pNumOrText)

-- then we start with entire relationalpredicates, and wrap them into BoolStructR
pBSR :: Parser BoolStructR
pBSR = toBoolStruct <$> expr pRelPred



