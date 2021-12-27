{-# LANGUAGE OverloadedStrings #-}

module LS.RelationalPredicates where

import Text.Megaparsec
import Control.Monad.Writer.Lazy
import Text.Parser.Permutation
import Data.Maybe (fromJust)
import Data.List.NonEmpty ( nonEmpty, toList )

import qualified AnyAll as AA

import LS.Types
import LS.Tokens
import LS.ParamText

pRelationalPredicate :: Parser RelationalPredicate
pRelationalPredicate = debugName "pRelationalPredicate" $ choice
  [ try ( debugName "pRP: RPConstraint"
          pConstraint )
  , try ( debugName "pRP: RPBoolStructR" $
          RPBoolStructR <$> pMultiTerm <*> tok2rel <*> pBoolStructR )
  , try ( debugName "pRP: RPParamText" $
          RPParamText <$> pParamText )
  ]
    

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


-- TODO: FIXME: this is a hack, because we don't have a good way to parse the thing
unLeaf :: BoolStructR -> RelationalPredicate
unLeaf (AA.Leaf x) = x
unLeaf _ = error "unLeaf: not a leaf"

-- let's do a nested and/or tree for relational predicates, not just boolean predicate structures
pBoolStructR :: Parser BoolStructR
-- pBoolStructR = debugName "pBoolStructR" $ do
  -- toBoolStruct <$> expr (unLeaf <$> rpElement)

-- first choice:  simple constraint
{-
    example: ["eyes"] RPis ["blue"]
    AA.Leaf (RPConstraint  ["eyes"] RPis ["blue"])
-}    

-- second choice: recursive boolstructR
{- example: eyes IS (left IS blue AND right IS brown)
    AA.Leaf (RPBoolStructR MultiTerm RPRel (AA.All [ RPConstraint ["left"]  RPis ["blue"]
                                                   , RPConstraint ["right"] RPis ["brown"] ]))

   example: blue OR brown
    AA.Any [ RPParamText ("blue"  :| [], Nothing) :| []
           , RPParamText ("brown" :| [], Nothing) :| [] ]
-}

-- third choice: wrapper for ParamText
{- example: cloudless blue skies
    AA.Leaf (RPParamText ("cloudless" :| ["blue", "skies"], Nothing) :| [])
-}

pBoolStructR = debugName "pBoolStructR" $ do
  (ands,unlesses) <- permute $ (,)
    <$$> Just <$> rpAndGroup
    <|?> (Nothing, Just <$> rpUnlessGroup)
  return $ fromJust $ addneg ands unlesses

rpUnlessGroup :: Parser BoolStructR
rpUnlessGroup = debugName "rpUnlessGroup" $ do
  pToken Unless *> myindented rpAndGroup

rpAndGroup :: Parser BoolStructR
rpAndGroup = debugName "rpAndGroup" $ do
    rpOrGroup1 <- manyIndentation rpOrGroup
    rpOrGroupN <- many $ pToken And *> manyIndentation rpOrGroup
    let toreturn = if null rpOrGroupN
                   then rpOrGroup1
                   else AA.All Nothing (rpOrGroup1 : rpOrGroupN)
    return toreturn

rpOrGroup :: Parser BoolStructR
rpOrGroup = debugName "rpOrGroup" $ do
  elem1    <- someIndentation rpElement <* optional dnl
  elems    <- many $ pToken Or *> someIndentation rpElement
  let toreturn = if null elems
                 then elem1
                 else AA.Any Nothing (elem1 : elems)
  return toreturn

-- i think we're going to need an rpUnlessGroup as well

rpElement :: Parser BoolStructR
rpElement = debugName "rpElement" $ do
  try (rpConstitutiveAsElement <$> tellIdFirst pConstitutiveRule)
    <|> do
    rpAtomicElement
  
rpAtomicElement :: Parser BoolStructR
rpAtomicElement = debugName "rpAtomicElement" $ do
  try rpNotElement
  <|> try rpAndGroup
  <|> rpLeafVal






rpConstitutiveAsElement :: Rule -> BoolStructR
rpConstitutiveAsElement = multiterm2bsr

rpNotElement :: Parser BoolStructR
rpNotElement = debugName "rpNotElement" $ do
  inner <- pToken MPNot *> someIndentation dBoolStructR
  return $ AA.Not inner

rpLeafVal :: Parser BoolStructR
rpLeafVal = debugName "rpLeafVal" $ do
  leafVal <- pRelationalPredicate
  myTraceM $ "rpLeafVal returning " ++ show leafVal
  return $ AA.Leaf leafVal



rpNestedBool :: Parser BoolStructR
rpNestedBool = debugName "rpNestedBool" $ do
  depth <- asks callDepth
  debugPrint $ "rpNestedBool lookahead looking for some pBoolConnector"
  (leftX,foundBool) <- lookAhead (rpLeafVal >> optional dnl >> (,) <$> lookAhead pXLocation <*> pBoolConnector)
  myTraceM $ "rpNestedBool lookahead matched " ++ show foundBool ++ " at location " ++ show leftX ++ "; testing if leftX " ++ show leftX ++ " > depth " ++ show depth
  guard (leftX > depth)
  myTraceM $ "rpNestedBool lookahead matched " ++ show foundBool ++ " at location " ++ show leftX ++ "; rewinding for dBoolStructR to capture."
  withDepth (leftX + 0) dBoolStructR
  
dBoolStructR :: Parser BoolStructR
dBoolStructR = debugName "dBoolStructR" $ do
  rpAndGroup

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

pConstitutiveRule :: Parser Rule
pConstitutiveRule = debugName "pConstitutiveRule" $ do
  leftY              <- lookAhead pYLocation
  namep              <- debugName "calling myindented pNameParens" $ manyIndentation pNameParens
  leftX              <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.

  ( (copula, mletbind), whenifs, unlesses, givens ) <-
    withDepth leftX $ permutationsCon [Means,Includes] [When,If] [Unless] [Given]
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
  ands <- manyIndentation pBoolStructR -- (foo AND (bar OR baz), [constitutive and regulative sub-rules])
  return (condWord, ands)






preambleParamText :: [MyToken] -> Parser (Preamble, ParamText)
preambleParamText preambles = do
  preamble <- choice (try . pToken <$> preambles)
  paramtext <- pPTParens -- pPTParens is a bit awkward here because of the multiline possibility of a paramtext
  return (preamble, paramtext)
