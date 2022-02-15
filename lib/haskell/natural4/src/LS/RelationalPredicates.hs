{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module LS.RelationalPredicates where

import Text.Megaparsec
import Control.Monad.Writer.Lazy
import Text.Parser.Permutation
import Debug.Trace
import qualified Data.Text.Lazy as Text

import qualified AnyAll as AA
import Data.List.NonEmpty ( NonEmpty((:|)), nonEmpty, toList )
import Data.Maybe (fromMaybe, fromJust, maybeToList, catMaybes)

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

aaLeaves :: AA.Item RelationalPredicate -> [MultiTerm]
aaLeaves = aaLeavesFilter (const True)

aaLeavesFilter :: (RelationalPredicate -> Bool) -> AA.Item RelationalPredicate -> [MultiTerm]
aaLeavesFilter f (AA.All _ xs) = concatMap (aaLeavesFilter f) xs
aaLeavesFilter f (AA.Any _ xs) = concatMap (aaLeavesFilter f) xs -- these actually need to be treated differently -- i think the Any needs a join transition in the Petri net? revisit this when more awake and thinking more clearly.
aaLeavesFilter f (AA.Not x) = aaLeavesFilter f x
aaLeavesFilter f (AA.Leaf rp) = if f rp then rp2mt rp else []
  where
    rp2mt (RPMT mt)                     = [mt]
    rp2mt (RPParamText    pt)           = [pt2multiterm pt]
    rp2mt (RPConstraint  _mt1 _rpr mt2) = [mt2]
    rp2mt (RPBoolStructR _mt1 _rpr bsr) = aaLeavesFilter f bsr

  
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
c2hornlike Constitutive { name, keyword, letbind, cond, given, rlabel, lsource, srcref, defaults, symtab } =
  let clauses = pure $ HC2 (RPBoolStructR name RPis letbind) cond
      upon = Nothing
  in Hornlike { name, keyword, given, upon, clauses, rlabel, lsource, srcref, defaults, symtab }
c2hornlike r = r

pConstitutiveRule :: Parser Rule
pConstitutiveRule = debugName "pConstitutiveRule" $ do
  maybeLabel <- optional pRuleLabel
  leftY              <- lookAhead pYLocation
  namep              <- debugName "calling myindented pNameParens" $ manyIndentation pNameParens
  leftX              <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.

  ( (copula, mletbind), whenifs, unlesses, givens ) <-
    manyIndentation $ permutationsCon [Means,Includes,Is] [When,If] [Unless] [Given]
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
    , rlabel = maybeLabel
    , lsource = noLSource
    , srcref = Just srcref'
    , defaults = mempty, symtab = mempty
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

-- sometimes we want a ParamText -- single line -- with possibility of an AKA on the right hand side
pPTParens :: Parser ParamText
pPTParens = debugName "pPTParens" $ pAKA slParamText pt2multiterm


preambleBoolStructR :: [MyToken] -> Parser (Preamble, BoolStructR)
preambleBoolStructR wanted = debugName ("preambleBoolStructR " <> show wanted)  $ do
  -- leftX     <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  condWord <- choice (try . pToken <$> wanted)
  -- myTraceM ("preambleBoolStructR: found: " ++ show condWord ++ " at depth " ++ show leftX)
  ands <- pBSR -- (foo AND (bar OR baz), [constitutive and regulative sub-rules])
  return (condWord, ands)






preambleParamText :: [MyToken] -> Parser (Preamble, ParamText)
preambleParamText preambles = debugName ("preambleParamText:" ++ show preambles) $ do
  (,)
    $>| choice (try . pToken <$> preambles)
    |>< pParamText

pHornlike :: Parser Rule
pHornlike = debugName "pHornlike" $ do
  (rlabel, srcref) <- pSrcRef
  ((keyword, name, clauses), given, upon, topwhen) <- debugName "pHornlike / permute" $ permute $ (,,,)
    <$$> (try ambitious <|> someStructure)
    <|?> (Nothing, fmap snd <$> optional givenLimb)
    <|?> (Nothing, fmap snd <$> optional uponLimb)
    <|?> (Nothing, whenCase)
  return $ Hornlike { name
                    , keyword = fromMaybe Means keyword
                    , given
                    , clauses = addWhen topwhen clauses
                    , upon, rlabel, srcref
                    , lsource = noLSource
                    , defaults = mempty, symtab = mempty
                    }
  where
    addWhen :: Maybe BoolStructR -> [HornClause2] -> [HornClause2]
    addWhen mbsr hcs = [ hc2 { hBody = hBody hc2 <> mbsr }
                       | hc2 <- hcs ]

    -- this is actually kind of a meta-rule, because it really means
    -- assert(X :- (Y1, Y2)) :- body.

    -- DECIDE x IS y WHEN Z IS Q

    ambitious = debugName "pHornlike/ambitious" $ do
      (keyword, subject) <- (,) $>| choice [ pToken Define, pToken Decide ] |*< slMultiTerm
      (iswhen, object)   <- (,) $>| choice [ pToken When,   pToken Is     ] |>< pNameParens
      (ifLimb,unlessLimb,andLimb,orLimb) <- debugName "pHornlike / someStructure / clauses permute" $ permute $ (,,,)
        <$?> (Nothing, Just <$> try ((,) <$> pToken If     <*> pBSR))
        <|?> (Nothing, Just <$> try ((,) <$> pToken Unless <*> pBSR))
        <|?> (Nothing, Just <$> try ((,) <$> pToken And    <*> pBSR))
        <|?> (Nothing, Just <$> try ((,) <$> pToken Or     <*> pBSR))
      let clauses = [HC2 (RPConstraint subject RPis object)
                     (maybe (Just $ AA.Leaf $ RPMT ["always"])
                      (Just . snd) $ mergePBRS (catMaybes [ifLimb,andLimb,orLimb,fmap AA.Not <$> unlessLimb]))]
      return (Just keyword, subject, clauses)

    someStructure = debugName "pHornlike/someStructure" $ do
      keyword <- optional $ choice [ pToken Define, pToken Decide ]
      (relPred, whenpart) <- manyIndentation (try relPredNextlineWhen <|> relPredSamelineWhen)
      return (keyword, inferRuleName relPred, [HC2 relPred whenpart])


    givenLimb = debugName "pHornlike/givenLimb" $ preambleParamText [Given]
    uponLimb  = debugName "pHornlike/uponLimb"  $ preambleParamText [Upon]

    inferRuleName :: RelationalPredicate -> RuleName
    inferRuleName (RPParamText pt)       = pt2multiterm pt
    inferRuleName (RPMT mt)              = mt
    inferRuleName (RPConstraint  mt _ _) = mt
    inferRuleName (RPBoolStructR mt _ _) = mt

pRelPred :: Parser RelationalPredicate
pRelPred = debugName "pRelPred" $ do
  slRelPred |<$ undeepers

relPredNextlineWhen :: Parser (RelationalPredicate, Maybe BoolStructR)
relPredNextlineWhen = debugName "relPredNextlineWhen" $ do
  (x,y) <- debugName "pRelPred optIndentedTuple whenCase" (pRelPred `optIndentedTuple` whenCase)
  return (x, join y)

relPredSamelineWhen :: Parser (RelationalPredicate, Maybe BoolStructR)
relPredSamelineWhen = debugName "relPredSamelineWhen" $ (,) $*| slRelPred |>< (join <$> (debugName "optional whenCase -- but we should still consume GoDeepers before giving up" $ optional whenCase))
whenCase :: Parser (Maybe BoolStructR)
whenCase = debugName "whenCase" $ do
  try (whenMeansIf *> (Just <$> pBSR))
  <|> Nothing <$ (debugName "Otherwise" $ pToken Otherwise)
whenMeansIf :: Parser MyToken
whenMeansIf = debugName "whenMeansIf" $ choice [ pToken When, pToken Means, pToken If ]

slRelPred :: Parser (RelationalPredicate, Int)
slRelPred = debugName "slRelPred" $ do
  try       ( debugName "RPConstraint"  rpConstraint )
    <|> try ( debugName "RPBoolStructR" rpBoolStructR )
    <|> try ( debugName "RPMT"          rpMT )
  
rpMT :: Parser (RelationalPredicate, Int)
rpMT          = RPMT          $*| slAKA slMultiTerm id
rpConstraint :: Parser (RelationalPredicate, Int)
rpConstraint  = RPConstraint  $*| slMultiTerm |>| tok2rel |*| slMultiTerm

rpBoolStructR :: Parser (RelationalPredicate, Int)
rpBoolStructR = RPBoolStructR $*| slMultiTerm |>| tok2rel |>| pBSR
-- then we start with entire relationalpredicates, and wrap them into BoolStructR
pBSR :: Parser BoolStructR
pBSR = debugName "pBSR" $
--  local (\rc -> rc { debug = True }) $
  do
  try noPrePost <|> try (withPrePost noPrePost) <|> withPreOnly noPrePost
  where
    noPrePost = debugName "pBSR inner" $ toBoolStruct <$> expr pRelPred

-- how many UnDeepers do we count between the end of this line and the start of the next (where, presumably, we get an OR)
expectUnDeepers :: Parser Int
expectUnDeepers = debugName "expectUnDeepers" $ lookAhead $ do
  ignored <- manyTill (pNumOrText <|> "GD" <$ pToken GoDeeper) (lookAhead (pToken UnDeeper))
  debugPrint $ "ignoring " ++ show ignored
  udps <- some (pToken UnDeeper)
  debugPrint $ "matched undeepers " ++ show udps
  return $ length udps


withPrePost, withPreOnly :: Show a => Parser (AA.Item a) -> Parser (AA.Item a)
withPrePost basep = debugName "withPrePost" $ do
  (pre, body, post) <- (,,)
   -- this places the "cursor" in the column above the OR, after a sequence of pOtherVals,
    -- and to the left of the first, topmost term in the boolstruct
    $*| debugName "pre part" (getLHS <$> (pOtherVal /+= aboveNextLineKeyword))
    |-| debugName "made it to inner base parser" basep
    |<* debugName "post part" slMultiTerm -- post part
    |<$ undeepers
  return $ relabelpp body (Text.unwords pre) (Text.unwords post)
  where
    getLHS ((x,_),z) = (x,z)
    relabelpp :: AA.Item a -> Text.Text -> Text.Text -> AA.Item  a
    relabelpp (AA.All Nothing xs) pre post = AA.All (Just $ AA.PrePost pre post) xs
    relabelpp (AA.Any Nothing xs) pre post = AA.Any (Just $ AA.PrePost pre post) xs
    relabelpp _ _ _ = error "RelationalPredicates: relabelpp failed"

withPreOnly basep = debugName "withPreOnly" $ do
  (pre, body) <- (,)
   -- this places the "cursor" in the column above the OR, after a sequence of pOtherVals,
    -- and to the left of the first, topmost term in the boolstruct
    $*| debugName "pre part" (getLHS <$> (pOtherVal /+= aboveNextLineKeyword))
    |-| debugName "made it to inner parser" basep
    |<$ undeepers
  return $ relabelp body (Text.unwords pre)
  where
    getLHS ((x,_),z) = (x,z)
    relabelp :: AA.Item a -> Text.Text -> AA.Item  a
    relabelp  (AA.All Nothing xs) pre      = AA.All (Just $ AA.Pre     pre)      xs
    relabelp  (AA.Any Nothing xs) pre      = AA.Any (Just $ AA.Pre     pre)      xs
    relabelp  _ _ = error "RelationalPredicates: relabelp failed"



aboveNextLineKeyword :: SLParser ([Text.Text],MyToken)
aboveNextLineKeyword = debugName "aboveNextLineKeyword" $ do
  undp_count <- expectUnDeepers
  debugPrint $ "aNLK: determined undp_count = " ++ show undp_count
  ((_,slmt),n) <- (,)
                 $*| return ((),0) -- this just gets us from (,,) into the SLParser context
                 ->| 1
                 |*| slMultiTerm
                 |-- (\d -> debugPrint $ "aNLK: current depth is " ++ show d)
  (tok,m) <- id
             +>| n
             |<| choice (pToken <$> [ LS.Types.Or, LS.Types.And, LS.Types.Unless ])

  debugPrint $ "aNLK: slMultiTerm is " ++ show slmt

  if n == undp_count
    then return ((slmt, tok), m)
    else fail $ "aNLK: expecting depth " ++ show undp_count ++ " but the cursor seems to be placed such that we have " ++ show n ++ "; a different backtrack will probably fare better"
  

-- aboveNextLineKeyword has returned ((["foo1","foo2","foo3"],Or),1)
-- aboveNextLineKeyword has returned ((["foo2","foo3"],       Or),0)
-- aboveNextLineKeyword has returned ((["foo3"],              Or),-1) -- to get this, maxDepth=0
