{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module LS.RelationalPredicates where

import Text.Megaparsec
import Control.Monad.Writer.Lazy
import Text.Parser.Permutation
import qualified AnyAll as AA
import Data.List.NonEmpty ( toList, nonEmpty )
import Data.Maybe (fromMaybe, catMaybes)

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

-- [TODO]: [FIXME]: this is a hack, because we don't have a good way to parse the thing
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

aaLeaves :: AA.ItemMaybeLabel RelationalPredicate -> [MultiTerm]
aaLeaves = aaLeavesFilter (const True)

aaLeavesFilter :: (RelationalPredicate -> Bool) -> AA.ItemMaybeLabel RelationalPredicate -> [MultiTerm]
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
  in Hornlike { name, super = Nothing, keyword, given, upon, clauses, rlabel, lsource, srcref, defaults, symtab }
c2hornlike r = r

pConstitutiveRule :: Parser Rule
pConstitutiveRule = debugName "pConstitutiveRule" $ do
  maybeLabel <- optional pRuleLabel -- TODO: Handle the SL
  leftY              <- lookAhead pYLocation
  namep              <- debugName "calling myindented pNameParens" $ manyIndentation pNameParens
  leftX              <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.

  -- [TODO] we should delete the Unless here because we've got it in Expr
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
  condWord <- choice (pToken <$> wanted)
  -- myTraceM ("preambleBoolStructR: found: " ++ show condWord ++ " at depth " ++ show leftX)
  ands <- pBSR -- (foo AND (bar OR baz), [constitutive and regulative sub-rules])
  return (condWord, ands)






preambleParamText :: [MyToken] -> Parser (Preamble, ParamText)
preambleParamText preambles = debugName ("preambleParamText:" ++ show preambles) $ do
  (,)
    $>| choice (try . pToken <$> preambles)
    |>< pParamText


-- | a Hornlike rule does double duty, due to the underlying logical/functional/object paradigms.
-- on the logical side of things,            it has to handle a DECIDE xx MEANS yy WHEN zz.
-- on the functional/objecty side of things, it has to handle a DEFINE xx HAS yy variable definition.
pHornlike :: Parser Rule
pHornlike = debugName "pHornlike" $ do
  (rlabel, srcref) <- debugName "pHornlike pSrcRef" (slPretendEmpty pSrcRef)
  let permutepart = debugName "pHornlike / permute" $ permute $ (,,,)
        <$$> -- (try ambitious <|>
                    someStructure -- we are trying to keep things more regular. to eliminate ambitious we need to add the unless/and/or machinery to someStructure, unless the pBSR is equal to it
        <|?> (Nothing, fmap snd <$> optional givenLimb)
        <|?> (Nothing, fmap snd <$> optional uponLimb)
        <|?> (Nothing, whenCase)
        -- [TODO] refactor the rule-label logic to allow outdentation of rule label line relative to main part of the rule
  ((keyword, name, clauses), given, upon, topwhen) <- permutepart
  return $ Hornlike { name
                    , super = Nothing -- [TODO] need to extract this from the DECIDE line -- can we involve a 'slAka' somewhere downstream?
                    , keyword = fromMaybe Means keyword
                    , given
                    , clauses = addWhen topwhen $ clauses
                    , upon, rlabel, srcref
                    , lsource = noLSource
                    , defaults = mempty, symtab = mempty
                    }
  where
    addHead :: Maybe ParamText -> [HornClause2] -> [HornClause2]
    addHead Nothing hcs   = hcs
    addHead (Just pt) hcs = -- trace ("addHead running, overwriting hHead with RPParamText " <> show pt) $
                            [ hc2 { hHead = RPParamText pt }
                            | hc2 <- hcs ]

    addWhen :: Maybe BoolStructR -> [HornClause2] -> [HornClause2]
    addWhen mbsr hcs = [ -- trace ("addWhen running, appending to hBody = " <> show (hBody hc2)) $
                         -- trace ("addWhen running, appending the mbsr " <> show mbsr) $
                         hc2 { hBody = hBody hc2 <> mbsr }
                       | hc2 <- hcs ]

    -- this is actually kind of a meta-rule, because it really means
    -- assert(X :- (Y1, Y2)) :- body.

    -- DECIDE X IS y
    --   WHEN Z IS Q

    ambitious = debugName "pHornlike/ambitious" $ do
      (keyword, subject) <- (,) $>| debugName "Decide" (choice [ pToken Decide ]) |*< slMultiTerm
      (iswhen, object)   <- (,) $>| debugName "When/Is"       (choice [ pToken When,   pToken Is     ]) |>< pNameParens
      (ifLimb,unlessLimb,andLimb,orLimb) <- debugName "pHornlike/ambitious / clauses permute" $ permute $ (,,,)
        <$?> (Nothing, Just <$> try ((,) <$> pToken If     <*> debugName "IF pBSR"     pBSR))
        <|?> (Nothing, Just <$> try ((,) <$> pToken Unless <*> debugName "UNLESS pBSR" pBSR))
        <|?> (Nothing, Just <$> try ((,) <$> pToken And    <*> debugName "AND pBSR"    pBSR))
        <|?> (Nothing, Just <$> try ((,) <$> pToken Or     <*> debugName "OR pBSR"     pBSR))
      debugPrint $ "ambitious: got back ifLimb     " ++ show ifLimb
      debugPrint $ "ambitious: got back unlessLimb " ++ show unlessLimb
      debugPrint $ "ambitious: got back andLimb    " ++ show andLimb
      debugPrint $ "ambitious: got back orLimb     " ++ show orLimb
      let clauses = [HC2 (RPConstraint subject RPis object)
                     (maybe (Just $ AA.Leaf $ RPMT ["always"])
                      (Just . snd) $ mergePBRS (catMaybes [ifLimb,andLimb,orLimb,fmap AA.Not <$> unlessLimb]))]
      return (Just keyword, subject, clauses)

    -- without the decide
    --        X IS y             -- nextlinewhen
    --   WHEN Z IS Q

    --        X IS Y WHEN Z IS Q -- samelinewhen
    someStructure = debugName "pHornlike/someStructure" $ do
      keyword <- optional $ choice [ pToken Decide ]
      (relPred, whenpart) <- debugName "pHornlike/someStructure going for the WHEN" $ manyIndentation (try relPredNextlineWhen <|> relPredSamelineWhen)
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

-- [TODO] unify these two if possible
relPredNextlineWhen :: Parser (RelationalPredicate, Maybe BoolStructR)
relPredNextlineWhen = debugName "relPredNextlineWhen" $ do
  (x,y) <- debugName "pRelPred , whenCase" ((,) <$> pRelPred <*> optional whenCase)
  return (x, join y)

relPredSamelineWhen :: Parser (RelationalPredicate, Maybe BoolStructR)
relPredSamelineWhen = debugName "relPredSamelineWhen" $
                      (,) $*| slRelPred
                      |>< (join <$> (debugName "optional whenCase -- but we should still consume GoDeepers before giving up" $
                                     optional whenCase))

whenCase :: Parser (Maybe BoolStructR)
whenCase = debugName "whenCase" $ do
  try (whenIf *> (Just <$> pBSR))
  <|> Nothing <$ (debugName "Otherwise" $ pToken Otherwise)

meansIs :: Parser MyToken
meansIs = debugName "meansIs" $ choice [ pToken Means, pToken Is ]

whenIf :: Parser MyToken
whenIf = debugName "whenIf" $ choice [ pToken When, pToken If ]
-- i think we need to distinguish WHEN/IF from MEANS/IS.
-- WHEN/IF  puts a BoolStructR in the hBody
-- MEANS/IS puts a RelationalPredicate in the hHead


slRelPred :: SLParser RelationalPredicate
slRelPred = debugName "slRelPred" $ do
        try ( debugName "RPConstraint"  rpConstraint )
    <|> try ( debugName "RPBoolStructR" rpBoolStructR )
    <|> try ( debugName "nested simpleHorn"  nestedHorn )
    -- we don't really have a rpParamText per se, do we? this is why line 78 and 79 of the pdpadbno are commented out.
    <|> try ( debugName "RPMT"          rpMT )

-- we'll return an RPMT, but write a nested simple hornlike rule to the Parser writer monad
nestedHorn :: SLParser RelationalPredicate
nestedHorn = do
  srcref <- liftSL getSrcRef
  (subj, meansTok, bsr) <- (,,)
                               $*| slMultiTerm
                               |^| liftSL meansIs
                               |-| pBSR
  let simpleHorn = Hornlike { name = subj
                            , super = Nothing
                            , keyword = meansTok
                            , given = Nothing
                            , upon = Nothing
                            , clauses = [ HC2 (RPBoolStructR subj RPis bsr) Nothing ]
                            , rlabel = Nothing
                            , lsource = Nothing
                            , srcref = Just srcref
                            , defaults = []
                            , symtab = [] }
  debugPrint "constructed simpleHorn; running tellIdFirst"
  _ <- liftSL $ tellIdFirst (return simpleHorn)
  return (RPMT subj)

  
rpMT :: SLParser RelationalPredicate
rpMT          = RPMT          $*| slAKA slMultiTerm id
rpConstraint :: SLParser RelationalPredicate
rpConstraint  = RPConstraint  $*| slMultiTerm |>| tok2rel |*| slMultiTerm

rpBoolStructR :: SLParser RelationalPredicate
rpBoolStructR = RPBoolStructR $*| slMultiTerm |>| tok2rel |>| pBSR
-- then we start with entire relationalpredicates, and wrap them into BoolStructR

pBSR :: Parser BoolStructR
pBSR = debugName "pBSR" $ prePostParse pRelPred


