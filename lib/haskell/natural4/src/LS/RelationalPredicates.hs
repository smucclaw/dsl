{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-|
This module provides parser and utility functions for the RelationalPredicate group of types.

= Why Relational Predicates?

L4 follows in the logic programming tradition, which uses first-order logic to organize information.

L4's controlled natural language interface attempts to give a
user-friendly gloss to predicate syntax, but under the hood, if you
are familiar with Prolog, you will recognize the Horn clauses under the skin.

== arity

=== How do we handle zero-argument predicates?

Prolog:

@
fact.
@

L4:

@
DECIDE fact
@

Prolog:

@
head :- body.
@

L4:

@
DECIDE head
  WHEN body
@

=== How do we handle single-argument predicates?

Prolog:

@
someAttribute(someEntity).
@

L4:

@
DECIDE someEntity someAttribute
@

Example:

Prolog:

@
isBlue(theSky).
@

L4:

@
DECIDE theSky isBlue
@

With bodies:

Prolog:

@
isBlue(theSky) :- duringDay(Time).
@

L4:

@
DECIDE theSky isBlue
  WHEN Time duringDay
@


=== How do we handle two-argument predicates?

Prolog:

@
isParentOf(alice,bob).
@

L4:

@
DECIDE alice isParentOf bob
@


=== How do we handle three-or-more-argument predicates?

Prolog:

@
isChildOf(bob,alice,carol).
@

L4:

@
DECIDE bob isChildOf alice carol
@

== Syntactic Sugar

=== IS True

Before:

@
DECIDE head IS True
  WHEN body
@

After:

@
DECIDE head
  WHEN body
@

=== IS False

Before:

@
DECIDE head IS False
  WHEN body
@

After:

@
DECIDE NOT head
  WHEN body
@

=== IS Attribute

Before:

    @
    DECIDE theSky IS Blue
      WHEN Time IS duringDay
    @

Rewrite:

    @
    isBlue(theSky) :- isDuringDay(Time).
    blue(theSky) :- duringDay(Time).
    @

We still need to decide on some conventions here. Maybe we do multiple forms just to maximize confusion?

== Reasoning with Relational Predicates

== Different Forms of RelationalPredicates

See the `RelationalPredicate` definition below for a detailed discussion of each constructor.

== BoolStructs of RelationalPredicates

We use the `AnyAll` library to combine RelationalPredicates.

[TODO] we will likely need to upgrade the head of a HornClause2 to BoolStructR if we want to allow NOT semantics.

This opens the door to having multiple heads in a single Horn clause, which is something that more advanced Prologs do, e.g. Flora.

== Horn Clauses

How everything above fits together into a HornClause2 structure

== How Other Decision Elements Fit In To A Rule

=== Regulative WHO and WHICH

=== Constitutive WHEN

@DECIDE@ x IS y
  @WHEN@ z IS q

= Future Work

We don't have a first-class way of talking about implication at the moment -- the "MUST BE" pattern.




-}

module LS.RelationalPredicates where

import Text.Megaparsec
import Control.Monad.Writer.Lazy
import Text.Parser.Permutation
import qualified AnyAll as AA
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( fromList, toList, nonEmpty, NonEmpty(..)  )
import qualified Data.Foldable as DF
import Data.Maybe (fromMaybe, catMaybes, maybeToList)
import Data.Semigroup (sconcat)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

import LS.Types
import LS.Rule
import LS.Tokens
import LS.Parser
import AnyAll.BoolStructTree (BoolStructDT, Formula (FAtom, FAll, FAny), mkAnyDT, mkAllDT)
import Data.Tree


-- * parse RelationalPredicates


pRelationalPredicate :: Parser RelationalPredicate
pRelationalPredicate = pRelPred

-- can we rephrase this as Either or Maybe so we only accept certain tokens as RPRels?
tok2rel :: Parser RPRel
tok2rel = choice
    [ RPis      <$ pToken Is      
    , RPhas     <$ pToken Has
    , RPeq      <$ pToken TokEQ   
    , RPlt      <$ pToken TokLT   
    , RPlte     <$ pToken TokLTE  
    , RPgt      <$ pToken TokGT   
    , RPgte     <$ pToken TokGTE  
    , RPelem    <$ pToken TokIn   
    , RPnotElem <$ pToken TokNotIn
    , RPnotElem <$ pToken TokNotIn
    , (RPTC TBefore) <$ pToken Before
    , (RPTC TAfter)  <$ pToken After
    , (RPTC TBy)     <$ pToken By
    , (RPTC TOn)     <$ pToken On
    , (RPTC TVague)  <$ pToken Eventually
    ]

rpConstitutiveAsElement :: Rule -> BoolStructR
rpConstitutiveAsElement = multiterm2bsr

rpHornlikeAsElement :: Rule -> BoolStructR
rpHornlikeAsElement =  multiterm2bsr

rpLeafVal :: Parser BoolStructR
rpLeafVal = debugName "rpLeafVal" $ do
  leafVal <- pRelationalPredicate
  myTraceM $ "rpLeafVal returning " ++ show leafVal
  return $ AA.mkLeaf leafVal

-- | in the body of a HornClause, any elements which are defined with a type signature are considered local/private existential variables internal to the body.
-- we partition the body of the Horn Clause into such existential variables, vs the rest of the logic.
partitionExistentials :: HornClause2 -> (BoolStructR, BoolStructR)
partitionExistentials c = ( aaFilter (\case { AA.Leaf (RPParamText x) ->     (hasTypeSig x) ; _ -> False }) (hc2preds c)
                          , aaFilter (\case { AA.Leaf (RPParamText x) -> not (hasTypeSig x) ; _ -> True  }) (hc2preds c) )
    where
      aaFilter :: (AA.BoolStruct lbl a -> Bool) -> AA.BoolStruct lbl a -> AA.BoolStruct lbl a
      aaFilter f (AA.Any lbl xs) = AA.mkAny lbl (filter f (aaFilter f <$> xs))
      aaFilter f (AA.All lbl xs) = AA.mkAll lbl (filter f (aaFilter f <$> xs))
      aaFilter f x = if f x then x else x -- not super great, should really replace the else with True or False or something?

partitionExistentialsDT :: HornClauseDT -> (BoolStructDTR, BoolStructDTR)
partitionExistentialsDT c = ( aaFilter (\case { Node (FAtom (RPParamText x)) _ ->     (hasTypeSig x) ; _ -> False }) (hc2preds c)
                            , aaFilter (\case { Node (FAtom (RPParamText x)) _ -> not (hasTypeSig x) ; _ -> True  }) (hc2preds c) )
    where
      aaFilter :: (BoolStructDT lbl a -> Bool) -> BoolStructDT lbl a -> BoolStructDT lbl a
      aaFilter f (Node (FAny lbl) xs) = mkAnyDT lbl (filter f (aaFilter f <$> xs))
      aaFilter f (Node (FAll lbl) xs) = mkAllDT lbl (filter f (aaFilter f <$> xs))
      aaFilter f x = if f x then x else x -- not super great, should really replace the else with True or False or something?

-- extract the ParamTexts from the existentials for use as "let" bindings. When extracting to CoreL4 they are basically treated as universals in the GIVEN part.
bsr2pt :: BoolStructR -> Maybe ParamText
bsr2pt bsr =
  let ptlist = [ pt | RPParamText pt <- DF.toList bsr ]
  in if ptlist == []
     then Nothing
     else Just $ sconcat $ fromList ptlist
-- we convert multiple ParamText to a single ParamText because a ParamText is just an NE of TypedMulti anyway    

-- At this time, none of the preconditions should be found in the head, so we ignore that.
hc2preds :: (MyBSR a) => HornClause a -> a
hc2preds (HC _headRP Nothing) = mkBSRLeaf (RPMT [MTT "TRUE"]) -- [TODO] turn this into MTB True
hc2preds (HC _headRP (Just bsr)) = bsr

aaLeaves :: BoolStructR -> [MultiTerm]
aaLeaves = aaLeavesFilter (const True)

aaLeavesFilter :: (RelationalPredicate -> Bool) -> BoolStructR -> [MultiTerm]
aaLeavesFilter f (AA.All _ xs) = concatMap (aaLeavesFilter f) xs
aaLeavesFilter f (AA.Any _ xs) = concatMap (aaLeavesFilter f) xs -- these actually need to be treated differently -- i think the Any needs a join transition in the Petri net? revisit this when more awake and thinking more clearly.
aaLeavesFilter f (AA.Not x) = aaLeavesFilter f x
aaLeavesFilter f (AA.Leaf rp) = if f rp then rp2mts rp else []
  where
    rp2mts :: RelationalPredicate -> [MultiTerm]
    rp2mts (RPMT mt)                     = [mt]
    rp2mts (RPParamText    pt)           = [pt2multiterm pt]
    rp2mts (RPConstraint  _mt1 _rpr mt2) = [mt2]
    rp2mts (RPBoolStructR _mt1 _rpr bsr) = aaLeavesFilter f bsr
    rp2mts (RPnary        _rprel rps)    = [rp2mt rps]

  
-- this is probably going to need cleanup
addneg :: Maybe BoolStructR -> Maybe BoolStructR -> Maybe BoolStructR
addneg Nothing  Nothing   = Nothing
addneg Nothing  (Just n)  = Just $ AA.mkNot n
addneg (Just p) (Just n)  = Just $ AA.mkAll Nothing [p, AA.mkNot n]
addneg (Just p) Nothing   = Just p

-- combine all the boolrules under the first preamble keyword
mergePBRS :: [(Preamble, BoolStructR)] -> Maybe (Preamble, BoolStructR)
mergePBRS [] = Nothing
mergePBRS [x] = Just x
mergePBRS xs         = Just (fst . head $ xs, AA.mkAll Nothing (snd <$> xs))

c2hornlike :: Rule -> Rule
c2hornlike Constitutive { name, keyword, letbind, cond, given, rlabel, lsource, srcref, defaults, symtab } =
  let clauses = pure $ HC (RPBoolStructR name RPis letbind) cond
      upon = Nothing
  in Hornlike { name, super = Nothing, keyword, given, upon, clauses, rlabel, lsource, srcref, defaults, symtab }
c2hornlike r = r

-- | there is some overlap with pHornlike. We should probably merge these two to a single rule.
--          Thing A
--  MEANS   Thing B
--  IF      Condition C

pConstitutiveRule :: Parser Rule
pConstitutiveRule = debugName "pConstitutiveRule" $ do
  maybeLabel <- optional pRuleLabel -- TODO: Handle the SL
  leftY              <- lookAhead pYLocation
  namep              <- debugName "calling someIndentation pNameParens, skipping over invisible DEFINE keyword" $ someIndentation pNameParens
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
                           -- preamble = copula   (means)
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
pHornlike = pHornlike' True

-- and sometimes, when pHornlike is being used to parse the WHERE limb of a regulative rule, we say we don't need the DEFINE/DECIDE keyword;
-- this tries to give the behaviour of the ambitious parser but in a someStructure parser.
pHornlike' :: Bool -> Parser Rule
pHornlike' needDkeyword = debugName ("pHornlike(needDkeyword=" <> show needDkeyword <> ")") $ do
  (rlabel, srcref) <- debugName "pHornlike pSrcRef" (slPretendEmpty pSrcRef)
  let dKeyword = if needDkeyword
                 then Just <$> choice [ pToken Decide ] -- [TODO] try allowing DEFINE in future, for things like simple-constitutive-1
                 else Nothing <$ pure ()
  let permutepart = debugName "pHornlike / permute" $ permute $ (,,,)
        <$$> -- (try ambitious <|> -- howerever, the ambitious parser is needed to handle "WHERE  foo IS bar" inserting a hornlike after a regulative.
               someStructure dKeyword -- we are trying to keep things more regular. to eliminate ambitious we need to add the unless/and/or machinery to someStructure, unless the pBSR is equal to it
             -- )
        <|?> (Nothing, Just . snd <$> givenLimb)
        <|?> (Nothing, Just . snd <$> uponLimb)
        <|?> (Nothing, whenCase)
        -- [TODO] refactor the rule-label logic to allow outdentation of rule label line relative to main part of the rule
  ((keyword, name, clauses), given, upon, topwhen) <- permutepart
  return $ Hornlike { name
                    , super = Nothing -- [TODO] need to extract this from the DECIDE line -- can we involve a 'slAka' somewhere downstream?
                    , keyword = fromMaybe Means keyword
                    , given
                    , clauses = addWhen topwhen clauses
                    , upon, rlabel, srcref
                    , lsource = noLSource
                    , defaults = mempty, symtab = mempty
                    }
  where
    addWhen :: Maybe BoolStructR -> [HornClause2] -> [HornClause2]
    addWhen mbsr hcs = [ -- trace ("addWhen running, appending to hBody = " <> show (hBody hc2)) $
                         -- trace ("addWhen running, appending the mbsr " <> show mbsr) $
                         hc2 { hBody = hBody hc2 <> mbsr }
                       | hc2 <- hcs ]

    -- this is actually kind of a meta-rule, because it really means
    -- assert(X :- (Y1, Y2)) :- body.

    -- DECIDE X IS y
    --   WHEN Z IS Q

    -- ambitious dKeyword = debugName "pHornlike/ambitious" $ do
    --   (keyword, subject) <- (,) $>| debugName "Decide" (choice [ pToken Decide ]) |*< slMultiTerm
    --   (iswhen, object)   <- (,) $>| debugName "When/Is"       (choice [ pToken When,   pToken Is     ]) |>< pNameParens
    --   (ifLimb,unlessLimb,andLimb,orLimb) <- debugName "pHornlike/ambitious / clauses permute" $ permute $ (,,,)
    --     <$?> (Nothing, Just <$> try ((,) <$> pToken If     <*> debugName "IF pBSR"     pBSR))
    --     <|?> (Nothing, Just <$> try ((,) <$> pToken Unless <*> debugName "UNLESS pBSR" pBSR))
    --     <|?> (Nothing, Just <$> try ((,) <$> pToken And    <*> debugName "AND pBSR"    pBSR))
    --     <|?> (Nothing, Just <$> try ((,) <$> pToken Or     <*> debugName "OR pBSR"     pBSR))
    --   debugPrint $ "ambitious: got back ifLimb     " ++ show ifLimb
    --   debugPrint $ "ambitious: got back unlessLimb " ++ show unlessLimb
    --   debugPrint $ "ambitious: got back andLimb    " ++ show andLimb
    --   debugPrint $ "ambitious: got back orLimb     " ++ show orLimb
    --   let clauses = [HC2 (RPConstraint subject RPis object)
    --                  (maybe (Just $ AA.Leaf $ RPMT ["always"])
    --                   (Just . snd) $ mergePBRS (catMaybes [ifLimb,andLimb,orLimb,fmap AA.Not <$> unlessLimb]))]
    --   return (Just keyword, subject, clauses)

    -- without the decide
    --        X IS y             -- nextlinewhen
    --   WHEN Z IS Q

    --        X IS Y WHEN Z IS Q -- samelinewhen
    someStructure dKeyword = debugName "pHornlike/someStructure" $ do
      keyword <- dKeyword -- usually testing for pToken Define or Decide or some such, but sometimes it's not needed, so dKeyword is a Nothing parser
      relwhens <- (if keyword == Nothing then manyIndentation else someIndentation) $ sameDepth rpSameNextLineWhen
      return (keyword
             , inferRuleName (fst . head $ relwhens)
             , [HC relPred whenpart
               | (relPred, whenpart) <- relwhens ])


    givenLimb = debugName "pHornlike/givenLimb" $ preambleParamText [Given]
    uponLimb  = debugName "pHornlike/uponLimb"  $ preambleParamText [Upon]

    inferRuleName :: RelationalPredicate -> RuleName
    inferRuleName (RPParamText pt)       = pt2multiterm pt
    inferRuleName (RPMT mt)              = mt
    inferRuleName (RPConstraint  mt _ _) = mt
    inferRuleName (RPBoolStructR mt _ _) = mt
    inferRuleName (RPnary     _rprel rp) = inferRuleName rp

rpSameNextLineWhen :: Parser (RelationalPredicate, Maybe BoolStructR)
rpSameNextLineWhen = slRelPred |&| (fmap join <$> liftSL $ optional whenCase)

pRelPred :: Parser RelationalPredicate
pRelPred = debugName "pRelPred" $ do
  slRelPred |<$ undeepers


-- foo IS bar                   Nothing                                becomes a fact
-- foo IS bar WHEN baz          Just Leaf baz                          becomes a body to the horn clause
-- foo IS bar OTHERWISE         Just Leaf __OTHERWISE__                becomes a default case, which feels like a fact, but isn't.
whenCase :: Parser (Maybe BoolStructR)
whenCase = debugName "whenCase" $ do
  try (whenIf *> (Just <$> pBSR))
--  <|> Nothing <$ debugName "Otherwise" (pToken Otherwise)
  <|> Just (AA.mkLeaf (RPMT [MTT "OTHERWISE"])) <$ debugName "Otherwise" (pToken Otherwise) -- consider RPDefault

whenIf :: Parser MyToken
whenIf = debugName "whenIf" $ choice [ pToken When, pToken If ]
-- i think we need to distinguish WHEN/IF from MEANS/IS.
-- WHEN/IF  puts a BoolStructR in the hBody
-- MEANS/IS puts a RelationalPredicate in the hHead


-- | sameline parser for a single RelationalPredicate
-- RelationalPredicates come in four forms. See Types.hs for documentation.
-- We add the ability to do nested hornlike rules inline, in the midst of some of these forms,
-- which is how you get support for the MEANS stuff that shows up sometimes in the middle of a relationalpredicate.

slRelPred :: SLParser RelationalPredicate
slRelPred = debugName "slRelPred" $ do
  try ( debugName "slRelPred/RPConstraint"  rpConstraint )
    <|> try ( debugName "slRelPred/RPBoolStructR" rpBoolStructR )
    <|> try ( debugName "slRelPred/nested simpleHorn" $ RPMT <$> mustNestHorn id id meansIsWhose pBSR slMultiTerm) -- special case, do the mustNestHorn here and then repeat the nonesthorn below.
    -- we don't really have a rpParamText per se, do we? this is why line 78 and 79 of the pdpadbno are commented out.
    <|> try ( debugName "slRelPred/RPParamText (with typesig)" rpParamTextWithTypesig )
    <|> try ( debugName "slRelPred/RPMT"          rpMT )
    -- <|> try ( debugName "slRelPred/RPParamText (to MT) (without typesig)" $ do
    --             pt <- slParamText
    --             if NE.length pt == 1
    --               then return $ RPMT (toList $ NE.head $ untypePT pt)
    --               else return $ RPParamText pt
    --         )

-- nuParamText :: SLParser ParamText
-- nuParamText = sameDepth slKeyValuesAka

rpParamTextWithTypesig :: SLParser RelationalPredicate
rpParamTextWithTypesig = do
  pt <- slParamText
  if hasTypeSig pt
    then return $ RPParamText pt
    else empty

 
rpMT :: SLParser RelationalPredicate
rpMT          = RPMT          $*| slAKA slMultiTerm id

rpConstraint :: SLParser RelationalPredicate
rpConstraint  = nestedHorn rpHead id meansIs pBSR (RPConstraint $*| slMultiTerm |>| tok2rel |*| slMultiTerm)

rpBoolStructR :: SLParser RelationalPredicate
rpBoolStructR = RPBoolStructR $*| slMultiTerm |>| tok2rel |>| pBSR
-- then we start with entire relationalpredicates, and wrap them into BoolStructR



-- this used to be in LS/ParamText.hs


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

-- | parse a ParamText
-- [TODO] the recursive structure of a HAS limb in a TypeDecl means that we need to mirror the same structure in a RPParamText, see pHornlike.
-- so a ParamText needs to be able to contain more ParamText; it's going to have to become a Tree
pParamText :: Parser ParamText
pParamText = pParamTextSameDepthOK

pParamText' :: Bool -> Parser ParamText
pParamText' mustIndent = do
  debugName ("pParamText " <> if mustIndent then "(subsequent lines must be indented)" else "(subsequent lines may be at same depth)") $
    (:|)
    <$> debugName "pParamText(flat) first line: pKeyValues" pKeyValuesAka <* optional (pToken EOL)
    <*> debugName "pParamText(flat) subsequent lines: sameMany pKeyValues"
    (try (someIndentation (sameMany pKeyValuesAka)) -- maybe the subsequent lines are indented; consume the indentation first.
     <|> if mustIndent then pure [] else
           manyIndentation (sameMany pKeyValuesAka))      -- consuming the indentation first is important because sameMany can over-return success on nothing.

pParamTextMustIndent :: Parser ParamText
pParamTextMustIndent = pParamText' True

pParamTextSameDepthOK :: Parser ParamText
pParamTextSameDepthOK = pParamText' False

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

-- | extract one or more type signatures from a paramtext, which, as you recall, could have multiple lines, each of which has their own typesig.
pt2typesigs :: ParamText -> [TypeSig]
pt2typesigs pt = mapMaybe snd (toList pt)

hasTypeSig :: ParamText -> Bool
hasTypeSig ((_,Nothing) :| _) = False
hasTypeSig ((_,_      ) :| _) = True
    

pTypeSig :: Parser TypeSig
pTypeSig = debugName "pTypeSig" $ do
  _           <- choice [ try (pToken TypeSeparator <* pToken A_An)
                        , pToken TypeSeparator
                        , pToken Is
                        ]
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
pOneOf = pToken OneOf *> someIndentation (fromList . concatMap toList <$> sameDepth pParamText)
                                         -- i thought we could use sequence, but i guess not?

-- sometimes we want a multiterm, just a list of text
pMultiTermAka :: Parser MultiTerm
pMultiTermAka = debugName "pMultiTermAka" $ pAKA slMultiTerm id

-- head of nonempty list
pSingleTermAka :: Parser KVsPair
pSingleTermAka = debugName "pSingleTermAka" $ pAKA slTypedMulti (toList . fst)

pSingleTerm :: Parser KVsPair
pSingleTerm = debugName "pSingleTerm" $ (pure . MTT <$> pAnyText) `optIndentedTuple` pTypeSig

-- [TODO] rewrite this in terms of slKeyValuesAka
slParamText :: SLParser ParamText
slParamText = debugNameSL "slParamText" $ pure <$> slTypedMulti

-- so it turns out we usually don't even ever get here because a TYPICALLY gets handled by slAKA
slTypedMulti :: SLParser KVsPair
slTypedMulti = debugNameSL "slTypedMulti with TYPICALLY" $ do
  (l,ts,typicalval) <- (,,)
    $*| slMultiTerm
    |*| (|?|) slTypeSig
    |*| (|?|) typically
  liftSL $ writeTypically l typicalval
  return (fromList l, ts)

-- | record a TYPICALLY annotation.
--
-- if the annotation is originating in a "subjective" context -- i.e. a WHO or WHICH instead of a WHEN or IF -- the caller must include the name of the subject in the key.

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
pKeyValuesAka = debugName "pKeyValuesAka" $ finishSL slKeyValuesAka

slKeyValuesAka :: SLParser KVsPair
slKeyValuesAka = debugNameSL "slKeyValuesAka" $ slAKA slKeyValues (toList . fst)

pKeyValues :: Parser KVsPair
pKeyValues = debugName "pKeyValues" $ do slKeyValues |<$ undeepers

-- | a ParamText key value pair is simply a (key : [v1,v2,v3]).
-- we use nestedHorn to allow a MEANS under the v1.
slKeyValues :: SLParser KVsPair
slKeyValues = debugNameSL "slKeyValues" $ do
  (lhs, (rhs, typesig))   <- try (
    (,) -- key followed by values, and the values can sit on top of a MEANS
      $>| pOtherVal
      ->| 1
      |*| nestedHorn fst id meansIsWhose pBSR
           ((,) $>| someDeep pMTExpr |*| (|?|) slTypeSig))
    <|> -- key without values, so we put the MEANS under the key
    nestedHorn (pure . MTT . fst) id meansIsWhose pBSR
    ((\l rt -> (l,([],rt)))
     $>| pOtherVal
     |*| (|?|) slTypeSig)
  return (fromList (MTT lhs : rhs), typesig)


getSrcRef :: Parser SrcRef
getSrcRef = do
  leftY  <- lookAhead pYLocation
  leftX  <- lookAhead pXLocation
  srcurl <- asks sourceURL
  return $ SrcRef srcurl srcurl leftX leftY Nothing


-- utility function for the above
pAKA :: (Show a) => SLParser a -> (a -> MultiTerm) -> Parser a
pAKA baseParser toMultiTerm = debugName "pAKA" $ do
  manyIndentation (slAKA baseParser toMultiTerm |<$ undeepers)

slAKA :: (Show a) => SLParser a -> (a -> MultiTerm) -> SLParser a
slAKA baseParser toMultiTerm = debugNameSL "slAKA" $ do
  (base, entityalias, typicalval) <-
    debugNameSL "slAKA with nestedHorn" $ nestedHorn (toMultiTerm.fst3) id meansIs pBSR $
    debugNameSL "slAKA base (multiterm,aka,typically)" $
    (,,)
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
    fst3 (x,_,_) = x
    akapart :: SLParser RuleName
    akapart = debugName "PAKA/akapart" $ do
      (_akatoken, akaval) <- (,)
                                $>| debugName "Aka Token" (pToken Aka)
                                |*| someLiftSL pOtherVal
      return (MTT <$> akaval)

-- | parse a TYPICALLY annotation and return its value.
--
-- You would expect the value to be able to be TRUE or FALSE
-- but here we are constrained to MultiTerm.
-- [TODO] this should change in the future to allow a mix of MultiTerm and True/False values.
typically :: SLParser MultiTerm
typically = debugName "typically" $ do
  (_typically, someterm) <- (,)
                                $>| pToken Typically
                                |*| slMultiTerm
  return someterm


-- | we'll return an RPMT as requested, but if there's a MEANS immediately below,
-- we'll also write a nested simple hornlike rule to the Parser writer monad
mustNestHorn, nestedHorn
  :: (Show a)
  => (a -> MultiTerm)   -- ^ turn the thing into the inner Hornlike's clause hornhead MultiTerm
  -> (MultiTerm -> RuleName)    -- ^ turn the thing into the inner Hornlike's RuleName
  -> Parser MyToken     -- ^ the connector, usually meansIs
  -> Parser BoolStructR -- ^ parser for the thing after the connector, usually pBSR
  -> SLParser a
  -> SLParser a
nestedHorn toMT toRN connector pbsr basesl =
  try (mustNestHorn toMT toRN connector pbsr basesl)
    <|> noNested
  where
    noNested = debugName "noNested horn clause, defaulting to base" basesl


mustNestHorn toMT toRN connector pbsr basesl =
  debugNameSL "trying hasNested" $ do
  srcref   <- liftSL getSrcRef
              |-- (\n -> debugPrint $ "mustNestHorn before basesl: " ++ show n ++ " UnDeepers")
  (subj, meansTok, bsr) <- (,,)
                           $*| basesl
                           |-- (\n -> debugPrint $ "mustNestHorn after basesl: " ++ show n ++ " UnDeepers")
                           |<<| ()
                           |-- (\n -> debugPrint $ "mustNestHorn after undeepering: " ++ show n ++ " UnDeepers")
                           |-| connector
                           |-| pbsr

  -- the conceptual positioning of the cursor above is critical

  let simpleHorn = Hornlike { name = toRN (toMT subj)
                            , super = Nothing
                            , keyword = meansTok
                            , given = Nothing
                            , upon = Nothing
                            , clauses = [ HC (RPBoolStructR (toMT subj) RPis bsr) Nothing ]
                            , rlabel = Nothing
                            , lsource = Nothing
                            , srcref = Just srcref
                            , defaults = []
                            , symtab = [] }
  debugPrint "constructed simpleHorn; running tellIdFirst"
  _ <- liftSL $ tellIdFirst (return simpleHorn)
  return subj


meansIs,meansIsWhose :: Parser MyToken
meansIs = debugName "meansIs" $ choice [ pToken Means, pToken Is ]
meansIsWhose = choice $ pToken <$> [ Means, Is, Who, Whose ]

pBSR :: Parser BoolStructR
pBSR = debugName "pBSR" $ prePostParse pRelPred

-- | convert all decision logic in a rule to BoolStructR format.
--   the `who` of a regulative rule gets shoehorned into the head of a BoolStructR.
--   the `cond` of a regulative rule is passed along.
--   some rules don't hold decision logic so we return Nothing.
--   [TODO] There is some overlap here with getAndOrTree. Can we converge?
--
-- for regulative rules,
-- we rephrase a rule to add the subject of the rule to the RelationalPredicate.
--
-- +----------------------------+-------------------------------------+-----------------------+
-- | input                      | output                              | intended NLG question |
-- +============================+=====================================+=======================+
-- | EVERY Singer WHO walks     | RPMT [ "Singer", "walks" ]          | Does Singer walk?     |
-- +----------------------------+-------------------------------------+-----------------------+
--
-- in future we should also support
-- +----------------------------+-------------------------------------+-----------------------+
-- | input                      | output                              | intended NLG question |
-- +============================+=====================================+=======================+
-- | EVERY Singer WHO IS Hungry | RPConstraint "Singer" RPis "Hungry" | Is Singer hungry?     |
-- +----------------------------+-------------------------------------+-----------------------+
--
getBSR :: Rule -> Maybe BoolStructR
getBSR Hornlike{..}   = Just $ AA.simplifyBoolStruct $ AA.mkAll Nothing $
                        catMaybes [ hbody | HC _hhead hbody <- clauses ] <//>
                        [ bsr | HC (RPBoolStructR _rp1 _rprel bsr) _hbody <- clauses ]
  where
    -- | monochrom on IRC commented that I'm basically doing Prolog's `cut`, here.
    -- I would have used (<||>) but that's already in use by the permutation parser
    (<//>) :: Foldable t => t a -> t a -> t a
    x <//> y = if not (null x) then x else y
    infix 1 <//>


getBSR Regulative{..} = Just $ AA.simplifyBoolStruct $ AA.mkAll Nothing $
                        maybeToList (prependSubject who) ++
                        maybeToList cond
  where
    prependSubject :: Maybe BoolStructR -> Maybe BoolStructR
    prependSubject mbsrwho = do
      whobsr <- mbsrwho
      return $ prependToRP [bsp2text subj] <$> whobsr
      where
        prependToRP :: [T.Text] -> RelationalPredicate -> RelationalPredicate
        prependToRP ts (RPMT        mt) = RPMT $ (MTT <$> ts) ++ mt
        prependToRP ts (RPParamText pt) = RPParamText (NE.fromList [ (myPrependList (MTT <$> ts) netext, mtypesig)
                                                                   | (netext, mtypesig) <- NE.toList pt ])
          where
            -- when we upgrade to base 4.17 we can use the real NE.prependList
            myPrependList pfix nelist = NE.fromList (pfix ++ NE.toList nelist)
        prependToRP ts (RPConstraint  mt1 rpr mt2) = RPConstraint  mt1 rpr ((MTT <$> ts) ++ mt2)
        prependToRP ts (RPBoolStructR mt1 rpr bsr) = RPBoolStructR mt1 rpr (prependToRP ts <$> bsr)
        prependToRP ts (RPnary        rprel rps)   = RPnary        rprel   (prependToRP ts  $  rps)

getBSR _              = Nothing

