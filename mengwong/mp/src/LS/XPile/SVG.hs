{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TransformListComp #-}

module LS.XPile.SVG where

import LS
import LS.XPile.Petri
import AnyAll as AA

import Data.GraphViz
import Data.GraphViz.Attributes
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import           Debug.Trace
import           Text.Pretty.Simple
import qualified Data.Text.Lazy     as Text
import           Data.Text.Lazy              (Text)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe                  (fromMaybe, listToMaybe, fromJust, isJust, maybeToList)
import qualified Data.Map           as Map
import Data.GraphViz.Printing (renderDot)
import Data.GraphViz.Attributes.Complete (Attribute(TailPort,HeadPort, Comment)
                                         , CompassPoint(..)
                                         , PortPos(..))
import Control.Monad.State.Strict (State, MonadState (get, put), evalState, runState, gets)
import Control.Applicative.Combinators
import Data.Foldable (find)
import System.IO.Unsafe (unsafePerformIO)
import WordNet.DB
import WordNet.Structured
import Data.List (isPrefixOf, sortOn, isSuffixOf)
import Text.EditDistance
import GHC.Base (join)
import Control.Monad (forM_)

data Deet = IsFirstNode | IsLastHappy | FromRuleAlias | IsParty | IsDeon | IsCond | IsThen
          | OrigRL Text
          | Temporal Text
          | IsInfra | HideIfChildless
          deriving (Eq,Show)

type PNodeD = PNode Deet
type PetriD = Petri Deet
type RuleSet = [Rule]

getRuleByName :: RuleSet -> RuleName -> Maybe Rule
getRuleByName rs rn = find (\r -> ruleName r == rn) rs

getRuleByLabel :: RuleSet -> Text -> Maybe Rule
getRuleByLabel rs t = find (\r -> (rl2text <$> rLabelR r) == Just t) rs

rLabelR :: Rule -> Maybe RuleLabel
rLabelR Regulative   {..} = rlabel
rLabelR Constitutive {..} = rlabel
rLabelR Hornlike     {..} = rlabel
rLabelR TypeDecl     {..} = rlabel
rLabelR Scenario     {..} = rlabel
rLabelR RuleGroup    {..} = rlabel
rLabelR _                 = Nothing

-- initially let's just draw the state diagram in a manner typical of GraphViz.
-- see the README

toPetri :: [Rule] -> Text.Text
toPetri rules =
  let petri1 = insrules rules startGraph
      rulesConnected = connectRules petri1 rules
      reordered = reorder rules rulesConnected
      merged = mergePetri rules reordered
  in renderDot $ unqtDot $ graphToDot (petriParams rulesConnected) merged

reorder :: [Rule] -> PetriD -> PetriD
reorder rules og = runGM og $ do
  -- x You if then must -> x if then you must
  forM_ [ (x0, you1, if2, then3, must4)
        | you1 <- nodes $ labfilter (hasDeet IsParty) og
        , x0            <- pre og you1
        , (if2,ifl)     <- lsuc og you1   , maybe False (hasDeet IsCond) (lab og if2)
        , (then3,thenl) <- lsuc og if2    , maybe False (hasDeet IsThen) (lab og then3)
        , (must4,mustl) <- lsuc og then3  , maybe False (hasDeet IsDeon) (lab og must4)
        ] $ \(x0, you1, if2, then3, must4) -> do
    delEdge' (x0, you1)
    delEdge' (    you1, if2)
    delEdge' (               then3, must4)
    newEdge' (x0,       if2, [Comment "reordered"])
    newEdge' (               then3, you1, [Comment "reordered"])
    newEdge' (                      you1, must4, [Comment "reordered"])


mergePetri :: [Rule] -> PetriD -> PetriD
mergePetri rules og = runGM og $ do
  -- rulealias split (x y 1 2 3) (x y 4 5 6) -> x y split (1 2 3) (4 5 6)
  return ()

fromRuleAlias :: Attribute
fromRuleAlias = Comment "from RuleAlias"

data SplitJoin = SJAny | SJAll deriving (Eq, Show)
splitJoin :: [Rule]      -- background input ruleset
          -> PetriD      -- original whole graph
          -> SplitJoin   -- whether we are doing an Any or an All wrapper
          -> PetriD    -- subgraphs which are to live together under the split/join.
          -> Node        -- entry point node that leads into the split
          -> PetriD      -- rewritten whole graph
splitJoin rs og sj sgs entry = runGM og $ do
  splitnode <- newNode (PN Trans "split (and)" [] [IsInfra])
  joinnode  <- newNode (PN Trans "join (and)" [] [IsInfra])
  let headsOfChildren = nodes $ labfilter (hasDeet IsFirstNode) sgs
      successTails    = [ n
                        | n <- nodes $ labfilter (hasDeet IsLastHappy) sgs
                        , m <- suc og n -- there is a direct link to FULFILLED
                        , m == fulfilledNode
                        ]
  newEdge' (entry,splitnode, [Comment "added by split from parent node"])
  newEdge' (joinnode,fulfilledNode, [Comment "added by join to fulfilledNode"])
  mapM_ newEdge' [ (splitnode, headnode, [Comment "added by split to headnode"]) | headnode <- headsOfChildren ]
  mapM_ newEdge' [ ( tailnode, joinnode, [Comment "added by join from tailnode"]) | tailnode <- successTails    ]
  mapM_ delEdge' [ ( tailnode, fulfilledNode ) | tailnode <- successTails ]

hasDeet :: Eq a => a -> PNode a -> Bool
hasDeet  x  (PN _ _ _ deets) =     x `elem` deets
hasDeets :: (Foldable t, Eq a) => t a -> PNode a -> Bool
hasDeets xs (PN _ _ _ deets) = all ( `elem` deets ) xs
addDeet :: PNode a -> a -> PNode a
addDeet  (PN a b c d) dt  = PN a b c (dt:d)
addDeets :: PNode a -> [a] -> PNode a
addDeets (PN a b c d) dts = PN a b c (dts++d)

getOrigRL :: PNodeD -> Maybe Text
getOrigRL (PN _ _ _ ds) = listToMaybe [ t | (OrigRL t) <- ds ]

connectRules :: PetriD -> [Rule] -> PetriD
connectRules sg rules =
  -- a node that is any HENCE GOTO RuleName will have the deet "FromRuleAlias"
  -- e.g. [label=Notification] [FromRuleAlias]
  -- and it will have no outdegeres
  --
  -- if the link can be fulfilled trivially, the r2fgl existing code already does that
  -- but the link might be to a Hornlike which expands to multiple rules
  --
  -- let's search for all ruleAlias nodes, and plan to expand them
  -- 

  let subgraphOfAliases = labfilter (hasDeet FromRuleAlias) sg
  -- another way to find it might be to look for head nodes that have no outdegrees

      aliasNodes = nodes subgraphOfAliases

      -- all labeled rules
      rls = trace "rls = " $ traceShowId $ fmap rl2text . rLabelR <$> rules

      -- if the ruleLabel is for a Hornlike, expand accordingly.
      -- we mirror the structure of the BoolStruct inside the head of the Hornlike,
      -- with combinations of split/join over AND/OR.

      -- later on we will probably want to use a join transition to model an OR.
      aliasRules = [ (n,outgraph)
                   | n <- aliasNodes
                   , let nrl = trace "OrigRL = " $ traceShowId $ getOrigRL =<< lab sg n
                   , let r = getRuleByLabel rules =<< nrl
                   , let outs = maybe [] (expandRule rules) r
                   , let rlouts = fmap rl2text <$> (rlabel <$> outs)
                   , let outgraph = labfilter (\pn -> any (\x -> x pn) [ hasDeet (OrigRL rlout')
                                                                       | rlout <- rlouts
                                                                       , isJust rlout
                                                                       , let rlout' = fromJust rlout
                                                                       ] ) sg
                   ]
      headNodes = [ headNode
                  | (orign, outgraph) <- aliasRules
                  , headNode <- nodes $ labfilter (hasDeet IsFirstNode) outgraph
                  ]
      tailNodes = [ tailNode
                  | (orign, outgraph) <- aliasRules
                  , tailNode <- nodes $ labfilter (hasDeet IsLastHappy) outgraph
                  ]
  in  trace (unlines ((\n -> "node " <> show n <> " is a ruleAlias: " <>
                        Text.unpack (maybe "(nothing)" showNode (lab subgraphOfAliases n)))
                       <$> aliasNodes))
      -- while our initial pass over rule expansion takes care of direct expansions, we need the Hornlike rules to expand also. bit of debugging, but we can get rid of this later when it works
      trace ("all known rulelabels are " ++ show rls)
      trace ("we need to expand RuleAlias nodes " ++ show aliasNodes)
      trace ("maybe they expand to headnodes " ++ show headNodes)
      trace ("and the outgraphs have happy tails " ++ show tailNodes)
      foldl (\g (n,outgraph) -> splitJoin rules g SJAll outgraph n) sg aliasRules
      -- now we set up the appropriate edges to the revealed rules, and delete the original rulealias node

expandRulesByLabel :: [Rule] -> Text -> [Rule]
expandRulesByLabel rules txt =
  let toreturn =
        [ q
        | r <- rules
        , let mt = rl2text <$> rLabelR r
        , Just txt == mt
        , let qs = expandRule rules r
        , q <- qs
        ]
  in trace ("expandRulesByLabel(" ++ show txt ++ ") about to return " ++ show (rlabel <$> toreturn))
     toreturn

expandRule :: [Rule] -> Rule -> [Rule]
expandRule rules r@Regulative{..} = [r]
expandRule rules r@Hornlike{..} =
  let toreturn =
        -- we support hornlike expressions of the form x is y and z; we return y and z
        [ q
        | clause <- clauses
        , let rlbl' = rl2text <$> rLabelR r
              bsr = trace ("expandRule: got head " ++ show (hHead clause)) $
                    hHead clause
        , isJust rlbl'
        , mt <- trace ("aaLeaves returned " ++ show (aaLeaves (AA.Leaf bsr))) $ aaLeaves (AA.Leaf bsr)
        -- map each multiterm to a rulelabel's rl2text, and if it's found, return the rule
        -- TODO: we have to add a layer of testing if each term returned is itself the name of a hornlike rulelabel
        -- for now we assume it is not.
        , q <- expandRulesByLabel rules (mt2text mt)
        ]
  in trace ("expandRule: called with input " ++ show rlabel)
     trace ("expandRule: about to return " ++ show (ruleName <$> toreturn))
     toreturn
expandRule _ _ = []


aaLeaves :: AA.Item RelationalPredicate -> [MultiTerm]
aaLeaves (AA.Leaf (RPMT mt) )       = [mt]
aaLeaves (AA.Leaf (RPParamText pt)) = [pt2multiterm pt]
aaLeaves (AA.Leaf (RPConstraint _mt1 _rpr mt2)) = [mt2]
aaLeaves (AA.Leaf (RPBoolStructR _mt1 _rpr bsr)) = aaLeaves bsr
aaLeaves (AA.All _ xs) = concatMap aaLeaves xs
aaLeaves (AA.Any _ xs) = concatMap aaLeaves xs -- these actually need to be treated differently -- i think the Any needs a join transition in the Petri net? revisit this when more awake and thinking more clearly.
aaLeaves (AA.Not _) = error "unable to handle a Not definition in expanding aaLeaves for expandRule"

showNode :: PNodeD -> Text
showNode (PN _ txt attrs deets) = txt <> " / " <> Text.unwords ((Text.pack . show <$> attrs) <>
                                                                (Text.pack . show <$> deets) )

nodeTxt :: PNodeD -> Text
nodeTxt (PN _ txt _ _) = txt

prefix :: Int -> Text.Text -> Text.Text
prefix n t = Text.pack (replicate n ' ') <> t

startGraph :: PetriD
startGraph = mkGraph [ (fulfilledNode, PN Place "FULFILLED" [] [IsInfra])
                     , (breachNode,    mkPlaceA [IsInfra] "BREACH"      ) ] []

fulfilledNode :: Node
fulfilledNode = 1

breachNode :: Node
breachNode = 0

getNodeByDeets :: PetriD -> [Deet] -> Maybe Node
getNodeByDeets gr ds = listToMaybe $ nodes $ labfilter (hasDeets ds) gr

insrules :: RuleSet -> PetriD -> PetriD
insrules rs sg = runGM sg $ mapM (r2fgl rs Nothing) rs

{-

Alternative imperative approach:

do
  node1 <- newNode
  node2 <- newNode
  let edge = (node1, node2, "")

-}

data GraphState = GS { lastNode :: Node, curentGraph :: PetriD }

newtype GraphMonad a = GM { runGM_ :: State GraphState a }
  deriving newtype (Functor, Applicative, Monad)

-- instance Semigroup a => Semigroup (GraphMonad a) where
--   a <> b = (<>) <$> a <*> b

-- instance Monoid a => Monoid (GraphMonad a) where
--   mempty = pure mempty

newNode :: PNodeD -> GraphMonad Node
newNode lbl = do
  gs@GS {lastNode = n, curentGraph = g} <- GM get
  let n' = succ n
  -- traceM $ "newNode: " <> show n' <> " " <> show lbl
  GM . put $ gs {lastNode = n' , curentGraph = insNode (n', lbl) g }
  return n'

newEdge :: Node -> Node -> PLabel -> GraphMonad ()
newEdge n1 n2 lbl = do
  gs@GS {curentGraph = g} <- GM get
  GM . put $ gs {curentGraph = insEdge (n1, n2, lbl) g }

newEdge' :: (Node, Node, PLabel) -> GraphMonad ()
newEdge' (a,b,c) = newEdge a b c

overwriteNode :: Node -> PNodeD -> GraphMonad Node
overwriteNode n pn = do
  gs@GS {curentGraph = g} <- GM get
  GM . put $ gs {curentGraph = insNode (n, pn) g }
  return n

delEdge' :: (Node, Node) -> GraphMonad ()
delEdge' (n1,n2) = do
  gs@GS {curentGraph = g} <- GM get
  GM . put $ gs {curentGraph = delEdge (n1, n2) g }

-- runGM :: PetriD -> GraphMonad a -> a
runGM :: PetriD -> GraphMonad a -> PetriD
runGM gr (GM m) = cg
-- runGM gr (GM m) = traceShow (neNodes res, neNodes cg) res
  where (_, n0) = nodeRange gr
        (_res, GS _ln cg) = runState m (GS n0 gr)

-- This is currently kind of inefficient, but when NE is replaced by a real graph, it becomes simpler and faster
getGraph :: GraphMonad PetriD
getGraph = do
    GM $ gets curentGraph

-- we convert each rule to a list of nodes and edges which can be inserted into an existing graph
r2fgl :: RuleSet -> Maybe Text -> Rule -> GraphMonad (Maybe Node)
r2fgl rs defRL RegFulfilled   = pure Nothing
r2fgl rs defRL RegBreach      = pure Nothing
-- what do we do with a RuleAlias? it only ever appears as the target of a Hence or a Lest,
-- so we just wire it up to whatever existing rule has been labeled appropriately.
-- however, if no existing rule in our list of rules bears that label (yet(, we put in a placeholder state.
-- the following function assumes the rulealias does not appear in the ruleset, so we are calling r2fgl as a last resort.
-- we will do another pass over the graph subsequently to rewire any rulealiases
r2fgl rs defRL (RuleAlias rn) = do
  sg <- getGraph
  let ntxt = Text.unwords rn
  let already = getNodeByDeets sg [IsFirstNode,OrigRL ntxt]
  maybe (fmap Just . newNode $
         mkPlaceA [IsFirstNode,FromRuleAlias,OrigRL ntxt] ntxt ) (pure . Just) already

r2fgl rs defRL r@Regulative{..} = do
  sg <- getGraph
  let myLabel = do
        rl <- rlabel
        return [IsFirstNode,OrigRL (rl2text rl), IsParty]
      origRLdeet = maybeToList (OrigRL <$> ((rl2text <$> rlabel) <|> defRL))
  let already = getNodeByDeets sg =<< myLabel

  let everywho = Text.unwords ( ( if keyword == Every then [ Text.pack (show keyword) ] else [] )
                                <> [ subj2nl NLen subj ] )

  let firstNodeLabel0 = case who of Nothing    -> mkPlace everywho
                                    Just _bsr  -> mkDecis everywho
      firstNodeLabel = maybe firstNodeLabel0 (addDeets firstNodeLabel0) myLabel
  everyN <- case already of
    Nothing -> newNode firstNodeLabel
    Just n  -> overwriteNode n firstNodeLabel
  whoN  <- case who of Nothing  -> pure everyN
                       Just bsr -> do whoN <- newNode $ mkTrans $ "who " <> bsr2text bsr
                                      newEdge everyN whoN []
                                      pure whoN
  upoN  <- case upon of Nothing -> pure whoN
                        Just pt -> do
                              uponN <- newNode $ mkPlace "upon"
                              uponCondN <- newNode $ mkTrans $ pt2text pt
                              newEdge' ( whoN,      uponN, [])
                              newEdge' ( uponN, uponCondN, [])
                              pure uponCondN
  conN  <- case cond of Nothing  -> pure upoN
                        Just bsr -> do
                            ifN     <- newNode $ (addDeet $ mkDecis ("if " <> bsr2textnl bsr)) IsCond
                            ifCondN <- newNode $ (addDeet $ mkTrans "then") IsThen
                            newEdge' ( upoN,    ifN, [] )
                            newEdge' ( ifN, ifCondN, [] )
                            pure ifCondN
  (onSuccessN, mbOnFailureN) <- do
    let deon = case deontic of { DMust -> "must"; DMay -> "may"; DShant -> "shant" }
        temp = tc2nl NLen temporal
        actn = actionFragments action
        oblLab = mkDecis (addnewlines [ deon
                                      , Text.unwords . NE.toList . fst . NE.head $ head actn
                                      , temp
                                      ])
        successLab = mkTransA ([Temporal temp, IsLastHappy] ++ origRLdeet) $
                     vp2np ( actionWord $ head $ actionFragments action) <> " " <> henceWord deontic

    obligationN <- newNode (addDeet oblLab IsDeon)
    onSuccessN <- newNode successLab
    mbOnFailureN <- if deontic /= DMay then do
        onFailureN <- newNode $ mkTrans $ lestWord deontic
        newEdge' ( obligationN, onFailureN, swport)
        pure (Just onFailureN)
      else pure Nothing
    -- let failureNE = NE [( onFailureN, mkTrans $ lestWord deontic ) | deontic /= DMay] [( obligationN, onFailureN, swport) | deontic /= DMay]
    newEdge' ( conN, obligationN, [] )
    newEdge' ( obligationN, onSuccessN, seport)
    pure (onSuccessN, mbOnFailureN)

  -- let sg1 = insertNE (dNE <> dtaNE) sg

  henceN <- fromMaybe fulfilledNode <$> maybe (pure Nothing) (r2fgl rs (rl2text <$> rlabel <|> defRL)) hence
  newEdge onSuccessN henceN []

  lestN  <- fromMaybe breachNode <$> maybe (pure Nothing) (r2fgl rs (rl2text <$> rlabel <|> defRL)) lest
  -- traceM $ "lestNEs: " <> show lestNEs
  -- let sg3 = insertNE lestNEs  sg2
      -- connect up the hence and lest bits
      -- the "hence" transition from dtaE should plug in to the first node in our henceContexts
  case mbOnFailureN of
    Just onFailureN -> newEdge onFailureN lestN []
    Nothing -> pure ()

  -- Return the first node
  pure $ Just whoN
  where
    vp2np "assess" = "assessment"
    vp2np "respond" = "response"
    vp2np x = x

    seport = [TailPort (CompassPoint SouthEast), Comment "southeast for positive"]
    swport = [TailPort (CompassPoint SouthWest), Comment "southwest for negative"]
    henceWord DMust  = "done"
    henceWord DMay   = "occurred"
    henceWord DShant = "avoided"
    lestWord DMust  = "not done"
    lestWord DMay   = error "a MAY has no LEST"  -- this should never arise
    lestWord DShant = "violation"

-- r2fgl rs r@Hornlike{} = pure Nothing
r2fgl rs defRL r = pure Nothing


-- TODO: make all this work so vp2np will use wordnet to generate the NPs
helper :: Text -> IO Text
helper ogWord = do
  -- Suppose that ogWord is "respond". The word "respond" belongs to three synsets, with the following glosses:
  -- 1) show a response or a reaction to something, 2) react verbally, 3) respond favorably.

  resultRaw <- getDerivations' $ Text.unpack ogWord :: IO [(Synset, [(SynsetLink, Synset)])]
  -- We get a list of derivs for "respond",
  -- and also three synsets for "respond":
  let
      hmm =
        [ (score, candidate, fromWord, toWord, defn derivSynset)
        | (ogSynset, derivs) <- resultRaw
        , (synsetLink, derivSynset) <- derivs -- Each of these
        , let fromWord = getWord ogSynset (lfrm synsetLink)
        , let toWord = getWord derivSynset (lto synsetLink)
        , isNoun derivSynset
        , not (isHuman derivSynset)
        , (score, candidate) <- sortBySimilarity ogWord derivSynset
        -- , not $ looksLikeHuman candidate
        , then sortOn by score
--        , then sortOn by
        , let sameWord = whichword ogSynset == lfrm synsetLink
        , let candidateEquals = candidate == toWord
        ]
      --candidateDerivations = sortBySimilarity ogWord
  mapM_ print hmm
  pure $ Text.pack $ (\(a,b,c,d,e) -> b) $ head hmm

-- sortingFun :: Bool -> Bool -> – -> Bool
-- sortingFun responseFollowsRespond candidateEqualsX _ = _

isHuman :: Synset -> Bool
isHuman synset = or [pref `isPrefixOf` def | pref <- humanPrefixes] || aPersonWho (words def)
  where
    def = defn synset
    humanPrefixes = ["(a person", "(someone", "(one who"]
    aPersonWho ("(a":_:"who":_) = True
    aPersonWho ("(an":_:"who":_) = True
    aPersonWho _ = False

looksLikeHuman :: String -> Bool
looksLikeHuman w = "or" `isSuffixOf` w || "ee" `isSuffixOf` w

isNoun :: Synset -> Bool
isNoun Synset {pos=Noun} = True
isNoun _ = False

type SimilarityScore = Int

sortBySimilarity :: Text -> Synset -> [(SimilarityScore, String)]
sortBySimilarity w synset =
  [ (score, candidate)
  | candidate <- sWords synset
  , let score = levenshteinDistance defaultEditCosts w' candidate
  ]
  where w' = Text.unpack w

exampleVerbs :: [Text]
exampleVerbs = map Text.toLower
  ["Achieve", "Assemble", "Accelerate", "Administer", "Allow",
   "Apply", "Appear", "Appoint", "Analyze", "Budget", "Buy",
   "Balance", "Bring", "Build", "Chase", "Check", "Choose", "Close",
   "Collaborate", "Collect", "Comment", "Communicate", "Compare",
   "Convince", "Continue", "Coordinate", "Cut", "Debate", "Defend",
   "Decide", "Discover", "Eat", "Encourage", "Establish", "Earn",
   "Examine", "Expect", "Experiment", "Explain", "Explore", "Fall",
   "Feed", "Fry", "Fight", "Fit", "Follow", "Go", "Give", "Grow",
   "Gain", "Generate", "Hang", "Happen", "Hate", "Hear", "Howl",
   "Hop", "Hug", "Help", "Hold", "Hurt", "Hide", "Identify", "Ignore",
   "Imply", "Illustrate", "Inform", "Include", "Introduce", "Invest",
   "Irritate", "Jog", "Joke", "Jump", "Judge", "Keep", "Knock",
   "Kick", "Kill", "Laugh", "Learn", "Lay", "Leave", "Lie", "Live",
   "Lose", "Listen", "Lift", "Love", "Like", "Make", "Manage",
   "Maintain", "Measure", "Meet", "Mix", "Mention", "Melt", "Move",
   "Need", "Negotiate", "Observe", "Obtain", "Order", "Offer", "Open",
   "Own", "Paint", "Pass", "Pay", "Performed", "Persist", "Promise",
   "Play", "Pinch", "Parse", "Participate", "Provide", "Put", "Pull",
   "Quit", "Quack", "Qualify", "Raise", "Read", "Realize", "Revere",
   "Reflect", "Recommend", "Reduce", "Relate", "Report", "Require",
   "Reset", "Renew", "Retire", "Resist", "Reach", "Roar", "Ride",
   "Roast", "Run", "Say", "Sing", "Sit", "Send", "Shake", "Shower",
   "Show", "Shame", "Shock", "Shrink", "Speak", "Solve", "Specify",
   "Steal", "Serve", "Stop", "Stretch", "Stick", "Submit", "Suggest",
   "Strike", "Study", "Snuggle", "Surprise", "Swim", "Take", "Talk",
   "Taste", "Tear", "Trap", "Tell", "Tend", "Teach", "Think", "Throw",
   "Understand", "Value", "Volunteer", "Wait", "Walk", "Warn", "Warm",
   "Want", "Win", "Wish", "Write", "Watch", "Wave", "Wear", "Yearn"]

{- use in ghci
:l LS.XPile.SVG
resultRaw <- getDerivations' "respond"
mapM_ print . concat $ (\(s,derivs) -> [(getWord s (lfrm l), getWord y (lto l) ,l,y{links=[]}) | (l,y) <- derivs, isNoun y, whichword s == lfrm l]) <$> resultRaw
-}

c2n :: Context a b -> Node
c2n (_, n, nl, _) = n

{-
  party Who is Nothing       (party)

  party Who is Just          <party>
                               [who-pos]

  upon is Just                   (Upon)
                                   [event]

  cond is Just                       (if) -- optional, only if the previous exit node is a transition
                                       [if conds]

  deontic                                (must (within temporal) action)

  hence                                    [hence]
  lest                                     [lest]
-}




subj2nl :: NatLang -> BoolStructP -> Text.Text
subj2nl NLen (AA.Leaf pt) = pt2text pt


deonticTemporal :: Rule -> [(Text.Text, Text.Text)]
deonticTemporal r@(Regulative{..}) =
  let d = case deontic of { DMust -> "must"; DMay -> "may"; DShant -> "shant" }
      temp = tc2nl NLen temporal
      actions = actionFragments action
  in dTshow d temp <$> actions
  where
    dTshow :: Text -> Text -> ParamText -> (Text,Text)
    dTshow deon temp actn =
      let aW = actionWord actn
          aLine1 = Text.unwords . NE.toList . fst . NE.head $ actn
      in (aW, addnewlines [ deon
                          , "(" <> temp <> ")"
                          , aLine1 ])

addnewlines :: [Text] -> Text
addnewlines = Text.intercalate "\\n"

-- the BoolStructP could be an AND  .. or an OR .. how do we represent all that in the petri net?
-- for now let's only handle
    -- a simple Leaf situation
    -- an AND situation

actionFragments :: BoolStructP -> [ParamText]
actionFragments (AA.All _ xs) = concatMap actionFragments xs
actionFragments (AA.Leaf x) = [x]
actionFragments _           = []

actionWord :: ParamText -> Text.Text
actionWord = NE.head . fst . NE.head



