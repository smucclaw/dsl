{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Data.GraphViz.Attributes.Complete (Attribute(TailPort,HeadPort, Comment, Style)
                                         , CompassPoint(..)
                                         , PortPos(..), StyleItem(..), StyleName (Invisible))
import Control.Monad.State.Strict (State, MonadState (get, put), evalState, runState, gets)
import Control.Applicative.Combinators
import Data.Foldable (find)
import System.IO.Unsafe (unsafePerformIO)
import LS.NLP.WordNet
import Data.List (isPrefixOf, sortOn, isSuffixOf, nub)
import GHC.Base (join)
import Control.Monad (forM_)

data Deet = IsFirstNode | IsLastHappy | FromRuleAlias | IsParty | IsDeon | IsCond | IsThen | IsUpon
          | OrigRL Text
          | Temporal Text
          | IsInfra | IsSplit | IsJoin | IsOr | IsAnd
          | HideIfChildless
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
      rewritten = mergePetri rules $
                  condElimination rules $
                  reorder rules $
                  connectRules petri1 rules
  in renderDot $ unqtDot $ graphToDot (petriParams rewritten) rewritten

reorder :: [Rule] -> PetriD -> PetriD
reorder rules og = runGM og $ do
  -- x You if then must -> x if then you must
  forM_ [ (x0, you1, if2, then3, must4)
        | you1  <- nodes $ labfilter (hasDeet IsParty) og
        , x0    <- pre og you1
        , if2   <- suc og you1   , maybe False (hasDeet IsCond) (lab og if2)
        , then3 <- suc og if2    , maybe False (hasDeet IsThen) (lab og then3)
        , must4 <- suc og then3  , maybe False (hasDeet IsDeon) (lab og must4)
        ] $ \(x0, you1, if2, then3, must4) -> do
    delEdge' (x0, you1)
    delEdge' (    you1, if2)
    delEdge' (               then3, must4)
    newEdge' (x0,       if2,                     [Comment "reordered"])
    newEdge' (               then3, you1,        [Comment "reordered"])
    newEdge' (                      you1, must4, [Comment "reordered"])


mergePetri :: [Rule] -> PetriD -> PetriD
mergePetri rules og = foldl (mergePetri' rules) og (nodes $ labfilter (hasDeet IsSplit) og)

mergePetri' :: [Rule] -> PetriD -> Node -> PetriD
mergePetri' rules og splitNode = runGM og $ do
  -- rulealias split (x y 1 2 3) (x y 4 5 6) -> x y split (1 2 3) (4 5 6)
  -- traceM ("mergePetri': considering node " ++ show splitNode ++ ": " ++ (show $ lab og splitNode))
  forM_ [ twins
        | let twins = suc og splitNode
        , length (nub $ fmap ntext <$> (lab og <$> twins)) == 1
        , length twins > 1
        ] $ \twins -> do
    let grandchildren = concatMap (suc og) twins
        survivor : excess = twins
        parents = pre og splitNode
    forM_ twins         (\n -> delEdge' (splitNode,n))
    forM_ twins         (\n -> forM_ grandchildren (\gc -> delEdge' (n,gc)))
    forM_ grandchildren (\gc -> newEdge' (splitNode,gc,[Comment "due to mergePetri"]))
    forM_ parents       (\n  -> do
                            newEdge' (n, survivor, [Comment "due to mergePetri"])
                            delEdge' (n, splitNode)
                        )
    forM_ excess        delNode'
    newEdge' (survivor, splitNode, [Comment "due to mergePetri"])

    -- traceM $ "mergePetri' " ++ show splitNode ++ ": leaving survivor " ++ show survivor
    -- traceM $ "mergePetri' " ++ show splitNode ++ ": recursing."
    newPetri <- getGraph
    gs <- GM get
    GM . put $ gs {curentGraph = mergePetri' rules newPetri splitNode }

condElimination :: [Rule] -> PetriD -> PetriD
condElimination rules og = runGM og $ do
  -- if (a) { ... if (x y z a) { ...
  --                        ^ delete
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
  let headsOfChildren = nodes $ labfilter (hasDeet IsFirstNode) sgs
      successTails    = [ n
                        | n <- nodes $ labfilter (hasDeet IsLastHappy) sgs
                        , m <- suc og n -- there is a direct link to FULFILLED
                        , m == fulfilledNode
                        ]
      -- TODO: handle Or splits
      -- but we have to think about the semantics of an "or" in a contract ...
      -- maybe you are allowed to take multiple courses of action in parallel, rather than choosing one up front.
      -- normally, an AND split/join looks like       P T (P ... P)+ T   so multiple tokens spawn and then wait for one another before enabling the last T.
      -- an OR split/join looks like                  T P (T ... T)+ P   but that means the choice is immediate, only one token is available to many paths
      -- however, what i'm thinking of looks like     P T (P ... T)+ P   so that execution can proceed in parallel but whoever is first to the end can win.

      splitText = if length headsOfChildren == 2 then "both" else "split (and)"
      joinText  = "all done"
  splitnode <- newNode (PN Trans splitText [] [IsInfra,IsAnd,IsSplit])
  joinnode  <- newNode (PN Trans  joinText [] [IsInfra,IsAnd,IsJoin])
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
      rls = {- trace "rls = " $ traceShowId $ -} fmap rl2text . rLabelR <$> rules

      -- if the ruleLabel is for a Hornlike, expand accordingly.
      -- we mirror the structure of the BoolStruct inside the head of the Hornlike,
      -- with combinations of split/join over AND/OR.

      -- later on we will probably want to use a join transition to model an OR.
      aliasRules = [ (n,outgraph)
                   | n <- aliasNodes
                   , let nrl = {- trace "OrigRL = " $ traceShowId $ -} getOrigRL =<< lab sg n
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
  in  -- trace (unlines ((\n -> "node " <> show n <> " is a ruleAlias: " <>
      --                   Text.unpack (maybe "(nothing)" showNode (lab subgraphOfAliases n)))
      --                  <$> aliasNodes))
      -- -- while our initial pass over rule expansion takes care of direct expansions, we need the Hornlike rules to expand also. bit of debugging, but we can get rid of this later when it works
      -- trace ("all known rulelabels are " ++ show rls)
      -- trace ("we need to expand RuleAlias nodes " ++ show aliasNodes)
      -- trace ("maybe they expand to headnodes " ++ show headNodes)
      -- trace ("and the outgraphs have happy tails " ++ show tailNodes)
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
  in -- trace ("expandRulesByLabel(" ++ show txt ++ ") about to return " ++ show (rlabel <$> toreturn))
     toreturn

expandRule :: [Rule] -> Rule -> [Rule]
expandRule rules r@Regulative{..} = [r]
expandRule rules r@Hornlike{..} =
  let toreturn =
        -- we support hornlike expressions of the form x is y and z; we return y and z
        [ q
        | clause <- clauses
        , let rlbl' = rl2text <$> rLabelR r
              bsr = -- trace ("expandRule: got head " ++ show (hHead clause))
                    hHead clause
        , isJust rlbl'
        , mt <- -- trace ("aaLeaves returned " ++ show (aaLeaves (AA.Leaf bsr)))
                aaLeaves (AA.Leaf bsr)
        -- map each multiterm to a rulelabel's rl2text, and if it's found, return the rule
        , q <- expandRulesByLabel rules (mt2text mt)
        ]
  in -- trace ("expandRule: called with input " ++ show rlabel)
     -- trace ("expandRule: about to return " ++ show (ruleName <$> toreturn))
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
                     , (breachNode,    mkPlaceA [IsInfra] "BREACH"      ) ]
             [ (breachNode, fulfilledNode, [ Comment "this will render as invisible, but will be on same rank"
                                           , Data.GraphViz.Attributes.Complete.Style [ SItem Invisible [] ] ]) ]

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

delNode' :: Node -> GraphMonad ()
delNode' n1 = do
  gs@GS {curentGraph = g} <- GM get
  GM . put $ gs {curentGraph = delNode n1 g }

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
      firstNodeLabel1 = addDeet firstNodeLabel0 IsParty
      firstNodeLabel = maybe firstNodeLabel1 (addDeets firstNodeLabel1) myLabel
  everyN <- case already of
    Nothing -> newNode firstNodeLabel
    Just n  -> overwriteNode n firstNodeLabel
  whoN  <- case who of Nothing  -> pure everyN
                       Just bsr -> do whoN <- newNode $ mkTrans $ "who " <> bsr2text bsr
                                      newEdge everyN whoN []
                                      pure whoN
  upoN  <- case upon of Nothing -> pure whoN
                        Just pt -> do
                              uponN <- newNode $ addDeet (mkPlace "upon") IsUpon
                              uponCondN <- newNode $ addDeets (mkTrans $ pt2text pt) [IsUpon,IsCond]
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
    vp2np :: Text -> Text
    vp2np = unsafePerformIO . wnNounDerivations

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



