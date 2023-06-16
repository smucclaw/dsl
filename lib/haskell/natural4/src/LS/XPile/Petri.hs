{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

{-| transpiler to Petri net visualizer -}

module LS.XPile.Petri(module LS.XPile.Petri) where

-- import           System.IO.Unsafe (unsafePerformIO)

import AnyAll as AA
import Control.Applicative.Combinators
import Control.Monad (forM_, when)
import Control.Monad.State.Strict (MonadState (get, put), State, gets, runState)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
  ( Attribute (Comment, Compound, FontName, Height, Rank, Style, TailPort),
    CompassPoint (..),
    PortPos (..),
    StyleItem (..),
    StyleName (..),
  )
import Data.GraphViz.Printing (renderDot)
import Data.List (nub)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import LS


--------------------------------------------------------------------------------
-- fgl
--------------------------------------------------------------------------------

data NType = Place | Trans | Decis deriving (Eq, Show)
type PLabel = Attributes -- used later for graphviz, basically things like (color,blue), (label,somestring)
data PNode a = PN { ntype  :: NType
                  , ntext  :: Text
                  , nlabel :: Attributes
                  , ndeets :: [a]
                  }
             deriving (Eq, Show)

type Petri a = Gr (PNode a) PLabel

-- see also Data.Function (&)
-- see also Flow (|>)
-- https://hackage.haskell.org/package/flow-2.0.0.0/docs/Flow.html
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 1 |>

mkPlace,mkTrans,mkDecis :: Text -> (PNode a)
mkPlace x = PN Place x [] []
mkTrans x = PN Trans x [] []
mkDecis x = PN Decis x [] []

-- usage: mkPlaceC "start node" "comment label for start node"
mkXC :: NType -> [a] -> Text -> PNode a
mkXC x a l = PN x l [] a

mkPlaceA,mkTransA,mkDecisA :: [a] -> Text -> PNode a
mkPlaceA = mkXC Place
mkTransA = mkXC Trans
mkDecisA = mkXC Decis

pAddAttribute :: Attribute -> PNode a -> PNode a
pAddAttribute x pn = pn {nlabel = x : nlabel pn}

mySecondFGL :: Gr (PNode ()) PLabel
mySecondFGL = mkGraph
              [ (1, PN Place "start"   [] [])
              , (2, PN Trans "compute" [] [])
              , (3, PN Place "end"     [color Black] []) ]
              [ (1,2, [])
              , (2,3, []) ]

--------------------------------------------------------------------------------
-- graphviz
--------------------------------------------------------------------------------

petriGP :: DotGraph Node
petriGP = graphToDot (petriParams mySecondFGL) mySecondFGL

-- main = putStrLn . Text.unpack $ renderDot $ unqtDot petriGP

petriParams :: (Show a) => Gr (PNode a) PLabel -> GraphvizParams Int (PNode a) PLabel Int (PNode a)
petriParams g = Params
  { isDirected       = True
  , globalAttributes = [GraphAttrs [Compound True]]
  , clusterBy        = clusterby
  , isDotCluster     = const False
  , clusterID        = clusterid
  , fmtCluster       = fmtcluster
  , fmtNode          = fmtPetriNode
  , fmtEdge          = fmtPetriEdge g
  }

clusterby :: (Int,PNode a) -> LNodeCluster Int (PNode a)
clusterby a@( 0,PN Place _ _ _) = C 3 (N a) -- breach
clusterby a@( 1,PN Place _ _ _) = C 3 (N a) -- fulfilled
clusterby a@(_n,PN Place _ _ _) = C 1 (N a)
clusterby a@(_n,PN Trans _ _ _) = C 2 (N a)
clusterby a@(_n,PN Decis _ _ _) = C 1 (N a)

clusterid :: Node -> GraphID
clusterid 1 = Str "places"
clusterid 2 = Str "transitions"
clusterid 3 = Str "breachfulfilled"
clusterid _ = Str "other"

fmtcluster :: Node -> [GlobalAttributes]
fmtcluster 1 = [NodeAttrs [ shape Circle ] ]
fmtcluster 2 = [NodeAttrs [ shape BoxShape
                          , Height 0.2
                          , Style [ SItem Filled [] ]
                          , fillColor Black
                          , fontColor White
                          , FontName "Monaco" ] ]
fmtcluster 3 = [GraphAttrs [ Rank SameRank ] ]
fmtcluster _ = []

tcsd :: (Show a) => [a] -> [Attribute]
tcsd = fmap $ Comment . LT.fromStrict . Text.pack . show

fmtPetriNode :: Show a => (Node, PNode a) -> [Attribute]
fmtPetriNode (_n,PN Place txt@"FULFILLED" lbls ds) = toLabel txt : color Green        : tcsd ds ++ lbls
fmtPetriNode (_n,PN Place txt@"BREACH"    lbls ds) = toLabel txt : color Brown        : tcsd ds ++ lbls
fmtPetriNode (_n,PN Place txt lbls ds)             = toLabel txt                      : tcsd ds ++ lbls
fmtPetriNode (_n,PN Trans txt lbls ds)             = toLabel txt                      : tcsd ds ++ lbls
fmtPetriNode (_n,PN Decis txt lbls ds)             = toLabel txt : shape DiamondShape : tcsd ds ++ lbls

fmtPetriEdge :: Graph gr => gr (PNode a) PLabel -> (Node, Node, PLabel) -> [Attribute]
fmtPetriEdge g (s,e,el) -- if the edge goes to BREACH then we paint the edge brown
  | (ntext <$> lab g e) == Just "BREACH"                = color Brown : el
  | (ntext <$> lab g e) == Just "FULFILLED" && s /= 0   = color Green : el
  | otherwise                                           = el

-- code below was previously in XPile/SVG.hs which imported Petri.hs
data Deet = IsFirstNode | IsLastHappy | FromRuleAlias | IsParty | IsDeon | IsCond | IsThen | IsUpon
          | OrigRL Text
          | Temporal Text
          | IsInfra | IsSplit | IsJoin | IsOr | IsAnd
          | HideIfChildless
          deriving (Eq,Show)

type PNodeD = PNode Deet
type PetriD = Petri Deet

-- initially let's just draw the state diagram in a manner typical of GraphViz.
-- see the README

toPetri :: [Rule] -> Text.Text
toPetri rules =
  let petri1 = insrules rules startGraph
      rewritten = rules
                  |> connectRules petri1
                  |> reorder rules
                  |> condElimination rules
                  |> mergePetri rules
                  |> elideNodes1 "consequently" (hasText "consequently")
                  |> elideNodesN "FromRuleAlias" (hasDeet FromRuleAlias)
  in LT.toStrict $ renderDot $ unqtDot $ graphToDot (petriParams rewritten) rewritten

elideNodes1 :: LT.Text -> (PNode Deet -> Bool) -> PetriD -> PetriD
elideNodes1 = elideNodes True

elideNodesN :: LT.Text -> (PNode Deet -> Bool) -> PetriD -> PetriD
elideNodesN = elideNodes False

-- | get rid of intermediary nodes y that fit the pattern `x -> y -> z`, where y passes the given predicate.
-- if |x| and |z| are each 1, use the elideNodes1 form
elideNodes :: Bool -> LT.Text -> (PNode Deet -> Bool) -> PetriD -> PetriD
elideNodes limit1 desc pnpred og = runGM og $ do
  -- awkward phrasing, shouldn't there be some sort of concatM
  forM_ [ do
             newEdge' (x,    z, [Comment $ "after elision of " <> desc <> " intermediary"])
             delEdge' (x, y)
             delEdge' (   y, z)
             delNode'     y
        | y <- nodes $ labfilter pnpred og
        , let indegrees  = pre og y
              outdegrees = suc og y
        , not limit1 || length  indegrees == 1
        , not limit1 || length outdegrees == 1
        , x   <- indegrees
        , z   <- outdegrees
        ] $ id
  

reorder :: [Rule] -> PetriD -> PetriD
reorder _rules og = runGM og $ do
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
  -- myTraceM ("mergePetri': considering node " ++ show splitNode ++ ": " ++ (show $ lab og splitNode))
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

    -- myTraceM $ "mergePetri' " ++ show splitNode ++ ": leaving survivor " ++ show survivor
    -- myTraceM $ "mergePetri' " ++ show splitNode ++ ": recursing."
    newPetri <- getGraph
    gs <- GM get
    GM . put $ gs {curentGraph = mergePetri' rules newPetri splitNode }

condElimination :: [Rule] -> PetriD -> PetriD
condElimination _rules og = runGM og $ do
  -- if (a) { ... if (x y z a) { ...
  --                        ^ delete
  return ()

fromRuleAlias :: Attribute
fromRuleAlias = Comment "from RuleAlias"

data SplitJoin = SJAny | SJAll deriving (Eq, Show)

-- | wire up splits and joins.
-- the pattern is this:
--        
--                   entry
--                     |
--                   split
--                    / \
--           childHead1 childHead2
--              |              |
--           child1         child2
--           ....              ....
--          /    \            /    \
--         B  successTail1   B   successTail2
--    breach        \      breach  /
--                   \            /
--                    [ joinNode ]
--                          |
--                      Fulfilled

-- previously the successTails would have gone directly to Fulfilled
-- but we relink them to go to the join node instead

splitJoin :: [Rule]      -- background input ruleset
          -> PetriD      -- original whole graph
          -> SplitJoin   -- whether we are doing an Any or an All wrapper
          -> PetriD    -- subgraphs which are to live together under the split/join.
          -> Node        -- entry point node that leads into the split
          -> PetriD      -- rewritten whole graph
splitJoin _rs og _sj sgs entry = runGM og $ do
      -- Sometimes, entry satisfies hasDeet IsFirstNode and so it appears
      -- in the resulting list of nodes as returned by the call to nodes below.
      -- Since we only want the children of entry and not entry itself, we can
      -- filter out all nodes that look like it.
      -- This helps avoid weird splits and joins like
      --           Something done
      --             ^     |
      --             |     v
      --              Both
      --               |
      --               v
      --              ...
  let headsOfChildren = 
        sgs |> labfilter (hasDeet IsFirstNode)
            |> nodes
            |> filter (/= entry)
      successTails    = [ n
                        | n <- nodes $ labfilter (hasDeet IsLastHappy) sgs
                        , m <- suc og n -- there is a direct link to FULFILLED
                        , m == fulfilledNode
                        ]
      -- [TODO]: handle Or splits
      -- but we have to think about the semantics of an "or" in a contract ...
      -- maybe you are allowed to take multiple courses of action in parallel, rather than choosing one up front.
      -- normally, an AND split/join looks like       P T (P ... P)+ T   so multiple tokens spawn and then wait for one another before enabling the last T.
      -- an OR split/join looks like                  T P (T ... T)+ P   but that means the choice is immediate, only one token is available to many paths
      -- however, what i'm thinking of looks like     P T (P ... T)+ P   so that execution can proceed in parallel but whoever is first to the end can win.

  -- If the entry node doesn't have any children, we simply connect it to
  -- Fulfilled and stop here. No need to make split and join nodes and link
  -- them up in this case.
  -- This is a quick hack. Ideally we have a way to determine if
  -- it should connect to Breach or Fulfilled.
  if null headsOfChildren then do
    newEdge' (entry, fulfilledNode, [])
  else do
    let splitText = case length headsOfChildren of
          0 -> undefined -- This should never happen since we already check if headsOfChildren is empty above.
          1 -> "consequently"
          2 -> "both"
          _ -> "split (and)"
        joinText = "all done"
    -- If the entry node has children, then we need to care about splitting and
    -- joining. We first make a split node and connect:
    --    entry node -> split node -> children of entry node
    splitnode <- newNode (PN Trans splitText [ Comment $ LT.pack $ "split node coming from entry " ++ show entry ] [IsInfra,IsAnd,IsSplit])
    newEdge' (entry,splitnode, [Comment "added by split from parent node"])
    mapM_ newEdge' [ (splitnode, headnode, [Comment "added by split to headnode"]) 
                   | headnode <- headsOfChildren ]
    -- Now we check how many tail nodes there are in successTails.
    -- If there's only 1, then there's no need to make a join node and link that up.
    -- Doing this prevents redundant "All done" join nodes like
    --         (Something not done)      (Something done)
    --                                          |
    --                                         All done
    --                                           |
    --                                        Fulfilled                      
    when (length successTails > 1) $ do
      joinnode  <- newNode (PN Trans joinText [ Comment $ LT.pack $ "corresponding to splitnode " ++ show splitnode ++ " and successTails " ++ show successTails] [IsInfra,IsAnd,IsJoin] )
      newEdge'         (           joinnode,fulfilledNode, [Comment "added by join to fulfilledNode", color Green])
      mapM_ newEdge' [ ( tailnode, joinnode,               [Comment "added by join from tailnode",    color Green]) | tailnode <- successTails    ]
      -- myTraceM $ "splitJoin for joinnode " ++ show joinnode ++ " now calling delEdge' for successTails " ++ show successTails ++ ", fulfilledNode " ++ show fulfilledNode
      mapM_ delEdge' [ ( tailnode, fulfilledNode ) | tailnode <- successTails ]

hasText :: Text -> PNode a -> Bool
hasText  x  (PN _ nt _ _) = x == nt
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

-- [TODO] we should refactor this into something that is output-independent and lives in Interpreter.
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

  -- meng would like to clear up `x -> (FromRuleAlias) -> (y)` to `x -> (y)`
  -- because it is ugly to have `() -> ()`, that's not really a Petri net
  
  let subgraphOfAliases = labfilter (hasDeet FromRuleAlias) sg
  -- another way to find it might be to look for head nodes that have no outdegrees

      aliasNodes = nodes subgraphOfAliases

      -- all labeled rules
      -- rls = {- trace "rls = " $ traceShowId $ -} fmap rl2text . getRlabel <$> rules

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
                                                                       , let rlout' = fromJust rlout -- safe due to above isJust test
                                                                       ] ) sg
                   ]
      -- headNodes = [ headNode
      --             | (_orign, outgraph) <- aliasRules
      --             , headNode <- nodes $ labfilter (hasDeet IsFirstNode) outgraph
      --             ]
      -- tailNodes = [ tailNode
      --             | (_orign, outgraph) <- aliasRules
      --             , tailNode <- nodes $ labfilter (hasDeet IsLastHappy) outgraph
      --           ]
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
  -- myTraceM $ "newNode: " <> show n' <> " " <> show lbl
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
r2fgl _rs _defRL RegFulfilled   = pure Nothing
r2fgl _rs _defRL RegBreach      = pure Nothing
-- what do we do with a RuleAlias? it only ever appears as the target of a Hence or a Lest,
-- so we just wire it up to whatever existing rule has been labeled appropriately.
-- however, if no existing rule in our list of rules bears that label (yet(, we put in a placeholder state.
-- the following function assumes the rulealias does not appear in the ruleset, so we are calling r2fgl as a last resort.
-- we will do another pass over the graph subsequently to rewire any rulealiases
r2fgl _rs _defRL (RuleAlias rn) = do
  sg <- getGraph
  let ntxt = mt2text rn
  let already = getNodeByDeets sg [IsFirstNode,OrigRL ntxt]
  maybe (fmap Just . newNode $
         mkPlaceA [IsFirstNode,FromRuleAlias,OrigRL ntxt] ntxt ) (pure . Just) already

r2fgl rs defRL Regulative{..} = do
  sg <- getGraph
  let myLabel = do
        rl <- rlabel
        return [IsFirstNode,OrigRL (rl2text rl), IsParty]
      origRLdeet = maybeToList (OrigRL <$> ((rl2text <$> rlabel) <|> defRL))
  let already = getNodeByDeets sg =<< myLabel
  myTraceM $ "Petri/r2fgl: rkeyword = " <> show rkeyword
  let everywho = Text.unwords ( ( if rkeyword == REvery
                                  then [ Text.pack (show (tokenOf rkeyword)) ]
                                  else [] )
                                <> [ subj2nl NLen subj ] )
  myTraceM $ "Petri/r2fgl: everywho = " <> show everywho

  let firstNodeLabel0 = case who of Nothing    -> mkPlace everywho
                                    Just _bsr  -> mkDecis everywho
      firstNodeLabel1 = addDeet firstNodeLabel0 IsParty
      firstNodeLabel = maybe firstNodeLabel1 (addDeets firstNodeLabel1) myLabel
 -- myTraceM $ "Petri: firstNodeLabel0 = " <> show firstNodeLabel0
  myTraceM $ "Petri/r2fgl: firstNodeLabel1 = " <> show firstNodeLabel1
  myTraceM $ "Petri/r2fgl: firstNodeLabel  = " <> show firstNodeLabel
  everyN <- case already of
    Nothing -> newNode firstNodeLabel
    Just n  -> overwriteNode n firstNodeLabel
  whoN  <- case who of Nothing  -> pure everyN
                       Just bsr -> do whoN <- newNode $ mkTrans $ "who " <> (bsr2textnl bsr)
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
    myTraceM $ "Petri/r2fgl: action = " <> show action
    let deon = case deontic of { DMust -> "must"; DMay -> "may"; DShant -> "shant" }
        temp = tc2nl NLen temporal
        actn = actionFragments action
        oblLab = mkDecis (addnewlines [ deon
                                      , mt2text . NE.toList . fst . NE.head $ head actn
                                      , temp
                                      ])
        successLab = mkTransA ([Temporal temp, IsLastHappy] ++ origRLdeet) $
                     -- vp2np
                     -- ( actionWord $ head $ actionFragments action) <> " " <>
                     henceWord deontic
    myTraceM $ "Petri/r2fgl: actn = " <> show actn
    myTraceM $ "Petri/r2fgl: oblLab = " <> show actn

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
  -- myTraceM $ "lestNEs: " <> show lestNEs
  -- let sg3 = insertNE lestNEs  sg2
      -- connect up the hence and lest bits
      -- the "hence" transition from dtaE should plug in to the first node in our henceContexts
  case mbOnFailureN of
    Just onFailureN -> newEdge onFailureN lestN []
    Nothing -> pure ()

  -- Return the first node
  pure $ Just whoN
  where
    -- vp2np :: Text -> Text
    -- vp2np = unsafePerformIO . wnNounDerivations . Text.toLower

    seport = [TailPort (CompassPoint SouthEast), Comment "southeast for positive", color Green]
    swport = [TailPort (CompassPoint SouthWest), Comment "southwest for negative"]
    henceWord DMust  = "done"
    henceWord DMay   = "occurred"
    henceWord DShant = "avoided"
    lestWord DMust  = "not done"
    lestWord DMay   = error "a MAY has no LEST"  -- this should never arise
    lestWord DShant = "violation"

-- r2fgl rs r@Hornlike{} = pure Nothing
r2fgl _rs _defRL _r = pure Nothing


c2n :: Context a b -> Node
c2n (_, n, _nl, _) = n

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
subj2nl _ bsp = "Petri/subj2nl: " <> Text.pack (show bsp)

deonticTemporal :: Rule -> [(Text.Text, Text.Text)]
deonticTemporal Regulative{..} =
  let d = case deontic of { DMust -> "must"; DMay -> "may"; DShant -> "shant" }
      temp = Text.replace "  " " " $ tc2nl NLen temporal
      actions = actionFragments action
  in dTshow d temp <$> actions
  where
    dTshow :: Text -> Text -> ParamText -> (Text,Text)
    dTshow deon temp actn =
      let aW = actionWord actn
          aLine1 = mt2text . NE.toList . fst . NE.head $ actn
      in (aW, addnewlines [ deon
                          , "(" <> temp <> ")"
                          , aLine1 ])

deonticTemporal _ = error "Petri/deonticTemporal called for a non-Regulative rule"

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
actionWord = mtexpr2text . NE.head . fst . NE.head




 
