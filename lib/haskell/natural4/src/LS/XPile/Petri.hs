{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

{-| transpiler to Petri net visualizer -}

module LS.XPile.Petri (toPetri) where

-- import           System.IO.Unsafe (unsafePerformIO)

import AnyAll as AA (BoolStruct (All, Leaf))
import Control.Applicative (Alternative ((<|>)))
import Control.Arrow ((>>>))
import Control.Monad (when)
import Control.Monad.Identity (runIdentity)
import Control.Monad.RWS (lift)
import Control.Monad.State.Strict
  ( MonadState (get, put),
    State (..),
    StateT (..),
    gets,
    runState,
  )
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Foldable (foldl', for_, traverse_)
import Data.Graph.Inductive.Graph
  ( Context,
    Graph (mkGraph, nodeRange),
    Node,
    delEdge,
    delNode,
    insEdge,
    insNode,
    lab,
    labfilter,
    nodes,
    pre,
    suc,
  )
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz
  ( Attribute,
    Attributes,
    DotGraph,
    GlobalAttributes (GraphAttrs, NodeAttrs),
    GraphID (Str),
    GraphvizParams (..),
    LNodeCluster,
    NodeCluster (C, N),
    PrintDot (unqtDot),
    RankType (SameRank),
    Shape (BoxShape, Circle, DiamondShape),
    X11Color (Black, Brown, Green, White),
    color,
    fillColor,
    fontColor,
    graphToDot,
    shape,
    toLabel,
  )
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
import Data.Maybe (fromMaybe, isJust, listToMaybe, maybeToList)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Data.Tuple.Extra (uncurry3)
import Flow ((|>))
import LS
  ( BoolStructP,
    Deontic (DMay, DMust, DShant),
    HasToken (tokenOf),
    NatLang (..),
    ParamText,
    RegKeywords (REvery),
    Rule
      ( RegBreach,
        RegFulfilled,
        Regulative,
        RuleAlias,
        action,
        cond,
        defaults,
        deontic,
        given,
        having,
        hence,
        lest,
        lsource,
        rkeyword,
        rlabel,
        srcref,
        subj,
        symtab,
        temporal,
        upon,
        who,
        wwhere
      ),
    RuleSet,
    bsr2textnl,
    expandRule,
    getRuleByLabel,
    mt2text,
    mtexpr2text,
    myTraceM,
    pt2text,
    rl2text,
    tc2nl,
  )
import LS.Utils ((|$>))
import LS.XPile.Logging
    ( mutterd, xpLog, xpReturn, XPileLog, XPileLogE )

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

mkPlace :: Text -> PNode a
mkPlace = mkPlaceTransDecis Place

mkTrans :: Text -> PNode a
mkTrans = mkPlaceTransDecis Trans

mkDecis :: Text -> PNode a
mkDecis = mkPlaceTransDecis Decis

-- usage: mkPlaceC "start node" "comment label for start node"
mkXC :: NType -> [a] -> Text -> PNode a
mkXC x a l = PN x l [] a

mkPlaceTransDecis :: NType -> Text -> PNode a
mkPlaceTransDecis x = mkXC x []

mkPlaceA :: [a] -> Text -> PNode a
mkPlaceA = mkXC Place

mkTransA :: [a] -> Text -> PNode a
mkTransA = mkXC Trans

mkDecisA :: [a] -> Text -> PNode a
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

petriParams ::
  (Show a) =>
  Gr (PNode a) PLabel ->
  GraphvizParams Int (PNode a) PLabel Int (PNode a)
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

clusterby :: (Int, PNode a) -> LNodeCluster Int (PNode a)
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

tcsd :: Show a => [a] -> [Attribute]
tcsd = fmap $ Comment . LT.fromStrict . Text.pack . show

fmtPetriNode :: Show a => (Node, PNode a) -> [Attribute]
fmtPetriNode (_n, PN Place txt@"FULFILLED" lbls ds) = toLabel txt : color Green        : tcsd ds ++ lbls
fmtPetriNode (_n, PN Place txt@"BREACH"    lbls ds) = toLabel txt : color Brown        : tcsd ds ++ lbls
fmtPetriNode (_n, PN Place txt lbls ds)             = toLabel txt                      : tcsd ds ++ lbls
fmtPetriNode (_n, PN Trans txt lbls ds)             = toLabel txt                      : tcsd ds ++ lbls
fmtPetriNode (_n, PN Decis txt lbls ds)             = toLabel txt : shape DiamondShape : tcsd ds ++ lbls

fmtPetriEdge :: Graph gr => gr (PNode a) PLabel -> (Node, Node, PLabel) -> [Attribute]
fmtPetriEdge g (s, e, el) =
  -- if the edge goes to BREACH then we paint the edge brown
  case (ntext <$> lab g e, s) of
    (Just "BREACH", _) -> color Brown : el
    (Just "Fulfilled", (/= 0) -> True) -> color Green : el
    _ -> el

-- code below was previously in XPile/SVG.hs which imported Petri.hs
data Deet = IsFirstNode | IsLastHappy | FromRuleAlias | IsParty | IsDeon | IsCond | IsThen | IsUpon
          | OrigRL Text
          | Temporal Text
          | IsInfra | IsSplit | IsJoin | IsOr | IsAnd
          | HideIfChildless
          deriving (Eq, Show)

type PNodeD = PNode Deet
type PetriD = Petri Deet

-- initially let's just draw the state diagram in a manner typical of GraphViz.
-- see the README

toPetri :: [Rule] -> XPileLogE String
toPetri rules = do
  petri1 <- insrules rules startGraph
  let rewritten =
        petri1
          |> (`connectRules` rules)
          |> reorder rules
          |> condElimination rules
          |> mergePetri rules
          |> elideNodes1 "consequently" (hasText "consequently")
          |> elideNodesN "FromRuleAlias" (hasDeet FromRuleAlias)
  mutterd 1 "toPetri: running"
  rewritten
    |> graphToDot (petriParams rewritten)
    |> unqtDot
    |> renderDot
    |> LT.toStrict
    |> Text.unpack
    |> xpReturn

elideNodes1 :: LT.Text -> (PNode Deet -> Bool) -> PetriD -> PetriD
elideNodes1 = elideNodes True

elideNodesN :: LT.Text -> (PNode Deet -> Bool) -> PetriD -> PetriD
elideNodesN = elideNodes False

-- | get rid of intermediary nodes y that fit the pattern `x -> y -> z`, where y passes the given predicate.
-- if |x| and |z| are each 1, use the elideNodes1 form
elideNodes :: Bool -> LT.Text -> (PNode Deet -> Bool) -> PetriD -> PetriD
elideNodes limit1 desc pnpred og = runGM og do
  -- awkward phrasing, shouldn't there be some sort of concatM
  for_ [ do
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
        ] id

reorder :: [Rule] -> PetriD -> PetriD
reorder _rules og = runGM og do
  -- x You if then must -> x if then you must
  for_ [ (x0, you1, if2, then3, must4)
        | you1  <- nodes $ labfilter (hasDeet IsParty) og
        , x0    <- pre og you1
        , if2   <- suc og you1   , maybe False (hasDeet IsCond) (lab og if2)
        , then3 <- suc og if2    , maybe False (hasDeet IsThen) (lab og then3)
        , must4 <- suc og then3  , maybe False (hasDeet IsDeon) (lab og must4)
        ] \(x0, you1, if2, then3, must4) -> do
    traverse_ delEdge' [(x0, you1), (you1, if2), (then3, must4)]
    traverse_
      newEdge'
      [ (x0, if2, [Comment "reordered"]),
        (then3, you1, [Comment "reordered"]),
        (you1, must4, [Comment "reordered"])
      ]

mergePetri :: [Rule] -> PetriD -> PetriD
mergePetri rules og =
  foldl' (mergePetri' rules) og $ nodes $ labfilter (hasDeet IsSplit) og

mergePetri' :: [Rule] -> PetriD -> Node -> PetriD
mergePetri' rules og splitNode = runGM og do
  -- rulealias split (x y 1 2 3) (x y 4 5 6) -> x y split (1 2 ) (4 5 6)
  -- myTraceM ("mergePetri': considering node " ++ show splitNode ++ ": " ++ (show $ lab og splitNode))
  for_ [ twins
        | let twins = suc og splitNode
        , length (nub $ fmap ntext <$> (lab og <$> twins)) == 1
        , length twins > 1
        ] \twins -> do
    let grandchildren = foldMap (suc og) twins
        survivor : excess = twins
        parents = pre og splitNode
    for_ twins \n -> do
      delEdge' (splitNode, n)
      for_ grandchildren \gc -> delEdge' (n, gc)
    for_ grandchildren \gc -> newEdge' (splitNode,gc,[Comment "due to mergePetri"])
    for_ parents       \n  -> do
                            newEdge' (n, survivor, [Comment "due to mergePetri"])
                            delEdge' (n, splitNode)
    for_ excess        delNode'
    newEdge' (survivor, splitNode, [Comment "due to mergePetri"])

    -- myTraceM $ "mergePetri' " ++ show splitNode ++ ": leaving survivor " ++ show survivor
    -- myTraceM $ "mergePetri' " ++ show splitNode ++ ": recursing."
    newPetri <- getGraph
    gs <- mkGM get
    mkGM $ put gs {currentGraph = mergePetri' rules newPetri splitNode }

condElimination :: [Rule] -> PetriD -> PetriD
condElimination _rules og = runGM og do
  -- if (a) { ... if (x y z a) { ...
  --                        ^ delete
  pure ()

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
splitJoin _rs og _sj sgs entry = runGM og do
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
        sgs
          |> labfilter (hasDeet IsFirstNode)
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
    splitnode <- newNode $
      PN Trans splitText
        [Comment [i|split node coming from entry #{entry}|]]
        [IsInfra,IsAnd,IsSplit]
    newEdge' (entry,splitnode, [Comment "added by split from parent node"])
    traverse_
      newEdge'
      [ (splitnode, headnode, [Comment "added by split to headnode"])
      | headnode <- headsOfChildren]
    -- Now we check how many tail nodes there are in successTails.
    -- If there's only 1, then there's no need to make a join node and link that up.
    -- Doing this prevents redundant "All done" join nodes like
    --         (Something not done)      (Something done)
    --                                          |
    --                                         All done
    --                                           |
    --                                        Fulfilled                      
    when (length successTails > 1) do
      joinnode  <- newNode (PN Trans joinText [ Comment [i|corresponding to splitnode #{splitnode} and successTails #{successTails}|]] [IsInfra,IsAnd,IsJoin] )
      newEdge' (joinnode, fulfilledNode, [Comment "added by join to fulfilledNode", color Green])
      traverse_ newEdge' [(tailnode, joinnode, [Comment "added by join from tailnode", color Green]) | tailnode <- successTails]
      -- myTraceM $ "splitJoin for joinnode " ++ show joinnode ++ " now calling delEdge' for successTails " ++ show successTails ++ ", fulfilledNode " ++ show fulfilledNode
      traverse_ delEdge' [(tailnode, fulfilledNode) | tailnode <- successTails]

hasText :: Text -> PNode a -> Bool
hasText x PN {ntext} = x == ntext

hasDeet :: Eq a => a -> PNode a -> Bool
hasDeet x PN {ndeets} = x `elem` ndeets

hasDeets :: (Foldable t, Eq a) => t a -> PNode a -> Bool
hasDeets xs pn = all (`hasDeet` pn) xs

addDeet :: PNode a -> a -> PNode a
addDeet pn@PN {ndeets} dt = pn {ndeets = dt : ndeets}

addDeets :: PNode a -> [a] -> PNode a
addDeets = foldl' addDeet

getOrigRL :: PNodeD -> Maybe Text
getOrigRL PN {ndeets} = listToMaybe [t | OrigRL t <- ndeets]

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
                   , let nrl = {- trace "OrigRL = " $ traceShowId $ -}
                          lab sg n >>= getOrigRL
                         r = nrl >>= getRuleByLabel rules
                         outs = maybe [] (expandRule rules) r
                         rlouts = fmap rl2text <$> (rlabel <$> outs)
                         outgraph =
                          sg
                            |> labfilter \pn -> any ($ pn)
                                [ hasDeet $ OrigRL rlout'
                                | rlout <- rlouts
                                , rlout' <- maybeToList rlout
                                ]
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
      foldl' (\g (n,outgraph) -> splitJoin rules g SJAll outgraph n) sg aliasRules
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

-- before: insrules :: RuleSet -> PetriD -> PetriD
-- before: insrules rs sg = runGM sg $ traverse (r2fgl rs Nothing) rs

-- | Insert the rules into an existing petri net. With logging.
insrules :: RuleSet -> PetriD -> XPileLog PetriD
insrules rs sg =
  rs                    -- [Rule]
    |> traverse rule2GM -- XPileLog [GraphMonad (Maybe Node)]
    |$> sequenceA       -- XPileLog (GraphMonad (Maybe Node))
    |$> runGM sg        -- XPileLog PetriD
  where
    -- Effectful transpilation of a rule to a stateful graph monad.
    rule2GM :: Rule -> XPileLog (GraphMonad (Maybe Node))
    rule2GM = r2fgl rs Nothing

{-

Alternative imperative approach:

do
  node1 <- newNode
  node2 <- newNode
  let edge = (node1, node2, "")

-}

data GraphState = GS { lastNode :: Node, currentGraph :: PetriD }

-- | pure imperative graph construction
newtype GraphMonad a = GM { runGM_ :: State GraphState a }
  deriving newtype (Functor, Applicative, Monad)

mkGM :: State GraphState a -> GraphMonad a
mkGM = coerce
{-# INLINE mkGM #-}

getGM :: GraphMonad a -> State GraphState a
getGM = coerce
{-# INLINE getGM #-}

-- instance Semigroup a => Semigroup (GraphMonad a) where
--   a <> b = (<>) <$> a <*> b

-- instance Monoid a => Monoid (GraphMonad a) where
--   mempty = pure mempty

newNode :: PNodeD -> GraphMonad Node
newNode lbl = do
  gs@GS {lastNode = n, currentGraph = g} <- mkGM get
  let n' = succ n
  -- myTraceM $ "newNode: " <> show n' <> " " <> show lbl
  mkGM $ put gs {lastNode = n' , currentGraph = insNode (n', lbl) g }
  pure n'

newEdge :: Node -> Node -> PLabel -> GraphMonad ()
newEdge n1 n2 lbl = do
  gs@GS {currentGraph} <- mkGM get
  mkGM $ put gs {currentGraph = insEdge (n1, n2, lbl) currentGraph}

newEdge' :: (Node, Node, PLabel) -> GraphMonad ()
newEdge' = uncurry3 newEdge

overwriteNode :: Node -> PNodeD -> GraphMonad Node
overwriteNode n pn = do
  gs@GS {currentGraph} <- mkGM get
  mkGM $ put gs {currentGraph = insNode (n, pn) currentGraph}
  pure n

delEdge' :: (Node, Node) -> GraphMonad ()
delEdge' (n1, n2) = do
  gs@GS {currentGraph} <- mkGM get
  mkGM $ put gs {currentGraph = delEdge (n1, n2) currentGraph}

delNode' :: Node -> GraphMonad ()
delNode' n1 = do
  gs@GS {currentGraph} <- mkGM get
  mkGM $ put gs {currentGraph = delNode n1 currentGraph}

-- runGM :: PetriD -> GraphMonad a -> a
runGM :: PetriD -> GraphMonad a -> PetriD
runGM gr (getGM -> m) = cg
-- runGM gr (GM m) = traceShow (neNodes res, neNodes cg) res
  where (_, n0) = nodeRange gr
        (_res, GS _ln cg) = runState m $ GS n0 gr
        -- [TODO] why not execState

-- This is currently kind of inefficient, but when NE is replaced by a real graph, it becomes simpler and faster
getGraph :: GraphMonad PetriD
getGraph = mkGM $ gets currentGraph

-- | discards the stderr log
runLog :: XPileLog a -> a
runLog = fst . xpLog

-- | we convert each rule to a list of nodes and edges which can be inserted into an existing graph
r2fgl :: RuleSet -> Maybe Text -> Rule -> XPileLog (GraphMonad (Maybe Node))
r2fgl _rs _defRL RegFulfilled   = pure $ pure Nothing
r2fgl _rs _defRL RegBreach      = pure $ pure Nothing
-- what do we do with a RuleAlias? it only ever appears as the target of a Hence or a Lest,
-- so we just wire it up to whatever existing rule has been labeled appropriately.
-- however, if no existing rule in our list of rules bears that label (yet(, we put in a placeholder state.
-- the following function assumes the rulealias does not appear in the ruleset, so we are calling r2fgl as a last resort.
-- we will do another pass over the graph subsequently to rewire any rulealiases
r2fgl _rs _defRL (RuleAlias rn) = pure do
  sg <- getGraph
  let ntxt = mt2text rn
  let already = getNodeByDeets sg [IsFirstNode, OrigRL ntxt]
  maybe (fmap Just . newNode $
         mkPlaceA [IsFirstNode, FromRuleAlias, OrigRL ntxt] ntxt ) (pure . Just) already

r2fgl rs defRL Regulative{..} = pure do
  sg <- getGraph
  let myLabel = do
        rl <- rlabel
        return [IsFirstNode,OrigRL (rl2text rl), IsParty]
      origRLdeet = maybeToList (OrigRL <$> ((rl2text <$> rlabel) <|> defRL))
  let already = getNodeByDeets sg =<< myLabel
  -- mutterd 2 $ "Petri/r2fgl: rkeyword = " <> show rkeyword
  let everywho = Text.unwords ( ( [[i|#{tokenOf rkeyword}|] | rkeyword == REvery] )
                                <> [ subj2nl NLen subj ] )
  -- mutterd 2 $ "Petri/r2fgl: everywho = " <> show everywho

  let firstNodeLabel0 = case who of Nothing    -> mkPlace everywho
                                    Just _bsr  -> mkDecis everywho
      firstNodeLabel1 = addDeet firstNodeLabel0 IsParty
      firstNodeLabel = maybe firstNodeLabel1 (addDeets firstNodeLabel1) myLabel
 -- myTraceM $ "Petri: firstNodeLabel0 = " <> show firstNodeLabel0
  -- mutterd 2 $ "Petri/r2fgl: firstNodeLabel1 = " <> show firstNodeLabel1
  -- mutterd 2 $ "Petri/r2fgl: firstNodeLabel  = " <> show firstNodeLabel
  everyN <- case already of
    Nothing -> newNode firstNodeLabel
    Just n  -> overwriteNode n firstNodeLabel
  whoN  <- case who of Nothing  -> pure everyN
                       Just bsr -> do whoN <- newNode $ mkTrans [i|who #{bsr2textnl bsr}|]
                                      newEdge everyN whoN []
                                      pure whoN
  upoN  <- case upon of Nothing -> pure whoN
                        Just pt -> do
                              uponN <- newNode $ addDeet (mkPlace "upon") IsUpon
                              uponCondN <- newNode $ addDeets (mkTrans $ pt2text pt) [IsUpon,IsCond]
                              traverse_ newEdge' [( whoN, uponN, []), ( uponN, uponCondN, [])]
                              pure uponCondN
  conN  <- case cond of Nothing  -> pure upoN
                        Just bsr -> do
                            ifN     <- newNode $ (addDeet $ mkDecis [i|if #{bsr2textnl bsr}|]) IsCond
                            ifCondN <- newNode $ (addDeet $ mkTrans "then") IsThen
                            traverse_ newEdge' [(upoN, ifN, []), (ifN, ifCondN, [])]
                            pure ifCondN
  (onSuccessN, mbOnFailureN) <- do
    myTraceM [i|Petri/r2fgl: action = #{action}|]
    -- convert DMUST/DMAY/DSHANT into must/may/shant
    let deon = deontic |> show |> Text.pack |> Text.tail |> Text.toLower
        temp = tc2nl NLen temporal
        actn = actionFragments action
        oblLab = mkDecis (Text.unlines [ deon
                                       , mt2text . NE.toList . fst . NE.head $ head actn
                                       , temp
                                       ])
        successLab = mkTransA ([Temporal temp, IsLastHappy] ++ origRLdeet) $
                     -- vp2np
                     -- ( actionWord $ head $ actionFragments action) <> " " <>
                     henceWord deontic
    traverse_
      myTraceM
      [ [i|Petri/r2fgl: actn = #{actn}|],
        [i|Petri/r2fgl: oblLab = #{actn}|]
      ]

    obligationN <- newNode $ addDeet oblLab IsDeon
    onSuccessN <- newNode successLab
    traverse_ myTraceM
      [ [i|Petri/r2fgl: deontic = #{deontic}|],
        [i|Petri/r2fgl: lestWord deontic = #{lestWord deontic}|]
      ]
    mbOnFailureN <- case (deontic /= DMay, lestWord deontic) of
                      (True, Right lWord) -> do
                        onFailureN <- newNode $ mkTrans lWord
                        traverse_
                          myTraceM
                          [ [i|Petri/r2fgl: mbOnFailureN: first branch; onFailureN=#{onFailureN}|],
                            [i|Petri/r2fgl: drawing edge between obligationN and onFailureN|]
                          ]
                        newEdge' (obligationN, onFailureN, swport)
                        pure $ Just onFailureN
                      _ -> do
                        myTraceM [i|Petri/r2fgl: mbOnFailureN: returning Nothing}|]
                        pure Nothing
    -- let failureNE = NE [( onFailureN, mkTrans $ lestWord deontic ) | deontic /= DMay] [( obligationN, onFailureN, swport) | deontic /= DMay]
    traverse_ newEdge' [(conN, obligationN, []), (obligationN, onSuccessN, seport)]
    pure (onSuccessN, mbOnFailureN)

  -- let sg1 = insertNE (dNE <> dtaNE) sg
  let r2fgl' = xpLog . r2fgl rs (rl2text <$> rlabel <|> defRL)
  let (henceN', henceErr) = case hence of
                              Just henceR -> first (fmap (fromMaybe fulfilledNode)) (r2fgl' henceR)
                              Nothing     -> (pure fulfilledNode, [])
  let ( lestN',  lestErr) = case lest of
                              Just lestR  -> first (fmap (fromMaybe     breachNode)) (r2fgl' lestR)
                              Nothing     -> (pure breachNode, [])

  henceN <- henceN'
  lestN <- lestN'

  newEdge onSuccessN henceN []
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
    lestWord DMust  = Right "not done"
    lestWord DMay   = Left "a MAY has no LEST"  -- this should never arise
    lestWord DShant = Right "violation"

-- r2fgl rs r@Hornlike{} = pure Nothing
r2fgl _rs _defRL _r = pure $ pure Nothing

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
subj2nl _ bsp = [i|Petri/subj2nl: #{bsp}|]

-- we previously had a function to 
-- deonticTemporal :: Rule -> XPileLogE [(Text.Text, Text.Text)]
-- if you ever need it, go look at commit 78d8f4ef058152861e08ff0209ea66251ce2f1b0

-- the BoolStructP could be an AND  .. or an OR .. how do we represent all that in the petri net?
-- for now let's only handle
    -- a simple Leaf situation
    -- an AND situation

actionFragments :: BoolStructP -> [ParamText]
actionFragments (AA.All _ xs) = foldMap actionFragments xs
actionFragments (AA.Leaf x) = [x]
actionFragments _           = []

actionWord :: ParamText -> Text.Text
actionWord = NE.head >>> fst >>> NE.head >>> mtexpr2text
