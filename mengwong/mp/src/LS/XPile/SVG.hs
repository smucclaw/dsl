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
import           Data.Maybe                  (fromMaybe, listToMaybe)
import qualified Data.Map           as Map
import Data.GraphViz.Printing (renderDot)
import Data.GraphViz.Attributes.Complete (Attribute(TailPort,HeadPort, Comment)
                                         , CompassPoint(..)
                                         , PortPos(..))
import Control.Monad.State.Strict (State, MonadState (get, put), evalState)

type RuleSet = [Rule]

getByName :: RuleSet -> RuleName -> Maybe Rule
getByName rs rn = listToMaybe (filter (\r -> ruleName r == rn) rs)

-- initially let's just draw the state diagram in a manner typical of GraphViz.
-- see the README

toPetri :: [Rule] -> Text.Text
toPetri rules =
  let newgraph = insrules rules startGraph
  in renderDot $ unqtDot $ graphToDot (petriParams newgraph) newgraph


prefix :: Int -> Text.Text -> Text.Text
prefix n t = Text.pack (replicate n ' ') <> t

startGraph :: Petri
startGraph = mkGraph [ (1, PN Place "FULFILLED" [])
                     , (0, PN Place "BREACH"    []) ] []


getNodeByLabel :: Petri -> Text -> Maybe Node
getNodeByLabel gr ntxt = listToMaybe $ nodes $ labnfilter (\ln -> ntext (snd ln) == ntxt) gr

insrules :: RuleSet -> Petri -> Petri
insrules rs sg = foldr (\r sg -> let nes = traceShowId $ runGM sg (r2fgl rs sg r)
                              in traceShowId $ insertNE nes sg
                       ) sg rs

data NodesEdges = NE { neNodes :: [LNode PNode], neEdges :: [LEdge PLabel] }
  deriving Show

instance Semigroup NodesEdges where
  (NE ns es) <> (NE ns' es') = NE (ns ++ ns') (es ++ es')
instance Monoid NodesEdges where
  mempty = NE [] []

insertNE :: DynGraph gr => NodesEdges -> gr PNode PLabel -> gr PNode PLabel
insertNE ne = insEdges (neEdges ne) . insNodes (neNodes ne)

{-

Alternative imperative approach:

do
  node1 <- newNode
  node2 <- newNode
  let edge = (node1, node2, "")

-}

newtype GraphState = GS { lastNode :: Node }

newtype GraphMonad a = GM { runGM_ :: State GraphState a }
  deriving newtype (Functor, Applicative, Monad)

instance Semigroup a => Semigroup (GraphMonad a) where
  a <> b = (<>) <$> a <*> b

instance Monoid a => Monoid (GraphMonad a) where
  mempty = pure mempty

newNode :: GraphMonad Node
newNode = do
  n <- lastNode <$> GM get
  GM . put $ GS (n + 1)
  return $ n + 1

runGM :: Petri -> GraphMonad a -> a
runGM gr (GM m) = evalState m (GS n0)
  where (_, n0) = nodeRange gr
  

-- we convert each rule to a list of nodes and edges which can be inserted into an existing graph
r2fgl :: RuleSet -> Petri -> Rule -> GraphMonad NodesEdges
r2fgl rs sg RegFulfilled   = pure mempty
r2fgl rs sg RegBreach      = pure mempty
-- what do we do with a RuleAlias? it only ever appears as the target of a Hence or a Lest,
-- so we just wire it up to whatever existing rule has been labeled appropriately.
-- however, if no existing rule in our list of rules bears that label (yet(, we put in a placeholder state.
-- the following function assumes the rulealias does not appear in the ruleset, so we are calling r2fgl as a last resort.
-- we will do another pass over the graph subsequently to rewire any rulealiases
r2fgl rs sg (RuleAlias rn) = do
  let ntxt = Text.unwords rn
      already = getNodeByLabel sg ntxt
  newNum <- newNode
  pure $ maybe (NE [(newNum, mkPlace ntxt)] []) (const mempty) already
r2fgl rs sg r@(Regulative{..}) = do
  -- let newN = newNodes 9 sg
  --     -- new n = newN !! n
  -- traceShowM newN
  let everywho = Text.unwords ( ( if keyword == Every then [ Text.pack (show keyword) ] else [] )
                                <> [ subj2nl NLen subj ] )
  whoNE <- case who of Nothing  -> do everyN <- newNode; pure $ NE [ ( everyN, mkPlace everywho) ] [] 
                       Just bsr -> do everyN <- newNode
                                      whoN <- newNode
                                      pure $ NE [ ( everyN, mkDecis everywho )
                                         , ( whoN, mkTrans ("who " <> bsr2text bsr) ) ]
                                         [ ( everyN, whoN, []) ]
  upoNE <- case upon of Nothing -> mempty
                        Just pt -> do 
                              uponN <- newNode
                              uponCondN <- newNode
                              pure $ NE [ ( uponN, mkPlace "upon")
                                         , ( uponCondN, mkTrans (pt2text pt) ) ]
                                         [ ( fst $ last $ neNodes whoNE, uponN, [] )
                                         , ( uponN,       uponCondN, []) ]
  let cNE = whoNE <> upoNE
  conNE <- case cond of Nothing  -> mempty
                        Just bsr -> do
                            ifN <- newNode
                            ifCondN <- newNode
                            pure $ NE [ ( ifN, mkDecis "if"  )
                                         , ( ifCondN, mkTrans (bsr2text bsr) ) ]
                                         [ ( fst $ last $ neNodes cNE,  ifN, [] )
                                         , ( ifN, ifCondN, [] ) ]
  let dNE = cNE <> conNE
  obligationN <- newNode
  onSuccessN <- newNode
  onFailureN <- newNode
  let dtaNE = let deon = case deontic of { DMust -> "must"; DMay -> "may"; DShant -> "shant" }
                  temp = tc2nl NLen temporal
                  actn = actionFragments action
                  in NE ([ ( obligationN, mkDecis (addnewlines [ deon
                                               , Text.unwords . NE.toList . fst . NE.head $ head actn
                                               , temp
                                                ]))
           , ( onSuccessN, mkTrans $ (vp2np $ actionWord $ head $ actionFragments action) <> " " <> henceWord deontic)
           ] ++ [( onFailureN, mkTrans $ lestWord deontic ) | deontic /= DMay])
           ([ ( fst $ last $ neNodes dNE, obligationN, [] )
           , ( obligationN, onSuccessN, seport)]
           ++ [( obligationN, onFailureN, swport) | deontic /= DMay]
           )
  let sg1 = insertNE (dNE <> dtaNE) sg

  henceNEs <- maybe (pure mempty) (r2fgl rs sg1) hence
  -- traceM $ "henceNEs: " <> show henceNEs
  let sg2 = insertNE henceNEs sg1

  lestNEs  <- maybe (pure mempty) (r2fgl rs sg2) lest
  -- traceM $ "lestNEs: " <> show lestNEs
  let sg3 = insertNE lestNEs  sg2
      -- connect up the hence and lest bits
      -- the "hence" transition from dtaE should plug in to the first node in our henceContexts
      toHence = if not (null (neNodes henceNEs))
                then NE [] [(onSuccessN, fst . head . neNodes $ henceNEs, [])]
                else NE [] [(onSuccessN, 1, [])]
      toLest
        | not (null (neNodes lestNEs)) = NE [] [(onFailureN, fst . head . neNodes $ lestNEs, [])]
        | deontic /= DMay = NE [] [(onFailureN, 0, [Comment "onoes, go to breach"])]
        | otherwise = mempty
  pure $ dNE <> dtaNE <> henceNEs <> toHence <> lestNEs <> toLest
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

r2fgl rs sg r = pure mempty

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

