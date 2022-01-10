{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.SVG where

import LS
import LS.XPile.Petri
import AnyAll as AA

import Data.GraphViz
import Data.GraphViz.Attributes
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree


import           Text.Pretty.Simple
import qualified Data.Text.Lazy     as Text
import           Data.Text.Lazy              (Text)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe                  (fromMaybe, listToMaybe)
import qualified Data.Map           as Map
import Data.GraphViz.Printing (renderDot)

type RuleSet = [Rule]

getByName :: RuleSet -> RuleName -> Maybe Rule
getByName rs rn = listToMaybe (filter (\r -> ruleName r == rn) rs)

-- initially let's just draw the state diagram in a manner typical of GraphViz.
-- see the README

toPetri :: [Rule] -> Text.Text
toPetri rules = Text.unlines $
  ( "* rules as a petri net"
    : Text.lines ( pShow rules) )
  <> ( "* rule names"
       : (Text.unwords . ruleName <$> rules)
     )
  <> ( "* to dotlike" :
     let newgraph = insrules rules startGraph
     in [ renderDot $ unqtDot $ graphToDot (petriParams newgraph) newgraph ] )


prefix :: Int -> Text.Text -> Text.Text
prefix n t = Text.pack (replicate n ' ') <> t

startGraph :: Petri
startGraph = mkGraph [ (1, PN Place "FULFILLED" [])
                     , (0, PN Place "BREACH"    []) ] []


getNodeByLabel :: Petri -> Text -> Maybe Node
getNodeByLabel gr ntxt = listToMaybe $ nodes $ labnfilter (\ln -> ntext (snd ln) == ntxt) gr

insrules :: RuleSet -> Petri -> Petri
insrules rs sg = foldr (\r sg -> let (ns, es) = r2fgl rs sg r
                              in insEdges es $ insNodes ns sg
                       ) sg rs
     
-- we convert each rule to a list of nodes and edges which can be inserted into an existing graph
r2fgl :: RuleSet -> Petri -> Rule -> ([LNode PNode], [LEdge PLabel])
r2fgl rs sg RegFulfilled   = ([],[])
r2fgl rs sg RegBreach      = ([],[])
-- what do we do with a RuleAlias? it only ever appears as the target of a Hence or a Lest,
-- so we just wire it up to whatever existing rule has been labeled appropriately.
-- however, if no existing rule in our list of rules bears that label (yet(, we put in a placeholder state.
-- the following function assumes the rulealias does not appear in the ruleset, so we are calling r2fgl as a last resort.
-- we will do another pass over the graph subsequently to rewire any rulealiases
r2fgl rs sg (RuleAlias rn) = let ntxt = Text.unwords rn
                                 already = getNodeByLabel sg ntxt
                             in maybe ([(head $ newNodes 1 sg, mkPlace ntxt)], []) (const ([],[])) already
r2fgl rs sg r@(Regulative{..}) =
  let newN = newNodes 10 sg
      everywho = Text.unwords ( ( if keyword == Every then [ Text.pack (show keyword) ] else [] )
                                <> [ subj2nl NLen subj ] )
      (whoN,whoE) = case who of Nothing  -> ( [ ( newN !! 0 , mkPlace everywho) ], [] )
                                Just bsr -> ( [ ( newN !! 0, mkDecis everywho )
                                              , ( newN !! 1, mkTrans ("who " <> bsr2text bsr) ) ]
                                            , [ ( newN !! 0, newN !! 1, []) ] )
      (upoN,upoE) = case upon of Nothing -> ( [], [] )
                                 Just pt -> ( [ ( newN !! 2, mkPlace "upon")
                                              , ( newN !! 3, mkTrans (pt2text pt) ) ]
                                            , [ ( fst $ last whoN, newN !! 2, [] )
                                              , ( newN !! 2,       newN !! 3, []) ] )
      (cN,cE) = (whoN ++ upoN, whoE ++ upoE)
      (conN,conE) = case cond of Nothing  -> ( [], [] )
                                 Just bsr -> ( [ ( newN !! 4, mkDecis "if"  )
                                               , ( newN !! 5, mkTrans (bsr2text bsr) ) ]
                                             , [ ( fst $ last cN,  newN !! 4, [] )
                                               , ( newN !! 4, newN !! 5, [] ) ] )
      (dN,dE) = (cN ++ conN, cE ++ conE)
      (dtaN,dtaE) = let deon = case deontic of { DMust -> "must"; DMay -> "may"; DShant -> "shant" }
                        temp = tc2nl NLen temporal
                        actn = actionFragments action
                    in ( [ ( newN !! 6, mkDecis (addnewlines [ deon
                                                             , "(" <> temp <> ")"
                                                             , Text.unwords . NE.toList . fst . NE.head $ head actn ])) -- TODO: fix this -- if we have multiple actions we need to show each one
                         , ( newN !! 7, mkTrans "hence" )
                         , ( newN !! 8, mkTrans "lest" )
                         ]
                       , [ ( fst $ last dN, newN !! 6, [] )
                         , ( newN !! 6, newN !! 7, [])
                         , ( newN !! 6, newN !! 8, [])
                         ] )
      sg1 = insNodes (dN++dtaN) $
            insEdges (dE++dtaE) sg
      
      henceNEs = maybe ([],[]) (r2fgl rs sg1) hence
      sg2 = insEdges (snd henceNEs) $ insNodes (fst henceNEs) sg1

      lestNEs  = maybe ([],[]) (r2fgl rs sg2) lest
      sg3 = insEdges (snd lestNEs)  $ insNodes (fst lestNEs)  sg2
      -- connect up the hence and lest bits
      -- the "hence" transition from dtaE should plug in to the first node in our henceContexts
      toHence = if length henceNEs > 0
                then ([],[(newN !! 7, fst . head . fst $ henceNEs, [])])
                else ([],[])
      toLest  = if length lestNEs > 0
                then ([],[(newN !! 7, fst . head . fst $ lestNEs, [])])
                else ([],[])
  in ((dN ++ dtaN ++ fst henceNEs ++ fst toHence ++ fst lestNEs ++ fst toLest)
     ,(dE ++ dtaE ++ snd henceNEs ++ snd toHence ++ snd lestNEs ++ snd toLest))


r2fgl rs sg r = ([],[])

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
    
