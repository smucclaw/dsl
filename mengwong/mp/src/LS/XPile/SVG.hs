{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.SVG where

import LS
import LS.XPile.Petri
import AnyAll as AA

import Text.Pretty.Simple
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy (Text)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

-- just draw SVGs


-- initially let's just draw the state diagram in a manner typical of GraphViz.
-- see the README

toPetri :: [Rule] -> Text.Text
toPetri rules = Text.unlines $
  ( "* rules as a petri net"
    : Text.lines ( pShow rules) )
  <> ( "* rule names"
       : (Text.unwords . ruleName <$> rules)
     )
  <> ( "* to dotlike"
     : concat (r2dotlike 2 <$> rules) )

prefix :: Int -> Text.Text -> Text.Text
prefix n t = Text.pack (replicate n ' ') <> t

-- going straight to the graphviz is a little too direct.
-- we should probably construct an intermediate representation using fgl.

r2dotlike :: Int -> Rule -> [Text.Text]
r2dotlike depth RegFulfilled   = prefix depth <$> [nodeFmtP("FULFILLED","FULFILLED")]
r2dotlike depth RegBreach      = prefix depth <$> [nodeFmtP("BREACH",   "BREACH")]
r2dotlike depth (RuleAlias rn) = prefix depth <$> [nodeFmtP(Text.unwords rn, Text.unwords $ "SEE" : rn)]
r2dotlike depth r@(Regulative{..}) =
  prefix depth <$>
  (Text.unwords $ "// rule " : ruleName r)
  : (partyWho depth r
     ( maybe [] (\u -> nodeFmtP ("upon","upon")
                     : [prefix 2 $ nodeFmtT (upon2dot u)]) upon
       <> ((\dt -> prefix 4 $ nodeFmtP dt) <$> deonticTemporal r)
       <> [ prefix 6 $ nodeFmtT("then", "and then") ]
       <> (r2dotlike 8 (fromMaybe RegFulfilled hence))
     )
     (r2dotlike 2 (fromMaybe RegBreach lest)))
                                 
r2dotlike depth r =
  pure $ prefix depth "// unhandled rule"

partyWho :: Int -> Rule -> [Text.Text] -> [Text.Text] -> [Text.Text]
partyWho depth r@(Regulative{..}) henceText lestText =
  prefix depth <$>
  let everywho = Text.unwords ( ( if keyword == Every then [ Text.pack (show keyword) ] else [] )
                                <> [ subj2nl NLen subj ] )
  in case who of Nothing -> [ nodeFmtP("START",everywho) ]
                 Just bsr -> -- show two branches -- the first branch goes to the right and is the happy path
                   [ nodeFmtD("START",everywho)
                   ,   prefix 2  $  nodeFmtT("start - pos", "who " <> bsr2text bsr) ]
                   <> (prefix 4 <$> henceText) <>
                   [   prefix 2  $  nodeFmtT("start - neg", "else") ]
                   <> (prefix 4 <$> lestText)
                  

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
    
nodeFmt, nodeFmtP, nodeFmtT :: (Text.Text, Text.Text) -> Text.Text
nodeFmtP(l,r) = nodeFmt("(" <> l <> ")",r) -- Place or State
nodeFmtT(l,r) = nodeFmt("[" <> l <> "]",r) -- Transition
nodeFmtD(l,r) = nodeFmt("<" <> l <> ">",r) -- Decision
nodeFmt (l,r) = l <> "\t\t[label=\"" <> r <> "\"]"

upon2dot :: ParamText -> (Text.Text, Text.Text)
upon2dot pt =
  let short = Text.unwords $ take 2 (Text.words (pt2text pt))
  in (short, pt2text pt)

  
