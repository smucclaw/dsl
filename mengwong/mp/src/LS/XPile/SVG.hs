{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.SVG where

import LS
import Text.Pretty.Simple
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy (Text)
import AnyAll as AA
import Data.List.NonEmpty (toList)

-- just draw SVGs


-- initially let's just draw the state diagram in a manner typical of GraphViz.
-- see the README

toPetri :: [Rule] -> Text.Text
toPetri rules = Text.unlines $
  ( "* rules as a petri net"
    : take 20 (Text.lines $ pShow rules) )
  <> ( "* rule names"
       : (Text.unwords . ruleName <$> rules)
     )
  <> ( "* to dotlike"
     : concat (r2dotlike 2 <$> rules) )

prefix :: Int -> Text.Text -> Text.Text
prefix n t = Text.pack (replicate n ' ') <> t

r2dotlike :: Int -> Rule -> [Text.Text]
r2dotlike depth r@(Regulative{..}) =
  prefix depth <$>
  (Text.unwords $ "// rule " : ruleName r)
  : maybe [] (\u -> "(upon)" : [prefix 2 $ nodeFmtT (upon2dot u)]) upon
  <> ((\dt -> prefix 4 $ nodeFmtP dt) <$> deonticTemporal r)

r2dotlike depth r =
  pure $ prefix depth (Text.unwords $ "// unhandled rule " : ruleName r)

deonticTemporal :: Rule -> [(Text.Text, Text.Text)]
deonticTemporal r@(Regulative{..}) =
  let d = case deontic of { DMust -> "must"; DMay -> "may"; DShant -> "shant" }
      temp = deadline temporal
      actions = actionFragments action
  in dTshow d temp <$> actions
  where
    dTshow :: Text -> Text -> ParamText -> (Text,Text)
    dTshow deon temp actn =
      let aW = actionWord actn
      in (aW, Text.unlines [ deon
                           , "(" <> temp <> ")"
                           , aW ])

deadline :: Maybe (TemporalConstraint Text) -> Text
deadline Nothing = "whenever"
deadline _       = "eventually"

-- the BoolStructP could be an AND  .. or an OR .. how do we represent all that in the petri net?
-- for now let's only handle
    -- a simple Leaf situation
    -- an AND situation

actionFragments :: Item' lbl [b] -> [b]
actionFragments (AA.All _ xs) = concatMap actionFragments xs
actionFragments (AA.Leaf x) = [x]
actionFragments _           = []
     
actionWord :: ParamText -> Text.Text
actionWord pt = head . fst . head pt
    
nodeFmt, nodeFmtP, nodeFmtT :: (Text.Text, Text.Text) -> Text.Text
nodeFmtP(l,r) = nodeFmt("(" <> l <> ")",r)
nodeFmtT(l,r) = nodeFmt("[" <> l <> "]",r)
nodeFmt (l,r) = l <> "\t\t[label=\"" <> r <> "\"]"

upon2dot :: ParamText -> (Text.Text, Text.Text)
upon2dot pt =
  let short = Text.unwords $ take 2 (Text.words (pt2text pt))
  in (short, pt2text pt)

  
