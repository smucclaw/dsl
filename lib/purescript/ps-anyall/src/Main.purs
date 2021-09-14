module Main ( main
            , fromNode1
            , fromNode2
            , fromNode3
            ) where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import AnyAll.Types
import AnyAll.Relevance
import Data.Map as Map
import Data.Either
import Data.Maybe
import Data.Tuple

import Simple.JSON as JSON

main :: Effect Unit
main = do
  log "🍝"
  log $ show $ fromNode3

fromNode1 = "hello node"
fromNode2 x = "hello node, you said " <> x
fromNode3 = output1

example1 :: Item String
example1 = (All (Pre "all of")
                 [ Leaf "walk"
                 , Leaf "run"
                 , Any (Pre "either")
                   [ Leaf "eat"
                   , Leaf "drink" ] ])

marking1 :: Marking String
marking1 = markup $ Map.fromFoldable [Tuple "walk"  $ Left ( Just true )
                                     ,Tuple "run"   $ Left ( Just true )
                                     ,Tuple "eat"   $ Left ( Just true )
                                     ,Tuple "drink" $ Left ( Just false)]

markup x = Marking $ Default <$> x

output1 = qblurt $ relevant Hard DPNormal marking1 Nothing example1

