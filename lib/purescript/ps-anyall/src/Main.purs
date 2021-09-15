module Main ( main
            , fromNode1
            , fromNode2
            , fromNode3
            , example1_nl
            , example1
            , example1_encoded
            , marking1
            , marking1_encoded
            , marking1_decoded
            , marking1_recoded
            , decodeMarking
            , pdpa_dbno_s1p1 , pdpa_dbno_s1p1_nl
            , paint
            , hard, soft
            , howDoWeEven
            , getItemByName
            , decodeItemString
            ) where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import AnyAll.Types
import AnyAll.Relevance(relevant)
import RuleLib.PDPADBNO as RuleLib.PDPADBNO

import Partial.Unsafe
import Data.Map as Map
import Data.Either(Either(..), fromRight, either)
import Data.Maybe(Maybe(..), fromJust)
import Data.Tuple(Tuple(..))
import Data.List(concatMap)
import Foreign.Generic
import Control.Monad.Except
import Foreign

main :: Effect Unit
main = do
  log "🍝"
  log $ show $ fromNode3

hard = Hard
soft = Soft

fromNode1 :: String
fromNode1 = "hello node"

fromNode2 :: String -> String
fromNode2 x = "hello node, you said " <> x

fromNode3 :: QoutJS
fromNode3 = output1

example1 :: Item String
example1 = (All (Pre "all of")
                 [ Leaf "walk"
                 , Leaf "run"
                 , Any (Pre "either")
                   [ Leaf "eat"
                   , Leaf "drink" ] ])

example1_nl :: NLDict
example1_nl =
  Map.fromFoldable [ Tuple "en" $ Map.fromFoldable
                     [ Tuple "walk"  "walk slowly"
                     , Tuple "run"   "run fast"
                     , Tuple "eat"   "eat food"
                     , Tuple "drink" "drink beverages"
                     ]
                   ]

example1_encoded = encode example1

pdpa_dbno_s1p1 = RuleLib.PDPADBNO.schedule1_part1
pdpa_dbno_s1p1_nl = RuleLib.PDPADBNO.schedule1_part1_nl

marking1 :: Marking
marking1 = markup $ Map.fromFoldable [Tuple "walk"  $ Right ( Just true )
                                     ,Tuple "run"   $ Left  ( Just true )
                                     ,Tuple "eat"   $ Right ( Just true )
                                     ,Tuple "drink" $ Left  ( Just false)]

marking1_encoded = encode marking1

decodeMarking :: Foreign -> Marking
decodeMarking marking =
  let eitherm = runExcept $ decode marking
  in either
     (\e -> unsafeCrashWith $ "error in decodeMarking" <> show e)
     (\m -> m)
     eitherm
                           
marking1_decoded = decodeMarking marking1_encoded

marking1_recoded x = decodeMarking $ encode x

output1 :: QoutJS
output1 = qoutjs $ relevant Hard DPNormal marking1 Nothing example1_nl example1

type ItemName = String

itemLibrary = Map.fromFoldable [Tuple "example1" example1]

paint :: Hardness -> Foreign -> NLDict -> Item String -> QoutJS
paint h fm nl item =
  qoutjs $ relevant h DPNormal (decodeMarking fm) Nothing nl item

getItemByName :: String -> Item String
getItemByName itemname =
  case Map.lookup itemname itemLibrary of
    Nothing     -> unsafeCrashWith $ "anyall: unable to find rule item named " <> itemname
    (Just item) -> item
                   
howDoWeEven :: String -> Int -> String
howDoWeEven arg1 arg2 = "arg 1 = " <> arg1 <> "; arg 2 = " <> show arg2


decodeItemString = decodeIt

