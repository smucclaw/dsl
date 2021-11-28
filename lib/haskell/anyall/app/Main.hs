{-# LANGUAGE DeriveGeneric      #-}   -- much of this due to optparse-generic
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE FlexibleInstances  #-}  -- One more extension.
{-# LANGUAGE StandaloneDeriving #-}  -- To derive Show

module Main where

import AnyAll
import qualified Data.Map.Strict        as Map
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Internal   as DTI
import qualified Data.ByteString.Lazy as B
import           Control.Monad (forM_, when, guard)
import System.Environment
import Data.Maybe
import Data.Either (isRight, fromRight)

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Options.Generic

-- the wrapping 'w' here is needed for <!> defaults and <?> documentation
data Opts w = Opts { demo :: w ::: Bool <!> "False"
                   , only :: w ::: String <!> "" <?> "native | tree | svg"
                   }
  deriving (Generic)
instance ParseRecord (Opts Wrapped)
deriving instance Show (Opts Unwrapped)

-- consume JSON containing
-- - an AnyAll Item
-- - a Marking showing user input to date and defaults

main :: IO ()
main = do
  opts <- unwrapRecord "anyall"
  -- print (opts :: Opts Unwrapped)
  when (demo opts) $ maindemo; guard (not $ demo opts)
  mycontents <- B.getContents
  let myinput = eitherDecode mycontents :: Either String (StdinSchema TL.Text)
  when (only opts == "native") $ print myinput
  guard (isRight myinput)
  let (Right myright) = myinput
  when (only opts == "tree") $
    ppQTree (andOrTree myright) (getDefault <$> (getMarking $ marking myright))
  when (only opts == "svg") $
    print $ makeSvg $ renderItem $ andOrTree myright
  
maindemo :: IO ()
maindemo = do
  forM_
    [ Map.empty
    , Map.fromList [("walk" :: TL.Text,  Left  $ Just True )
                   ,("run",   Left  $ Just True )
                   ,("eat",   Left  $ Just True )
                   ,("drink", Left  $ Just False)]
    , Map.fromList [("walk",  Left  $ Just True )
                   ,("run",   Left  $ Just False)
                   ,("eat",   Left  $ Just True )
                   ,("drink", Left  $ Just False)]
    , Map.fromList [("walk",  Right $ Just True )
                   ,("run",   Right $ Just True )
                   ,("eat",   Right $ Just True )
                   ,("drink", Left  $ Just False)]
    , Map.fromList [("walk",  Right $ Just True )
                   ,("run",   Left  $ Just False)
                   ,("eat",   Right $ Just True )
                   ,("drink", Right $ Just True )]
    , Map.fromList [("walk",  Right $ Just True )
                   ,("run",   Right $ Just False)
                   ,("eat",   Right $ Just True )
                   ,("drink", Left  $ Just True )]
    ] $ ppQTree (AnyAll.All (Just $ Pre "all of")
                 [ Leaf "walk"
                 , Not (Leaf "run")
                 , AnyAll.Any (Just $ Pre "either")
                   [ Leaf "eat"
                   , Leaf "drink" ] ])
  putStrLn "* LEGEND"
  putStrLn ""
  putStrLn "  <    >  View: UI should display this node or subtree."
  putStrLn "                Typically this marks either past user input or a computed value."
  putStrLn "  [    ]  Ask:  UI should ask user for input."
  putStrLn "                Without this input, we cannot make a hard decision."
  putStrLn "  (    )  Hide: UI can hide subtree or display it in a faded, grayed-out way."
  putStrLn "                This subtree has been made irrelevant by other input."
  putStrLn ""
  putStrLn "   YES    user input True"
  putStrLn "    NO    user input False"
  putStrLn "     ?    user input Unknown"
  putStrLn ""
  putStrLn "   yes    default True"
  putStrLn "    no    default False"
  putStrLn "          default Unknown"
  putStrLn ""
  putStrLn "  Hard means we ignore defaults and consider user input only."
  putStrLn "  Soft means we consider defaults as well to arrive at the answer."
