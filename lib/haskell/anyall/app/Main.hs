{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import AnyAll
  ( AAVConfig (cdebug, cscale),
    BoolStruct (All, Any, Leaf, Not),
    Default (..),
    Label (Pre),
    Marking (..),
    Scale (Full, Tiny),
    StdinSchema (andOrTree, marking),
    defaultAAVConfig,
    getDefault,
    hardnormal,
    makeSvg,
    nnf,
    ppQTree,
    q2svg',
  )
import Control.Monad (guard, when)
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as B
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Coerce (coerce)
import Data.Either (fromLeft, fromRight, isLeft, isRight)
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as Map
import Data.String.Interpolate (i, __i)
import Data.Text qualified as T
import Options.Generic
  ( Generic,
    ParseRecord,
    Unwrapped,
    Wrapped,
    unwrapRecord,
    type (:::),
    type (<!>),
    type (<?>),
  )
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (FilePath, (</>), (<.>))

-- the wrapping 'w' here is needed for <!> defaults and <?> documentation
data Opts w = Opts { demo :: w ::: Bool <!> "False"
                   , only :: w ::: String <!> "" <?> "native | tree | svg | svgtiny"
                   , debug :: w ::: Bool <!> "False"
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
  when (demo opts) (maindemo >> exitSuccess)

  mycontents <- B.getContents
  let myinput = eitherDecode mycontents :: Either String (StdinSchema T.Text)
  when (isLeft myinput) do
    let fileName :: FilePath =
          "dsl" </> "lib" </> "haskell" </> "anyall" </> "app" </> "Main" <.> "hs"
        s :: String = [i|see #{fileName} source|]
    putStrLn [i|JSON decoding error: #{fromLeft s myinput}|]
    exitFailure
  when (only opts == "native") $ print myinput

  let (Right myright0) = myinput
      mytree = {- addJust $ -} nnf $ andOrTree myright0
      myright = myright0 { andOrTree = mytree }

  when (only opts == "json") $
    putStrLn $ toString $ encodePretty myright

  when (only opts == "tree") $
    ppQTree mytree $ coerce $ marking myright

  when (only opts `elem` words "svg svgtiny") $
    print (makeSvg $
           q2svg' (defaultAAVConfig { cscale = if only opts == "svgtiny" then Tiny else Full
                                    , cdebug = debug opts
                                    }) $
           hardnormal (marking myright) mytree )

maindemo :: IO ()
maindemo = do
  let myqtree = AnyAll.All (Just $ Pre "all of")
                [ Leaf "walk"
                , Not (Leaf "run")
                , AnyAll.Any (Just $ Pre "either")
                  [ Leaf "eat"
                  , Leaf "drink" ]

                , AnyAll.Any Nothing
                  [ Leaf "eat"
                  , Leaf "drink" ]
                ]
  for_
    [ Map.empty
    , Map.fromList [("walk" :: T.Text,  Left  $ Just True )
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
    ] $ ppQTree myqtree

  putStrLn [__i|
    * just the AndOr tree as JSON"
    #{encodePretty myqtree}

    * LEGEND

    <    >  View: UI should display this node or subtree.
                  Typically this marks either past user input or a computed value.
    [    ]  Ask:  UI should ask user for input.
                  Without this input, we cannot make a hard decision.
    (    )  Hide: UI can hide subtree or display it in a faded, grayed-out way.
                  This subtree has been made irrelevant by other input.

      YES    user input True
       NO    user input False
        ?    user input Unknown

      yes    default True
       no    default False
             default Unknown

    Hard means we ignore defaults and consider user input only.
    Soft means we consider defaults as well to arrive at the answer.
  |]
