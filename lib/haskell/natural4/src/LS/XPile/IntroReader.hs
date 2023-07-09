{-# LANGUAGE OverloadedStrings #-}

{-| a basic transpiler, part of the Introductory series
-}

module LS.XPile.IntroReader (toReader, defaultReaderEnv, MyEnv(..)) where

import LS.Interpreter       ( qaHornsT )
import LS.PrettyPrinter     ( myrender, (</>), (<//>) )
import LS.Rule              ( Interpreted(..) )
import Prettyprinter        ( Doc, pretty )
import Text.Pretty.Simple   ( pShowNoColor )
import Data.Text qualified as Text

import           Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Reader ( runReader, Reader, asks )

toReader :: Interpreted -> MyEnv -> String
toReader l4i myenv = Text.unpack $ myrender $ runReader (inner l4i) myenv

inner :: Interpreted -> MyReader (Doc ann)
inner l4i = do
  let rules = origrules l4i
  genv <- asks globalEnv
  aenv <- asks appEnv
  return $
    "* output from the IntroReader transpiler" </>
    "** the global environment is" <//> pretty (pShowNoColor genv) </>
    "** the app environment is"    <//> pretty (pShowNoColor aenv) </>
    "** qaHornsT is"               <//> pretty (pShowNoColor (qaHornsT l4i))

-- | we equip our basic transpiler with an environment reader

data MyEnv = MyEnv
  { globalEnv :: Map String String
  , appEnv    :: Map String String
  }
  deriving (Show)

defaultReaderEnv :: MyEnv
defaultReaderEnv = MyEnv
  { globalEnv = Map.fromList [("foo","bar")]
  , appEnv    = Map.fromList [("baz","quux")]
  }

type MyReader = Reader MyEnv

