{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-| a basic transpiler, part of the Introductory series
-}

module LS.XPile.IntroReader (toReader, defaultReaderEnv, MyEnv(..)) where

import Control.Monad.Reader (Reader, asks, runReader)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as Text
import LS.Interpreter (qaHornsT)
import LS.PrettyPrinter (myrender, (<//>), (</>))
import LS.Rule (Interpreted (..))
import Prettyprinter (Doc, pretty)
import Text.Pretty.Simple (pShowNoColor)

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
  { globalEnv :: HashMap String String
  , appEnv    :: HashMap String String
  }
  deriving (Show)

defaultReaderEnv :: MyEnv
defaultReaderEnv = MyEnv
  { globalEnv = [("foo","bar")]
  , appEnv    = [("baz","quux")]
  }

type MyReader = Reader MyEnv

