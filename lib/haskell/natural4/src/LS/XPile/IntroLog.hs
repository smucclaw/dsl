{-# LANGUAGE OverloadedStrings #-}

{-| a basic transpiler, part of the Introductory series
-}

module LS.XPile.IntroLog (toLog) where

import Control.Monad.Reader (Reader, asks, runReader)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as Text
import LS.Interpreter (qaHornsT)
import LS.PrettyPrinter (myrender, (<//>), (</>))
import LS.Rule (Interpreted (..))
import LS.XPile.IntroReader (MyEnv (..), defaultReaderEnv)
import LS.XPile.Logging (XPileLog, mutter)
import Prettyprinter (Doc, pretty)
import Text.Pretty.Simple (pShowNoColor)

toLog :: Interpreted -> MyEnv -> XPileLog String
toLog l4i myenv = Text.unpack . myrender <$> inner l4i myenv

inner :: Interpreted -> MyEnv -> XPileLog (Doc ann)
inner l4i myenv = do
  let rules = origrules l4i
      genv = globalEnv myenv
      aenv = appEnv    myenv
  mutter . Text.unpack . myrender $
    "** the global environment is" <//> pretty (pShowNoColor genv) </>
    "** the app environment is"    <//> pretty (pShowNoColor aenv)
  return $
    "* output from the IntroLogging transpiler" </>
    "** qaHornsT is"               <//> pretty (pShowNoColor (qaHornsT l4i))

