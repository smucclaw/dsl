{-# LANGUAGE OverloadedStrings #-}

{-| a basic transpiler, part of the Introductory series
-}

module LS.XPile.IntroLog (toLog) where

import Data.Text qualified as Text
import LS.Interpreter (qaHornsT)
import LS.PrettyPrinter (myrender, (<//>), (</>))
import LS.Rule (Interpreted (..))
import LS.XPile.IntroReader (MyEnv (..))
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
  pure $
    "* output from the IntroLogging transpiler" </>
    "** qaHornsT is"               <//> pretty (pShowNoColor (qaHornsT l4i))

