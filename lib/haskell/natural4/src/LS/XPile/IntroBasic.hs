{-# LANGUAGE OverloadedStrings #-}

{-| a basic transpiler, part of the Introductory series
-}

module LS.XPile.IntroBasic (toBasic) where

import LS.Interpreter       ( qaHornsT )
import LS.PrettyPrinter     ( myrender, (</>) )
import LS.Rule              ( Interpreted(..) )
import Prettyprinter        ( Doc, pretty )
import Text.Pretty.Simple   ( pShowNoColor )
import Data.Text qualified as Text

toBasic :: Interpreted -> String
toBasic = Text.unpack . myrender . toBasicPP

toBasicPP :: Interpreted -> Doc ann
toBasicPP l4i = do
  let rules = origrules l4i
  "output from the IntroBasic transpiler"
    </>
    pretty (pShowNoColor (qaHornsT l4i))
