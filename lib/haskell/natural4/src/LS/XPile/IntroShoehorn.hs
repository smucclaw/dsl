{-# LANGUAGE OverloadedStrings #-}

{-| a basic transpiler, part of the Introductory series

here we shoehorn the environment into the R of the XPileLog's RWST
-}

module LS.XPile.IntroShoehorn (toShoehorn) where

import LS.Interpreter       ( qaHornsT )
import LS.PrettyPrinter     ( myrender, (</>), (<//>) )
import LS.Rule              ( Interpreted(..) )
import LS.XPile.Logging     ( XPileLogW, XPileLogS )
import LS.XPile.IntroReader ( MyEnv(..), defaultReaderEnv  )
import Prettyprinter        ( Doc, pretty )
import Text.Pretty.Simple   ( pShowNoColor )
import Data.Text qualified as Text
import Data.Bifunctor       ( first )

import           Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Identity ( Identity )
import Control.Monad.RWS ( ask, tell, RWST, evalRWS, evalRWST )

toShoehorn :: Interpreted -> MyEnv -> (String, [String])
toShoehorn l4i myenv = first (Text.unpack . myrender) $ xpLog' (inner l4i) myenv

inner :: Interpreted -> MyLog (Doc ann)
inner l4i = do
  myenv <- ask
  let rules = origrules l4i
      genv  = globalEnv myenv
      aenv  = appEnv    myenv
  mutter . Text.unpack . myrender $
    "** the global environment is" <//> pretty (pShowNoColor genv) </>
    "** the app environment is"    <//> pretty (pShowNoColor aenv)
  return $
    "* output from the IntroShoehorn transpiler" </>
    "** qaHornsT is"               <//> pretty (pShowNoColor (qaHornsT l4i))

type MyLogT m = RWST MyEnv XPileLogW XPileLogS m
type MyLog    = MyLogT Identity

xpLog' :: MyLog a -> MyEnv -> (a, XPileLogW)
xpLog' x r = evalRWS x r mempty

mutter = tell . pure
