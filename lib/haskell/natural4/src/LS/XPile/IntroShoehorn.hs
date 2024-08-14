{-# LANGUAGE OverloadedStrings #-}

{-| a basic transpiler, part of the Introductory series

here we shoehorn the environment into the R of the XPileLog's RWST
-}

module LS.XPile.IntroShoehorn (toShoehorn) where

import Control.Monad.Identity (Identity)
import Control.Monad.RWS (RWST, ask, evalRWS, tell)
import Data.Bifunctor (first)
import Data.Text qualified as Text
import LS.Interpreter (Interpreted (..), qaHornsT)
import LS.PrettyPrinter (myrender, (<//>), (</>))
import LS.XPile.IntroReader (MyEnv (..))
import LS.XPile.Logging (XPileLogS, XPileLogW)
import Prettyprinter (Doc, pretty)
import Text.Pretty.Simple (pShowNoColor)

toShoehorn :: Interpreted -> MyEnv -> (String, [String])
toShoehorn l4i = first (Text.unpack . myrender) . xpLog' (inner l4i)

inner :: Interpreted -> MyLog (Doc ann)
inner l4i = do
  myenv <- ask
  let rules = origrules l4i
      genv  = globalEnv myenv
      aenv  = appEnv    myenv
  mutter . Text.unpack . myrender $
    "** the global environment is" <//> pretty (pShowNoColor genv) </>
    "** the app environment is"    <//> pretty (pShowNoColor aenv)
  pure $
    "* output from the IntroShoehorn transpiler" </>
    "** qaHornsT is"               <//> pretty (pShowNoColor (qaHornsT l4i))

type MyLogT m = RWST MyEnv XPileLogW XPileLogS m
type MyLog    = MyLogT Identity

xpLog' :: MyLog a -> MyEnv -> (a, XPileLogW)
xpLog' x r = evalRWS x r mempty

mutter :: String -> MyLog ()
mutter = tell . pure
