{-# LANGUAGE OverloadedStrings #-}

{-| a basic transpiler, part of the Introductory series

here we make the environment Reader the base monad of the XPileLog's RWST, replacing Identity
-}

module LS.XPile.IntroBase (toBase) where

import Data.Bifunctor (first)
import Data.Text qualified as Text
import LS.Interpreter (qaHornsT)
import LS.PrettyPrinter (myrender, (<//>), (</>))
import LS.Rule (Interpreted (..))
import LS.XPile.IntroReader (MyEnv (..))
import LS.XPile.Logging (XPileLogS, XPileLogW)
import Prettyprinter (Doc, pretty)
import Text.Pretty.Simple (pShowNoColor)

-- import Control.Monad.Identity ( Identity )
import Control.Monad.Reader ( MonadReader(ask), runReader, Reader )
import Control.Monad.RWS ( tell, RWST, evalRWST )

toBase :: Interpreted -> MyEnv -> (String, [String])
toBase l4i myenv = first (Text.unpack . myrender) $ runReader (xpLog' (inner l4i)) myenv

inner :: Interpreted -> MyLog (Doc ann)
inner l4i = do
  myenv <- ask
  let rules = origrules l4i
  mutter . Text.unpack . myrender $
    "** the reader environment is" <//> pretty (pShowNoColor myenv)
  return $
    "* output from the IntroShoehorn transpiler" </>
    "** qaHornsT is"               <//> pretty (pShowNoColor (qaHornsT l4i))

type MyLogT m = RWST XPileLogW XPileLogW XPileLogS m
type MyLog    = MyLogT (Reader MyEnv)

xpLog' :: MyLog a -> Reader MyEnv (a, XPileLogW)
xpLog' x = evalRWST x mempty mempty

mutter = tell . pure
