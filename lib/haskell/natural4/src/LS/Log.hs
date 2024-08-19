module LS.Log (
  -- * Logger Type and utility functions
  Tracer (..),
  traceWith,
  cmap,
  module CoFunctor,

  -- * Experimental logger backends
  prettyTracer,
)
where

import Data.Functor.Contravariant as CoFunctor
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import System.IO (stderr)
import qualified Data.Text.IO as Text

newtype Tracer m a = Tracer {runTracer :: a -> m ()}

instance Contravariant (Tracer m) where
  contramap f (Tracer m) = Tracer (m . f)

instance (Applicative m) => Semigroup (Tracer m a) where
  tracer1 <> tracer2 = Tracer $ \a -> runTracer tracer1 a *> runTracer tracer2 a

instance (Applicative m) => Monoid (Tracer m a) where
  mempty = Tracer $ \_ -> pure ()

traceWith :: Tracer m a -> a -> m ()
traceWith tracer msg = runTracer tracer msg

-- | Shorter name for 'contramap' specialised to
cmap :: forall a' a m. (a' -> a) -> Tracer m a -> Tracer m a'
cmap = contramap

prettyTracer :: (Pretty a) => Tracer IO a
prettyTracer =
  Tracer $
    Text.hPutStrLn stderr
      . renderStrict
      . layoutPretty defaultLayoutOptions
      . pretty
