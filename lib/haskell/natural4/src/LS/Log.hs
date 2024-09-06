module LS.Log (
  -- * Logger Type and utility functions
  Tracer (..),
  IOTracer,
  traceWith,
  liftIOTracer,
  cmap,
  module CoFunctor,

  -- * Experimental logger backends
  prettyTracer,
)
where

import Control.Monad.IO.Class
import Data.Functor.Contravariant as CoFunctor
import Data.Text.IO qualified as Text
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import System.IO (stderr)

newtype Tracer m a = Tracer
  { runTracer :: a -> m ()
  }

type IOTracer = Tracer IO

instance Contravariant (Tracer m) where
  contramap f (Tracer m) = Tracer (m . f)

instance (Applicative m) => Semigroup (Tracer m a) where
  tracer1 <> tracer2 = Tracer $ \a -> runTracer tracer1 a *> runTracer tracer2 a

instance (Applicative m) => Monoid (Tracer m a) where
  mempty = Tracer $ \_ -> pure ()

traceWith :: Tracer m a -> a -> m ()
traceWith tracer msg = runTracer tracer msg

-- tracerWith :: (MonadIO m) => IOTracer a -> a -> m ()
-- tracerWith tracer msg = runTracer (runIOTracer tracer) msg

-- | Shorter name for 'contramap' specialised to
cmap :: forall a' a m. (a' -> a) -> Tracer m a -> Tracer m a'
cmap = contramap

prettyTracer :: (Pretty a, MonadIO m) => Tracer m a
prettyTracer =
  Tracer $
    liftIO
      . Text.hPutStrLn stderr
      . renderStrict
      . layoutPretty defaultLayoutOptions
      . pretty

liftIOTracer :: MonadIO m => IOTracer a -> Tracer m a
liftIOTracer tr = Tracer $ \msg -> liftIO $ runTracer tr msg
