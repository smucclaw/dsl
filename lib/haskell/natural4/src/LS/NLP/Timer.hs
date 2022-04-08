module LS.NLP.Timer where
import Data.IORef (IORef, writeIORef, newIORef, readIORef)
import System.CPUTime (getCPUTime)
import GHC.IO (unsafePerformIO)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Time.Clock

globalTimer :: IORef Integer
globalTimer = unsafePerformIO $ newIORef 0
{-# NOINLINE globalTimer #-}

startTimer :: String -> IO ()
startTimer msg = do
    ePutStrLn $ "Starting " ++ msg
    start <- getCPUTime
    -- getTime
    writeIORef globalTimer start

tickTimer :: String -> IO ()
tickTimer msg = do
    start <- readIORef globalTimer
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^(12 :: Int)) :: Double
    ePutStrLn $ "Tick " ++ msg ++ ": " ++ printf "%.3f" diff
    writeIORef globalTimer end

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr


-- | Wrap a 'MonadIO' computation so that it prints out the execution time.
timeIt :: MonadIO m => m a -> m a
timeIt = timeItNamed "CPU time"

-- | Like 'timeIt', but uses the 'show' rendering of @a@ as label for the
-- timing.
--
-- @since 2.0
timeItShow :: (MonadIO m, Show a) => m a -> m a
timeItShow ioa = do
    ((t, t'), a) <- timeItT ioa
    liftIO $ printf (show a ++ ": %6.2fs, Wall time: %s\n") t (show t')
    return a

-- | Like 'timeIt', but uses the 'String' as label for the timing.
--
-- @since 2.0
timeItNamed :: MonadIO m => String -> m a -> m a
timeItNamed name ioa = do
    ((t, t'), a) <- timeItT ioa
    liftIO $ printf (name ++ ": %6.2fs, Wall time: %s\n") t (show t')
    return a

-- | Wrap a 'MonadIO' computation so that it returns execution time in seconds,
-- as well as the result value.
timeItT :: MonadIO m => m a -> m ((Double, NominalDiffTime), a)
timeItT ioa = do
    t1 <- liftIO getCPUTime
    t1' <- liftIO getCurrentTime
    a <- ioa
    t2 <- liftIO getCPUTime
    t2' <- liftIO getCurrentTime
    let t :: Double
        t = fromIntegral (t2-t1) * 1e-12
        t' = diffUTCTime t2' t1'
    return ((t, t'), a)