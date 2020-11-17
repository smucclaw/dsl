
import System.Process(callProcess)


main :: IO ()
main = do
    -- _ <- runL4 ["l4/test.l4"] ""
    -- pure ()
    runL4 ["l4/test.l4"]
    -- runL4 ["tmp/foo.l4"]

-- runL4 :: [String] -> String -> IO String
-- runL4 = readProcess "l4"
runL4 :: [String] -> IO ()
runL4 = callProcess "l4"

-- stack exec l4 l4/test.l4 > out/test.out 2> out/test.err || stack run showbug l4/test.l4 out/test.out out/test.err


-- TODO:
-- * Run showbug on errors (As library or as executable)
-- * Run L4 as a library (but maybe also keep some integration test)
-- * Use a test framework