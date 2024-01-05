import LS qualified as SFL4
import Criterion.Main

main :: IO ()
main = defaultMain [
        bench "dummy benchmark" $ whnf SFL4.dumpRules undefined
    ]