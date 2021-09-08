
import AnyAll.Types
import AnyAll.Relevance
import qualified Data.Map.Strict as Map

main :: IO ()
main = putStrLn "Test suite not yet implemented"


type SingLabel = String

mustSing :: Item SingLabel
mustSing =
  All (Pre "both")
  [ Leaf "walk"
  , Any (Pre "either")
    [ Leaf "eat"
    , Leaf "drink" ] ]
  
mS_marking0t :: Marking SingLabel
mS_marking0t = Map.fromList [("walk",  Left $ Just True)
                            ,("eat",   Left $ Just True)
                            ,("drink", Left $ Just True)]

mS_r0t = relevant mustSing mS_marking0t
