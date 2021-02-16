
newtype Month = M String deriving (Show, Eq)
type    PlaneFare = Int
data    Employee = E { state :: State, country :: Country } deriving (Show, Eq)
newtype StaffDirectory = SD [Employee] deriving (Show, Eq)
type    State = String
type    Country = String

travelBudget :: Month -> PlaneFare -> StaffDirectory -> [State] -> Int
travelBudget month fare staffdir luckyStates
  | month `elem` [M "jun", M "sep"] = fare * 2 * sum (employeesIn staffdir <$> luckyStates)
  | otherwise = 0

employeesIn :: StaffDirectory -> State -> Int
employeesIn (SD es) s =
  length $ filter (s ==) (state <$> es)

main = do
  let staffdir = SD [ E "CA" "US"
                    , E "BC" "CA"
                    , E "ON" "CA"
                    , E "PA" "US"]
  let pf = 100
  print $ travelBudget (M "jun") 100 staffdir ["CA", "BC"]
