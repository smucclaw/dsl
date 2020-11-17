-- Output Timed Automata to the XTA format in Uppaal
-- see https://www.it.uu.se/research/group/darts/uppaal/help.php?file=System_Descriptions/Model.shtml

module TaToUppaal where
import AbsSyntax
import Data.List

-- TODO: move into preamble file - better use predefined nub
remove_duplicates :: Eq a => [a] -> [a]
remove_duplicates = foldl (\r x -> if elem x r then r else x : r) []


channels_to_uppaal :: [ClassName] -> String
channels_to_uppaal chans = "chan " ++ (intercalate ", " (map (\(ClsNm nm) -> nm) chans)) ++ ";\n"

system_to_uppaal :: [String] -> String 
system_to_uppaal nms = "system " ++ (intercalate ", " nms) ++ ";"

clock_decls_to_uppaal :: [Clock] -> String
clock_decls_to_uppaal [] = ""
clock_decls_to_uppaal ta_clks = "clock " ++ (intercalate ", " (map (\(Cl cln) -> cln) ta_clks)) ++ ";\n"

bcomparop_to_uppaal :: BComparOp -> String
bcomparop_to_uppaal bop = case bop of
  BCeq -> "=="
  BClt -> "<"
  BClte -> "<="
  BCgt -> ">"
  BCgte -> ">="
  BCne -> "!="


clock_constr_to_uppaal :: ClConstr -> String
clock_constr_to_uppaal (ClCn (Cl cln) bop n) = cln ++ bcomparop_to_uppaal bop ++ show n

clock_constrs_to_uppaal :: String -> [ClConstr] -> String -> String
clock_constrs_to_uppaal before [] after = ""
clock_constrs_to_uppaal before ccs after = before ++ (intercalate " and " (map clock_constr_to_uppaal ccs)) ++ after

-- TODO: what about lbls ???
state_to_uppaal :: [(Loc, [ClConstr])] -> [(Loc, Exp t)] -> Loc -> String
state_to_uppaal invs lbls loc@(Lc ln) =
  "    " ++ ln ++
  case lookup loc invs of
    Nothing ->  ""
    Just ccs -> clock_constrs_to_uppaal " {" ccs "}"

states_to_uppaal :: [(Loc, [ClConstr])] -> [(Loc, Exp t)] -> [Loc] -> String
states_to_uppaal invs lbls ta_locs =
  "state\n" ++
  (intercalate ",\n" (map (state_to_uppaal invs lbls) ta_locs)) ++ ";\n"

-- TODO: several init states, or just one?
init_to_uppaal :: [Loc] -> String
init_to_uppaal init_locs = "init\n" ++ (intercalate ",\n" (map (\(Lc ln) -> "    " ++ ln) init_locs)) ++ ";\n"

clock_reset_to_uppaal :: Clock -> String
clock_reset_to_uppaal (Cl clnm) = clnm ++ " = 0"

clock_resets_to_uppaal :: [Clock] -> String
clock_resets_to_uppaal [] = ""
clock_resets_to_uppaal clks = " assign " ++ (intercalate "," (map clock_reset_to_uppaal clks)) ++ "; "

-- TODO: also take into consideration that actions may be anonymous (maybe just a transition "Event"?)
action_to_uppaal :: Action -> String
action_to_uppaal Internal = ""
action_to_uppaal (Act (ClsNm cnm) Snd) = "sync " ++ cnm ++ "!; "
action_to_uppaal (Act (ClsNm cnm) Rec) = "sync " ++ cnm ++ "?; "

transition_to_uppaal :: Transition -> String
transition_to_uppaal (Trans l1@(Lc l1n) ccs a clks l2@(Lc l2n)) =
  "    " ++ l1n ++ " -> " ++ l2n ++ " {" ++
  clock_constrs_to_uppaal " guard " ccs "; " ++
  action_to_uppaal a ++
  clock_resets_to_uppaal clks ++
  "}"

transitions_to_uppaal :: [Transition] -> String
transitions_to_uppaal trans = "trans\n" ++ (intercalate ",\n" (map transition_to_uppaal trans)) ++ ";\n"

process_to_uppaal :: TA t -> String
process_to_uppaal (TmdAut nm ta_locs ta_act_clss ta_clks trans init_locs invs lbls) =
  "process " ++ nm ++ "() {\n" ++
  clock_decls_to_uppaal ta_clks ++
  states_to_uppaal invs lbls ta_locs ++
  init_to_uppaal init_locs ++
  transitions_to_uppaal trans ++
  "\n}\n"
  

ta_sys_to_uppaal :: TASys Tp -> String
ta_sys_to_uppaal (TmdAutSys tas) =
  let chans = remove_duplicates (concatMap channels_of_ta tas)
      nms = (map name_of_ta tas) 
  in
    channels_to_uppaal chans ++
    concatMap process_to_uppaal tas ++
    system_to_uppaal nms
  



