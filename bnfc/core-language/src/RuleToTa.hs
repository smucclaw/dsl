-- Translation of L4 Rules to an internal format and then to Timed Automata

module RuleToTa where
import Data.List
import AbsL
import AbsSyntax
import TaToUppaal
import ExampleInput

----------------------------------------------------------------------
-- Internal representation of L4 Rules
----------------------------------------------------------------------

-- A type of simple rules representing events.
-- Rules for class definitions: to be done


data HenceClause = Hence [String]
  deriving (Eq, Ord, Show, Read)

data SimpleEventRule
  = EvRule2State String String PartyName Action HenceClause
              -- Rule name, given state, party, action, hence
  | EvRule3State String String Action PartyName Action HenceClause
             -- Rule name, given state, upon event, party, action, hence
  deriving (Eq, Ord, Show, Read)

serPartyName :: SimpleEventRule -> PartyName
serPartyName (EvRule2State rn gv pt act (Hence hcs)) = pt
serPartyName (EvRule3State rn gv upon pt act (Hence hcs)) = pt


----------------------------------------------------------------------
-- Translation from L4 BNFC to  internal representation 
----------------------------------------------------------------------


isEventRule :: Rule -> Bool
isEventRule ( Rule _ _ _ _ ( RModal _ _ _)) = True
isEventRule _ = False


ruleDefToName :: RuleDef -> String
ruleDefToName (RID ( OA_dots [ ObjAttrElemUIdent ( UIdent nm ) ])) = nm


givenToString :: GivenLimb -> String
givenToString GivenLimb0 = error "GivenLimb0 not supported"
givenToString (GivenLimb1 (GivenExpr1 [ UnifyE ( UnifyExpr1  [ UnifyElemObjAttrElem  ( ObjAttrElemUIdent ( UIdent nm ) )  ] ) ])) = nm

--givenWoUponToString :: GivenUpon -> String
--givenWoUponToString (GUGiven gv UponLimb1) 

-- uponLimbToFoo :: UponLimb -> ???
-- uponLimbToFoo UponLimb1 = error "UponLimb1 not supported"
-- uponLimbToFoo (UponLimb2 ur gv) = ???

partyLimbToPartyName :: PartyLimb -> PartyName
partyLimbToPartyName (PartyLimb (PSome (OA_dots [ObjAttrElemUIdent ( UIdent nm ) ])) OptAsAlias0) = PtNm nm
partyLimbToPartyName _ = error "in partyLimbToPartyName: not supported"


unifyExprToSync :: UnifyExpr -> Sync
unifyExprToSync (UnifyExpr1 [ UnifyElemObjAttrElem ( ObjAttrElemIdent ( Ident "send" ) ) ]) = Snd
unifyExprToSync (UnifyExpr1 [ UnifyElemObjAttrElem ( ObjAttrElemIdent ( Ident "recv" ) ) ]) = Rec

objMethodToClassName :: ObjMethod -> ClassName
objMethodToClassName ( ObjMethod1 [ UnifyElemObjAttrElem ( ObjAttrElemUIdent ( UIdent cn ) )] _ _) = ClsNm cn

actionLimbToAction :: ActionLimb -> Action
actionLimbToAction ( ActionSingle ( Op2E ( BRel_Fat ( UnifyE ue) ( ObjME om ) )) [] OptAsAlias0 )
  = Act (objMethodToClassName om) (unifyExprToSync ue)
actionLimbToAction ( ActionSingle ( UnifyE _ ) [] OptAsAlias0 ) = Internal

deonticLimbToAction :: DeonticLimb -> Action
deonticLimbToAction ( DeonticLimb1 _ OptLangStrings1 act) = actionLimbToAction act

gotoString :: AbsL.Exp -> String
gotoString (UnifyE ( UnifyExpr1  [ UnifyElemObjAttrElem ( ObjAttrElemUIdent ( UIdent nm ) )  ] )) = nm

gotoToHence :: Goto -> HenceClause
gotoToHence (RGotoOne rd) = Hence [ruleDefToName rd]
gotoToHence (RGotoLst (ListAnd exps exp)) = Hence ((map gotoString exps) ++ [gotoString exp])
gotoToHence RFulfilled = Hence ["FULFILLED"]
gotoToHence RBreach =  Hence ["BREACH"]

henceLimbToHence :: HenceLimb -> HenceClause
henceLimbToHence ( DHence  gt Args1 OptLangStrings1 ) = gotoToHence gt


givenUponToAction :: GivenExpr -> Action
givenUponToAction ( GivenExpr1 [ Op2E ( BRel_Fat ( UnifyE ue ) ( ObjME om) ) ] ) = Act (objMethodToClassName om) (unifyExprToSync ue)
givenUponToAction ( GivenExpr1 [ Op2E ( BRel_Fat ( UnifyE ue ) gt ) ] ) =  Act (ClsNm (gotoString gt)) (unifyExprToSync ue)



uponLimbToAction :: UponLimb -> Action
uponLimbToAction (UponLimb2 Upon0 gvup) = givenUponToAction gvup

ruleToSER :: Rule -> SimpleEventRule
ruleToSER ( Rule rid _ _ _ (RModal (GUGiven gv UponLimb1) (MD1 pl dl DL0) (WHW WhenLimb0 hence WhereLimb0)))
  = let rn = ruleDefToName rid
        gn = givenToString gv
        pn = partyLimbToPartyName pl
        act = deonticLimbToAction dl
        hc = henceLimbToHence hence
    in EvRule2State rn gn pn act hc 
ruleToSER ( Rule rid _ _ _ (RModal (GUGiven gv upon) (MD1 pl dl _) (WHW wh hence WhereLimb0)))
  = let rn = ruleDefToName rid
        gn = givenToString gv
        pn = partyLimbToPartyName pl
        act_upon = uponLimbToAction upon
        act = deonticLimbToAction dl
        hc = henceLimbToHence hence
    in EvRule3State rn gn act_upon pn act hc


toplevelsToSERs :: [Toplevels] -> [SimpleEventRule]
toplevelsToSERs tls =  map ruleToSER (filter isEventRule (map (\(ToplevelsRule rl) -> rl) tls))


----------------------------------------------------------------------
-- Translation from internal representation to Timed Automata
----------------------------------------------------------------------

serToTransition :: SimpleEventRule -> [Transition]
serToTransition (EvRule2State rn gv pt act (Hence hcs)) =
  let start_loc = Lc gv
      end_loc = Lc (head hcs)
      clcstr = []    -- temporarily
      clreset = []   -- temporarily
  in [Trans start_loc clcstr act clreset end_loc]
serToTransition (EvRule3State rn gv upon pt act (Hence hcs)) =
  let start_loc = Lc gv
      interm_loc = Lc rn
      end_loc = Lc (head hcs)
      clcstr1 = []    -- temporarily
      clreset1 = []   -- temporarily
      clcstr2 = []    -- temporarily
      clreset2 = []   -- temporarily
  in [Trans start_loc clcstr1 upon clreset1 interm_loc,
      Trans interm_loc clcstr2 act clreset2 end_loc]

-- [EvRule2State "OrderBike" "Start" (PtNm "Buyer") (Act (ClsNm "Order") Snd) (Hence ["BuyerPays","DeliverBike"]),EvRule3State "DeliverBike" "Start" (Act (ClsNm "Order") Rec) (PtNm "Seller") (Act (ClsNm "Delivery") Snd) (Hence ["ReceivesPay","BuyerPays"]),EvRule3State "BuyerPays" "DeliverBike" (Act (ClsNm "Delivery") Rec) (PtNm "Buyer") (Act (ClsNm "Payment") Snd) (Hence ["FULFILLED"]),EvRule3State "ReceivesPay" "BuyerPays" (Act (ClsNm "Payment") Rec) (PtNm "Sender") Internal (Hence ["FULFILLED"])]


-- the list of sublists having the same value under f
quotientByResult :: Eq b => (a -> b) -> [a] -> [(b, [a])]
quotientByResult f [] = []
quotientByResult f (x:xs) = 
  let ec = (f x)
      (pos, neg) = partition (\e -> ec == (f e)) xs 
  in (ec, (x:pos)) : quotientByResult f neg

transitionsToTA :: (PartyName, [Transition]) -> TA t
transitionsToTA ((PtNm pn), trans) =
  let locs = nub (concatMap (\(Trans l1 _ _ _ l2) -> [l1, l2]) trans)
      chans = nub (concatMap (\(Trans _ _ act _ _) -> action_name act) trans)
      clcks = []   -- TODO
      init_loc = [Lc "Start"]   -- TODO
      invs = []    -- TODO
      lbls = []   -- TODO
  in TmdAut pn locs chans clcks trans init_loc invs lbls

-- TODO: exact type parameter of TASys to be determined
sersToTASys :: [SimpleEventRule] -> TASys t
sersToTASys sers =
  let ruleQuot = quotientByResult serPartyName sers
      transQuot = map (\(p, rls) -> (p, concatMap serToTransition rls)) ruleQuot
  in TmdAutSys (map transitionsToTA transQuot)
