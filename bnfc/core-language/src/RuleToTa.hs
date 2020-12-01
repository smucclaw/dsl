-- Translation of L4 Rules to an internal format and then to Timed Automata

module RuleToTa where
import Data.List
import AbsL
import AbsSyntax
import TaToUppaal
-- import ExampleInput



import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( when )

import Text.Pretty.Simple
import qualified Data.Text.Lazy as T

import LexL    ( Token )
import ParL    ( pTops, myLexer )
import SkelL   ()
import PrintL  ( printTree )
--import AbsL    ( Tops(..), Rule(..), RuleBody(..), MatchVars(..), Toplevels(..) )
import LayoutL ( resolveLayout )
import L4


----------------------------------------------------------------------
-- Internal representation of L4 Rules
----------------------------------------------------------------------

-- A type of simple rules representing events.
-- Rules for class definitions: to be done


data SimpleDefRule
  = DefRule String String [(String, String)]
        -- class name, superclass name, list of field name / type pairs
  deriving (Eq, Ord, Show, Read)

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


data SimpleToplevel = STL [SimpleDefRule] [SimpleEventRule]
  deriving (Eq, Ord, Show, Read)

----------------------------------------------------------------------
-- Translation from L4 BNFC to  internal representation 
----------------------------------------------------------------------

----------------------------------------------------------------------
-- Definitional Rules for defining classes
----------------------------------------------------------------------

isDefRule :: Rule -> Bool
isDefRule ( Rule _ _ _ _ ( RuleDeem _ _ _)) = True
isDefRule _ = False

subDefn :: AbsL.Exp -> (String, String)
subDefn (Op2E (BRel_Isa sbcls spcls )) = (unifyEToString sbcls, unifyEToString spcls)

withInExpToField :: WithIn -> (String, String)
withInExpToField (WithInExp ( Op2E ( BCmp_Eq1 ue1 ue2))) = (unifyEToString ue1, unifyEToString ue2)

withLimbToFields :: WithLimb -> [(String, String)]
withLimbToFields WithLimb1 = []
withLimbToFields (WithLimb2 WithHas_WITH wies) = map withInExpToField wies

defLimbToSDR :: DefineLimb -> SimpleDefRule
defLimbToSDR (DefLimb DefDefine [ CComma e] wl AsofNull) =
  let (cn, scn) = subDefn e  in DefRule cn scn (withLimbToFields wl)

defRuleToSDRs :: Rule -> [SimpleDefRule]
defRuleToSDRs (Rule rid _ _ _ ( RuleDeem _ defs _)) = map defLimbToSDR defs

topDefRulesToSDRs :: [Toplevels] -> [SimpleDefRule]
topDefRulesToSDRs tls  =  concatMap defRuleToSDRs (filter isDefRule (map (\(ToplevelsRule rl) -> rl) tls))

----------------------------------------------------------------------
-- Event Rules expressing state changes
----------------------------------------------------------------------

isEventRule :: Rule -> Bool
isEventRule ( Rule _ _ _ _ ( RModal _ _ _)) = True
isEventRule _ = False


ruleDefToName :: RuleDef -> String
ruleDefToName (RID ( OA_dots [ ObjAttrElemUIdent ( UIdent nm ) ])) = nm


unifyEToString :: AbsL.Exp -> String
unifyEToString (UnifyE ( UnifyExpr1  [ UnifyElemObjAttrElem  ( ObjAttrElemIdent ( Ident nm ) )  ] )) = nm
unifyEToString (UnifyE ( UnifyExpr1  [ UnifyElemObjAttrElem  ( ObjAttrElemUIdent ( UIdent nm ) )  ] )) = nm

givenToString :: GivenLimb -> String
givenToString GivenLimb0 = error "GivenLimb0 not supported"
givenToString (GivenLimb1 (GivenExpr1 [ e ])) = unifyEToString e

--givenWoUponToString :: GivenUpon -> String
--givenWoUponToString (GUGiven gv UponLimb1) 

-- uponLimbToFoo :: UponLimb -> ???
-- uponLimbToFoo UponLimb1 = error "UponLimb1 not supported"
-- uponLimbToFoo (UponLimb2 ur gv) = ???

partyLimbToPartyName :: PartyLimb -> PartyName
partyLimbToPartyName (PartyLimb (PSome (OA_dots [ObjAttrElemUIdent ( UIdent nm ) ])) OptAsAlias0) = PtNm nm
partyLimbToPartyName _ = error "in partyLimbToPartyName: not supported"

stringToSync :: String -> Sync
stringToSync "send" = Snd
stringToSync "recv" = Rec

-- still needed?
unifyExprToSync :: UnifyExpr -> Sync
unifyExprToSync (UnifyExpr1 [ UnifyElemObjAttrElem ( ObjAttrElemIdent ( Ident "send" ) ) ]) = Snd
unifyExprToSync (UnifyExpr1 [ UnifyElemObjAttrElem ( ObjAttrElemIdent ( Ident "recv" ) ) ]) = Rec

objMethodToClassName :: ObjMethod -> ClassName
objMethodToClassName ( ObjMethod1 [ UnifyElemObjAttrElem ( ObjAttrElemUIdent ( UIdent cn ) )] _ _) = ClsNm cn

actionLimbToAction :: ActionLimb -> Action
actionLimbToAction ( ActionSingle ( Op2E ( BRel_Fat ue ( ObjME om ) )) [] OptAsAlias0 )
  = Act (objMethodToClassName om) (stringToSync (unifyEToString ue))
actionLimbToAction ( ActionSingle ( UnifyE _ ) [] OptAsAlias0 ) = Internal

deonticLimbToAction :: DeonticLimb -> Action
deonticLimbToAction ( DeonticLimb1 _ OptLangStrings1 act) = actionLimbToAction act

-- was the same as unifyEToString
--gotoString :: AbsL.Exp -> String
--gotoString (UnifyE ( UnifyExpr1  [ UnifyElemObjAttrElem ( ObjAttrElemUIdent ( UIdent nm ) )  ] )) = nm

gotoToHence :: Goto -> HenceClause
gotoToHence (RGotoOne rd) = Hence [ruleDefToName rd]
gotoToHence (RGotoLst (ListAnd exps exp)) = Hence ((map unifyEToString exps) ++ [unifyEToString exp])
gotoToHence RFulfilled = Hence ["FULFILLED"]
gotoToHence RBreach =  Hence ["BREACH"]

henceLimbToHence :: HenceLimb -> HenceClause
henceLimbToHence ( DHence  gt Args1 OptLangStrings1 ) = gotoToHence gt


givenUponToAction :: GivenExpr -> Action
givenUponToAction ( GivenExpr1 [ Op2E ( BRel_Fat ( UnifyE ue ) ( ObjME om) ) ] ) = Act (objMethodToClassName om) (unifyExprToSync ue)
givenUponToAction ( GivenExpr1 [ Op2E ( BRel_Fat ( UnifyE ue ) gt ) ] ) =  Act (ClsNm (unifyEToString gt)) (unifyExprToSync ue)



uponLimbToAction :: UponLimb -> Action
uponLimbToAction (UponLimb2 Upon0 gvup) = givenUponToAction gvup

eventRuleToSER :: Rule -> SimpleEventRule
eventRuleToSER ( Rule rid _ _ _ (RModal (GUGiven gv UponLimb1) (MD1 pl dl DL0) (WHW WhenLimb0 hence WhereLimb0)))
  = let rn = ruleDefToName rid
        gn = givenToString gv
        pn = partyLimbToPartyName pl
        act = deonticLimbToAction dl
        hc = henceLimbToHence hence
    in EvRule2State rn gn pn act hc 
eventRuleToSER ( Rule rid _ _ _ (RModal (GUGiven gv upon) (MD1 pl dl _) (WHW wh hence WhereLimb0)))
  = let rn = ruleDefToName rid
        gn = givenToString gv
        pn = partyLimbToPartyName pl
        act_upon = uponLimbToAction upon
        act = deonticLimbToAction dl
        hc = henceLimbToHence hence
    in EvRule3State rn gn act_upon pn act hc


topEventRulesToSERs :: [Toplevels] -> [SimpleEventRule]
topEventRulesToSERs tls =  map eventRuleToSER (filter isEventRule (map (\(ToplevelsRule rl) -> rl) tls))

-- to be removed, still used in the run function below
topsToSERs :: Tops -> [SimpleEventRule]
topsToSERs (Toplevel tls) = (topEventRulesToSERs tls)

topsToSTL :: Tops -> SimpleToplevel
topsToSTL (Toplevel tls) = STL (topDefRulesToSDRs tls) (topEventRulesToSERs tls)


----------------------------------------------------------------------
-- Translation SimpleDefRule (internal representation) to Classes
----------------------------------------------------------------------

internalFieldTpToTp :: String -> Tp
internalFieldTpToTp "Bool" = BoolT
internalFieldTpToTp "Int" = IntT
internalFieldTpToTp nm = ClassT (ClsNm nm)

internalFieldToFieldDecl :: (String, String) -> FieldDecl
internalFieldToFieldDecl (fnm, ftp) = FldDecl (FldNm fnm) (internalFieldTpToTp ftp)

sdrToClassDecl :: SimpleDefRule -> ClassDecl (Maybe ClassName)
sdrToClassDecl (DefRule cn scn flds) = ClsDecl (ClsNm cn) (ClsDef (Just (ClsNm scn)) (map internalFieldToFieldDecl flds))

----------------------------------------------------------------------
-- Translation Event Rules (internal representation) to Timed Automata
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

{-
 [
   EvRule2State "OrderBike" "Start" (PtNm "Buyer") (Act (ClsNm "Order") Snd) (Hence ["BuyerPays","DeliverBike"]),
   EvRule3State "DeliverBike" "Start" (Act (ClsNm "Order") Rec) (PtNm "Seller") (Act (ClsNm "Delivery") Snd) (Hence ["ReceivesPay","BuyerPays"]),
   EvRule3State "BuyerPays" "DeliverBike" (Act (ClsNm "Delivery") Rec) (PtNm "Buyer") (Act (ClsNm "Payment") Snd) (Hence ["FULFILLED"]),
   EvRule3State "ReceivesPay" "BuyerPays" (Act (ClsNm "Payment") Rec) (PtNm "Sender") Internal (Hence ["FULFILLED"])]
-}

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

----------------------------------------------------------------------


type Err = Either String
type ParseFun a = [Token] -> Err a

myLLexer = resolveLayout True . myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Tops -> FilePath -> String -> IO ()
runFile v p f_in f_out = putStrLn f_in >> readFile f_in >>= run v p f_out


run :: Verbosity -> ParseFun Tops -> String -> String -> IO ()
run v p f_out s = case p ts of
    Left s -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      putStrV v $ show ts
      putStrLn s
      exitFailure
    Right tree -> -- do
      -- putStrLn "\nParse Successful!"
      -- showTree v tree
       writeFile f_out (ta_sys_to_uppaal (sersToTASys (topsToSERs tree)))
      -- exitSuccess
  where
  ts = myLLexer s

showTree :: Int -> Tops -> IO ()
showTree v tree0
 = let tree = rewriteTree tree0 in do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ T.unpack (pShowNoColor tree)
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree
      let ruleList = getRules tree
      putStrV v $ "\n[Just the Names]\n\n" ++ (unlines $ showRuleName <$> ruleList)


rewriteTree :: Tops -> Tops
rewriteTree (Toplevel tops) = Toplevel $ do
  (ToplevelsRule r@(Rule rdef rname asof metalimb rulebody)) <- tops
  ToplevelsRule <$> case rulebody of
    RMatch mvs -> do
      (MatchVars23 innerRule) <- mvs
      rewrite innerRule
    otherwise -> rewrite r
  

-- mymain :: [String] -> IO ()
-- mymain fs = mapM_ (runFile 2 pTops) fs

-- generate Uppaal code from L4 file
-- for example:
-- genUppaal "../l4/deon_bike_meng_detail.l4" "/home/strecker/Systems/Uppaal/Examples/deon_bike_gen.xta"
genUppaal :: String -> String -> IO ()
genUppaal f_in f_out = runFile 2 pTops f_in f_out

--showTransl :: String -> SimpleTopLevel
-- showTransl :: String -> IO (Either String Tops)
-- showTransl "../l4/deon_bike_meng_detail.l4"
showTransl :: FilePath -> IO ()
showTransl f_in =
  readFile f_in
  >>= (\s -> case (pTops (myLLexer s)) of
               Left _ -> (error "failed")
               Right t -> putStrLn (show (topsToSTL t)))
          
