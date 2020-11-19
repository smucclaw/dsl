{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module ToGF
  ( bnfc2str,
  )
where

import AbsL
import Data.Char (toLower)
import PGF (Expr, PGF, linearizeAll, showExpr)
import Top
import Data.Maybe (mapMaybe)

-- BNFC to GF tree

bnfc2str :: PGF -> Tops -> String
bnfc2str gr tops =
  unlines
    [ unlines
        [ unlines $ linearizeAll gr tree,
          "Abstract syntax tree in GF:",
          showExpr [] tree
        ]
      | tree <- toGF tops
    ]

toGF :: Tops -> [Expr]
toGF (Toplevel tops) = concat $ mapMaybe toTree tops
  where
    toTree (ToplevelsRule rule) = Just $ rule2gf rule
    toTree (ToplevelsScenario _) = error "Scenarios not yet translated to GF"
    toTree _ = Nothing

rule2gf :: AbsL.Rule -> [Expr]
rule2gf (Rule def (RName name) asof meta rulebody) = map (gf . namefun) (body2gf rulebody)
  where
    namefun = case name of
      Description nm -> GRuleName (GString nm)
      _ -> id

body2gf :: RuleBody -> [GSentence]
body2gf (RModal given modal whw) = [modal2gf modal]
body2gf (RuleDeem given defs whw) = map def2gf defs
body2gf x = error $ "body2gf doesn't handle yet " ++ show x
--body2gf _ = GMAction GNobody (GMay GFailure)

whw2gf :: WhenHenceWhere -> [GSentence]
whw2gf (WHW WhenLimb0 henc wher) = []
whw2gf (WHW (WhenLimb1 exp) henc wher) = []


def2gf :: DefineLimb -> GSentence
def2gf (DefLimb _ [cc] with asof) = GMDefTermIs kind term
  where -- TODO: support many defs, return [GSentence]
    (kind,term) = case cc of
      ArgEq e f  -> (uexp2kind e, uexp2term GASg f) 
      DefIs e f  -> (uexp2kind e, uexp2term GASg f)
      DefIsa e f -> (uexp2kind e, uexp2term GASg f)
      Mul e f -> (uexp2kind e, uexp2term GASg f)
      _ -> error $ "def2gf doesn't support yet " ++ show cc      
def2gf x = error $ "def2gf doesn't support yet " ++ show x
modal2gf :: ModalLimb -> GSentence
modal2gf (MD1 party deonticl deadline) = GMAction (party2gf party) (deontic2gf deonticl)

party2gf :: PartyLimb -> GParty
party2gf (PartyLimb partydef _) = partydef2gf partydef
party2gf x = error $ "party2gf doesn't handle yet " ++ show x

partydef2gf :: PartyDef -> GParty
partydef2gf (PEvery _) = GEverybody
partydef2gf (PNobody _) = GNobody
partydef2gf (PSome oa) = oa2party oa

oa2party :: ObjAttr -> GParty
oa2party (OA_dots es) = toParty $ unwords $ map oa2str es -- "Foo.Bar.Baz" becomes "foo bar baz"

deontic2gf :: DeonticLimb -> GDeontic
deontic2gf (DeonticLimb1 deonexpr langstrs actionl) = deonticFun $ action2gf actionl
  where
    deonticFun :: GActionAlias -> GDeontic
    deonticFun = case deonexpr of
      DEMust -> GMust
      DEMay -> GMay
      DEShant -> GShant

action2gf :: ActionLimb -> GActionAlias
action2gf (ActionSingle (ExpAction a) blahs OptAsAlias0) = GAAlias a
action2gf (ActionSingle (ExpAction a) blahs alias) = GAAlias a -- TODO: use actual alias
action2gf (ActionSingle e blahs alias) = -- Object is in the BlahExp, transform into => structure
  action2gf (ActionSingle (Op2E (BRel_Fat e obj)) blahs alias')
  where 
    (obj, alias') = case blahs of
      BlahExp (AsAliasE o a):_ -> (o, OptAsAlias1 a)
      BlahExp o:_ -> (o, alias)
      x -> error $ "action2gf doesn't handle yet BlahExp" ++ show x
action2gf x = error $ "action2gf doesn't handle yet " ++ show x
-- action2gf ActionSingle {} = GFailure
-- action2gf ActionMulti {} = GFailure

exp2action :: Exp -> Maybe GAction
exp2action exp = case exp of
  -- Op1E unaOp exp ->
  Op2E binExp -> binexp2action binExp
  -- Op3E triOp e f g ->
  -- ConstE constVal ->
  -- CaseE caseExpr ->
  -- ListE lstExp ->
  -- BracesE braceList ->
  -- TempE dateTime ->
  -- UnifyE unifyExpr ->
  -- ObjME objMethod ->
  -- Op3ETern1 e f g ->
  -- Op3ETern2 exp stms ->
  -- Op3ETern3 exp ss ts ->
  -- AsAliasE exp asAlias ->
  _ -> Nothing

ue2oa :: UnifyElem -> ObjAttrElem
ue2oa (UnifyElemObjAttrElem oa) = oa
ue2oa _ = error "ue2oa: expected ObjAttr"

binexp2action :: BinExp -> Maybe GAction
binexp2action (BRel_Fat e1 e2) = vp
  where
    verbs = exp2verb e1
    (obj, f) = exp2obj e2
    vp = case (verbs, obj) of
      (v : _, o) -> Just $ f $ GAComplDir v o
      _ -> Nothing
binexp2action _ = Nothing

exp2obj :: Exp -> (GTerm, GAction -> GAction)
exp2obj (ObjME (ObjMethod1 es args langstrs)) = (uexp2term GTheSg (UnifyE1 es), f)
  where
    f = case args of
      Args1 -> id
      Args2 ccs -> head $ map ccs2fun ccs
    ccs2fun (ArgEq e p) =
      let f = uexp2fun e
          x = uexp2party p
       in f x
    unifyelem2party = toParty . oa2str . ue2oa
exp2obj e@(UnifyE1 _) = (uexp2term GTheSg e, id)
exp2obj x = error $ "exp2obj can't handle: " ++ show x

exp2verb :: Exp -> [GAction_Dir]
exp2verb (UnifyE1 es) = verbs
  where
    verbs = map unifyelem2verb es
    unifyelem2verb e = case e of
      UnifyElemObjAttrElem (OaStr oa) -> toV2 oa
      _ -> GCook -- default option, silly on purpose
exp2verb (ObjME objMethod) = []
exp2verb _ = []

-----------------------------------------------------------------------------
-- Patterns to reduce the complexity of AbsL

-- LangStrings
type Lang = String

pattern LangId :: Lang -> LangID
pattern LangId lang = LangID1 [LangLabelIdent (Ident lang)]

describeLang :: Lang -> OptLangStrings -> [String]
describeLang lang (OptLangStringsLangStrings (ELangStrings es)) =
  [str | (ELangString (LangId l) str) <- es, l == lang]
describeLang _ _ = []

pattern Description :: String -> OptLangStrings
pattern Description str <- (describeLang "en" -> str : _) -- hardcoded for Eng

-- Aliases
pattern AliasStr :: Ident -> AsAlias
--pattern AliasStr str <- (AsAlias (OA_dots [OaStr str]))
pattern AliasStr str = AsAlias (OA_dots [ObjAttrElemIdent str]) 

pattern AliasExp :: UIdent -> Ident -> Exp
pattern AliasExp s a = 
  AsAliasE (UnifyE (UnifyExpr1 [UnifyElemObjAttrElem (ObjAttrElemUIdent s)])) (AliasStr a)

-- ObjAttr
oa2str :: ObjAttrElem -> String
oa2str e = case e of
  ObjAttrElemIdent (Ident str) -> str
  ObjAttrElemUIdent (UIdent str) -> str

pattern OaStr :: String -> ObjAttrElem
pattern OaStr str <- (oa2str -> str)

-- Action
pattern ExpAction :: GAction -> Exp
pattern ExpAction act <- (exp2action -> Just act)

-- UnifyExp
pattern UnifyE1 :: [UnifyElem] -> Exp
pattern UnifyE1 xs = UnifyE (UnifyExpr1 xs)

-- Single element
pattern Single :: UnifyElem -> Exp
pattern Single item = UnifyE1 [item]

-- The Item's species 
pattern GenitiveExp :: UnifyElem -> UnifyElem -> Exp
pattern GenitiveExp item species = UnifyE1 [item, species]

uexp2kind :: Exp -> GKind
uexp2kind (Single item) = toKind $ oa2str $ ue2oa item
uexp2kind (GenitiveExp item species) = GComplKind (uexp2kind $ Single item) (uexp2term GTheSg $ Single species)
uexp2kind _ = GItem

uexp2term :: GDeterminer -> Exp -> GTerm
uexp2term det = GTDet det . uexp2kind

uexp2fun :: Exp -> (GParty -> GAction -> GAction)
uexp2fun (Single fun) = toAdv $ oa2str $ ue2oa fun
uexp2fun (GenitiveExp _ fun) = uexp2fun (Single fun)
uexp2fun _ = \_ a -> a

uexp2party :: Exp -> GParty
uexp2party (Single item) = toParty $ oa2str $ ue2oa item
uexp2party (UnifyE1 xs) = toParty $ unwords $ map (oa2str . ue2oa) xs
uexp2party x = error $ "uexp2party doesn't yet support " ++ show x

-- Arguments
pattern ArgEq :: Exp -> Exp -> ConstraintComma
pattern ArgEq e p = CComma (Op2E (BCmp_Eq1 e p))

pattern DefIs :: Exp -> Exp -> ConstraintComma
pattern DefIs e p = CComma (Op2E (BRel_Is e p))

pattern DefIsa :: Exp -> Exp -> ConstraintComma
pattern DefIsa e p = CComma (Op2E (BRel_Isa e p))

pattern Mul :: Exp -> Exp -> ConstraintComma
pattern Mul e p = CComma (Op2E (BArith_Mul e p))

-----------------------------------------------------------------------------
-- Match strings to GF functions.
-- In the future, we'll use WordNet directly, and support WN sense ids in L4.

toV2 :: String -> GAction_Dir
toV2 str = case map toLower str of
  "deliver" -> GDeliver
  "hypothecate" -> GHypothecate
  "issue" -> GIssue
  "offer" -> GOffer
  "pledge" -> GPledge
  "raise" -> GRaise
  "receive" -> GReceive
  "sell" -> GSell
  "send" -> GSend
  "return" -> GReturn
  "refund" -> GRefundV2
  "transfer" -> GTransfer
  _ -> GCook

toKind :: String -> GKind
toKind str = case map toLower str of
  "order" -> GOrder
  "delivery" -> GDelivery
  "payment" -> GPayment
  "bike" -> GBike
  "amount" -> GAmount
  "cabbage" -> GCabbage
  "potato" -> GPotato
  _ -> GItem

toParty :: String -> GParty
toParty str = case map toLower str of
  "buyer" -> GBuyer
  "seller" -> GSeller
  _ -> GStrParty (GString str)

toAdv :: String -> (GParty -> GAction -> GAction)
toAdv str = case map toLower str of
  "to" -> GTo
  "from" -> GFrom
  _ -> \_ a -> a
