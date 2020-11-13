{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module ToGF (bnfc2str) where

import Data.Char (toLower)
import AbsL
import PGF
import Top

-- BNFC to GF tree

bnfc2str :: PGF -> Tops -> String
bnfc2str gr tops = unlines [
  unlines [
      unlines $ linearizeAll gr tree
    -- , "Abstract syntax tree in GF:"
    -- , showExpr [] tree
    ]
  | tree <- toGF tops
  ]

toGF :: Tops -> [Expr]
toGF (Toplevel tops) = map toTree tops
  where
    toTree (ToplevelsRule rule) = rule2gf rule

rule2gf :: AbsL.Rule -> Expr
rule2gf (Rule _ _ _ _ rulebody) = gf $ body2gf rulebody

body2gf :: RuleBody -> GSentence
body2gf (RModal given modal whenhencewhere) = modal2gf modal
body2gf _ = GMAction GNobody (GMay GFailure)

modal2gf :: ModalLimb -> GSentence
modal2gf (MD1 party deonticl _) = GMAction (party2gf party) (deontic2gf deonticl)

party2gf :: PartyLimb -> GParty
party2gf (PartyLimb partydef _) = partydef2gf partydef

partydef2gf :: PartyDef -> GParty
partydef2gf (PEvery _) = GEverybody
partydef2gf (PNobody _) = GNobody
partydef2gf (PSome oa) = oa2party oa

oa2party :: ObjAttr -> GParty
oa2party (OA_dots (e:_)) = str2party $ oa2str e -- TODO: this only takes the first element
oa2party _ = error "oa2party: empty ObjAttr"

str2party :: String -> GParty
str2party s = case map toLower s of
  "buyer" -> GBuyer
  "seller" -> GSeller
  x -> GStrParty (GString x)

oa2str :: ObjAttrElem -> String
oa2str e = case e of
  ObjAttrElemIdent (Ident str) -> str
  ObjAttrElemUIdent (UIdent str) -> str

pattern OaStr :: String -> ObjAttrElem
pattern OaStr str <- (oa2str -> str)

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
action2gf (ActionSingle (ExpAction a) blahs alias) = GAAliasNamed GCompany a -- TODO: use actual alias
action2gf ActionSingle {} = GFailure
action2gf ActionMulti {} = GFailure

pattern ExpAction :: GAction -> Exp
pattern ExpAction act <- (exp2action -> Just act)

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
    (objs, f) = exp2obj e2
    vp = case (verbs,objs) of
      (v:_, o:_) -> Just $ f $ GAComplDir v o
      _ -> Nothing
binexp2action _ = Nothing

pattern UnifyE1 :: [UnifyElem] -> Exp
pattern UnifyE1 xs = UnifyE (UnifyExpr1 xs)

pattern ArgEq :: [UnifyElem] -> [UnifyElem] -> ConstraintComma
pattern ArgEq es ps = CComma (Op2E (BCmp_Eq1 (UnifyE1 es) (UnifyE1 ps)))

exp2obj :: Exp -> ([GTerm], GAction -> GAction)
exp2obj (ObjME (ObjMethod1 es args langstrs)) = (terms, f)
  where
    f = case args of
          Args1 -> id
          Args2 ccs -> head $ map ccs2fun ccs
    terms = map unifyelem2term es

    unifyelem2term e = GTDet GTheSg $
      case e of
        UnifyElemObjAttrElem oa -> oa2kind oa
        _ -> GItem -- default option, call it "the item"
    oa2kind (OaStr oa) = case map toLower oa of
      "order" -> GOrder
      "delivery" -> GDelivery
      "payment" -> GPayment
      "bike" -> GBike
      _ -> GItem

    ccs2fun (ArgEq es ps) =
        let f:_ = map unifyelem2fun es
            x:_ = map unifyelem2party ps
         in f x

    unifyelem2fun e = case oa2str $ ue2oa e of
      "to" -> GTo
      "from" -> GFrom
      _ -> \_ a -> a

    unifyelem2party = str2party . oa2str . ue2oa
exp2obj x =  error $ "exp2obj can't handle: " ++ show x

exp2verb :: Exp -> [GAction_Dir]
exp2verb (UnifyE1 es) = verbs
  where
    verbs = map unifyelem2verb es
    unifyelem2verb e = case e of
      UnifyElemObjAttrElem oa -> oa2verb oa
      _ -> GCook -- default option, silly on purpose
    oa2verb oa = case map toLower $ oa2str oa of
      "deliver" -> GDeliver
      "hypothecate" -> GHypothecate
      "issue" -> GIssue
      "offer" -> GOffer
      "pledge" -> GPledge
      "raise" -> GRaise
      "receive" -> GReceive
      "sell" -> GSell
      "send" -> GSend
      "transfer" -> GTransfer
      _ -> GCook
exp2verb (ObjME objMethod) = []
exp2verb _ = []
