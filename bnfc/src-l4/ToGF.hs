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
toGF (Toplevel tops) = map toTree tops
  where
    toTree (ToplevelsRule rule) = rule2gf rule

pattern Description :: String -> OptLangStrings
pattern Description str <- (describeLang "en" -> str : _)

pattern LangId :: String -> LangID
pattern LangId lang = LangID1 [LangLabelIdent (Ident lang)]

type Lang = String

describeLang :: Lang -> OptLangStrings -> [String]
describeLang lang (OptLangStringsLangStrings (ELangStrings es)) =
  [str | (ELangString (LangId l) str) <- es, l == lang]
describeLang _ _ = []

rule2gf :: AbsL.Rule -> Expr
rule2gf (Rule def (RName name) asof meta rulebody) = gf $ GRuleName descr (body2gf rulebody)
  where
    descr = case name of
      Description nm -> GString nm
      _ -> GString []

body2gf :: RuleBody -> GSentence
body2gf (RModal given modal whenhencewhere) = modal2gf modal
body2gf _ = GMAction GNobody (GMay GFailure)

modal2gf :: ModalLimb -> GSentence
modal2gf (MD1 party deonticl deadline) = GMAction (party2gf party) (deontic2gf deonticl)

party2gf :: PartyLimb -> GParty
party2gf (PartyLimb partydef _) = partydef2gf partydef

partydef2gf :: PartyDef -> GParty
partydef2gf (PEvery _) = GEverybody
partydef2gf (PNobody _) = GNobody
partydef2gf (PSome oa) = oa2party oa

oa2party :: ObjAttr -> GParty
oa2party (OA_dots es) = toParty $ unwords $ map oa2str es -- "Foo.Bar.Baz" becomes "foo bar baz"

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
action2gf (ActionSingle (ExpAction a) blahs alias) = GAAlias a -- TODO: use actual alias
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
    vp = case (verbs, objs) of
      (v : _, o : _) -> Just $ f $ GAComplDir v o
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
        UnifyElemObjAttrElem (OaStr oa) -> toKind oa
        _ -> GItem -- default option, call it "the item"
    ccs2fun (ArgEq es ps) =
      let f : _ = map unifyelem2fun es
          x : _ = map unifyelem2party ps
       in f x
    unifyelem2fun = toAdv . oa2str . ue2oa
    unifyelem2party = toParty . oa2str . ue2oa
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

------------------------------------------------------------
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
  "transfer" -> GTransfer
  _ -> GCook

toKind :: String -> GKind
toKind str = case map toLower str of
  "order" -> GOrder
  "delivery" -> GDelivery
  "payment" -> GPayment
  "bike" -> GBike
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
