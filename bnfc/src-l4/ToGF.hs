{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module ToGF
  ( bnfc2lang, bnfc2str
  )
where

import AbsL
import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import PGF (Expr, PGF, linearizeAll, showExpr)
import Top
import L4 (GFlang)

bnfc2lang :: GFlang -> PGF -> Tops -> String
bnfc2lang somelang = bnfc2str
-- TODO: be able to distinguish english output from malay etc.
-- GFlang is GFeng | GFmalay | ...
-- maybe we switch to just a String?

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
    --toTree (ToplevelsScenario _) = error "Scenarios not yet translated to GF"
    toTree _ = Nothing

rule2gf :: AbsL.Rule -> [Expr]
rule2gf (Rule def (RName name) asof meta rulebody) = map (gf . namefun) (body2gf rulebody)
  where
    namefun = case name of
      Description nm -> GRuleName (GString nm)
      _ -> id

body2gf :: RuleBody -> [GSentence]
body2gf (RModal given modal whw) = [whw2gf whw (modal2gf modal)]
body2gf (RuleDeem given [def] whw) = [whw2gf whw (def2gf def)]
body2gf (RuleDeem given defs whw) = [whw2gf whw def | def <- map def2gf defs]
body2gf _ = [GMAction GNobody (GMay GFailure)]
body2gf x = error $ "body2gf doesn't handle yet " ++ show x


whw2gf :: WhenHenceWhere -> (GSentence -> GSentence)
whw2gf (WHW NoWhen henc wher) = id
whw2gf (WHW (WhenMatch when) henc wher) = \sent -> GWhen sent (when2gf when)

when2gf :: Exp -> GSentence
when2gf (RelE e1 BRel_Is e2) = GMDefTermMatch (exp2term GTheSg e1) (exp2term GASg e2)
when2gf (RelE e1 BRel_Isa e2) = GMDefTermMatch (exp2term GTheSg e1) (exp2term GASg e2)
when2gf (CompE e1 BCmp_Match1 e2) = GMDefTermMatch (exp2term GTheSg e1) (exp2term GTheSg e2)
when2gf (RelE e1 BRel_Has e2) =
  let party = case e1 of
        UnifyNoArgs xs -> oa2party (OA_dots [oa | UnifyElemObjAttrElem oa <- xs])
        _ -> error $ "when2gf (RelE … BRel_Has …) doesn't support yet " ++ show e1
   in GMTermHas party (exp2term GTheSg e2)
when2gf (BBool_And e1 _ e2) = GConjSentence GAnd (GListSentence [when2gf e1, when2gf e2])
when2gf (BBool_Or e1 _ e2) = GConjSentence GOr (GListSentence [when2gf e1, when2gf e2])
when2gf (LikelyE UBool_Unlikely e) = GRuleName (GString "(unlikely)") $ when2gf e
when2gf _ = GMAction GNobody (GMay GFailure)
when2gf x = error $ "when2gf doesn't support yet " ++ show x

def2gf :: DefineLimb -> GSentence
def2gf (DefLimb _ [cc] with asof) = GMDefTermMatch kind term
  where
    -- TODO: support many defs, return [GSentence]
    (kind, term) = case cc of
      ArgEq e f -> (exp2term GASg e, exp2term GASg f)
      DefIs e f -> (exp2term GASg e, exp2term GASg f)
      DefIsa e f -> (exp2term GASg e, exp2term GASg f)
      _ -> error $ "def2gf doesn't support yet " ++ show cc
def2gf x = GMAction GNobody (GMay GFailure)
-- error $ "def2gf doesn't support yet " ++ show x

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
oa2party (OA_dots (OaStrs es)) = toParty es

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
action2gf (ActionSingle e blahs alias) =
  -- Object is in the BlahExp, transform into => structure
  action2gf (ActionSingle (RelE e BRel_Fat obj) blahs alias')
  where
    (obj, alias') = case blahs of
      BlahExp o : _ -> (o, alias)
      x -> error $ "action2gf doesn't handle yet BlahExp" ++ show x
action2gf _ = GFailure
action2gf x = error $ "action2gf doesn't handle yet " ++ show x

-- action2gf ActionSingle {} = GFailure
-- action2gf ActionMulti {} = GFailure

exp2action :: Exp -> Maybe GAction
exp2action exp = case exp of
  -- Op1E unaOp exp ->
  binExp -> binexp2action binExp
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

binexp2action :: Exp -> Maybe GAction
binexp2action (RelE e1 BRel_Fat e2) = vp
  where
    verbs = exp2verb e1
    (obj, f) = exp2obj e2
    vp = case (verbs, obj) of
      (v : _, o) -> Just $ f $ GAComplDir v o
      _ -> Nothing
binexp2action _ = Nothing

exp2obj :: Exp -> (GTerm, GAction -> GAction)
exp2obj (ObjME (OMArgs es args langstrs)) = (uexp2term GTheSg (UnifyNoArgs es), f)
  where
    f = case args of
      --      Args1 -> id
      Args1 ccs -> head $ map ccs2fun ccs
    ccs2fun (ArgEq e p) =
      let f = uexp2fun e
          x = uexp2party p
       in f x
exp2obj e@(UnifyNoArgs _) = (uexp2term GTheSg e, id)
exp2obj x = error $ "exp2obj can't handle: " ++ show x

exp2verb :: Exp -> [GAction_Dir]
exp2verb (UnifyNoArgs es) = verbs
  where
    verbs = map unifyelem2verb es
    unifyelem2verb e = case e of
      UnifyElemObjAttrElem (OaStr oa) -> toV2 oa
      x -> error $ "exp2verb doesn't yet support " ++ show x
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

-- ObjAttr
oa2str :: ObjAttrElem -> String
oa2str e = case e of
  ObjAttrElemIdent (Ident str) -> str
  ObjAttrElemUIdent (UIdent str) -> str

pattern OaStr :: String -> ObjAttrElem
pattern OaStr str <- (oa2str -> str)

-- "Foo.Bar.Baz" becomes "foo bar baz"
pattern OaStrs :: String -> [ObjAttrElem]
pattern OaStrs str <- (unwords . map oa2str -> str)

-- UnifyElem
ue2oa :: UnifyElem -> ObjAttrElem
ue2oa (UnifyElemObjAttrElem oa) = oa
ue2oa UnifyElem1 = ObjAttrElemIdent (Ident "it") -- TODO: anaphora in the grammar?
ue2oa x = error $ "ue2oa: expected ObjAttr, got " ++ show x

pattern UE :: String -> UnifyElem
pattern UE str <- (oa2str . ue2oa -> str)

pattern UEs :: String -> [UnifyElem]
pattern UEs str <- (unwords . map (oa2str . ue2oa) -> str)

-- Action
pattern ExpAction :: GAction -> Exp
pattern ExpAction act <- (exp2action -> Just act)

-- UnifyExp
pattern UnifyNoArgs :: [UnifyElem] -> Exp
pattern UnifyNoArgs xs = ObjME (OMNoAargs xs OptLangStrings1)

pattern ObjMethodArgs :: [UnifyElem] -> [ConstraintComma] -> Exp
pattern ObjMethodArgs xs args = ObjME (OMArgs xs (Args1 args) OptLangStrings1)

-- ConstExp
pattern ConstExp :: String -> Exp
pattern ConstExp str <- (const2term -> Just str)

const2term :: Exp -> Maybe String
const2term (ConstE cv) = Just $ case cv of
  BoolV_T _ -> "True"
  BoolV_F _ -> "False"
  BoolV_N _ -> "Nothing"
  IntV i -> show i
  FloatV d -> show d
  StringV str -> str
  FloatPercent d -> show d ++ "%"
  IntPercent i -> show i ++ "%"
const2term _ = Nothing

-- Single element
pattern Single :: UnifyElem -> Exp
pattern Single item = (UnifyNoArgs [item])

-- The Item's species
pattern GenitiveExp :: UnifyElem -> UnifyElem -> Exp
pattern GenitiveExp item species = UnifyNoArgs [item, species]

uexp2kind :: Exp -> Maybe GKind
uexp2kind (Single (UE item)) = toKind item
uexp2kind (GenitiveExp item species) = do
  spec <- uexp2kind $ Single species -- TODO: make string literals available at any stage?
  return $ GComplKind spec (exp2term GTheSg $ Single item)
uexp2kind _ = Nothing
uexp2kind x = error $ "uexp2kind: Not yet supported: " ++ show x

uexp2term :: GDeterminer -> Exp -> GTerm
uexp2term det exp@(UnifyNoArgs xs) =
  case uexp2kind exp of
    Just kind -> GTDet det kind
    Nothing -> case xs of
      [UE x] -> strTerm x
      UEs x -> strTerm x
      _ -> error $ "uexp2term doesn't support yet " ++ show exp
uexp2term _ exp = error $ "uexp2term doesn't support yet " ++ show exp

uexp2fun :: Exp -> (GParty -> GAction -> GAction)
uexp2fun (Single (UE fun)) = toAdv fun
uexp2fun (GenitiveExp _ fun) = uexp2fun (Single fun)
uexp2fun _ = \_ a -> a

uexp2party :: Exp -> GParty
uexp2party (Single (UE item)) = toParty item
uexp2party (UnifyNoArgs (UEs xs)) = toParty xs
uexp2party x = GStrParty (GString $ "uexp2party doesn't support " ++ show x)
--uexp2party x = error $ "uexp2party doesn't yet support " ++ show x

-- Arguments
pattern ArgEq :: Exp -> Exp -> ConstraintComma
pattern ArgEq e p = CComma (CompE e BCmp_Eq1 p)

pattern DefIs :: Exp -> Exp -> ConstraintComma
pattern DefIs e p = CComma (RelE e BRel_Is p)

pattern DefIsa :: Exp -> Exp -> ConstraintComma
pattern DefIsa e p = CComma (RelE e BRel_Isa p)

-- Arithmetic expressions
pattern Mul :: Exp -> Exp -> Exp
pattern Mul e p = MulE e BArith_Mul p

pattern Add :: Exp -> Exp -> Exp
pattern Add e p = AddE e BArith_Plus p

-- Lists
pattern OrList1 :: Exp -> Exp
pattern OrList1 e = ListE (ListOr [] e)

pattern OrList2 :: Exp -> Exp -> Exp
pattern OrList2 e1 e2 = ListE (ListOr [e1] e2)

-- Relations

pattern PredAnd :: Exp -> Exp -> Exp
pattern PredAnd e1 e2 = BBool_And e1 AND_AND e2

exp2term :: GDeterminer -> Exp -> GTerm
exp2term _ (ConstExp s) = GStrTerm (GString s)
exp2term det e@(UnifyNoArgs _) = uexp2term det e
exp2term det (ObjMethodArgs xs args) = uexp2term det (UnifyNoArgs xs) -- TODO: incorporate args into VP as Adv
exp2term det (OrList2 e1 e2) = GConjTerm GOr (GListTerm [exp2term det e1, exp2term det e2])
exp2term det (PredAnd e1 e2) = GConjTerm GAnd (GListTerm [exp2term det e1, exp2term det e2])
exp2term _ _ = GStrTerm (GString "<not supported yet>")
exp2term _ x = error $ "exp2term doesn't yet support " ++ show x

-- Match
pattern QualifiedExp1 :: Exp -> Exp
pattern QualifiedExp1 e = QualExp MQuantNull e OptAsAlias0 MQualNull []

pattern QuantifiedExp1 :: Exp -> Exp
pattern QuantifiedExp1 e = QualifiedExp1 e

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

strTerm :: String -> GTerm
strTerm s = GStrTerm (GString s)

toKind :: String -> Maybe GKind
toKind str = case map toLower str of
  "order" -> Just GOrder
  "delivery" -> Just GDelivery
  "payment" -> Just GPayment
  "bike" -> Just GBike
  "amount" -> Just GAmount
  "cabbage" -> Just GCabbage
  "potato" -> Just GPotato
  "item" -> Just GItem
  "species" -> Just GSpecies
  _ -> Nothing -- Not found in lexicon, create a string literal

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
