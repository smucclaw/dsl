{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE PatternSynonyms #-}

module L4 where

import AbsL
import Data.Maybe (fromMaybe, maybeToList)
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Data.Char (isUpper, isLower)

-- for use by L4/Executable and by ToGF.hs
data GFlang   = GFeng  | GFmalay deriving (Show, Eq)

data BoolGroup a = AndGroup [a]
                 |  OrGroup [a]
                 |  IdGroup  a
                 deriving (Show, Eq, Foldable)

type MyRuleName = String

parseRuleGroup :: Goto -> BoolGroup MyRuleName
parseRuleGroup RBreach                   = IdGroup ("BREACH")
parseRuleGroup RFulfilled                = IdGroup ("FULFILLED")
parseRuleGroup (RGotoOne ruledef)          = IdGroup (showRuleDef ruledef)
parseRuleGroup (RGotoOne ruledef)          = IdGroup (showRuleDef ruledef)
parseRuleGroup (RGotoLst (ListComma xs))   = AndGroup (exp2rn <$> xs)
parseRuleGroup (RGotoLst (ListAnd   xs x)) = AndGroup (exp2rn <$> (xs <> [x]))
parseRuleGroup (RGotoLst (ListOr    xs x)) =  OrGroup (exp2rn <$> (xs <> [x]))
parseRuleGroup wut                         = error $ "unsupported syntax for rule group " ++ show wut


--------------------------------------------------------------------------------
type InterpErr a = Either String (Exit (BoolGroup a))
--------------------------------------------------------------------------------

data Exit r  = NoExit
             | Solo   r
             | Choice r r -- left=sinister, right=rite=dexter
             | Close  r
             deriving (Eq, Ord, Show, Read)

ruleExits :: Rule -> InterpErr MyRuleName
ruleExits r@(Rule rdef rname asof metalimb rulebody) =
  case rulebody of
    RBNoop                         -> Right $ NoExit
    RulePerform gu pl pw cs wl whw -> whwHenceLest Nothing whw
    RuleDeem    gu      dls    whw -> whwHenceLest Nothing whw
    RModal      gu ml          whw -> whwHenceLest (Just $ modalDeontic ml) whw
    RMatch mvs                     -> Right $ Solo $ AndGroup $ do
      (MatchRule innerRule) <- mvs
      return $ showRuleName innerRule

modalDeontic :: ModalLimb -> DeonticExpr
modalDeontic (MD1 pl (DeonticLimb1 deonticexpr ols al) dll) = deonticexpr

whwHenceLest :: Maybe DeonticExpr -> WhenHenceWhere -> InterpErr MyRuleName
whwHenceLest dexp (WHW whenl hencel wherel) = henceHL dexp hencel

-- we need some smarts here.
-- if the modal is MUST or SHANT, then an omitted "LEST" means breach.
henceHL :: Maybe DeonticExpr -> HenceLimb -> InterpErr MyRuleName
henceHL       Nothing  _                                  = Right $ NoExit
henceHL (Just DEMay  ) DNoHence                           = Right $ NoExit
henceHL (Just DEMust ) DNoHence                           = Right $ Choice (IdGroup "BREACH") (IdGroup "FULFILLED")
henceHL (Just DEShant) DNoHence                           = Left  $ "SHANT without HENCE or LEST"
henceHL (Just DEMay  ) (DHeLe  hgoto hos lgoto los) = Left  $ "MAY doesn't go with LEST"
henceHL (Just DEMust ) (DHeLe  hgoto hos lgoto los) = Right $ Choice (parseRuleGroup lgoto) (parseRuleGroup hgoto) -- TODO: allow multiple HENCE and LEST outputs in the DSL syntax
henceHL (Just DEShant) (DHeLe  hgoto hos lgoto los) = Right $ Choice (parseRuleGroup lgoto) (parseRuleGroup hgoto)
henceHL (Just DEMay  ) (DHence RFulfilled      _)       = Right $ Solo   (IdGroup "FULFILLED") -- collapsing both left and right; we may want to uncollapse in future.
henceHL (Just DEMust ) (DHence RFulfilled      _)       = Right $ Choice (IdGroup "BREACH") (IdGroup "FULFILLED")
henceHL (Just DEShant) (DHence RFulfilled      _)       = Right $ Choice (IdGroup "BREACH") (IdGroup "FULFILLED")
henceHL (Just DEMay  ) (DHence RBreach      _)       = Left  $   "MAY shouldn't cause BREACH; L4 probably should desugar"
henceHL (Just DEMust ) (DHence RBreach      _)       = Left  $  "MUST shouldn't cause BREACH; L4 probably should desugar"
henceHL (Just DEShant) (DHence RBreach      _)       = Left  $ "SHANT shouldn't cause BREACH; L4 probablyh should desugar"
henceHL (Just DEMay  ) (DHence hgoto _)                 = Right $ Choice (IdGroup "FULFILLED") (parseRuleGroup hgoto) -- unusually, if a MAY is not exercised, we go to FULFILLED
henceHL (Just DEMust ) (DHence hgoto _)                 = Right $ Choice (IdGroup "BREACH")    (parseRuleGroup hgoto)
henceHL (Just DEShant) (DHence hgoto _)                 = Right $ Choice (IdGroup "BREACH")    (parseRuleGroup hgoto)
henceHL (Just DEMay  ) (DLest  RBreach      _)       = Left  $ "LEST BREACH is assumed; but where's the HENCE? Or does some other state pick up from here?"
henceHL (Just DEMust ) (DLest  RBreach      _)       = Left  $ "LEST BREACH is assumed; but where's the HENCE? Or does some other state pick up from here?"
henceHL (Just DEShant) (DLest  RBreach      _)       = Left  $ "LEST BREACH is assumed; but where's the HENCE? Or does some other state pick up from here?"
henceHL (Just DEMay  ) (DLest  RFulfilled   _)       = Left  $ "LEST FULFILLED makes no sense; where's the HENCE?"
henceHL (Just DEMust ) (DLest  RFulfilled   _)       = Left  $ "LEST FULFILLED makes no sense; where's the HENCE?"
henceHL (Just DEShant) (DLest  RFulfilled   _)       = Left  $ "LEST FULFILLED makes no sense; where's the HENCE?"
henceHL (Just DEMay  ) (DLest  lgoto _)       = Left  $ "LEST without HENCE -- " ++ (show $ parseRuleGroup lgoto)
henceHL (Just DEMust ) (DLest  lgoto _)       = Left  $ "LEST without HENCE -- " ++ (show $ parseRuleGroup lgoto)
henceHL (Just DEShant) (DLest  lgoto _)       = Left  $ "LEST without HENCE -- " ++ (show $ parseRuleGroup lgoto)


--------------------------------------------------------------------------------
-- LOGICAL REWRITES
--------------------------------------------------------------------------------

rewrite :: Rule -> [Rule]
rewrite r0@(Rule rdef rname asof rmeta mL@( RBNoop                         )) = pure r0
rewrite r0@(Rule rdef rname asof rmeta mL@( RulePerform gu pl pw cs wl whw )) = pure r0
rewrite r0@(Rule rdef rname asof rmeta mL@( RuleDeem    gu      dls    whw )) = pure r0
rewrite r0@(Rule rdef rname asof rmeta mL@( RMatch mvs                     )) = pure r0
rewrite r0@(Rule rdef rname asof rmeta mL@( RModal      gu ml          whw )) =
  (\x -> case x of
      (rewritten,Just (remark,log)) -> Rule (rw rdef $ remark) rname asof rmeta rewritten
      (original, Nothing)           -> r0
  ) <$> rewriteModal gu ml whw
  where
    rw :: RuleDef -> String -> RuleDef
    rw (RID      oa) s = RID      $ appendOA oa s
    rw (RNumID i oa) s = RNumID i $ appendOA oa s
    rw other         s = other

appendOA :: ObjAttr -> String -> ObjAttr
appendOA (OA_dots oaes) str = OA_dots oaes <> str2dots str

instance Semigroup ObjAttr where
  (<>) (OA_dots oad1) (OA_dots oad2) = OA_dots (oad1 <> oad2)

str2dots :: String -> ObjAttr
str2dots str = mkDots str

mkDots s = OA_dots $ (mkoae <$> splitOn "." s)
  where
    mkoae (s:tring)
      | isUpper s = ObjAttrElemUIdent $ UIdent $ s:tring
      | otherwise = ObjAttrElemIdent  $  Ident $ s:tring


-- Helper patterns to make rewrite rules shorter
pattern MayUnless :: Exp -> Exp -> WhenLimb
pattern MayUnless expwhen expunless = WhenUnless expwhen  expunless

pattern MayWhenMatch :: Exp -> Exp -> WhenLimb
pattern MayWhenMatch expwhen expunless = WhenMatch ( (BBool_And expwhen AND1 expunless))

pattern ShantWhenNoMatch :: Exp -> Exp -> WhenLimb
pattern ShantWhenNoMatch expwhen expunless = WhenMatch ( (BBool_And expwhen AND2 (UBool_Not NOT1 expunless)))

-- where's DMNMD when we need it?
rewriteModal :: GivenUpon -> ModalLimb -> WhenHenceWhere -> [(RuleBody,Maybe(String,[String]))]
rewriteModal gu mL@(MD1 (PartyLimb (PSome  oa) pAS) (DeonticLimb1 DEMust  ols actL) dlL) whw = pure $ (RModal gu mL whw,Nothing)
rewriteModal gu mL@(MD1 (PartyLimb (PSome  oa) pAS) (DeonticLimb1 DEMay   ols actL) dlL) whw = pure $ (RModal gu mL whw,Nothing)
rewriteModal gu mL@(MD1 (PartyLimb (PSome  oa) pAS) (DeonticLimb1 DEShant ols actL) dlL) whw = pure $ (RModal gu mL whw,Nothing)
rewriteModal gu mL@(MD1 (PartyLimb (PEvery  _) pAS) (DeonticLimb1 DEMust  ols actL) dlL) whw = pure $ (RModal gu mL whw,Nothing)
rewriteModal gu mL@(MD1 (PartyLimb (PEvery  _) pAS) (DeonticLimb1 DEMay   ols actL) dlL) whw = pure $ (RModal gu mL whw,Nothing)
rewriteModal gu mL@(MD1 (PartyLimb (PEvery  _) pAS) (DeonticLimb1 DEShant ols actL) dlL) whw = pure $ (RModal gu mL whw,Nothing)

rewriteModal gu mL@(MD1 (PartyLimb (PNobody _)            pAS) (DeonticLimb1 DEMay ols actL) dlL) whw@(WHW (MayUnless expwhen expunless) hL whL) =
  pure ( RModal gu (MD1 (PartyLimb (PEvery PEvery_ANYONE) pAS) (DeonticLimb1 DEMay ols actL) dlL)     (WHW (MayWhenMatch expwhen expunless) hL whL), Just ("rewrite1a",[]) )
  <>
  pure ( RModal gu (MD1 (PartyLimb (PEvery PEvery_ANYONE) pAS) (DeonticLimb1 DEShant ols actL) dlL)   (WHW (ShantWhenNoMatch expwhen expunless) hL whL), Just ("rewrite1b",[]) )
-- >> PARTY NOBODY  MAY   act WHEN w1 UNLESS  u1 HENCE h1
-- -> PARTY ANYBODY MAY   act WHEN w1 AND     u1 HENCE h1
-- -> PARTY ANYBODY SHANT act WHEN w1 AND NOT u1 HENCE FULFILLED LEST BREACH

rewriteModal gu mL@(MD1 (PartyLimb (PNobody _)            pAS) (DeonticLimb1 DEMay   ols actL) dlL) whw@(WHW (WhenMatch wL) hL whL) =
  pure ( RModal gu (MD1 (PartyLimb (PEvery PEvery_ANYONE) pAS) (DeonticLimb1 DEShant ols actL) dlL) whw, Just ("rewrite2",[]) )
-- >> PARTY NOBODY  MAY   act WHEN w1 (no UNLESS) ...
-- -> PARTY ANYBODY SHANT act WHEN w1 (no UNLESS) ...

rewriteModal gu mL@(MD1 (PartyLimb (PNobody _) pAS) (DeonticLimb1 DEMust  ols actL) dlL) whw = pure (RModal gu mL whw, Nothing)
rewriteModal gu mL@(MD1 (PartyLimb (PNobody _) pAS) (DeonticLimb1 DEShant ols actL) dlL) whw = pure (RModal gu mL whw, Nothing)
rewriteModal gu mL whw = pure (RModal gu mL whw, Nothing)



--------------------------------------------------------------------------------
-- BASICS
--------------------------------------------------------------------------------

pattern MatchRule r = MatchVars1 r

-- retrieve all rules from a parsed module
getRules :: Tops -> [Rule]
getRules (Toplevel tops) = fakeRules ++ do
  (ToplevelsRule r@(Rule rdef rname asof metalimb rulebody)) <- tops
  case rulebody of
    RMatch mvs              -> do
      (MatchRule innerRule) <- mvs
      pure innerRule
    otherwise               -> pure r

fakeRules :: [Rule]
fakeRules = mkRule <$> ["FULFILLED", "BREACH"]

mkRule rulename = Rule (RID $ mkDots rulename) (RName OptLangStrings1) AsofNull Meta0 RBNoop

exp2rn :: Exp -> MyRuleName
exp2rn (ObjME (OMArgs uelist args ols)) = uelist2rn uelist
exp2rn (ObjME (OMNoAargs ueoaes ols)) = intercalate "." (showUEOAE <$> ueoaes)
exp2rn other = error ("don't know how to convert expression to rulename: " ++ show other)

uelist2rn :: [UnifyElem] -> MyRuleName
uelist2rn uel = filter (/= ' ') $ intercalate "." $ show <$> uel

showRuleName :: Rule -> MyRuleName
showRuleName (Rule rdef rname asof metalimb rulebody) = showRuleDef rdef

showRuleDef :: RuleDef -> MyRuleName
showRuleDef (RID      oa) = showOA oa
showRuleDef (RNumID i oa) = showOA oa
showRuleDef (RNum   i)    = "rule_" ++ show i

-- we might want to add a preprocessor step to the compiler toolchain to rewrite the OA_dots type to something more compact

showOA :: ObjAttr -> String
showOA (OA_dots oaes) = intercalate "." (showOAE <$> oaes)

showOAE :: ObjAttrElem -> String
showOAE (ObjAttrElemIdent  ( Ident x)) = x
showOAE (ObjAttrElemUIdent (UIdent x)) = x

showUEOAE :: UnifyElem -> String
showUEOAE (UnifyElemObjAttrElem oae) = showOAE oae
showUEOAE (UnifyElem1)               = "(null)"
showUEOAE (UnifyElemUnifyBracket (UnifyBracket1 ces)) = "[" ++ (intercalate "," (showCE <$> ces)) ++ "]"
showUEOAE (UnifyElemUnifyStar UnifyStar1)                = "*"

showCE :: CommaElem -> String
showCE (CommaElemObjAttr oa) = showOA oa

-- all rules naming another rule in HENCE, LEST, or GIVEN segments
-- allDeonticNodes = _

