
module ToGraphViz where

import Control.Monad      ( when, guard )
import Data.List (intercalate)
import Data.Char (isUpper, isLower)
import Data.Tree
import Data.Maybe
import Data.Either
import Control.Applicative
import qualified Data.Map as Map
import           Data.Map ((!))
import Debug.Trace

import Text.Pretty.Simple
import qualified Data.Text.Lazy as T
import qualified SkelL
import PrintL  ( Print, printTree )
import AbsL

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Data.Graph.Inductive.Monad
import Data.Graph.Inductive.Monad.IOArray

--------------------------------------------------------------------------------
type RuleMap = Map.Map MyRuleName Rule
--------------------------------------------------------------------------------

asMap :: [Rule] -> [(MyRuleName, Rule)]
asMap rs = do
  r <- rs
  let rname = showRuleName r
  guard (isJust rname)
  return (fromJust rname, r)


--------------------------------------------------------------------------------
type RuleGraph = Gr MyRuleName EdgeLabel
--------------------------------------------------------------------------------

data EdgeLabel = Next  -- solo
               | Sin   -- choice left
               | Dex   -- choice right
               | Hup   -- close ... "hangup"
            deriving (Eq, Ord, Show, Read)

-- children
children :: Rule -> [MyRuleName]
children r = case ruleExits r of
               Left  _      -> mempty
               Right NoExit -> mempty
               Right (Solo  rules)    -> rules
               Right (Close rules)    -> rules
               Right (Choice sin dex) -> sin ++ dex


-- transitive closure -- note this assumes no loops in the input. if the program fails to halt we need to introduce an accumulator of seen nodes
connectedTo :: (Map.Map MyRuleName Rule) -> [Rule] -> [Rule]
connectedTo rnr rs = do
  r <- rs
  let cs = (rnr !) <$> children r
  cs ++ connectedTo rnr cs

haveExits = filter (\r -> case ruleExits r of
                             Left  _      -> False
                             Right NoExit -> False
                             otherwise    -> True
                         )

asGraph :: [Rule] -> RuleGraph
asGraph rs =
  let ruleName2Rule  = Map.fromList $ (\r -> (fromJust . showRuleName $ r, r)) <$> rs
      rulesWithExits = haveExits rs
      ofInterest     = rulesWithExits ++ connectedTo ruleName2Rule rulesWithExits
      ruleName2Num   = Map.fromList $ zip (fromJust . showRuleName <$> ofInterest) [1..]
  in
    buildGr $
    (\(ruleName,n) ->
        let rule     = ruleName2Rule ! ruleName
            exitto   = fromRight NoExit $ ruleExits rule
        in ([], n, ruleName,
             (case exitto of
                 Solo ens       ->   (\exitnode -> (Next, ruleName2Num ! exitnode)) <$> ens
                 Choice sin dex ->  ((\exitnode -> (Sin,  ruleName2Num ! exitnode)) <$> sin)
                                     <>
                                    ((\exitnode -> (Dex,  ruleName2Num ! exitnode)) <$> dex)
                 Close ens      ->   (\exitnode -> (Hup,  ruleName2Num ! exitnode)) <$> ens
                 NoExit         -> []
             ))
    ) <$> Map.toList ruleName2Num

    
printGraph :: [Rule] -> IO ()
printGraph = prettyPrint . asGraph

--------------------------------------------------------------------------------
type InterpErr a = Either String (Exit [a])
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
    RMatch mvs                     -> Right $ Solo $ do
      (MatchVars22 innerRule) <- mvs
      maybeToList $ showRuleName innerRule

modalDeontic :: ModalLimb -> DeonticExpr
modalDeontic (MD1 pl (DeonticLimb1 deonticexpr ols al) dll) = deonticexpr

whwHenceLest :: Maybe DeonticExpr -> WhenHenceWhere -> InterpErr MyRuleName
whwHenceLest dexp (WHW whenl hencel wherel) = henceHL dexp hencel

-- we need some smarts here.
-- if the modal is MUST or SHANT, then an omitted "LEST" means breach.
henceHL :: Maybe DeonticExpr -> HenceLimb -> Either String (Exit [MyRuleName])
henceHL       Nothing  DNoHence                           = Right $ NoExit
henceHL (Just DEMay  ) DNoHence                           = Right $ NoExit
henceHL (Just DEMust ) DNoHence                           = Right $ Choice ["BREACH"] ["FULFILLED"]
henceHL (Just DEShant) DNoHence                           = Left  $ "SHANT without HENCE or LEST"
henceHL (Just DEMay  ) (DHeLe  hgoto ha hos lgoto la los) = Left  $ "MAY doesn't go with LEST"
henceHL (Just DEMust ) (DHeLe  hgoto ha hos lgoto la los) = Right $ Choice [showPart lgoto] [showPart hgoto] -- TODO: allow multiple HENCE and LEST outputs in the DSL syntax
henceHL (Just DEShant) (DHeLe  hgoto ha hos lgoto la los) = Right $ Choice [showPart lgoto] [showPart hgoto]
henceHL (Just DEMay  ) (DHence (RGoto ruledef) _ _)       = Right $ Choice ["FULFILLED"] [showRuleDef ruledef] -- unusually, if a MAY is not exercised, we go to FULFILLED
henceHL (Just DEMust ) (DHence (RGoto ruledef) _ _)       = Right $ Choice ["BREACH"] [showRuleDef ruledef]
henceHL (Just DEShant) (DHence (RGoto ruledef) _ _)       = Right $ Choice ["BREACH"] [showRuleDef ruledef]
henceHL (Just DEMay  ) (DHence (RFulfilled)    _ _)       = Right $ Solo ["FULFILLED"] -- collapsing both left and right; we may want to uncollapse in future.
henceHL (Just DEMust ) (DHence (RFulfilled)    _ _)       = Right $ Choice ["BREACH"] ["FULFILLED"]
henceHL (Just DEShant) (DHence (RFulfilled)    _ _)       = Right $ Choice ["BREACH"] ["FULFILLED"]
henceHL (Just DEMay  ) (DHence (RBreach)       _ _)       = Left  $   "MAY shouldn't cause BREACH; L4 probably should desugar"
henceHL (Just DEMust ) (DHence (RBreach)       _ _)       = Left  $  "MUST shouldn't cause BREACH; L4 probably should desugar"
henceHL (Just DEShant) (DHence (RBreach)       _ _)       = Left  $ "SHANT shouldn't cause BREACH; L4 probablyh should desugar"
henceHL (Just DEMay  ) (DLest  (RBreach)       _ _)       = Left  $ "LEST BREACH is assumed; but where's the HENCE? Or does some other state pick up from here?"
henceHL (Just DEMust ) (DLest  (RBreach)       _ _)       = Left  $ "LEST BREACH is assumed; but where's the HENCE? Or does some other state pick up from here?"
henceHL (Just DEShant) (DLest  (RBreach)       _ _)       = Left  $ "LEST BREACH is assumed; but where's the HENCE? Or does some other state pick up from here?"
henceHL (Just DEMay  ) (DLest  (RFulfilled)    _ _)       = Left  $ "LEST FULFILLED makes no sense; where's the HENCE?"
henceHL (Just DEMust ) (DLest  (RFulfilled)    _ _)       = Left  $ "LEST FULFILLED makes no sense; where's the HENCE?"
henceHL (Just DEShant) (DLest  (RFulfilled)    _ _)       = Left  $ "LEST FULFILLED makes no sense; where's the HENCE?"
henceHL (Just DEMay  ) (DLest  (RGoto ruledef) _ _)       = Left  $ "LEST without HENCE -- " ++ showRuleDef ruledef
henceHL (Just DEMust ) (DLest  (RGoto ruledef) _ _)       = Left  $ "LEST without HENCE -- " ++ showRuleDef ruledef
henceHL (Just DEShant) (DLest  (RGoto ruledef) _ _)       = Left  $ "LEST without HENCE -- " ++ showRuleDef ruledef

showPart (RGoto ruledef)  = showRuleDef ruledef
showPart (RFulfilled)     = "FULFILLED"
showPart (RBreach)        = "BREACH"

--------------------------------------------------------------------------------
-- LOGICAL REWRITES
--------------------------------------------------------------------------------

rewrite :: Rule -> [Rule]
-- >> PARTY NOBODY  MAY   act WHEN w1 UNLESS  u1 HENCE h1
-- -> PARTY ANYBODY MAY   act WHEN w1 AND     u1 HENCE h1
-- -> PARTY ANYBODY SHANT act WHEN w1 AND NOT u1 HENCE FULFILLED LEST BREACH
rewrite r0@(Rule rdef rname asof rmeta ( RBNoop                         )) = pure r0
rewrite r0@(Rule rdef rname asof rmeta ( RulePerform gu pl pw cs wl whw )) = pure r0
rewrite r0@(Rule rdef rname asof rmeta ( RuleDeem    gu      dls    whw )) = pure r0
rewrite r0@(Rule rdef rname asof rmeta ( RMatch mvs                     )) = pure r0
rewrite r0@(Rule rdef rname asof rmeta ( RModal      gu ml          whw )) = Rule rdef rname asof rmeta <$> rewriteModal gu ml whw

rewriteModal :: GivenUpon -> ModalLimb -> WhenHenceWhere -> [RuleBody]
rewriteModal givenupon modallimb whenhencewhere =
  pure $ RModal givenupon modallimb whenhencewhere

--------------------------------------------------------------------------------
-- BASICS
--------------------------------------------------------------------------------

-- retrieve all rules from a parsed module
getRules :: Tops -> [Rule]
getRules (Toplevel tops) = concatMap rewrite $ fakeRules ++ do
  (ToplevelsRule r@(Rule rdef rname asof metalimb rulebody)) <- tops
  case rulebody of
    RMatch mvs              -> do
      (MatchVars22 innerRule) <- mvs
      pure innerRule
    otherwise               -> pure r

fakeRules :: [Rule]
fakeRules = mkRule <$> ["FULFILLED", "BREACH"]

mkRule rulename = Rule (mkRID rulename) (RName OptLangStrings1) AsofNull Meta0 RBNoop

mkRID [] = error "blank argument to mkRID (mkRuleID)"
mkRID (s:tring)
  | isUpper s = RID $ OA_dots [ ObjAttrElemUIdent $ UIdent $ s:tring ]
  | otherwise = RID $ OA_dots [ ObjAttrElemIdent  $  Ident $ s:tring ]

type MyRuleName = String

showRuleName :: Rule -> Maybe MyRuleName
showRuleName (Rule rdef rname asof metalimb rulebody) = pure $ showRuleDef rdef

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

-- all rules naming another rule in HENCE, LEST, or GIVEN segments
-- allDeonticNodes = _

