
module ToGraphViz where

import Control.Monad      ( when, guard )
import Data.List (intercalate)
import Data.Tree
import Data.Maybe
import Control.Applicative
import qualified Data.Map as Map

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
               | Multi -- launch
               | Kill  -- close
            deriving (Eq, Ord, Show, Read)

asGraph :: [Rule] -> RuleGraph
asGraph rs =
  let haveExits = filter (\r -> case ruleExits r of
                             Left  _      -> False
                             Right NoExit -> False
                             otherwise    -> True
                         ) rs
  in
    mkGraph (zip [1..length haveExits] (fromJust . showRuleName <$> haveExits)) []

printGraph :: [Rule] -> IO ()
printGraph = prettyPrint . asGraph

--------------------------------------------------------------------------------
type InterpErr a = Either String (Exit a)
--------------------------------------------------------------------------------

data Exit a = NoExit
            | Solo   a
            | Choice a a -- left=sinister, right=rite=dexter
            | Close  a
            deriving (Eq, Ord, Show, Read)
            
--------------------------------------------------------------------------------
type RuleGraph a = Map.Map a (Exit [a])
-- a MAY   rule has a soloExit
-- a MUST  rule has a Choice exit
-- a SHANT rule has a Choice exit
-- each exit can be to multiple nodes, so we basically map from a to [a]
--------------------------------------------------------------------------------

type InterpErr a = Either String (Exit [a])

ruleExits :: Rule -> InterpErr MyRuleName
ruleExits r@(Rule rdef rname asof metalimb rulebody) =
  case rulebody of
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
henceHL (Just DEMay  ) (DHence (RGoto ruledef) _ _)       = Right $ Solo   [showRuleDef ruledef]
henceHL (Just DEMust ) (DHence (RGoto ruledef) _ _)       = Right $ Choice ["BREACH"] [showRuleDef ruledef]
henceHL (Just DEShant) (DHence (RGoto ruledef) _ _)       = Right $ Choice ["BREACH"] [showRuleDef ruledef]
henceHL (Just DEMay  ) (DHence (RFulfilled)    _ _)       = Right $ Solo ["FULFILLED"]
henceHL (Just DEMust ) (DHence (RFulfilled)    _ _)       = Right $ Solo ["FULFILLED"]
henceHL (Just DEShant) (DHence (RFulfilled)    _ _)       = Right $ Solo ["FULFILLED"]
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
-- BASICS
--------------------------------------------------------------------------------

-- retrieve all rules from a parsed module
getRules :: Tops -> [Rule]
getRules (Toplevel tops) = do
  (ToplevelsRule r@(Rule rdef rname asof metalimb rulebody)) <- tops
  case rulebody of
    RBNoop                  -> mempty 
    RulePerform _ _ _ _ _ _ -> mempty
    RuleDeem    _ _ _       -> pure r
    RModal      _ _ _       -> pure r
    RMatch mvs              -> do
      (MatchVars22 innerRule) <- mvs
      pure innerRule


type MyRuleName = String

showRuleName :: Rule -> Maybe MyRuleName
showRuleName (Rule rdef rname asof metalimb RBNoop)   = mempty
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

