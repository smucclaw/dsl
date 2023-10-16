{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}


{-|

The Interpreter runs after the Parser. It prepares for transpilation by organizing the ruleset and providing helper functions used by multiple XPile backends.

In future, we may be so ambitious as to attempt some type checking and type inference, and so on, here, though that may be better left to corel4 or other backends.

See also documentation at https://github.com/smucclaw/dsl/tree/tab-mustsing#interpretation-requirements

Typical usage: an XPile module is handed the output of `l4interpret`, and makes use of the values returned in the `l4i` object.

Sometimes it will access the original rules directly. It is preferable, however, that downstream transpilers should access via the Interpreter's API.

The Interpreter can thereby handle expansion and other transformations in ways that are consistent across multiple transpilers.

To view the various endpoints exposed by the Interpreter, see @org/LATEST.org@.

-}

module LS.Interpreter where

import AnyAll qualified as AA
import Control.Applicative ((<|>))
import Control.Monad (guard, join, forM)
import Data.Bifunctor (first)
import Data.Either (partitionEithers, fromRight)
import Data.Graph.Inductive
import Data.HashMap.Strict ((!))
import Data.HashMap.Strict qualified as Map
import Data.List (find, (\\))
import Data.List qualified as DL
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Traversable (for)
import Data.Tree
import Data.Tuple (swap)
import Debug.Trace
import LS.XPile.Logging (mutter, mutters, mutterd, mutterdhsf
                        , XPileLogE, XPileLog
                        , pShowNoColorS, xpReturn, xpError, xpLog)
import LS.PrettyPrinter
import LS.RelationalPredicates
import LS.Rule
import LS.Types
import Prettyprinter
import Text.Pretty.Simple (pShowNoColor)

-- | interpret the parsed rules based on some configuration options.
-- This is a canonical intermediate representation used by downstream
-- functions. It is typically run once, in the high-level caller, and
-- handed to each transpiler for use, as an `l4i` argument.
--

l4interpret :: InterpreterOptions -> [Rule] -> Interpreted
l4interpret iopts rs =
  let ct = classHierarchy rs
      st = symbolTable    iopts rs
      (vp, vpErr) = xpLog $ attrsAsMethods rs
      (rDGout, rDGerr) = xpLog $ ruleDecisionGraph rs
  in
    L4I { classtable = ct
        , scopetable = st
        , origrules  = rs
        , valuePreds = fromRight [] vp
        , ruleGraph  = rDGout
        , ruleGraphErr = rDGerr
        }

-- | Provide the fully expanded, exposed, decision roots of all rules in the ruleset,
--   grouped ("nubbed") into rule groups (since multiple rules may have the same decision body).
--
--   This is used for:
--
--   * user-facing Q&A (see XPile/Purescript)
--
--   * visualization of the decision logic
--

qaHornsT :: Interpreted -> [([RuleName], BoolStructT)]
qaHornsT l4i = (fmap . fmap) rp2text <$> qaHornsR l4i

-- | where `qaHornsT` returns a `BoolStructT`, `qaHornsR` returns a `BoolStructR`.
--
-- The `T` version is used for applications that lie closer to the end-user's eyeballs.
--
-- The `R` version is used when the internal structure of the RelationalPredicates is still needed.

qaHornsR :: Interpreted -> [([RuleName], BoolStructR)]
qaHornsR l4i =
     [ ( ruleLabelName <$> uniqrs
       , expanded)
     | (grpval, uniqrs) <- groupedByAOTree l4i $ -- NUBBED
                           let (eRout, eRerr) = xpLog $ exposedRoots l4i      -- EXPOSED
                           in eRout
     , not $ null grpval
     , expanded <- expandBSR l4i 1 <$> maybeToList (getBSR (DL.head uniqrs))
     ]

-- | interpret the parsed rules and construct the symbol tables. The
-- output of this function is exposed in the `scopetable` attribute of
-- the `l4i` record. We use the name "Scope" here because in the
-- future we will organize the symbol tables according to section
-- scope.

symbolTable :: InterpreterOptions -> [Rule] -> ScopeTabs
symbolTable _iopts rs =
  Map.fromListWith (<>) (fromGivens <> fromDefines <> fromDecides)
  -- [BUG] this marshalling produces duplicate entries, one from where a thing is DEFINEd and one where its attributes are DECIDEd.
  -- <> trace ("all rules = " ++ TL.unpack (pShow rs)) []
  where
    fromGivens :: [(RuleName, SymTab)]
    fromGivens = -- trace "fromGivens:" $ traceShowId $
                 [ (rname, symtable)
                 | r   <- rs
                 , hasGiven r
                 , gPT <- maybeToList (given r)
                 , let rname = ruleLabelName r
                       symtable = Map.fromList ( pt2inferrable <$> toList gPT )
                 ]
    pt2inferrable :: TypedMulti -> (MultiTerm, (Inferrable TypeSig, [HornClause2]))
    pt2inferrable (mt, maybeTS) = (toList mt, ((maybeTS , []) -- [TODO] if we ever introduce default values, this might be the place to implement that
                                              ,                     []) -- we don't expect to see value definitions in givens, just type assignments
                                  )

    fromDefines :: [(RuleName, SymTab)]
    fromDefines = -- trace "fromDefines:" $ traceShowId $
                  [ (scopename, symtable)
                  | r@Hornlike{}   <- rs
                  , keyword r `elem` [Define, Means]
                  , let scopename = -- trace ("fromDefines: working rule " ++ show (ruleLabelName r)) $
                                    ruleLabelName r
                        symtable = Map.fromList [(name r, ((super r,[]), clauses r))]
                  ]

    fromDecides :: [(RuleName, SymTab)]
    fromDecides = -- trace "fromDecides:" $ traceShowId $
                  [ (scopename, symtable)
                  | r@Hornlike{}   <- rs
                  , keyword r `elem` [Decide, Is]
                  , let scopename = -- trace ("fromDecides: working rule " ++ show (ruleLabelName r)) $
                                    ruleLabelName r
                        symtable = Map.fromList [(name r, ((super r,[]), clauses r))]
                  ]

-- | A map of all the classes we know about.
--
-- Currently this function returns both IS-A and HAS-A relationships
-- for a given class, in a tuple of superclass (IS-A) and attributes
-- (HAS-A).
--
-- Classes can contain other classes. Here the hierarchy represents the "has-a" relationship, conspicuous when a DECLARE HAS HAS HAS.
--
-- Most of the time, though, classes just contain attributes.
--
-- The output of this function is exposed in the `classtable` attribute of the `l4i` record.
classHierarchy :: [Rule] -> ClsTab
classHierarchy rs =
  -- multiple DECLARE of the same class are allowed, so we have to merge.
  -- we do some violence to the inferred types here.
  CT $ Map.fromListWith (\((ts1inf,ts1s),CT clstab1)
                          ((ts2inf,ts2s),CT clstab2) ->
                            ( (listToMaybe (maybeToList ts1inf <> maybeToList ts2inf)
                              ,ts1s <> ts2s)
                            , CT $ clstab1 <> clstab2))
  [ (thisclass, (superclass, attributes))
  | r@TypeDecl{} <- rs
  , let thisclass = mt2text (name r)
        superclass = (Just $ defaultToSuperType $ super r, [])
        attributes = classHierarchy (has r)
  ]

-- | A graph of all the classes we know about.
--
-- redraw the class hierarchy as a rooted graph, where the fst in the
-- pair contains all the breadcrumbs to the current node. root to the
-- right. I think this is overproducing a bit, because it's
-- considering the attributes.

classGraph :: ClsTab -> [EntityType] -> [([EntityType], TypedClass)]
classGraph (CT ch) ancestors = concat
  [ pure (nodePath, (_itypesig, childct))
  | (childname, (_itypesig, childct)) <- Map.toList ch
  , let nodePath = childname : ancestors
  ]

-- | classes at the root of the container hierarchies.
-- Basically, these classes have other classes, but no other classes have them.
classRoots :: ClsTab -> [(EntityType, TypedClass)]
classRoots ct@(CT ch) =
  let cg = classGraphFGL ct
  in [ (className, typedClass)
     | (n, className) <- labNodes cg
     , indeg cg n == 0
     , (Just typedClass) <- [Map.lookup className ch]
     ]

-- | deprecated, use classGraph instead.
allCTkeys :: ClsTab -> [EntityType]
allCTkeys o@(CT ct) = getCTkeys o ++ [ T.replace " " "_" (childname <> "." <> gcname)
                                     | (childname, (_ts, childct)) <- Map.toList ct
                                     , gcname <- allCTkeys childct
                                     ]

-- | attributes of a given class. Enums are dealt with separately.
getCTkeys :: ClsTab -> [EntityType]
getCTkeys (CT ct) = Map.keys ct

-- | passthrough type, present in case we decide to represent the names of classes differently.
type MyClassName = EntityType

-- | class names, topologically sorted by inheritance to eliminate forward references in "extends" relationships
-- note: this code will probably fail silently on any input that isn't as expected
-- [TODO] we should align this with classGraph, which isn't actually an fgl inductive graph yet.
topsortedClasses :: ClsTab -> [MyClassName]
topsortedClasses ct =
  let cGraph = classGraphFGL ct
  in [ cn
     | n <- topsort cGraph
     , (Just cn) <- [lab cGraph n]
     ]

classGraphFGL :: ClsTab -> Gr MyClassName ()
classGraphFGL ct =
  -- there are a couple different kinds of dependencies...
  let child2parent = getInheritances ct       -- child depends on parent
      class2attrtypes = [ (cn, ut)            -- class depends on attribute types
                        | cn <- getCTkeys ct
                        , ts <- getAttrTypesIn ct cn
                        , Right ut <- [getUnderlyingType ts]
                        ]
  in mkGraph (Map.toList idToType) (myEdges (child2parent ++ class2attrtypes))
  where
    myEdges :: [(MyClassName, MyClassName)] -> [(Int, Int, ())]
    myEdges abab = [ (aid, bid, ())
                   | (a,b) <- abab
                   , (Just aid) <- [Map.lookup a typeToID]
                   , (Just bid) <- [Map.lookup b typeToID]
                   ]
    allTypes :: [MyClassName]
    allTypes = allClasses ct -- ++ allSymTypes stabs [TODO] if it turns out there are hidden classnames lurking in the symbol table
    allClasses :: ClsTab -> [MyClassName]
    allClasses = getCTkeys
    -- first let's assign integer identifiers to each type found in the class hierarchy
    typeToID = Map.fromList (Prelude.zip allTypes [1..])
    idToType = Map.fromList $ swap <$> Map.toList typeToID

-- | Extract all Enum declarations recursing through class declarations, so we can hoist them to top-level for use by, say, the Typescript transpiler.
--
-- This covers the following situations:
-- - DECLARE toplevelEnum                  IS ONEOF enum1 enum2
--
-- - DECLARE class1 HAS attr1              IS ONEOF enum3 enum4
--
-- - DECLARE class2 HAS attr2 HAS attr3    IS ONEOF enum5 enum6
--
-- - GIVEN x                               IS ONEOF x1 x2 x3      DECIDE ...
--
-- We return a list of rules rewritten into a standardized toplevel format, preserving the srcref information
--
-- There are probably a handful of other places inside a `Rule` where
-- a `TypeSig` could appear, and we need to exhaustively traverse all
-- of those places. Please add code as you find such places. [TODO]
extractEnums :: Interpreted -> [Rule]
extractEnums l4i =
  let rs = origrules l4i
  in concatMap go rs
  where
    go :: Rule -> [Rule]
    go r@TypeDecl{super = Just (InlineEnum enumtype enumtext)} =
      [r]
    go TypeDecl{has = has} = concatMap go has
    go Hornlike{given = Just givens, srcref=srcref} =
      concatMap go [ defaultTypeDecl { name = nameEnum
                                     , super = gEnum
                                     , srcref = srcref}
                   | (gName, gEnum@(Just (InlineEnum _ _))) <- NE.toList givens
                   -- consider using getSymType in case the type is inferred, not explicit
                   , let nameEnum = (\case
                                        (MTT mtt) -> MTT $ mtt <> "Enum"
                                        x         -> x) <$> NE.toList gName
                   ]
    go _ = []

-- | Group decision boolstructs into AndOrTrees that comprise multiple original rules.
--
-- Sometimes multiple rules will have the same decision content: X depends on Z; Y also depends on Z.
-- For the sake of the UI, we group such rules together and return basically a Map, of AndOrTree (Z) to one or more rules (X and Y).
groupedByAOTree :: Interpreted -> [Rule] -> [(Maybe BoolStructT, [Rule])]
groupedByAOTree l4i rs =
  Map.toList $ Map.fromListWith (++) $
  (\r -> (getAndOrTree l4i 1 r, [r])) <$> rs


-- | The top-level decision roots which we expose to the web UI, and also visualize with SVGLadder.
--
-- We exclude rules which are the target of a GOTO RuleAlias, because those are just infrastructure.
-- The SVG outputter likes to exclude things that have only a single element and are therefore visually uninteresting.
-- We want the SVG Xpiler to reuse this code as authoritative.

exposedRoots :: Interpreted -> XPileLog [Rule]
exposedRoots l4i = do
  let decisionGraph = ruleGraph l4i
  decisionroots <- decisionRoots decisionGraph
  return [ r | r <- decisionroots, not $ isRuleAlias l4i (ruleLabelName r) ]

-- | the (inner) type of a particular class's attribute
attrType :: ClsTab -> EntityType -> Maybe TypeSig
attrType (CT clstab) attrName = do
  (t, CT _ct) <- Map.lookup attrName clstab
  getSymType t


-- | extract all inheritance relationships.
--
getInheritances :: ClsTab -> [(EntityType, EntityType)]
getInheritances ct =
  [ (child, parent)
  | child <- getCTkeys ct
  , let parent = defaultToSuperClass $ clsParent ct child
  ]

-- | If a class was declared with no extension ("IS A") we assign it to DefaultSuperClass.
defaultToSuperClass :: Maybe EntityType -> EntityType
defaultToSuperClass = fromMaybe "DefaultSuperClass"

-- | same thing but for typesigs
defaultToSuperType :: Maybe TypeSig -> TypeSig
defaultToSuperType = fromMaybe (SimpleType TOne (defaultToSuperClass Nothing))

-- | recursively return all attribute types found under a given class, i think?
getAttrTypesIn :: ClsTab -> EntityType -> [TypeSig]
getAttrTypesIn ct classname =
  case thisAttributes ct classname of
    Nothing         -> []
    (Just (CT ct')) -> concat [ ts : concatMap (getAttrTypesIn ct'') (getCTkeys ct'')
                              | (_attrname, (its, ct'')) <- Map.toList ct' -- EntityType (Inferrable TypeSig, ClsTab)
                              , Just ts <- [getSymType its]
                              ]


-- | used by `ruleDecisionGraph`; a map from a rule to a unique integer identifier for that rule, used in the `RuleGraph`
type RuleIDMap = Map.HashMap Rule Int

-- | which decision rules depend on which other decision rules?
-- And which ground terms don't expand any further?
--
-- We answer these questions in two passes.
--
-- First, we construct a rulegraph of only rules and their relations. In a conventional programming language we might call this the interprocedural graph.
-- In the first version of this codebase we stopped here and just returned the graph of rules, which is why the type is called `RuleGraph`.
--
-- But that's not enough! We want to know about the leaf nodes, the ground terms, as well.
-- So, in a second pass, we traverse the rulegraph from the first pass, and return all the leaf nodes found in the RelationalPredicates;
-- then we eliminate all the tokens which appear in the graph from the first pass. That should leave us with only leaf nodes.
--
-- With that clarity, we elevate the leaf nodes into stub rules and return a rule graph of the combined rules + ground terms.
--
-- If, downstream, you want to distinguish between rule and ground term, just look for those nodes in the graph which are leaves!
--
ruleDecisionGraph :: RuleSet -> XPileLog RuleGraph
ruleDecisionGraph rs = do

  "(1.1) for first pass, we begin with decisionrules" ***-> decisionRules

  let ruleOnlyMap = Map.fromList (Prelude.zip decisionRules [1..])
  "(1.2) ruleOnlyMap" ***-> ruleOnlyMap

  mutterd 3 "ruleDecisionGraph: (1.3) ruleOnlyGraph construction log using relPredRefsAll"
  
  ruleOnlyGraph :: RuleGraph <- mkGraph
                                (swap <$> Map.toList ruleOnlyMap) -- the nodes
                                <$> relPredRefsAll rs ruleOnlyMap -- The <$> lifts into the XPileLog monad

  "(1.4) ruleOnlyGraph result" ***-> ruleOnlyGraph

  mutterd 3 "as a flex, just to show what's going on, we extract all the leaf terms, if we can, by starting with all the terms entirely. Well, MultiTerms."

  let allTerms = DL.nub $ concat (concatMap rp2bodytexts . concatMap AA.extractLeaves . getBSR <$> rs)
  "(2.1) allTerms" ***-> allTerms

  mutterd 3 "(2.2) we filter for the leaf terms by excluding all the ruleNames that we know from the original ruleset. This may not be a perfect match with the MultiTerms used in the rule graph. [TODO]"

  let (ruleNames, ruleLabelNames) = (ruleName <$> rs, ruleLabelName <$> rs)
  "(2.3) ruleNames to omit" ***-> ruleNames
  "(2.3 alt) what if we used ~ruleLabelName~ instead of ~ruleName~?)" ***-> ruleLabelNames

  let difference = (allTerms \\ ruleNames) \\ [[ MTT "OTHERWISE" ]] --  special case: Otherwise drops out
  "(2.4) that leaves" ***-> difference

  mutterd 3 "(2.5) let's elevate all the leaf terms to stubby little rules in their own right"
  let stubRules = [ defaultHorn { name = rulename, keyword = Define, srcref = Nothing
                                , clauses = stubClause rulename }
                  | rulename <- difference ]

  mutterd 3 "(2.6) then we rebuild the graph with those rules included"
  let expandedRuleMap   = Map.fromList (Prelude.zip (decisionRules ++ stubRules) [1..])
  expandedRuleGraph :: RuleGraph <- mkGraph
                                    (swap <$> Map.toList expandedRuleMap)
                                    <$> relPredRefsAll (rs ++ stubRules) expandedRuleMap

  "(2.7) expandedRuleGraph" ***-> expandedRuleGraph

  mutterd 3 "(3.1) finally we strip the reflexive BSR from the stub rules while leaving the nodes themselves in place."
  
  let prunedRuleGraph = dereflexed $ nmap (\r -> if hasClauses r && clauses r == stubClause (name r) then r { clauses = [] } else r ) expandedRuleGraph
  "(3.2.7) prunedRuleGraph" ***-> prunedRuleGraph

  return prunedRuleGraph

  where
     -- [NOTE] for the purposes of generating the graph in the 2nd pass,
    -- leaf nodes are reflexive. Let's just say we meant for it to be that way, cuz they "bottom out", lol.
    -- This gets removed in the third pass.
    stubClause rulename =
      [ HC { hHead = RPMT rulename
           , hBody = Just $ AA.mkLeaf (RPMT rulename) }
      ]

    -- filter for just those rules which involve decisions
    decisionRules = [ r | r <- rs, not . null . getBSR $ r ]

  -- we want to represent the leaf nodes in the rule decision graph, so we elevate those to the status of rules by including them in the map
    groundTerms :: Map.HashMap Rule Int -> [RuleName]
    groundTerms knownRules = []
      -- find all the body elements which 

    (***->) str hs = mutterdhsf 3 ("ruleDecisionGraph: " <> str) pShowNoColorS hs
     
-- | walk all relationalpredicates in a set of rules, and return the list of edges showing how one rule relies on another.
relPredRefsAll :: RuleSet -> RuleIDMap -> XPileLog [LEdge RuleGraphEdgeLabel]
relPredRefsAll rs ridmap = do
  let headElements :: Map.HashMap MultiTerm Rule -- does this get recomputed each time or cached?
      -- given a term, see which rule defines it
      headElements = Map.fromList $
                     [ (headName,r')
                     | r' <- rs
                     , headName <- getDecisionHeads r' -- [TODO] this is quadratic
                     ]
  mutterdhsf 5 "relPredRefs: headElements"  pShowNoColorS headElements

  concat <$> mapM (relPredRefs rs ridmap headElements) rs

-- | in a particular rule, walk all the relational predicates available, and show outdegree links
-- that correspond to known rule heads from the entire ruleset.
--
-- in other words, if a rule R1 says something like (a WHEN b OR c), it defines a, and relies on b and c;
-- if we find a rule R2 which defines (c MEANS c1 AND c2), then it defines c, and relies on c1 and c2.
-- so we show that rule R1 relies on, or refers to, rule R2: R1 -> R2.
-- there is some overlap here with the idea of scopetabs in the symbol table, but let's just do it
-- the brute way first and then refactor later once we have a better idea if this approach even works.
relPredRefs :: RuleSet -> RuleIDMap -> Map.HashMap MultiTerm Rule
            -> Rule
            -> XPileLog [LEdge RuleGraphEdgeLabel]
relPredRefs rs ridmap headElements r = do
      -- given a rule, see which terms it relies on
  let myGetBSR = getBSR r
      myLeaves = concatMap AA.extractLeaves myGetBSR
      bodyElements = concatMap rp2bodytexts myLeaves

  mutterd 4 (T.unpack $ mt2text $ ruleLabelName r)

  mutterdhsf 5 "relPredRefs: original rule" pShowNoColorS r
  mutterdhsf 5 "relPredRefs: getBSR"        pShowNoColorS myGetBSR
  mutterdhsf 5 "relPredRefs: extractLeaves" pShowNoColorS myLeaves
  mutterdhsf 5 "relPredRefs: bodyElements"  pShowNoColorS bodyElements

  -- [BUG] at some point we lose the moon
  mutterd 5 "relPredReffs: will exclude various things not found in headElements"
  -- given a rule R, for each term relied on by rule R, identify all the subsidiary rules which define those terms.
  toreturn <- sequence
    [ (rid, targetRuleId', ()) <$ mutterd 6 ("relPredRefs list comp: returning " <> show rid <> ", " <> show targetRuleId')
    | bElem <- bodyElements
     , let targetRule = Map.lookup bElem headElements
     , isJust targetRule
     , let targetRule' = fromJust targetRule -- safe due to above isJust test
     , let targetRuleId = Map.lookup targetRule' ridmap
     , isJust targetRuleId
     , let targetRuleId' = fromJust targetRuleId -- safe due to above isJust test
           rid = ridmap ! r
     ]

  mutterdhsf 5 "relPredRefs: returning" pShowNoColorS toreturn
  return toreturn

-- | Which rules are "top-level", "entry-point" rules?
--
-- Those are the rules we should put in front of the user.
--
-- They subsume (via the @expand*@ family of functions) other sub-rules.
--
-- Examine the rulegraph for rules which have no indegrees, as far as decisioning goes.

decisionRoots :: RuleGraph -> XPileLog [Rule]
decisionRoots rg = do
  let rg' = dereflexed rg
  return $
    catMaybes [ lab rg' r
              | r <- nodes rg'
              ,  indeg rg' r == 0
              -- , outdeg rg' r  > 0
              ]

-- remove reflexive edges that go from node n to node n
dereflexed :: Gr a b -> Gr a b
dereflexed gr =
  foldr (\n g -> delEdge (n,n) g) gr (nodes gr)


-- | extract a data flow graph
-- suitable for drawing as SVG
-- from the rulegraph.
--
-- we're interested in a data flow graph whose leaves are the leaf elements in the rulegraph;
-- the intermediate nodes can be the rules; and
-- the graph roots out at the decisionRoots.


-- | return the internal conditions of the rule, if any, as an and-or tree.
--
-- a Regulative rule exposes its `who` and `cond` attributes, rewritten so the subject of the rule is prefixed to the WHO.
--
-- a Constitutive rule exposes the body of its `clauses`.
--
-- [TODO] multiple rules with the same head should get &&'ed together and jammed into a single big rule
-- Or perhaps this depends on the mode in which the interpreter is running.
-- It's one thing to merge class declarations for ontology purposes.
-- It's another thing to merge decision rules.
-- Perhaps we can have an interpreter pragma decide whether to return an error, or just give a warning that the merge is happening.

getAndOrTree :: Interpreted -> Int -> Rule -> Maybe BoolStructT -- Vue wants AA.Item T.Text
getAndOrTree _l4i _depth r@Regulative{who=whoMBSR, cond=condMBSR} =
  (fmap (((bsp2text (subj r) <> " ") <>) . rp2text) <$> whoMBSR)  <> -- WHO is relative to the subject
  (fmap                                    rp2text  <$> condMBSR)    -- the condition is absolute

getAndOrTree  l4i depth r@Hornlike{} = expandTrace "getAndOrTree" depth "fmap extractRPMT2Text ..." $
                                       fmap extractRPMT2Text <$>
                                       (expandTrace "getAndOrTree" depth "mconcat bsmtOfClauses..." $
                                        mconcat (-- traceShowId $
                                           bsmtOfClauses l4i (depth+1) $
                                           r { clauses = expandClauses l4i (depth+1) (clauses r) } ))

getAndOrTree l4i depth _r@(RuleAlias rn) = do
  r' <- getRuleByName (origrules l4i) rn
  getAndOrTree l4i (depth+1) r'

getAndOrTree _l4i _depth _r = -- trace ("ERROR: getAndOrTree called invalidly against rule " <> show r) $
  Nothing

-- convert clauses to a boolStruct MT
bsmtOfClauses :: Interpreted -> Int -> Rule -> [Maybe BoolStructR]
bsmtOfClauses l4i depth r
  | hasClauses r =
      let toreturn =
            [ listToMaybe $ maybeToList $ mbody <|> mhead
            | c <- expandClauses l4i 2 (clauses r)
            , (hhead, hbody)  <- [(hHead c, hBody c)]
            , let (_bodyEx, bodyNonEx) = partitionExistentials c
            , let mhead, mbody :: Maybe BoolStructR
                  mhead = case hhead of
                            RPBoolStructR _mt1 _rprel1 bsr1 -> expandTrace "bsmtOfClauses" depth "returning bsr part of head's RPBoolStructRJust" $
                                                               Just (bsr2bsmt bsr1)
                            _                               -> expandTrace "bsmtOfClauses" depth ("returning nothing for " <> show hhead) $
                                                               Nothing
                  mbody = case hbody of
                            Nothing -> Nothing
                            _       ->
                              let output = bsr2bsmt bodyNonEx in
                                expandTrace "bsmtOfClauses" depth ("got output " <> show output) $
                                Just output
            ]
      in expandTrace "bsmtOfClauses" depth ("either mbody or mhead") toreturn
  | otherwise = []

-- * Expansion of decision rules: we insert sub-rules into parent rules.

-- | What does clause expansion mean?
-- We walk through the RelationalPredicates found in the head and the body of HornClause.
-- If we encounter a term that is itself the head of a different rule, we substitute it with the body of that rule.
-- That's the general idea. As always, the devil is in the details, complicated by the fact that we're dealing with predicates, not propositions.

expandClauses, expandClauses' :: Interpreted -> Int -> [HornClause2] -> [HornClause2]
expandClauses l4i depth hcs = (expandTrace "expandClauses" depth $ "running on " ++ show (Prelude.length hcs) ++ " hornclauses") $ expandClauses' l4i (depth+1) hcs
expandClauses' l4i depth hcs =
  let toreturn = [ newhc
                 | oldhc <- hcs
                 , let newhead = (expandTrace "expandClauses" depth $ "expanding the head") $                expandRP l4i (depth+1)   $  hHead oldhc
                       newbody = (expandTrace "expandClauses" depth $ "expanding the body") $ unleaf . fmap (expandRP l4i (depth+1)) <$> hBody oldhc
                       newhc = case oldhc of
                                 HC _oldh Nothing -> HC newhead Nothing
                                 HC  oldh _       -> HC oldh    newbody
                 ]
  in expandTrace "expandClauses" depth ("returning " ++ show toreturn) $
     toreturn

-- | Simple transformation to remove the "lhs IS" part of a BolStructR, leaving on the "rhs".
unleaf :: BoolStructR -> BoolStructR
unleaf (AA.Leaf (RPBoolStructR _b RPis bsr)) = unleaf bsr
unleaf (AA.All  lbl xs) = AA.mkAll lbl (unleaf <$> xs)
unleaf (AA.Any  lbl xs) = AA.mkAny lbl (unleaf <$> xs)
unleaf (AA.Not      x ) = AA.mkNot     (unleaf     x )
unleaf (AA.Leaf x     ) = AA.mkLeaf    x

-- take out the Leaf ( RPBoolStructR [ "b" ] RPis
-- from the below:
--        [ HC
--            { hHead = RPMT [ "c" ]
--            , hBody = Just
--                ( Any Nothing
--                    [ Leaf
--                        ( RPMT [ "a" ] )
--                    , Leaf
--                        ( RPBoolStructR [ "b" ] RPis
--                            ( All Nothing
--                                [ Leaf
--                                    ( RPMT [ "b1" ] )
--                                , Leaf
--                                    ( RPMT [ "b2" ] )
--                                ]
--                            )
--                        )
--                    ]
--                )
--            }
--        ]
--

-- | Set true for debugging, False for prod
expandTraceDebugging :: Bool
expandTraceDebugging = False

-- | a little helper function to do trace debugging of the expansion process
expandTrace :: (Show a) => String -> Int -> String -> a -> a
expandTrace fname dpth toSay toShow =
  if expandTraceDebugging
  then trace (replicate dpth '*' ++ " " ++ fname ++ ": " {- ++ replicate dpth '|' ++ " " -} ++ toSay ++ "\n" ++
               "#+BEGIN_SRC haskell\n" ++ (TL.unpack (pShowNoColor toShow)) ++ "\n#+END_SRC") $
       toShow
  else toShow

-- | is a given multiterm defined as a head somewhere in the ruleset?
-- later, we shall have to limit the scope of such a definition based on UPON \/ WHEN \/ GIVEN preconditions.
-- for now we just scan across the entire ruleset to see if it matches.
expandRP :: Interpreted -> Int -> RelationalPredicate -> RelationalPredicate
expandRP l4i depth og@(RPMT                   mt2)   = expandMT  l4i (depth + 1) og mt2
expandRP l4i depth og@(RPConstraint  mt1 RPis mt2)   = expandMT  l4i (depth + 1) og (mt1 ++ MTT (rel2txt RPis) : mt2)
expandRP l4i depth (RPBoolStructR mt1 RPis bsr)   = RPBoolStructR mt1 RPis (expandBSR' l4i (depth + 1) bsr)
expandRP _l4i _depth x                            = x

-- | Search the scopetable's symbol tables for a given multiterm. Expand its clauses, and return the expanded.
expandMT :: Interpreted -> Int -> RelationalPredicate -> MultiTerm -> RelationalPredicate
expandMT l4i depth ogRP mt0 =
  let expanded = listToMaybe
                 [ outrp
                 | (_scopename, symtab) <- Map.toList (scopetable l4i)
                 , (_mytype, cs) <- maybeToList $ Map.lookup mt0 symtab
                 , c <- cs
                 , let outs = expandClause l4i depth c
                 , outrp <- outs
                 ]
  in fromMaybe ogRP expanded

-- | Expand a horn clause that may have both head and body containing stuff we want to fill.
-- Despite the name, this is not directly related to expandClauses.
-- It happens deeper in, under `expandMT`.
expandClause :: Interpreted -> Int -> HornClause2 -> [RelationalPredicate]
expandClause _l4i _depth (HC   (RPMT          _mt            ) (Nothing) ) = [          ] -- no change
expandClause _l4i _depth (HC   (RPParamText   _pt            ) (Nothing) ) = [          ] -- no change
expandClause _l4i _depth (HC   (RPConstraint   mt  RPis   rhs) (Nothing) ) = [ RPMT (mt ++ MTT "IS" : rhs) ] -- substitute with rhs -- [TODO] weird, fix.
expandClause _l4i _depth (HC o@(RPConstraint  _mt _rprel _rhs) (Nothing) ) = [     o    ] -- maintain inequality
expandClause  l4i  depth (HC   (RPBoolStructR  mt  RPis   bsr) (Nothing) ) = [ RPBoolStructR mt RPis (expandBSR' l4i (depth + 1) bsr) ]
expandClause  l4i  depth (HC   (RPMT          mt          )    (Just bodybsr) ) = [ RPBoolStructR mt RPis (expandBSR' l4i (depth + 1) bodybsr) ]
expandClause _l4i _depth (HC   (RPParamText   _pt           )  (Just _bodybsr) ) = [          ] -- no change
expandClause _l4i _depth (HC   (RPConstraint  _mt RPis   _rhs) (Just _bodybsr) ) = [          ] -- x is y when z ... let's do a noop for now, and think through the semantics later.
expandClause _l4i _depth (HC o@(RPConstraint  _mt _rprel _rhs) (Just _bodybsr) ) = [    o     ] -- maintain inequality
expandClause _l4i _depth (HC   (RPBoolStructR _mt  RPis  _bsr) (Just _bodybsr) ) = [          ] -- x is y when z ... let's do a noop for now, and think through the semantics later.
expandClause _l4i _depth _                                                        = [          ] -- [TODO] need to add support for RPnary


-- | expand a BoolStructR. If any terms in a BoolStructR are names of other rules, insert the content of those other rules intelligently.
expandBSR :: Interpreted -> Int -> BoolStructR -> BoolStructR
expandBSR  l4i depth x = expandTrace "expandBSR" depth (show x) $ AA.nnf $ expandBSR' l4i depth x

-- | monadic version with logging turned on
expandBSRM :: Interpreted -> Int -> BoolStructR -> XPileLog BoolStructR
expandBSRM l4i depth x = do
  mutterdhsf depth "expandBSR() called with" pShowNoColorS x
  let toreturn = expandBSR l4i depth x
  mutterdhsf depth "expandBSR() returning" pShowNoColorS toreturn
  return toreturn

-- | Do expansion, throwing away the LHS IS part of any `RPBoolStructR` elements we encounter.
expandBSR' :: Interpreted -> Int -> BoolStructR -> BoolStructR
expandBSR' l4i depth (AA.Leaf rp)  =
  case expandRP l4i (depth + 1) rp of
    RPBoolStructR _mt1 RPis bsr -> bsr
    o                           -> AA.mkLeaf o
expandBSR' l4i depth (AA.Not item)   = {- AA.nnf $ -} AA.mkNot     (expandBSR' l4i (depth + 1) item)
expandBSR' l4i depth (AA.All lbl xs) = AA.mkAll lbl (expandBSR' l4i (depth + 1) <$> xs)
expandBSR' l4i depth (AA.Any lbl xs) = AA.mkAny lbl (expandBSR' l4i (depth + 1) <$> xs)

-- | unimplemented
expandBody :: Interpreted -> Maybe BoolStructR -> Maybe BoolStructR
expandBody _l4i = id


-- | used by the Petri xpiler.
expandRulesByLabel :: [Rule] -> T.Text -> [Rule]
expandRulesByLabel rules txt =
  let toreturn =
        [ q
        | r <- rules
        , let mt = rl2text <$> getRlabel r
        , Just txt == mt
        , let qs = expandRule rules r
        , q <- qs
        ]
  in -- trace ("expandRulesByLabel(" ++ show txt ++ ") about to return " ++ show (rlabel <$> toreturn))
     toreturn

-- | Perform expansion of a single rule, in the context of a larger ruleset; return multiple expanded rules.
--
-- This supports the functionality of a rule being defined in terms of other rules, a la R1 MEANS R2 AND R3
expandRule :: [Rule] -> Rule -> [Rule]
expandRule _rules r@Regulative{} = [r]
expandRule rules r@Hornlike{..} =
  let toreturn =
        -- we support hornlike expressions of the form x is y and z; we return y and z
        [ q
        | clause <- clauses
        , let rlbl' = rl2text <$> getRlabel r
              bsr = -- trace ("expandRule: got head " ++ show (hHead clause))
                    hHead clause
        , isJust rlbl'
        , mt <- -- trace ("aaLeaves returned " ++ show (aaLeaves (AA.Leaf bsr)))
                aaLeaves (AA.mkLeaf bsr)
        -- map each multiterm to a rulelabel's rl2text, and if it's found, return the rule
        , q <- expandRulesByLabel rules (mt2text mt)
        ]
  in -- trace ("expandRule: called with input " ++ show rlabel)
     -- trace ("expandRule: about to return " ++ show (ruleName <$> toreturn))
     toreturn
expandRule _ _ = []


-- | What are the leaf nodes -- "items" in AnyAll parlance -- that
-- form the basis for the input widgets, or questions, in the end-user
-- interview interface?
--
-- used for purescript output -- this is the toplevel function called by Main
onlyTheItems :: Interpreted -> BoolStructT
onlyTheItems l4i =
  let myitem = AA.mkAll Nothing (catMaybes $ getAndOrTree l4i 1 <$> origrules l4i)
      simplified = AA.simplifyBoolStruct myitem
  in simplified

-- | subsidiary to the above function, look for only one item by name.
onlyItemNamed :: Interpreted -> [Rule] -> [RuleName] -> BoolStructT
onlyItemNamed l4i rs wanteds =
  let ibr = itemsByRule l4i rs
      found = DL.filter (\(rn, _simp) -> rn `elem` wanteds) ibr
  in
    if null found
    then AA.mkLeaf $ T.pack ("L4 Interpreter: unable to isolate rule named " ++ show wanteds)
    else snd $ DL.head found

-- | return those Q&A leaf items arranged by the rule to which they contribute.
itemsByRule :: Interpreted -> [Rule] -> [(RuleName, BoolStructT)]
itemsByRule l4i rs =
  [ (ruleLabelName r, simplified)
  | r <- rs
  , let aot = getAndOrTree l4i 1 r
        simplified = fromJust aot
  , isJust aot
  ]

-- | we must be certain it's always going to be an RPMT
-- we extract so that it's easier to convert to JSON or to purescript Item Text
-- [TODO] would it make sense for this to simply become extractRPMT2MT?
-- We probably should extend this to other RelationalPredicate constructors like RPnary.
-- Should we relocate this to the RelationalPredicate module?
extractRPMT2Text :: RelationalPredicate -> T.Text
extractRPMT2Text (RPMT ts) = mt2text ts
extractRPMT2Text _         = error "extractRPMT2Text: expecting RPMT only, other constructors not supported."

ruleNameStr :: Rule -> String
ruleNameStr r = T.unpack (mt2text (ruleLabelName r))

-- | A RuleSet is a list of rules. We occasionally see this alias used here and there across the codebase.
type RuleSet = [Rule]

-- * the getRuleBy* family
-- 
-- | Retrieve a rule by name, using `ruleName`
getRuleByName :: RuleSet -> RuleName -> Maybe Rule
getRuleByName rs rn = find (\r -> ruleName r == rn) rs

-- | Retrieve a rule by label, using `getRlabel`
--
-- note that this matches by `getRlabel` not by `ruleLabelName`
getRuleByLabel :: RuleSet -> T.Text -> Maybe Rule
getRuleByLabel rs t = find (\r -> (rl2text <$> getRlabel r) == Just t) rs

-- | Retrieve a rule by either `getRlabel` or `ruleName`
--
-- Note that this is not exactly the same thing as `ruleLabelName`.
getRuleByLabelName :: RuleSet -> T.Text -> Maybe Rule
getRuleByLabelName rs t = find (\r -> (rl2text <$> getRlabel r) == Just t
                                      ||
                                      ruleName r == [MTT t]
                               ) rs

-- | Transform every RelationalPredicate in a `BoolStructR` to use only the data constructor `RPMT`.
bsr2bsmt :: BoolStructR -> BoolStructR
bsr2bsmt (AA.Leaf (RPMT mt)                      ) = AA.mkLeaf (RPMT mt)
bsr2bsmt (AA.Leaf (RPParamText pt)               ) = AA.mkLeaf (RPMT $ pt2multiterm pt)
bsr2bsmt (AA.Leaf (RPConstraint   mt1  rpr mt2)  ) = AA.mkLeaf (RPMT (mt1 ++ MTT (rel2txt rpr) : mt2))
bsr2bsmt (AA.Leaf (RPBoolStructR  mt1  rpr bsr2) ) = let output = (\(RPMT rpmt) -> RPMT (mt1 ++ MTT (rel2txt rpr) : rpmt)) <$> bsr2bsmt bsr2
                                                     in -- trace ("bsr2bsmt handling a boolstructr, input = " <> show bsr2) $
                                                        -- trace ("bsr2bsmt handling a boolstructr, returning " <> show output) $
                                                        output
bsr2bsmt (AA.Leaf (RPnary     RPgt rps) )          = AA.mkAll (Just $ AA.Pre "max of:") (bsr2bsmt . AA.Leaf <$> rps)
bsr2bsmt (AA.Leaf (RPnary     RPlt rps) )          = AA.mkAll (Just $ AA.Pre "min of:") (bsr2bsmt . AA.Leaf <$> rps)
bsr2bsmt (AA.Leaf (RPnary     _rprel rps) )        = bsr2bsmt (AA.All Nothing (AA.Leaf <$> rps))
bsr2bsmt (AA.All lbl xs) = AA.mkAll lbl (bsr2bsmt <$> xs)
bsr2bsmt (AA.Any lbl xs) = AA.mkAny lbl (bsr2bsmt <$> xs)
bsr2bsmt (AA.Not     x ) = AA.mkNot     (bsr2bsmt x)

-- | is a given RuleName the target of a Hence or Lest "GOTO"-style pointer?
-- If it is, we deem it a RuleAlias.
isRuleAlias :: Interpreted -> RuleName -> Bool
isRuleAlias l4i rname =
  any matchHenceLest (origrules l4i)
  where
    matchHenceLest Regulative{..} = testMatch hence || testMatch lest
    matchHenceLest _              = False
    testMatch :: Maybe Rule -> Bool
    testMatch r = r == Just (RuleAlias rname) || maybe False matchHenceLest r

-- | extract all TYPICALLY annotations for use by XPilers to indicate default markings.
-- This is used by the Purescript and SVG transpilers.

getMarkings :: Interpreted -> AA.TextMarking
getMarkings l4i =
  AA.Marking $ Map.fromList $
  [ (defkey, defval)
  | DefTypically{..} <- origrules l4i
  , (defkey,defval) <- catMaybes $ markings <$> defaults
  ]
  where
    markings :: RelationalPredicate -> Maybe (T.Text, AA.Default Bool)
    markings (RPConstraint (MTT "has" : xs) RPis rhs) = Just (mt2text xs, AA.Default (Left $ rhsval rhs))
    markings (RPConstraint (MTT "is"  : xs) RPis rhs) = Just (mt2text xs, AA.Default (Left $ rhsval rhs))
    markings (RPConstraint          xs  RPis rhs) = Just (mt2text xs, AA.Default (Left $ rhsval rhs))
    markings _                                    = Nothing

    rhsval [MTB rhs] = Just rhs
    rhsval [MTF rhs] = if rhs == 0 then Just False else Just True
    rhsval [MTT rhs] = case T.toLower rhs of
                   "does not" -> Just False
                   "doesn't"  -> Just False
                   "hasn't"   -> Just False
                   "false"    -> Just False
                   "not"      -> Just False
                   "no"       -> Just False
                   "f"        -> Just False
                   "t"        -> Just True
                   "so"       -> Just True
                   "yes"      -> Just True
                   "has"      -> Just True
                   "true"     -> Just True
                   "does"     -> Just True
                   _            -> Nothing
    rhsval [] = Nothing
    -- [TODO] we need to think through a situation where the RHS multiterm has multiple elements in it ... we're a bit brittle here
    rhsval _  = Nothing


-- | local variables
-- a list of the typed multiterms which show up inside the GIVEN and GIVETH attributes of a rule.
ruleLocals, ruleLocalsIn, ruleLocalsOut :: Interpreted -> Rule -> [TypedMulti]
ruleLocals l4i r = ruleLocalsIn l4i r ++ ruleLocalsOut l4i r

-- | input variables -- GIVEN
ruleLocalsIn _l4i r
  | not (hasGiven r) = []
  | otherwise = concatMap NE.toList (maybeToList (given r))

-- | output variables -- GIVETH
ruleLocalsOut _l4i r
  | not (hasGiveth r) = []
  | otherwise = concatMap NE.toList (maybeToList (giveth r))



type NestedClass = Tree ParamText

-- | top-level DEFINEs
-- DEFINEs that have horn clause heads but no bodies are constant facts, so we'll define them as such here.
-- DEFINEs that have horn clauses with bodies are functions that need to be set up a little differently. We'll deal with those separately.
globalFacts :: Interpreted -> [NestedClass]
globalFacts l4i =
  [ Node (NE.singleton (NE.fromList (name r), super r) :: ParamText)
    [ Node pt []
    | HC { hHead = RPParamText pt, hBody = Nothing } <- clauses r
    ]
  | r@Hornlike{} <- origrules l4i
  , hasClauses r, Define == keyword r
  ]

-- * Extract everything that looks like a method.
--
-- A common form is:
-- @
--    DECIDE ClassA's RecordAttr's AttributeName IS foo WHEN bar
--           ClassA's RecordAttr's AttributeName IS baz WHEN quux
--           ClassA's RecordAttr's AttributeName IS baz OTHERWISE
-- @
-- 
-- Extract and return all of these decisions in the form:
--
-- @
--    [([ClassA, RecordAttr, AttributeName], foo, Just bar
-- @
-- 
-- | we extract the methods to a fully qualified and annotatable form defined as `ValuePredicate` -- see Types.hs
--
-- Go through every rule and break it down into this structure.

attrsAsMethods :: RuleSet -> XPileLogE [ValuePredicate]
attrsAsMethods rs = do
  outs <-
    for [ r | r@Hornlike{keyword=Decide} <- rs ] $ \r -> do
    for (clauses r) $ \hc -> do
      gone1 <- go hc
      case gone1 of
        Left errs1 -> xpError errs1
        Right (headLHS, attrVal, attrCond) -> do
          gone2 <- toObjectPath headLHS
          mutterd 3 $ show headLHS <> " ... got back gone2: " <> show gone2
          case gone2 of
            Left errs2 -> xpError errs2
            Right (objPath, attrName) -> do
              let toreturn = defaultValuePredicate
                    { origHC = Just hc
                    , origBSR = hBody hc
                    , objPath
                    , attrName
                    , attrVal
                    , attrCond
                    , origRule = Just r
                    }
              mutterd 3 $ show headLHS <> " returning"
              mutter $ show $ srchs toreturn
              xpReturn toreturn

  let (errs, successes) = partitionEithers (concat outs)
  mutters (concat errs)
  xpReturn successes
            
  where go :: HornClause2 -> XPileLogE (MultiTerm, Maybe RelationalPredicate, Maybe BoolStructR)
        go hc@HC{..} =
          case hHead of
            (RPnary RPis (RPMT headLHS : headRHS : [])) -> xpReturn (headLHS, Just headRHS, hBody)

            (RPnary RPis (RPMT headLHS : headRHS)) -> do
              mutterd 3 $ "unexpected RHS in RPnary RPis: " <> show hHead
              xpReturn (headLHS, listToMaybe headRHS, hBody)
            
            (RPConstraint mt1 RPis mt2) -> do
              mutterd 3 $ "converting RPConstraint in hHead: " <> show hHead
              xpReturn (mt1, Just (RPMT mt2), hBody)

            _ -> do
              mutterd 3 "attrsAsMethods: encountered unexpected form of RelationalPredicate"
              mutter $ show $ srchs hHead
              xpError ["unhandled RelationalPredicate", show hHead]


-- | input: [MTT "foo's", MTT "bar's", MTT "baz"]
-- 
--  output: (["foo", "bar"], "baz")
toObjectPath :: MultiTerm -> XPileLogE ([EntityName], EntityName)
toObjectPath [] = do mutter "error: toObjectPath given an empty list!" >> xpReturn ([], "errorEntityname")
toObjectPath mt = do
  mutterd 4 $ "toObjectPath input = " <> show mt
  mutterd 4 $ "DL.init mt = " <> show (DL.init mt)
  mutterd 4 $ "mt2text = " <> show (mt2text $ DL.init mt)
  mutterd 4 $ "T.replace = " <> show (T.replace "'s" "'s" $ mt2text $ DL.init mt)
  mutterd 4 $ "T.splitOn = " <> show (T.splitOn "'s" (T.replace "'s" "'s" $ mt2text $ DL.init mt))
  mutterd 4 $ "T.strip = " <> show (T.strip <$> T.splitOn "'s" (T.replace "'s" "'s" $ mt2text $ DL.init mt))
  mutterd 4 $ "DL.filter = " <> show (DL.filter (not . T.null) $ T.strip <$> T.splitOn "'s" (T.replace "'s" "'s" $ mt2text $ DL.init mt))
  xpReturn (DL.filter (not . T.null) $
            T.strip <$> T.splitOn "'s" (T.replace "'s" "'s" $ mt2text $ DL.init mt)
           , mt2text [DL.last mt])

-- | text version of the above: foo.bar.baz.
--
-- We should do slightly
-- better error handling here to deal with a case where MultiTerm is
-- empty -- maybe we switch to a TypedMulti with NE guarantees on the
-- multiterm?
toObjectStr :: MultiTerm -> XPileLogE EntityName
toObjectStr mt = do
  objPath <- toObjectPath mt
  case objPath of
    Right (oP,objName) -> xpReturn $ T.intercalate "." (oP ++ [objName])
    Left err           -> xpError err

-- | is a particular attribute typed as an enum?
--
-- 1. does the current rule define the attribute as a GIVEN parameter typed as an enum?
--
-- 2. does the current rule define the attribute as a parameter whose type is explicitly known to be enum at the toplevel?
isAnEnum :: Interpreted -> Maybe ParamText -> MultiTerm -> Bool
isAnEnum l4i mgiven mt =
  let enumNames  = fmap lowerMT . ruleLabelName <$> extractEnums l4i
      toreturn   = isGivenEnum mgiven enumNames mt

  in -- trace ("lowerMT = " <> show myAttrName <> "; enumNames = " <> show enumNames <> "; toreturn = " <> show toreturn) $
     toreturn
    -- lowerMT = [MTT "planaf"]; ruleLabelName = [[MTT "outcome"],[MTT "planaf"],[MTT "plan14"],[MTT "injury"]] = True

-- | does the current multiterm attribute match a GIVEN parameter which was annotated as an Enum, either explicitly or as a known enum type?
-- [TODO] refactor this together with the above function to a single function
isGivenEnum :: Maybe ParamText -> [MultiTerm] -> MultiTerm -> Bool
isGivenEnum Nothing _ mt = False
isGivenEnum (Just typedMultis) enumNames mt = any (enumMatch enumNames mt) $ NE.toList typedMultis

-- | does a specific typedmulti match an enum
enumMatch :: [MultiTerm] -> MultiTerm -> TypedMulti -> Bool
enumMatch _ mt (givenName, Nothing) = False
enumMatch _ mt (givenName, Just mgiven@(InlineEnum _ _)) =
  -- trace ("mt = " <> show mt <> "; givenName = " <> show givenName <> "; given = " <> show mgiven) $
  mt == NE.toList givenName
enumMatch enumNames mt (givenName, Just mgiven@(SimpleType TOne etype)) =
  (pure . MTT . T.toLower . (<> "enum") $ etype) `elem` enumNames

-- | lowercase a multiterm to support isAnEnum comparison
lowerMT :: MTExpr -> MTExpr
lowerMT (MTT t) = MTT (T.toLower t)
lowerMT x       = x

