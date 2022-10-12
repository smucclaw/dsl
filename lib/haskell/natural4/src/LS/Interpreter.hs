{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-|

The Interpreter runs after the Parser. It prepares for transpilation by organizing the ruleset and providing helper functions used by multiple XPile backends.

In future, we may be so ambitious as to attempt some type checking and type inference, and so on, here, though that may be better left to corel4.

See also documentation at https://github.com/smucclaw/dsl/tree/tab-mustsing#interpretation-requirements

Typical usage runs `l4interpret` and then makes use of the attributes returned in the `l4i` object.

-}

module LS.Interpreter where

import LS.Types
import LS.RelationalPredicates
import LS.PrettyPrinter
import qualified AnyAll as AA
import Data.List.NonEmpty
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Map as Map
import Prettyprinter
import Text.Pretty.Simple
import Debug.Trace
import Data.Maybe
import Data.Graph.Inductive
import Data.Tuple (swap)
import Data.List (find, intersperse)
import qualified Data.List as DL
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import Data.Bifunctor (first)

-- | interpret the parsed rules based on some configuration options. This is a canonical intermediate representation used by downstream functions.
l4interpret :: InterpreterOptions -> [Rule] -> Interpreted
l4interpret iopts rs =
  let ct = classHierarchy rs
      st = symbolTable    iopts rs
  in
    L4I { classtable = ct
        , scopetable = st
        , origrules  = rs
        }

-- | introspect a little bit about what we've interpreted. This gets saved to the workdir's org/ directory.
musings :: Interpreted -> [Rule] -> Doc ann
musings l4i rs =
  let cg = classGraph (classtable l4i) []
      expandedRules = DL.nub $ concatMap (expandRule rs) rs
      ridmap = Map.fromList (Prelude.zip rs [1..])
      decisionGraph = ruleDecisionGraph l4i rs
      decisionroots = decisionRoots decisionGraph
      groupedByAOTree = Map.toList $ Map.fromListWith (++) $ (\r -> (getAndOrTree l4i 1 r, [r])) <$> decisionroots
  in vvsep [ "* musings"
           , "** Class Hierarchy"
           , vvsep [ "*** Class:" <+> pretty (Prelude.head cname) <> if null (Prelude.tail cname) then emptyDoc
                                                                     else hsep (" belongs to" : (pretty <$> Prelude.tail cname))
                   | (cname, child) <- cg ]
           , "*** The entire classgraph"
           , srchs cg
           , "** Symbol Table"
           , "we know about the following scopes"
           , vvsep [ "*** Rule:" <+> hsep (pretty <$> rn) </>
                     vvsep [ "**** symbol:" <+> tildes (pretty mt)
                             </> srchs hc
                             </> "**** typesig:" <+> tildes (viaShow its)
                             
                           | (mt, (its, hc)) <- Map.toList st ]
                   | (rn, st) <- Map.toList $ scopetable l4i ]

           , "** the Rule Decision Graph"
           , example (pretty (prettify (first ruleLabelName decisionGraph)))

           , "** Decision Roots"
           , "rules which are not relied on by any other rule"
           , srchs (ruleLabelName <$> decisionroots)

           , "*** Nubbed Decision Roots"
           , "maybe some of the decision roots are identical and don't need to be repeated"
           , vvsep [ vsep [ "-" <+> pretty (T.unwords $ ruleLabelName r) | r <- uniqrs ]
                     </> srchs (expandBSR l4i 0 <$> getBSR (DL.head uniqrs))
                   | (_, uniqrs) <- groupedByAOTree
                   ]

           , "** TODO Expanded rules"
           , "this isn't quite what we want yet -- we're looking for the rules to be expanded not just in terms of inter-rule HENCE/LEST connections, but also in terms of the defined terms"

           , if expandedRules == DL.nub rs
             then "(ahem, they're actually the same as unexpanded, not showing)"
             else vvsep [ "***" <+> hsep (pretty <$> ruleLabelName r) </> srchs r | r <- expandedRules ]
           , "** getAndOrTrees, direct"
           , vvsep [ "***" <+> hsep (pretty <$> ruleLabelName r) </> srchs (getAndOrTree l4i 1 r) | r <- rs ]
           , "** The original rules"
           , vvsep [ "***" <+> pretty (ruleLabelName r) </> srchs r | r <- rs ]
           ]
  where
    srchs :: (Show a) => a -> Doc ann
    srchs = src "haskell" . pretty . pShowNoColor
    src lang x = vsep [ "#+begin_src" <+> lang, x, "#+end_src" ]
    example  x = vsep [ "#+begin_example", x, "#+end_example" ]

-- | interpret the parsed rules and construct the symbol tables
symbolTable :: InterpreterOptions -> [Rule] -> ScopeTabs
symbolTable iopts rs =
  Map.fromListWith (<>) (fromGivens <> fromDefines <> fromDecides)
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

-- | classes can contain other classes. Here the hierarchy represents the "has-a" relationship, conspicuous when a DECLARE HAS HAS HAS.
classHierarchy :: [Rule] -> ClsTab
classHierarchy rs =
  -- multiple DECLARE of the same class are allowed, so we have to merge.
  -- we do some violence to the inferred types here.
  CT $ Map.fromListWith (\((ts1inf,ts1s),CT clstab1)
                          ((ts2inf,ts2s),CT clstab2) ->
                            ( (listToMaybe (maybeToList ts1inf <> maybeToList ts2inf)
                              ,ts1s <> ts2s)
                            , CT $ clstab1 <> clstab2))
  [ (thisclass, (classtype, attributes))
  | r@TypeDecl{} <- rs
  , let thisclass = mt2text (name r)
        classtype = (super r, [])
        attributes = classHierarchy (has r)
  ]

-- | redraw the class hierarchy as a rooted graph, where the fst in the pair contains all the breadcrumbs to the current node. root to the right.
classGraph :: ClsTab -> [EntityType] -> [([EntityType], TypedClass)]
classGraph (CT ch) ancestors = concat
  [ (nodePath, (_itypesig, childct)) : classGraph childct nodePath
  | (childname, (_itypesig, childct)) <- Map.toList ch
  , let nodePath = childname : ancestors
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
  [ cn
  | n <- topsort asGraph
  , (Just cn) <- [Map.lookup n idToType]
  ]
  where
    allTypes = allClasses ct -- ++ allSymTypes stabs [TODO] if it turns out there are hidden classnames lurking in the symbol table
    allClasses :: ClsTab -> [MyClassName]
    allClasses = getCTkeys
    -- first let's assign integer identifiers to each type found in the class hierarchy
    typeToID = Map.fromList (Prelude.zip allTypes [1..])
    idToType = Map.fromList $ swap <$> Map.toList typeToID
    asGraph :: Gr MyClassName ()
    asGraph =
      -- there are a couple different kinds of dependencies...
      let child2parent = getInheritances ct       -- child depends on parent
          class2attrtypes = [ (cn, ut)            -- class depends on attribute types
                            | cn <- getCTkeys ct
                            , ts <- getAttrTypesIn ct cn
                            , Right ut <- [getUnderlyingType ts]
                            ]
      in mkGraph (Map.toList idToType) (myEdges (child2parent ++ class2attrtypes))
    myEdges :: [(MyClassName, MyClassName)] -> [(Int, Int, ())]
    myEdges abab = [ (aid, bid, ())
                   | (a,b) <- abab
                   , (Just aid) <- [Map.lookup a typeToID]
                   , (Just bid) <- [Map.lookup b typeToID]
                   ]

-- | the (inner) type of a particular class's attribute
attrType :: ClsTab -> EntityType -> Maybe TypeSig
attrType (CT clstab) attrName = do
  (t, CT _ct) <- Map.lookup attrName clstab
  getSymType t


-- | extract all inheritance relationships
getInheritances :: ClsTab -> [(EntityType, EntityType)]
getInheritances ct =
  [ (child, parent)
  | child <- getCTkeys ct
  , (Just parent) <- [clsParent ct child]
  ]

-- | recursively return all attribute types found under a given class, i think?
getAttrTypesIn :: ClsTab -> EntityType -> [TypeSig]
getAttrTypesIn ct classname =
  case thisAttributes ct classname of
    Nothing         -> []
    (Just (CT ct')) -> concat [ ts : concatMap (getAttrTypesIn ct'') (getCTkeys ct'')
                              | (_attrname, (its, ct'')) <- Map.toList ct' -- EntityType (Inferrable TypeSig, ClsTab)
                              , Just ts <- [getSymType its]
                              ]


-- | reduce the ruleset to an organized set of rule trees.
-- where the HENCE and LEST are fully expanded; once a HENCE/LEST child has been incorporated into its parent, remove the child from the top-level list of rules.
-- similarly with DECIDE rules defined inline with MEANS
-- and references to defined terms; if a rule talks about Degustates, expand that to eats/drinks

stitchRules :: Interpreted -> [Rule] -> [Rule]
stitchRules l4i rs = rs
  -- partition rules into consumers and providers
  -- a consumer is one that is not mentioned by any other rule.
  -- a provider is one that is mentioned by some other rule, or is a rulealias.
  -- filter out providers; return only consumers.
  -- the Petri transpiler uses fgl to think about this stuff.
  -- here we do it directly.


type RuleIDMap = Map.Map Rule Int

-- | structure the rules as a graph.
-- in the simple case, the graph is one or more trees, each rooted at a "top-level" rule which is not "used" by any another rule.
-- if we walk the roots, we will sooner or later encounter all the decision elements relevant to each root.
-- in a less simple case, the graph is cyclic! everything depends on everything else! but we can recognize that as an error condition.
--
-- note that a regulative rule R1 HENCE R2 is recorded as a single rule, even if we think of the R2 as a separate rule
-- perhaps we should have a notion of anonymous rules, that are internally labelled and structured, so R2 is equal to R1 in the graph.

type RuleGraphEdgeLabel = ()
type RuleGraph = Gr Rule RuleGraphEdgeLabel



ruleDecisionGraph :: Interpreted -> [Rule] -> RuleGraph
ruleDecisionGraph l4i rs =
  let ruleIDmap = Map.fromList (Prelude.zip decisionRules [1..])
  in mkGraph
  (swap <$> Map.toList ruleIDmap) -- the nodes
  (relPredRefsAll l4i rs ruleIDmap)
  where
    decisionRules = [ r | r <- rs, not . null . getBSR $ r ]
    
-- | walk all relationalpredicates in a set of rules, and return the list of edges showing how one rule relies on another.
relPredRefsAll :: Interpreted -> [Rule] -> RuleIDMap -> [LEdge RuleGraphEdgeLabel]
relPredRefsAll l4i rs ridmap =
  concatMap (relPredRefs l4i rs ridmap) rs

-- | in a particular rule, walk all the relational predicates available, and show outdegree links
-- that correspond to known BSR heads from the entire ruleset.
-- in other words, if a rule R1 says something like (a WHEN b OR c), it defines a, and relies on b and c;
-- if we find a rule R2 which defines (c MEANS c1 AND c2), then it defines c, and relies on c1 and c2.
-- so we show that rule R1 relies on, or refers to, rule R2: R1 -> R2.
-- there is some overlap here with the idea of scopetabs in the symbol table, but let's just do it
-- the brute way first and then refactor later once we have a better idea if this approach even works.
relPredRefs :: Interpreted -> [Rule] -> RuleIDMap -> Rule -> [LEdge RuleGraphEdgeLabel]
relPredRefs l4i rs ridmap r =
  let headElements :: Map.Map MultiTerm Rule -- does this get recomputed each time or cached?
      -- given a term, see which rule defines it
      headElements = Map.fromList $
                     [ (headName,r')
                     | r' <- rs
                     , headName <- getDecisionHeads r'
                     ]
      -- given a rule, see which terms it relies on
      bodyElements = concat $ rp2bodytexts <$> concatMap AA.extractLeaves (getBSR r)
  -- given a rule R, for each term relied on by rule R, identify all the subsidiary rules which define those terms.
  in [ (rid, targetRuleId', ())
     | bElem <- bodyElements
     , let targetRule = Map.lookup bElem headElements
           targetRule' = fromJust targetRule
           targetRuleId = Map.lookup targetRule' ridmap
           targetRuleId' = fromJust targetRuleId
           rid = ridmap Map.! r
     , isJust targetRule
     , isJust targetRuleId
     ]
  

-- | all the rules which have no indegrees, as far as decisioning goes
decisionRoots :: RuleGraph -> [Rule]
decisionRoots rg =
  let rg' = dereflexed
  in
  catMaybes [ lab rg' r
            | r <- nodes rg'
            ,  indeg rg' r == 0
--            , outdeg rg' r  > 0
            ]
  where
    -- remove reflexive edges that go from node n to node n
    dereflexed :: RuleGraph
    dereflexed =
      let toreturn = foldr (\n g -> delEdge (n,n) g) rg (nodes rg)
      in
--        trace ("dereflexed before: " ++ prettify rg) $
--        trace ("dereflexed after:  " ++ prettify toreturn) $
        toreturn


-- | return the internal conditions of the rule, if any, as an and-or tree.
-- a Regulative rule exposes its `who` and `cond` attributes
-- a Constitutive rule exposes the body of its `clauses`

-- todo: multiple rules with the same head should get &&'ed together and jammed into a single big rule

getAndOrTree :: Interpreted -> Int -> Rule -> Maybe (AA.ItemMaybeLabel T.Text) -- Vue wants AA.Item T.Text
getAndOrTree _l4i depth r@Regulative{who=whoMBSR, cond=condMBSR} =
  (fmap (((bsp2text (subj r) <> " ") <>) . rp2text) <$> whoMBSR)  <> -- WHO is relative to the subject
  (fmap                                    rp2text  <$> condMBSR)    -- the condition is absolute
  
getAndOrTree  l4i depth r@Hornlike{} = expandTrace "getAndOrTree" depth "fmap extractRPMT2Text ..." $
                                       fmap extractRPMT2Text <$>
                                       (expandTrace "getAndOrTree" depth "mconcat bsmtOfClauses..." $
                                        mconcat (-- traceShowId $
                                           bsmtOfClauses l4i (depth+1) $
                                           r { clauses = expandClauses l4i (depth+1) (clauses r) } ))
  where
    defaultElem :: a -> [a] -> [a]
    defaultElem dflt []  = [ dflt ]
    defaultElem _    lst = lst

getAndOrTree l4i depth _r@(RuleAlias rn) = do
  r' <- getRuleByName (origrules l4i) rn
  getAndOrTree l4i (depth+1) r'

getAndOrTree _l4i depth r = -- trace ("ERROR: getAndOrTree called invalidly against rule " <> show r) $
  Nothing

-- convert clauses to a boolStruct MT
bsmtOfClauses :: Interpreted -> Int -> Rule -> [Maybe (AA.ItemMaybeLabel RelationalPredicate)]
bsmtOfClauses l4i depth r =
  let toreturn =
        [ listToMaybe $ maybeToList $ mbody <> mhead
        | c <- expandClauses l4i 2 (clauses r)
        , (hhead, hbody)  <- [(hHead c, hBody c)]
        , let (_bodyEx, bodyNonEx) = partitionExistentials c
        , let mhead, mbody :: Maybe (AA.ItemMaybeLabel RelationalPredicate)
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

expandClauses, expandClauses' :: Interpreted -> Int -> [HornClause2] -> [HornClause2]
expandClauses l4i depth hcs = (expandTrace "expandClauses" depth $ "running on " ++ show (Prelude.length hcs) ++ " hornclauses") $ expandClauses' l4i (depth+1) hcs
expandClauses' l4i depth hcs =
  let toreturn = [ newhc
                 | oldhc <- hcs
                 , let newhead = (expandTrace "expandClauses" depth $ "expanding the head") $                expandRP l4i (depth+1)   $  hHead oldhc
                       newbody = (expandTrace "expandClauses" depth $ "expanding the body") $ unleaf . fmap (expandRP l4i (depth+1)) <$> hBody oldhc
                       newhc = case oldhc of
                                 HC2 oldh Nothing -> HC2 newhead Nothing
                                 HC2 oldh _       -> HC2 oldh    newbody
                 ]
  in expandTrace "expandClauses" depth ("returning " ++ show toreturn) $
     toreturn

unleaf :: BoolStructR -> BoolStructR
unleaf (AA.Leaf (RPBoolStructR b RPis bsr)) = unleaf bsr
unleaf (AA.All  lbl xs) = AA.All lbl (unleaf <$> xs)
unleaf (AA.Any  lbl xs) = AA.Any lbl (unleaf <$> xs)
unleaf (AA.Not      x ) = AA.Not     (unleaf     x )
unleaf (AA.Leaf x     ) = AA.Leaf    x

-- take out the Leaf ( RPBoolStructR [ "b" ] RPis
-- from the below:
--        [ HC2
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

-- True for debugging, False for prod
expandTraceDebugging :: Bool
expandTraceDebugging = True

expandTrace :: (Show a) => String -> Int -> String -> a -> a
expandTrace fname dpth toSay toShow =
  if expandTraceDebugging
  then trace (replicate dpth '*' ++ " " ++ fname ++ ": " {- ++ replicate dpth '|' ++ " " -} ++ toSay ++ "\n" ++
               "#+BEGIN_SRC haskell\n" ++ (TL.unpack (pShowNoColor toShow)) ++ "\n#+END_SRC") $
       toShow
  else toShow

-- | is a given multiterm defined as a head somewhere in the ruleset?
-- later, we shall have to limit the scope of such a definition based on UPON / WHEN / GIVEN preconditions.
-- for now we just scan across the entire ruleset to see if it matches.
expandRP :: Interpreted -> Int -> RelationalPredicate -> RelationalPredicate
expandRP l4i depth (RPMT                   mt2) = (expandTrace "expandRP" depth $ "RPMT " ++ show mt2 ++ ": calling expandMT on " ++ show mt2) $
                                                  expandMT  l4i (depth + 1) mt2
expandRP l4i depth (RPConstraint  mt1 RPis mt2) = (expandTrace "expandRP" depth $ "RPConstraint " ++ show mt1 ++ " is " ++ show mt2 ++ ": calling expandMT on " ++ show mt2) $
                                                  expandMT  l4i (depth + 1) mt2
expandRP l4i depth (RPBoolStructR mt1 RPis bsr) = (expandTrace "expandRP" depth $ "RPBoolStructR " ++ show mt1 ++ " is BSR: calling expandBSR on " ++ show bsr) $
                                                  RPBoolStructR mt1 RPis (expandBSR l4i (depth + 1) bsr)
expandRP l4i depth x                            = (expandTrace "expandRP" depth $ "returning unchanged " ++ show x) $
                                                  x

expandMT :: Interpreted -> Int -> MultiTerm -> RelationalPredicate
expandMT l4i depth mt0 =
  let expanded = listToMaybe
                 [ out
                 | (scopename,symtab) <- Map.toList (scopetable l4i)
                 , (mytype, cs) <- maybeToList $
                                   (expandTrace "expandMT" depth $ "considering scope " ++ show scopename ++ ", looking up " ++ show mt0 ++ " in symtab") $
                                   Map.lookup mt0 symtab
                 , c <- (expandTrace "expandMT" depth $ "working through clauses " ++ show cs)
                        cs
                 , let outs = expandClause l4i depth c
                 , out <- (expandTrace "expandMT" depth $ "may  return " ++ show outs)
                          outs
                 ]
      toreturn = fromMaybe (
        (expandTrace "expandMT" depth $ "defaulting to RPMT " ++ show mt0) $
          RPMT mt0
        ) expanded
  in -- (expandTrace "expandMT" depth $ "expanded = " ++ show expanded) $
     (expandTrace "expandMT" depth $ "will return " ++ show toreturn) $
     toreturn

-- | expand a horn clause that may have both head and body containing stuff we want to fill.
-- not directly related to expandClauses
expandClause :: Interpreted -> Int -> HornClause2 -> [RelationalPredicate]
expandClause l4i depth (HC2   (RPMT          mt          ) (Nothing) ) = [          ] -- no change
expandClause l4i depth (HC2   (RPParamText   pt          ) (Nothing) ) = [          ] -- no change
expandClause l4i depth (HC2   (RPConstraint  mt RPis  rhs) (Nothing) ) = [ RPMT rhs ] -- substitute with rhs
expandClause l4i depth (HC2 o@(RPConstraint  mt rprel rhs) (Nothing) ) = [     o    ] -- maintain inequality
expandClause l4i depth (HC2   (RPBoolStructR mt RPis  bsr) (Nothing) ) = [ (expandTrace "expandMT" depth $ "body=Nothing; returning from head: BSR " ++ show mt ++ " RPis expandBSR") $
                                                                           RPBoolStructR mt RPis (expandBSR l4i (depth + 1) bsr) ]
                              
expandClause l4i depth (HC2   (RPMT          mt          ) (Just bodybsr) ) = [ (expandTrace "expandMT" depth $ "body=Just, returning from body: BSR " ++ show mt ++ " RPis expandBSR") $
                                                                                RPBoolStructR mt RPis (expandBSR l4i (depth + 1) bodybsr) ]
expandClause l4i depth (HC2   (RPParamText   pt          ) (Just bodybsr) ) = [          ] -- no change
expandClause l4i depth (HC2   (RPConstraint  mt RPis  rhs) (Just bodybsr) ) = [          ] -- x is y when z ... let's do a noop for now, and think through the semantics later.
expandClause l4i depth (HC2 o@(RPConstraint  mt rprel rhs) (Just bodybsr) ) = [    o     ] -- maintain inequality
expandClause l4i depth (HC2   (RPBoolStructR mt RPis  bsr) (Just bodybsr) ) = [          ] -- x is y when z ... let's do a noop for now, and think through the semantics later.



expandBSR, expandBSR' :: Interpreted -> Int -> BoolStructR -> BoolStructR
expandBSR  l4i depth x = let y = expandBSR' l4i depth x in (expandTrace "expandBSR" depth $ "given " ++ show x) $
                                                           (expandTrace "expandBSR" depth $ "returning " ++ show y) $
                                                           y
expandBSR' l4i depth (AA.Leaf rp)    = (expandTrace "expandBSR" depth $ "handling Leaf " ++ show rp ++ " by expanding; next will test output of expansion") $
  case expandRP l4i (depth + 1) rp of
    RPBoolStructR mt1 RPis bsr -> (expandTrace "expandBSR" depth $ "bsr track: " ++ show bsr)
                                  bsr
    o                          -> (expandTrace "expandBSR" depth $ "o track: Leaf " ++ show o) $
                                  AA.Leaf o
expandBSR' l4i depth (AA.Not item)   = (expandTrace "expandBSR" depth $ "recursing into Not") $
                                       AA.Not     (expandBSR l4i (depth + 1) item)
expandBSR' l4i depth (AA.All lbl xs) = (expandTrace "expandBSR" depth $ "recursing into All") $
                                       AA.All lbl (expandBSR l4i (depth + 1) <$> xs)
expandBSR' l4i depth (AA.Any lbl xs) = (expandTrace "expandBSR" depth $ "recursing into Any") $
                                       AA.Any lbl (expandBSR l4i (depth + 1) <$> xs)

expandBody :: Interpreted -> Maybe BoolStructR -> Maybe BoolStructR
expandBody l4i = id


-- | used by the Petri xpiler.
expandRulesByLabel :: [Rule] -> T.Text -> [Rule]
expandRulesByLabel rules txt =
  let toreturn =
        [ q
        | r <- rules
        , let mt = rl2text <$> rLabelR r
        , Just txt == mt
        , let qs = expandRule rules r
        , q <- qs
        ]
  in -- trace ("expandRulesByLabel(" ++ show txt ++ ") about to return " ++ show (rlabel <$> toreturn))
     toreturn

expandRule :: [Rule] -> Rule -> [Rule]
expandRule rules r@Regulative{..} = [r]
expandRule rules r@Hornlike{..} =
  let toreturn =
        -- we support hornlike expressions of the form x is y and z; we return y and z
        [ q
        | clause <- clauses
        , let rlbl' = rl2text <$> rLabelR r
              bsr = -- trace ("expandRule: got head " ++ show (hHead clause))
                    hHead clause
        , isJust rlbl'
        , mt <- -- trace ("aaLeaves returned " ++ show (aaLeaves (AA.Leaf bsr)))
                aaLeaves (AA.Leaf bsr)
        -- map each multiterm to a rulelabel's rl2text, and if it's found, return the rule
        , q <- expandRulesByLabel rules (mt2text mt)
        ]
  in -- trace ("expandRule: called with input " ++ show rlabel)
     -- trace ("expandRule: about to return " ++ show (ruleName <$> toreturn))
     toreturn
expandRule _ _ = []


-- | used for purescript output -- this is the toplevel function called by Main
onlyTheItems :: Interpreted -> AA.ItemMaybeLabel T.Text
onlyTheItems l4i =
  let myitem = AA.All Nothing (catMaybes $ getAndOrTree l4i 1 <$> origrules l4i)
      simplified = AA.simplifyItem myitem
  in -- trace ("onlyTheItems: before calling simplifyItem: " <> (TL.unpack $ pShowNoColor myitem)) $
     -- trace ("onlyTheItems: after  calling simplifyItem: " <> (TL.unpack $ pShowNoColor simplified)) $
     simplified

onlyItemNamed :: Interpreted -> [Rule] -> [RuleName] -> AA.ItemMaybeLabel T.Text
onlyItemNamed l4i rs wanteds =
  let ibr = itemsByRule l4i rs
      found = DL.filter (\(rn, _simp) -> rn `elem` wanteds) ibr
  in
    if null found
    then AA.Leaf $ T.pack ("L4 Interpreter: unable to isolate rule named " ++ show wanteds)
    else snd $ DL.head found

-- | let's hazard a guess that the item with the mostest is the thing we should put in front of the user.
biggestItem :: Interpreted -> [Rule] -> AA.ItemMaybeLabel T.Text
biggestItem l4i rs =
  let ibr = itemsByRule l4i rs
      flattened = (\(x,y) -> (x, AA.extractLeaves y)) <$> ibr
      sorted = DL.reverse $ DL.sortOn (DL.length . snd) flattened
  in (Map.fromList ibr) Map.! (fst $ DL.head sorted)

itemsByRule :: Interpreted -> [Rule] -> [(RuleName, AA.ItemMaybeLabel T.Text)]
itemsByRule l4i rs =
  [ (ruleLabelName r, simplified)
  | r <- rs
  , let aot = getAndOrTree l4i 1 r
        simplified = fromJust aot
  , isJust aot
  ]

alwaysLabel :: AA.ItemMaybeLabel T.Text -> AA.ItemJSONText
alwaysLabel (AA.All Nothing xs)  = AA.All (AA.Pre "all of the following") (alwaysLabel <$> xs)
alwaysLabel (AA.Any Nothing xs)  = AA.Any (AA.Pre "any of the following") (alwaysLabel <$> xs)
alwaysLabel (AA.All (Just x) xs) = AA.All x (alwaysLabel <$> xs)
alwaysLabel (AA.Any (Just x) xs) = AA.Any x (alwaysLabel <$> xs)
alwaysLabel (AA.Leaf x)          = AA.Leaf x
alwaysLabel (AA.Not x)           = AA.Not (alwaysLabel x)

-- we must be certain it's always going to be an RPMT
-- we extract so that it's easier to convert to JSON or to purescript Item Text
extractRPMT2Text :: RelationalPredicate -> T.Text
extractRPMT2Text (RPMT ts) = T.unwords ts
extractRPMT2Text _         = error "extractRPMT2Text: expecting RPMT only, other constructors not supported."

ruleNameStr :: Rule -> String
ruleNameStr r = T.unpack (mt2text (ruleLabelName r))

type RuleSet = [Rule]

getRuleByName :: RuleSet -> RuleName -> Maybe Rule
getRuleByName rs rn = find (\r -> ruleName r == rn) rs

getRuleByLabel :: RuleSet -> T.Text -> Maybe Rule
getRuleByLabel rs t = find (\r -> (rl2text <$> rLabelR r) == Just t) rs

getRuleByLabelName :: RuleSet -> T.Text -> Maybe Rule
getRuleByLabelName rs t = find (\r -> (rl2text <$> rLabelR r) == Just t
                                      ||
                                      T.unwords (ruleName r) == t
                               ) rs

-- where every RelationalPredicate in the boolstruct is narrowed to RPMT only
bsr2bsmt :: BoolStructR -> BoolStructR
bsr2bsmt (AA.Leaf (RPMT mt)                      ) = AA.Leaf (RPMT mt)
bsr2bsmt (AA.Leaf (RPParamText pt)               ) = AA.Leaf (RPMT $ pt2multiterm pt)
bsr2bsmt (AA.Leaf (RPConstraint  _mt1 _rpr mt2)  ) = AA.Leaf (RPMT mt2)
bsr2bsmt (AA.Leaf (RPBoolStructR _mt1 _rpr bsr2) ) = let output = bsr2bsmt bsr2
                                                     in -- trace ("bsr2bsmt handling a boolstructr, input = " <> show bsr2) $
                                                        -- trace ("bsr2bsmt handling a boolstructr, returning " <> show output) $
                                                        output
bsr2bsmt (AA.All lbl xs) = AA.All lbl (bsr2bsmt <$> xs)
bsr2bsmt (AA.Any lbl xs) = AA.Any lbl (bsr2bsmt <$> xs)
bsr2bsmt (AA.Not     x ) = AA.Not     (bsr2bsmt x)

