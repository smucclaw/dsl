{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|

The Interpreter runs after the Parser. It prepares for transpilation by organizing the ruleset and providing helper functions used by multiple XPile backends.

In future, we may be so ambitious as to attempt some type checking and type inference, and so on, here, though that may be better left to corel4 or other backends.

See also documentation at https://github.com/smucclaw/dsl/tree/tab-mustsing#interpretation-requirements

Typical usage: an XPile module is handed the output of `l4interpret`, and  makes use of the values returned in the `l4i` object.

-}

module LS.Interpreter where

import LS.Types
import LS.Rule
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
import Control.Monad (guard)
import Data.Maybe
import Data.Graph.Inductive
import Data.Tuple (swap)
import Data.List (find)
import qualified Data.List as DL
import Data.Bifunctor (first)
import Data.Map ((!))
import Data.Tree
import Control.Applicative ((<|>))

-- | interpret the parsed rules based on some configuration options.
-- This is a canonical intermediate representation used by downstream
-- functions. It is typically run once, in the high-level caller, and
-- handed to each transpiler for use, as an `l4i` argument.
--

l4interpret :: InterpreterOptions -> [Rule] -> Interpreted
l4interpret iopts rs =
  let ct = classHierarchy rs
      st = symbolTable    iopts rs
  in
    L4I { classtable = ct
        , scopetable = st
        , origrules  = rs
        }

-- | the fully expanded, exposed, decision roots of all rules in the ruleset,
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
                           exposedRoots l4i      -- EXPOSED
     , not $ null grpval
     , expanded <- expandBSR l4i 1 <$> maybeToList (getBSR (DL.head uniqrs))
     ]

-- | Talk a little bit about what we've interpreted.
-- The output of this function gets saved to the workdir's @org/@ directory
-- and can be viewed inside the @LATEST.org@ output file.
-- If you are working on the Interpreter and want to see what it is thinking,
-- this is a good place to add "printf debugging".
--
-- When you view the @LATEST.org@ output file, org-mode is recommended.
-- This comes naturally in Emacs. In VS Code you will need to install plugins.

musings :: Interpreted -> [Rule] -> Doc ann
musings l4i rs =
  let cg = classGraph (classtable l4i) []
      expandedRules = DL.nub $ concatMap (expandRule rs) rs
      decisionGraph = ruleDecisionGraph l4i rs
  in vvsep [ "* musings"
           , "** Class Hierarchy"
           , vvsep [ vvsep [ "*** Class:" <+> pretty cname <>
                             if null (Prelude.tail cname) then emptyDoc
                             else hsep (" belongs to" : (pretty <$> Prelude.tail cname))
                           , if null cchild
                             then emptyDoc
                             else "**** extends" <+> maybe "" viaShow (fst . fst $ cchild) <+> "with new attributes"
                                  </> srchs (snd cchild)
                           , "**** deets" </> srchs cname
                           ]
                   | (cname, cchild) <- cg ]
           , "** The entire classgraph"
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
           , "rules which are not just RuleAlises, and which are not relied on by any other rule"
           , srchs (ruleLabelName <$> exposedRoots l4i)

           , "*** Nubbed, Exposed, Decision Roots"
           , "maybe some of the decision roots are identical and don't need to be repeated; so we nub them"
           , vvsep [ "**** Decision Root" <+> viaShow (n :: Int)
                     </> vsep [ "-" <+> pretty (ruleLabelName r) | r <- uniqrs ]
                     </> "***** grpval" </> srchs grpval
                     </> "***** head uniqrs" </> srchs (DL.head uniqrs)
                     </> "***** getAndOrTree (head uniqrs)" </> srchs (getAndOrTree l4i 1 $ DL.head uniqrs)
                     </> "***** getBSR [head uniqrs]" </> srchs (mapMaybe getBSR [DL.head uniqrs])
                     </> "***** expandBSR" </> srchs (expandBSR l4i 1 <$> mapMaybe getBSR uniqrs)
                     </> vvsep [ "****** uniq rules" </> srchs r
                                 </> "******* givens" </> srchs (given r)
                                 </> vvsep [ "******* horn clause" </> srchs c
                                             </> "******** partitionExistentials"
                                             </> srchs (partitionExistentials c)
                                           | c <- clauses r ]
                               | r <- uniqrs ]

                   | ((grpval, uniqrs),n) <- Prelude.zip (groupedByAOTree l4i $ -- NUBBED
                                                          exposedRoots l4i      -- EXPOSED
                                                         ) [1..]
                   , not $ null uniqrs
                   ]

           , "** qaHornsR" , vvsep [ "***" <+> viaShow (concat names) </> srchs boolstruct | (names, boolstruct) <- qaHornsR l4i ]
           , "** qaHornsT" , vvsep [ "***" <+> viaShow (concat names) </> srchs boolstruct | (names, boolstruct) <- qaHornsT l4i ]
           , "** expandedRules"
           , if expandedRules == DL.nub rs
             then "(ahem, they're actually the same as unexpanded, not showing)"
             else vvsep [ "***" <+> hsep (pretty <$> ruleLabelName r) </> srchs r | r <- expandedRules ]
           , "** getAndOrTrees, direct"
           , vvsep [ "***" <+> hsep (pretty <$> ruleLabelName r) </> srchs (getAndOrTree l4i 1 r) | r <- rs ]
           , vvsep [ "** Things that are RuleAliases"
                   , vsep [ "-" <+> pretty rlname
                          | r <- rs -- this is AccidentallyQuadratic in a pathological case.
                          , let rlname = ruleLabelName r
                          , isRuleAlias l4i rlname ]
                   ]
           , vvsep [ "** default markings"
                   , "terms annotated with TYPICALLY so we tell XPile targets what their default values are"
                   , srchs (getMarkings l4i)
                   ]
           , "** symbol tables (~scopetable l4i~)"
           , vvsep [ "***" <+> pretty lhs </> srchs rhs | (lhs, rhs) <- Map.toList (scopetable l4i) ]

           , "** class tables (~classtable l4i~)" </> srchs (classtable l4i)
           , vvsep [ "***" <+> pretty lhs </> srchs rhs | (lhs, rhs) <- Map.toList (unCT $ classtable l4i) ]

           , "** The original rules (~origrules l4i~)"
           , vvsep [ "***" <+> pretty (ruleLabelName r) </> srchs r | r <- rs ]
           ]
  where
    srchs :: (Show a) => a -> Doc ann
    srchs = src "haskell" . pretty . pShowNoColor
    src lang x = vsep [ "#+begin_src" <+> lang, x, "#+end_src" ]
    example  x = vsep [ "#+begin_example", x, "#+end_example" ]

-- | interpret the parsed rules and construct the symbol tables
symbolTable :: InterpreterOptions -> [Rule] -> ScopeTabs
symbolTable _iopts rs =
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

-- | Sometimes multiple rules will have the same decision content: X depends on Z; Y also depends on Z.
-- For the sake of the UI, we group such rules together and return basically a Map, of AndOrTree (Z) to one or more rules (X and Y).
groupedByAOTree :: Interpreted -> [Rule] -> [(Maybe BoolStructT, [Rule])]
groupedByAOTree l4i rs =
  Map.toList $ Map.fromListWith (++) $
  (\r -> (getAndOrTree l4i 1 r, [r])) <$> rs


-- | The top-level decision roots which we expose to the web UI, and also visualize with SVGLadder.
-- We exclude rules which are the target of a GOTO RuleAlias, because those are just infrastructure.
-- The SVG outputter likes to exclude things that have only a single element and are therefore visually uninteresting.
-- We want the SVG Xpiler to reuse this code as authoritative.

exposedRoots :: Interpreted -> [Rule]
exposedRoots l4i =
  let rs = origrules l4i
      decisionGraph = ruleDecisionGraph l4i rs
      decisionroots = decisionRoots decisionGraph
  in [ r | r <- decisionroots, not $ isRuleAlias l4i (ruleLabelName r) ]

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


-- | which decision rules depend on which other decision rules?
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
relPredRefs _l4i rs ridmap r =
  let headElements :: Map.Map MultiTerm Rule -- does this get recomputed each time or cached?
      -- given a term, see which rule defines it
      headElements = Map.fromList $
                     [ (headName,r')
                     | r' <- rs
                     , headName <- getDecisionHeads r'
                     ]
      -- given a rule, see which terms it relies on
      bodyElements = concatMap rp2bodytexts (concatMap AA.extractLeaves (getBSR r))
  -- given a rule R, for each term relied on by rule R, identify all the subsidiary rules which define those terms.
  in [ (rid, targetRuleId', ())
     | bElem <- bodyElements
     , let targetRule = Map.lookup bElem headElements
     , isJust targetRule
     , let targetRule' = fromJust targetRule -- safe due to above isJust test
     , let targetRuleId = Map.lookup targetRule' ridmap
     , isJust targetRuleId
     , let targetRuleId' = fromJust targetRuleId -- safe due to above isJust test
           rid = ridmap ! r
     ]


-- | All the rules which have no indegrees, as far as decisioning goes.
-- This is based solely on the rule graph.
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
bsmtOfClauses l4i depth r =
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
expandRP l4i depth (RPMT                   mt2)   = expandMT  l4i (depth + 1) mt2
expandRP l4i depth (RPConstraint  mt1 RPis mt2)   = expandMT  l4i (depth + 1) (mt1 ++ MTT (rel2txt RPis) : mt2)
expandRP l4i depth (RPBoolStructR mt1 RPis bsr)   = RPBoolStructR mt1 RPis (expandBSR' l4i (depth + 1) bsr)
expandRP _l4i _depth x                            = x

-- | Search the scopetable's symbol tables for a given multiterm. Expand its clauses, and return the expanded.
expandMT :: Interpreted -> Int -> MultiTerm -> RelationalPredicate
expandMT l4i depth mt0 =
  let expanded = listToMaybe
                 [ outrp
                 | (_scopename, symtab) <- Map.toList (scopetable l4i)
                 , (_mytype, cs) <- maybeToList $ Map.lookup mt0 symtab
                 , c <- cs
                 , let outs = expandClause l4i depth c
                 , outrp <- outs
                 ]
  in fromMaybe (RPMT mt0) expanded

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
expandBSR  l4i depth x = trace (show x) $ AA.nnf $ expandBSR' l4i depth x

expandBSR' :: Interpreted -> Int -> BoolStructR -> BoolStructR
expandBSR' l4i depth (AA.Leaf rp)  =
  case expandRP l4i (depth + 1) rp of
    RPBoolStructR _mt1 RPis bsr -> bsr
    o                           -> AA.mkLeaf o
expandBSR' l4i depth (AA.Not item)   = AA.mkNot     (expandBSR' l4i (depth + 1) item)
expandBSR' l4i depth (AA.All lbl xs) = AA.mkAll lbl (expandBSR' l4i (depth + 1) <$> xs)
expandBSR' l4i depth (AA.Any lbl xs) = AA.mkAny lbl (expandBSR' l4i (depth + 1) <$> xs)

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


-- | used for purescript output -- this is the toplevel function called by Main
onlyTheItems :: Interpreted -> BoolStructT
onlyTheItems l4i =
  let myitem = AA.mkAll Nothing (catMaybes $ getAndOrTree l4i 1 <$> origrules l4i)
      simplified = AA.simplifyBoolStruct myitem
  in simplified

onlyItemNamed :: Interpreted -> [Rule] -> [RuleName] -> BoolStructT
onlyItemNamed l4i rs wanteds =
  let ibr = itemsByRule l4i rs
      found = DL.filter (\(rn, _simp) -> rn `elem` wanteds) ibr
  in
    if null found
    then AA.mkLeaf $ T.pack ("L4 Interpreter: unable to isolate rule named " ++ show wanteds)
    else snd $ DL.head found

-- | let's hazard a guess that the item with the mostest is the thing we should put in front of the user.
biggestItem :: Interpreted -> [Rule] -> Maybe BoolStructT
biggestItem l4i rs = do
  let ibr = itemsByRule l4i rs
      flattened = (\(x,y) -> (x, AA.extractLeaves y)) <$> ibr
      sorted = DL.reverse $ DL.sortOn (DL.length . snd) flattened
  guard (not $ null sorted)
  return ((Map.fromList ibr) ! (fst $ DL.head sorted))

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
extractRPMT2Text :: RelationalPredicate -> T.Text
extractRPMT2Text (RPMT ts) = mt2text ts
extractRPMT2Text _         = error "extractRPMT2Text: expecting RPMT only, other constructors not supported."

ruleNameStr :: Rule -> String
ruleNameStr r = T.unpack (mt2text (ruleLabelName r))

type RuleSet = [Rule]

getRuleByName :: RuleSet -> RuleName -> Maybe Rule
getRuleByName rs rn = find (\r -> ruleName r == rn) rs

getRuleByLabel :: RuleSet -> T.Text -> Maybe Rule
getRuleByLabel rs t = find (\r -> (rl2text <$> getRlabel r) == Just t) rs

getRuleByLabelName :: RuleSet -> T.Text -> Maybe Rule
getRuleByLabelName rs t = find (\r -> (rl2text <$> getRlabel r) == Just t
                                      ||
                                      ruleName r == [MTT t]
                               ) rs

-- where every RelationalPredicate in the boolstruct is narrowed to RPMT only
bsr2bsmt :: BoolStructR -> BoolStructR
bsr2bsmt (AA.Leaf (RPMT mt)                      ) = AA.mkLeaf (RPMT mt)
bsr2bsmt (AA.Leaf (RPParamText pt)               ) = AA.mkLeaf (RPMT $ pt2multiterm pt)
bsr2bsmt (AA.Leaf (RPConstraint   mt1  rpr mt2)  ) = AA.mkLeaf (RPMT (mt1 ++ MTT (rel2txt rpr) : mt2))
bsr2bsmt (AA.Leaf (RPBoolStructR  mt1  rpr bsr2) ) = let output = (\(RPMT rpmt) -> RPMT (mt1 ++ MTT (rel2txt rpr) : rpmt)) <$> bsr2bsmt bsr2
                                                     in -- trace ("bsr2bsmt handling a boolstructr, input = " <> show bsr2) $
                                                        -- trace ("bsr2bsmt handling a boolstructr, returning " <> show output) $
                                                        output
bsr2bsmt (AA.Leaf (RPnary     _rprel rp) )         = AA.mkLeaf rp
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
    rhsval [MTN rhs] = if rhs == 0 then Just False else Just True
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


