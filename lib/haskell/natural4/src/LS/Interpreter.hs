{-# LANGUAGE OverloadedStrings #-}

-- | see documentation at https://github.com/smucclaw/dsl/tree/tab-mustsing#interpretation-requirements
-- This runs after the parser and prepares for transpilation by organizing the ruleset, performing type checking and type inference, and so on.

module LS.Interpreter where

import LS.Types
import qualified AnyAll as AA
import Data.List.NonEmpty
import qualified Data.Text as T
import qualified Data.Map as Map
import Debug.Trace
import Data.Maybe
import Data.Graph.Inductive
import Data.Tuple (swap)
import Data.List (find)

-- | interpret the parsed rules and construct the symbol tables
symbolTable :: [Rule] -> ScopeTabs
symbolTable rs =
  Map.fromListWith (<>) $ fromGivens <> fromDefines
  where
    fromGivens :: [(RuleName, SymTab)]
    fromGivens = [ (rname, symtable)
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
    fromDefines = [ (scopename, symtable)
                  | r@Hornlike{keyword=Define}   <- rs
                  , let scopename = maybe ["GLOBAL"] (\x -> [rl2text x]) (getRlabel r)
                        symtable = Map.fromList [(name r, ((super r,[]), clauses r))]
                  ]

hasGiven :: Rule -> Bool
hasGiven     Hornlike{} = True
hasGiven   Regulative{} = True
hasGiven     TypeDecl{} = True
hasGiven Constitutive{} = True
hasGiven             __ = False

l4interpret :: [Rule] -> Interpreted
l4interpret rs = L4I { classtable = classHierarchy rs
                     , scopetable = symbolTable    rs }

classHierarchy :: [Rule] -> ClsTab
classHierarchy rs =
  CT $ Map.fromList
  [ (thisclass, (classtype, attributes))
  | r@TypeDecl{} <- rs
  , let thisclass = mt2text (name r)
        classtype = (super r, [])
        attributes = classHierarchy (has r)
  ]
  
getCTkeys :: ClsTab -> [EntityType]
getCTkeys (CT ct) = Map.keys ct


type MyClassName = EntityType
-- | class names, topologically sorted to eliminate forward references
-- note: this code will probably fail silently on any input that isn't as expected
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
  

-- multiple rules with the same head should get &&'ed together and jammed into a single big rule

-- some helper functions that are used by multiple XPile modules
getAndOrTree :: RuleSet -> Rule -> AA.ItemMaybeLabel [T.Text]
getAndOrTree _rs r@Regulative{}  = AA.Leaf ("to expand to the conditions in the regulative rule " : ruleLabelName r)
getAndOrTree _rs r@Hornlike{}    = trace ("getAndOrTree on Hornlike rule " <> ruleNameStr r <> " starting") $
                                   foldr1 (<>) $ catMaybes $ bsmtOfClauses r

getAndOrTree rs r@(RuleAlias rn) = case getRuleByName rs rn of
                                     Nothing -> AA.Leaf ("ERROR: unable to expand rule alias " : rn)
                                     Just r' -> getAndOrTree rs r'
getAndOrTree _rs r = trace ("ERROR: getAndOrTree called invalidly against rule " <> ruleNameStr r) $
                     AA.Leaf ("ERROR: can't call getAndOrTree against" : ruleLabelName r)

bsmtOfClauses r = [ mhead <> mbody
                  | c <- clauses r
                  , (hhead, hbody)  <- [(hHead c, hBody c)]
                  , let mhead, mbody :: Maybe (AA.ItemMaybeLabel MultiTerm)
                        mhead = case hhead of
                                  RPBoolStructR _mt1 _rprel1 bsr1 -> trace "returning bsr part of head's RPBoolStructRJust" (Just (bsr2bsmt bsr1))
                                  _                               -> trace "returning nothing" Nothing
                        mbody = bsr2bsmt <$> hbody
                  ]
                                   
ruleNameStr :: Rule -> String
ruleNameStr r = T.unpack (mt2text (ruleLabelName r))
                          
type RuleSet = [Rule]

getRuleByName :: RuleSet -> RuleName -> Maybe Rule
getRuleByName rs rn = find (\r -> ruleName r == rn) rs

getRuleByLabel :: RuleSet -> T.Text -> Maybe Rule
getRuleByLabel rs t = find (\r -> (rl2text <$> rLabelR r) == Just t) rs

-- we don't have a type alias for BoolStructMT, but if we did, it would appear here
bsr2bsmt :: BoolStructR -> AA.ItemMaybeLabel MultiTerm
bsr2bsmt (AA.Leaf (RPMT mt)                      ) = AA.Leaf mt
bsr2bsmt (AA.Leaf (RPParamText pt)               ) = AA.Leaf (pt2multiterm pt)
bsr2bsmt (AA.Leaf (RPConstraint  _mt1 _rpr mt2)  ) = AA.Leaf mt2 -- by right we should pay closer attention to the rprel
bsr2bsmt (AA.Leaf (RPBoolStructR _mt1 _rpr bsr2) ) = bsr2bsmt bsr2
bsr2bsmt (AA.All lbl xs) = AA.All lbl (bsr2bsmt <$> xs)
bsr2bsmt (AA.Any lbl xs) = AA.All lbl (bsr2bsmt <$> xs)
bsr2bsmt (AA.Not     x ) = AA.Not     (bsr2bsmt x)
    
