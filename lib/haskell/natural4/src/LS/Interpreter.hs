{-# LANGUAGE OverloadedStrings #-}

-- | see documentation at https://github.com/smucclaw/dsl/tree/tab-mustsing#interpretation-requirements
-- This runs after the parser and prepares for transpilation by organizing the ruleset, performing type checking and type inference, and so on.

module LS.Interpreter where

import LS.Types
import qualified AnyAll as AA
import Data.List.NonEmpty
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Map as Map
import Text.Pretty.Simple
import Debug.Trace
import Data.Maybe
import Data.Graph.Inductive
import Data.Tuple (swap)
import Data.List (find)

-- | interpret the parsed rules and construct the symbol tables
symbolTable :: [Rule] -> ScopeTabs
symbolTable rs =
  Map.fromListWith (<>) $ fromGivens <> fromDefines <> fromDecides -- <> trace ("all rules = " ++ TL.unpack (pShow rs)) []
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

hasGiven :: Rule -> Bool
hasGiven     Hornlike{} = True
hasGiven   Regulative{} = True
hasGiven     TypeDecl{} = True
hasGiven Constitutive{} = True
hasGiven             __ = False

-- | it's beginning to look like we need to break out the Rule Types into different types not just constructors
hasClauses :: Rule -> Bool
hasClauses     Hornlike{} = True
hasClauses             __ = False

l4interpret :: [Rule] -> Interpreted
l4interpret rs = L4I { classtable = classHierarchy rs
                     , scopetable = symbolTable    rs
                     , origrules  = id             rs
                     }

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
getAndOrTree :: Interpreted -> Rule -> AA.ItemMaybeLabel T.Text -- Vue wants AA.Item T.Text
getAndOrTree _l4i r@Regulative{}  = AA.Leaf ("[TODO]: to expand to the cond and the who from the regulative rule " <> T.unwords (ruleLabelName r))
getAndOrTree  l4i r@Hornlike{}    = trace ("[TODO]: getAndOrTree on Hornlike rule \"" <> ruleNameStr r <> "\"") $
                                    extractRPMT2Text <$> foldr1 (<>) (defaultElem (AA.Leaf (RPMT ["BASE CASE TRUE"]))
                                                                      $ catMaybes
                                                                      $ traceShowId
                                                                      $ bsmtOfClauses
                                                                      $ r { clauses = expandClauses l4i (clauses r) } )
  where
    defaultElem :: a -> [a] -> [a]
    defaultElem dflt []  = [ dflt ]
    defaultElem _    lst = lst
    
getAndOrTree l4i r@(RuleAlias rn) = case getRuleByName (origrules l4i) rn of
                                     Nothing -> AA.Leaf ("ERROR: unable to expand rule alias " <> T.unwords rn)
                                     Just r' -> getAndOrTree l4i r'
getAndOrTree _l4i r = trace ("ERROR: getAndOrTree called invalidly against rule " <> ruleNameStr r) $
                      AA.Leaf ("ERROR: can't call getAndOrTree against" <> T.unwords (ruleLabelName r))

-- convert clauses to a boolStruct MT
bsmtOfClauses r = [ mhead <> mbody
                  | c <- clauses r
                  , (hhead, hbody)  <- [(hHead c, hBody c)]
                  , let mhead, mbody :: Maybe (AA.ItemMaybeLabel RelationalPredicate)
                        mhead = case hhead of
                                  RPBoolStructR _mt1 _rprel1 bsr1 -> trace "bsmtOfClauses: returning bsr part of head's RPBoolStructRJust" (Just (bsr2bsmt bsr1))
                                  _                               -> trace ("bsmtOfClauses: returning nothing for " <> show hhead) Nothing
                        mbody = let output = bsr2bsmt <$> hbody in trace ("bsmtOfClauses: got output " <> show output) $ output
                  ]

expandClauses :: Interpreted -> [HornClause2] -> [HornClause2]
expandClauses l4i hcs =
  [ newhc
  | oldhc <- hcs
  , let newhc = HC2 { hHead =                expandRP l4i 1   $  hHead oldhc
                    , hBody = unleaf . fmap (expandRP l4i 1) <$> hBody oldhc }
  ]

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

-- | is a given multiterm defined as a head somewhere in the ruleset?
-- later, we shall have to limit the scope of such a definition based on UPON / WHEN / GIVEN preconditions.
-- for now we just scan across the entire ruleset to see if it matches.
expandRP :: Interpreted -> Int -> RelationalPredicate -> RelationalPredicate
expandRP l4i depth (RPMT                   mt2) = trace ("expandRP: " ++ replicate depth '|' ++ "RPMT " ++ show mt2 ++ ": calling expandMT on " ++ show mt2) $
                                                  expandMT  l4i (depth + 1) mt2
expandRP l4i depth (RPConstraint  mt1 RPis mt2) = trace ("expandRP: " ++ replicate depth '|' ++ "RPConstraint " ++ show mt1 ++ " is " ++ show mt2 ++ ": calling expandMT on " ++ show mt2) $
                                                  expandMT  l4i (depth + 1) mt2
expandRP l4i depth (RPBoolStructR mt1 RPis bsr) = trace ("expandRP: " ++ replicate depth '|' ++ "RPBoolStructR " ++ show mt1 ++ " is BSR: calling expandBSR on " ++ show bsr) $
                                                  RPBoolStructR mt1 RPis (expandBSR l4i (depth + 1) bsr)
expandRP l4i depth x                            = trace ("expandRP: " ++ replicate depth '|' ++ "returning unchanged " ++ show x) $
                                                  x

expandMT :: Interpreted -> Int -> MultiTerm -> RelationalPredicate
expandMT l4i depth mt0 =
  let expanded = listToMaybe
                 [ out
                 | (scopename,symtab) <- Map.toList (scopetable l4i)
                 , (mytype, cs) <- maybeToList $
                                   trace ("expandMT: " ++ replicate depth '|' ++ "considering scope " ++ show scopename ++ ", looking up " ++ show mt0 ++ " in symtab") $
                                   Map.lookup mt0 symtab
                 , c <- trace ("expandMT: " ++ replicate depth '|' ++ "working through clauses " ++ show cs) $ cs
                 , let outs = case hHead c of
                                RPMT          mt           -> [          ] -- no change
                                RPParamText   pt           -> [          ] -- no change
                                RPConstraint  mt RPis  rhs -> [ RPMT rhs ] -- substitute with rhs
                                RPConstraint  mt rprel rhs -> [ hHead c  ] -- maintain inequality
                                RPBoolStructR mt RPis  bsr -> [ -- trace ("expandMT: " ++ replicate depth '|' ++ " big return: BSR " ++ show mt ++ " RPis expandBSR") $
                                                                RPBoolStructR mt RPis (expandBSR l4i (depth + 1) bsr) ]
                 , out <- trace("expandMT: " ++ replicate depth '|' ++ "will return outs " ++ show outs) outs
                 ]
      toreturn = fromMaybe (RPMT mt0) $ expanded
  in -- trace ("expandMT: scopetable toplevel is " ++ TL.unpack (pShow $ scopetable l4i)) $
     trace ("expandMT: " ++ replicate depth '|' ++ "expanded = " ++ show expanded) $
     trace ("expandMT: " ++ replicate depth '|' ++ "will return " ++ show toreturn) $
     toreturn


expandBSR, expandBSR' :: Interpreted -> Int -> BoolStructR -> BoolStructR
expandBSR  l4i depth x = let y = expandBSR' l4i depth x in trace ("expandBSR:" ++ replicate depth '|' ++ "given " ++ show x ++ ", returning " ++ show y) y
expandBSR' l4i depth (AA.Leaf rp)    =
  case expandRP l4i (depth + 1) rp of
    RPBoolStructR mt1 RPis bsr -> trace ("expandBSR:" ++ replicate depth '|' ++ " bsr track: " ++ show bsr) $
                                  bsr
    o                          -> trace ("expandBSR:" ++ replicate depth '|' ++ " o track: Leaf " ++ show o) $
                                  AA.Leaf o
expandBSR' l4i depth (AA.Not item)   = AA.Not     (expandBSR l4i (depth + 1) item)
expandBSR' l4i depth (AA.All lbl xs) = AA.All lbl (expandBSR l4i (depth + 1) <$> xs)
expandBSR' l4i depth (AA.Any lbl xs) = AA.Any lbl (expandBSR l4i (depth + 1) <$> xs)

expandBody :: Interpreted -> Maybe BoolStructR -> Maybe BoolStructR
expandBody l4i = id

onlyTheItems :: Interpreted -> AA.ItemMaybeLabel T.Text
onlyTheItems l4i = AA.All (Just (AA.Pre "all of the following:")) (getAndOrTree l4i <$> origrules l4i)

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

-- where every RelationalPredicate in the boolstruct is narrowed to RPMT only
bsr2bsmt :: BoolStructR -> BoolStructR
bsr2bsmt (AA.Leaf (RPMT mt)                      ) = AA.Leaf (RPMT mt)
bsr2bsmt (AA.Leaf (RPParamText pt)               ) = AA.Leaf (RPMT $ pt2multiterm pt)
bsr2bsmt (AA.Leaf (RPConstraint  _mt1 _rpr mt2)  ) = AA.Leaf (RPMT mt2)
bsr2bsmt (AA.Leaf (RPBoolStructR _mt1 _rpr bsr2) ) = let output = bsr2bsmt bsr2
                                                     in trace ("bsr2bsmt handling a boolstructr, input = " <> show bsr2) $
                                                        trace ("bsr2bsmt handling a boolstructr, returning " <> show output) $
                                                        output
bsr2bsmt (AA.All lbl xs) = AA.All lbl (bsr2bsmt <$> xs)
bsr2bsmt (AA.Any lbl xs) = AA.Any lbl (bsr2bsmt <$> xs)
bsr2bsmt (AA.Not     x ) = AA.Not     (bsr2bsmt x)
    
