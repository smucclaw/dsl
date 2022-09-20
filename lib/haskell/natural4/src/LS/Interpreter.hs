{-# LANGUAGE OverloadedStrings #-}

-- | see documentation at https://github.com/smucclaw/dsl/tree/tab-mustsing#interpretation-requirements
-- This runs after the parser and prepares for transpilation by organizing the ruleset and providing helper functions used by multiple XPile backends.
-- in future, we may be so ambitious as to attempt some type checking and type inference, and so on, though that may be better left to corel4.

module LS.Interpreter where

import LS.Types
import LS.RelationalPredicates
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

-- | deprecated, use classGraph instead.
allCTkeys :: ClsTab -> [EntityType]
allCTkeys o@(CT ct) = getCTkeys o ++ [ T.replace " " "_" (childname <> "." <> gcname)
                                     | (childname, (_ts, childct)) <- Map.toList ct
                                     , gcname <- allCTkeys childct
                                     ]

-- | attributes of a given class. Enums are dealt with separately.
getCTkeys :: ClsTab -> [EntityType]
getCTkeys (CT ct) = Map.keys ct


type MyClassName = EntityType
-- | class names, topologically sorted by inheritance to eliminate forward references in "extends" relationships
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


-- multiple rules with the same head should get &&'ed together and jammed into a single big rule

getAndOrTree :: Interpreted -> Rule -> Maybe (AA.ItemMaybeLabel T.Text) -- Vue wants AA.Item T.Text
getAndOrTree _l4i r@Regulative{who=whoMBSR, cond=condMBSR} =
  (fmap (((bsp2text (subj r) <> " ") <>) . rp2text) <$> whoMBSR)  <> -- WHO is relative to the subject
  (fmap                                    rp2text  <$> condMBSR)    -- the condition is absolute
  
getAndOrTree  l4i r@Hornlike{} = fmap extractRPMT2Text <$>
                                 mconcat (-- traceShowId $
  bsmtOfClauses $
  r { clauses = expandClauses l4i (clauses r) } )
  where
    defaultElem :: a -> [a] -> [a]
    defaultElem dflt []  = [ dflt ]
    defaultElem _    lst = lst

getAndOrTree l4i _r@(RuleAlias rn) = do
  r' <- getRuleByName (origrules l4i) rn
  getAndOrTree l4i r'

getAndOrTree _l4i r = -- trace ("ERROR: getAndOrTree called invalidly against rule " <> show r) $
                      Nothing

-- convert clauses to a boolStruct MT
bsmtOfClauses :: Rule -> [Maybe (AA.ItemMaybeLabel RelationalPredicate)]
bsmtOfClauses r = [ mhead <> mbody
                  | c <- clauses r
                  , (hhead, hbody)  <- [(hHead c, hBody c)]
                  , let (_bodyEx, bodyNonEx) = partitionExistentials c
                  , let mhead, mbody :: Maybe (AA.ItemMaybeLabel RelationalPredicate)
                        mhead = case hhead of
                                  RPBoolStructR _mt1 _rprel1 bsr1 -> -- trace "bsmtOfClauses: returning bsr part of head's RPBoolStructRJust"
                                                                     Just (bsr2bsmt bsr1)
                                  _                               -> -- trace ("bsmtOfClauses: returning nothing for " <> show hhead)
                                                                     Nothing
                        mbody = case hbody of
                                  Nothing -> Nothing
                                  _       -> 
                                    let output = bsr2bsmt bodyNonEx in
                                      -- trace ("bsmtOfClauses: got output " <> show output)
                                      Just output
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
expandRP l4i depth (RPMT                   mt2) = -- trace ("expandRP: " ++ replicate depth '|' ++ "RPMT " ++ show mt2 ++ ": calling expandMT on " ++ show mt2) $
                                                  expandMT  l4i (depth + 1) mt2
expandRP l4i depth (RPConstraint  mt1 RPis mt2) = -- trace ("expandRP: " ++ replicate depth '|' ++ "RPConstraint " ++ show mt1 ++ " is " ++ show mt2 ++ ": calling expandMT on " ++ show mt2) $
                                                  expandMT  l4i (depth + 1) mt2
expandRP l4i depth (RPBoolStructR mt1 RPis bsr) = -- trace ("expandRP: " ++ replicate depth '|' ++ "RPBoolStructR " ++ show mt1 ++ " is BSR: calling expandBSR on " ++ show bsr) $
                                                  RPBoolStructR mt1 RPis (expandBSR l4i (depth + 1) bsr)
expandRP l4i depth x                            = -- trace ("expandRP: " ++ replicate depth '|' ++ "returning unchanged " ++ show x) $
                                                  x

expandMT :: Interpreted -> Int -> MultiTerm -> RelationalPredicate
expandMT l4i depth mt0 =
  let expanded = listToMaybe
                 [ out
                 | (scopename,symtab) <- Map.toList (scopetable l4i)
                 , (mytype, cs) <- maybeToList $
                                   -- trace ("expandMT: " ++ replicate depth '|' ++ "considering scope " ++ show scopename ++ ", looking up " ++ show mt0 ++ " in symtab") $
                                   Map.lookup mt0 symtab
                 , c <- -- trace ("expandMT: " ++ replicate depth '|' ++ "working through clauses " ++ show cs)
                        cs
                 , let outs = case hHead c of
                                RPMT          mt           -> [          ] -- no change
                                RPParamText   pt           -> [          ] -- no change
                                RPConstraint  mt RPis  rhs -> [ RPMT rhs ] -- substitute with rhs
                                RPConstraint  mt rprel rhs -> [ hHead c  ] -- maintain inequality
                                RPBoolStructR mt RPis  bsr -> [ -- trace ("expandMT: " ++ replicate depth '|' ++ " big return: BSR " ++ show mt ++ " RPis expandBSR") $
                                                                RPBoolStructR mt RPis (expandBSR l4i (depth + 1) bsr) ]
                 , out <- -- trace("expandMT: " ++ replicate depth '|' ++ "will return outs " ++ show outs)
                          outs
                 ]
      toreturn = fromMaybe (RPMT mt0) $ expanded
  in -- trace ("expandMT: scopetable toplevel is " ++ TL.unpack (pShow $ scopetable l4i)) $
     -- trace ("expandMT: " ++ replicate depth '|' ++ "expanded = " ++ show expanded) $
     -- trace ("expandMT: " ++ replicate depth '|' ++ "will return " ++ show toreturn) $
     toreturn


expandBSR, expandBSR' :: Interpreted -> Int -> BoolStructR -> BoolStructR
expandBSR  l4i depth x = let y = expandBSR' l4i depth x in -- trace ("expandBSR:" ++ replicate depth '|' ++ "given " ++ show x ++ ", returning " ++ show y)
                                                           y
expandBSR' l4i depth (AA.Leaf rp)    =
  case expandRP l4i (depth + 1) rp of
    RPBoolStructR mt1 RPis bsr -> -- trace ("expandBSR:" ++ replicate depth '|' ++ " bsr track: " ++ show bsr)
                                  bsr
    o                          -> -- trace ("expandBSR:" ++ replicate depth '|' ++ " o track: Leaf " ++ show o) $
                                  AA.Leaf o
expandBSR' l4i depth (AA.Not item)   = AA.Not     (expandBSR l4i (depth + 1) item)
expandBSR' l4i depth (AA.All lbl xs) = AA.All lbl (expandBSR l4i (depth + 1) <$> xs)
expandBSR' l4i depth (AA.Any lbl xs) = AA.Any lbl (expandBSR l4i (depth + 1) <$> xs)

expandBody :: Interpreted -> Maybe BoolStructR -> Maybe BoolStructR
expandBody l4i = id

onlyTheItems :: Interpreted -> AA.ItemMaybeLabel T.Text
onlyTheItems l4i =
  let myitem = AA.All Nothing (catMaybes $ getAndOrTree l4i <$> origrules l4i)
      simplified = AA.simplifyItem myitem
  in -- trace ("onlyTheItems: before calling simplifyItem: " <> (TL.unpack $ pShowNoColor myitem)) $
     -- trace ("onlyTheItems: after  calling simplifyItem: " <> (TL.unpack $ pShowNoColor simplified)) $
     simplified

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

