{-# LANGUAGE OverloadedStrings #-}

-- | see documentation at https://github.com/smucclaw/dsl/tree/tab-mustsing#interpretation-requirements
-- This runs after the parser and prepares for transpilation by organizing the ruleset, performing type checking and type inference, and so on.

module LS.Interpreter where

import LS.Types
import qualified AnyAll as AA
import Data.List.NonEmpty
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe

-- | a basic symbol table to track "variable names" and their associated types.

-- what's the difference between SymTab, ClsTab, and ScopeTabs?

-- ClsTab: things that are explicitly defined in a Type Definition (DEFINE ... HAS ...) end up in the ClsTab
-- and they qualify to be used as types on the RHS of a :: definition which could appear anywhere.

-- ScopeTabs: In the course of a program we will sometimes see ad-hoc variables used in GIVEN and elsewhere.
-- those end up in the ScopeTabs object returned by the `symbolTable` function.

-- SymTabs are a helper data structure used by ScopeTabs.
-- ClsTabs are keyed by class name.


type SymTab = Map.Map MultiTerm (Inferrable TypeSig) -- similar to TypedMulti, but with room for adding inferred types

newtype ClsTab = CT (Map.Map EntityType (Inferrable EntityType, ClsTab))  -- a class has attributes; those attributes live in a map keyed by classname.
            -- the fst part is the parent
            -- the snd part is the recursive HAS

-- | the explicitly annotated types from the L4 source text are recorded in the fst of Inferrable
--   the confirmed & inferred types after the type checker & inferrer has run, are recorded in the snd of Inferrable.
type Inferrable ts = (Maybe ts, [ts])

getSymType :: Inferrable ts -> Maybe ts
getSymType (Just x, _)    = Just x
getSymType (Nothing, x:_) = Just x
getSymType (Nothing, [])  = Nothing

-- | each scope maintains its own symbol table, plus global scope, which is represented by Rulename=[]
-- The keys to ScopeTabs are from ruleLabelName.
type ScopeTabs = Map.Map RuleName SymTab

-- | interpret the parsed rules and construct the symbol tables
symbolTable :: [Rule] -> ScopeTabs
symbolTable rs =
  Map.fromList
  [ (rname, symtable)
  | r   <- rs
  , hasGiven r
  , gPT <- maybeToList (given r)
  , let rname = ruleLabelName r
        symtable = Map.fromList (pt2inferrable <$> toList gPT)
  ]
  where
    pt2inferrable :: TypedMulti -> (MultiTerm, Inferrable TypeSig)
    pt2inferrable (mt, maybeTS) = (toList mt, (maybeTS, []))

hasGiven     Hornlike{} = True
hasGiven   Regulative{} = True
hasGiven     TypeDecl{} = True
hasGiven Constitutive{} = True
hasGiven             __ = False

getUnderlyingType :: TypeSig -> Either String EntityType
getUnderlyingType o@(SimpleType TOne      s1) = Right s1
getUnderlyingType   (SimpleType TOptional s1) = Left "type declaration cannot inherit from _optional_ superclass"
getUnderlyingType   (SimpleType TList0    s1) = Left "type declaration cannot inherit from _list_ superclass"
getUnderlyingType   (SimpleType TList1    s1) = Left "type declaration cannot inherit from _list_ superclass"
getUnderlyingType   (InlineEnum pt1       s1) = Left "type declaration cannot inherit from _enum_ superclass"

classHierarchy :: [Rule] -> ClsTab
classHierarchy rs =
  CT $ Map.fromList
  [ (thisclass, (superclass, attributes))
  | r@TypeDecl{} <- rs
  , let thisclass = mt2text (name r)
        superclass = do
          case super r of
            Nothing -> (Nothing, [])
            Just s  -> case getUnderlyingType s of
              Left err -> (Nothing, [])
              Right mt -> (Just mt, [])
        attributes = classHierarchy (has r)
  ]
  
clsParent :: ClsTab -> EntityType -> Maybe EntityType
-- do we want to support diamond inheritance? if so, the EntityType we return should be a [EntityType].
clsParent (CT clstab) subclass = do
  ((met, tss), st) <- Map.lookup subclass clstab
  superclass <- met
  return superclass

thisAttributes, extendedAttributes :: ClsTab -> EntityType -> Maybe ClsTab

-- | attributes defined in the type declaration for this class specifically
thisAttributes (CT clstab) subclass = do
  ((mts, tss), ct) <- Map.lookup subclass clstab
  return ct

extendedAttributes o@(CT clstab) subclass = do
  ((met, tss), CT ct) <- Map.lookup subclass clstab
  let (Just (CT eAttrs)) = case met of
                      Nothing -> Nothing
                      Just et -> extendedAttributes o et
  return $ CT $ ct <> eAttrs

