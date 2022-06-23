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

type SymTab = Map.Map MultiTerm (Inferrable TypeSig) -- similar to TypedMulti, but with room for adding inferred types

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
  , gPT <- maybeToList (given r)
  , let rname = ruleLabelName r
        symtable = Map.fromList (pt2inferrable <$> toList gPT)
  ]
  where
    pt2inferrable :: TypedMulti -> (MultiTerm, Inferrable TypeSig)
    pt2inferrable (mt, maybeTS) = (toList mt, (maybeTS, []))
