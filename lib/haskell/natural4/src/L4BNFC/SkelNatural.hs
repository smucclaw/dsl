-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module SkelNatural where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsNatural

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transPIdent :: AbsNatural.PIdent -> Result
transPIdent x = case x of
  AbsNatural.PIdent string -> failure x

transPInteger :: AbsNatural.PInteger -> Result
transPInteger x = case x of
  AbsNatural.PInteger string -> failure x

transIfPart :: AbsNatural.IfPart -> Result
transIfPart x = case x of
  AbsNatural.IfPart bsrany -> failure x

transBoolStructR :: AbsNatural.BoolStructR -> Result
transBoolStructR x = case x of
  AbsNatural.BSRAny bsranys boolstructr -> failure x
  AbsNatural.BSRLeaf bsrleaf -> failure x
  AbsNatural.BSRAll bsralls boolstructr -> failure x

transBSRLeaf :: AbsNatural.BSRLeaf -> Result
transBSRLeaf x = case x of
  AbsNatural.LeafNoLabel relationalpredicate -> failure x
  AbsNatural.LeafPostLabel bsrleaf multiterm -> failure x
  AbsNatural.LeafPreLabel multiterm boolstructr -> failure x

transBSRAny :: AbsNatural.BSRAny -> Result
transBSRAny x = case x of
  AbsNatural.BSRAnyList boolstructr -> failure x

transBSRAll :: AbsNatural.BSRAll -> Result
transBSRAll x = case x of
  AbsNatural.BSRAllList boolstructr -> failure x

transRelationalPredicate :: AbsNatural.RelationalPredicate -> Result
transRelationalPredicate x = case x of
  AbsNatural.RPConstraint multiterm1 multiterm2 -> failure x
  AbsNatural.RPBoolStructR multiterm boolstructr -> failure x
  AbsNatural.RPMT multiterm -> failure x

transRPRel :: AbsNatural.RPRel -> Result
transRPRel x = case x of
  AbsNatural.RPis -> failure x

transMultiTerm :: AbsNatural.MultiTerm -> Result
transMultiTerm x = case x of
  AbsNatural.MultiTerm mtexprs -> failure x

transMTExpr :: AbsNatural.MTExpr -> Result
transMTExpr x = case x of
  AbsNatural.MTExpr mtt -> failure x

transMTT :: AbsNatural.MTT -> Result
transMTT x = case x of
  AbsNatural.MTT pident -> failure x

transMTN :: AbsNatural.MTN -> Result
transMTN x = case x of
  AbsNatural.MTN pinteger -> failure x
