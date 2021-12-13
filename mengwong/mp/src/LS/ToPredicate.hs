{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module LS.ToPredicate where


import LS.UDExt
import PGF hiding (Tree)
import Data.List.Split (splitOn)
import Data.Char (toUpper)


data Predicate = Not Predicate | Unary String
  deriving (Show, Eq)

convertToPredicate :: Expr -> Predicate
convertToPredicate expr = findHead (fg expr :: GUDS)

removeType :: String -> String
removeType str | [prefix, _suffix] <- splitOn "_" str = prefix
 | otherwise = error $ "Can't remove type from: " ++ show str

headName :: CId -> String
headName = removeType . showCId

data SomeTree = forall b. SomeTree (Tree b)

findHead :: Gf (Tree a) => Tree a -> Predicate
findHead (Groot_only x) = findHead x
findHead (GrootN_ x) = Unary $ headNP x
findHead (Groot_cop_advmod rt _cp Gnot_advmod) = Not $ findHead rt
findHead x = error $ "don't know how to find the head from " ++ showExpr [] (gf x)

headNP :: GNP -> String
headNP (GMassNP cn) = headCN cn
headNP (GDetCN _ cn) = headCN cn
headNP (GAdvNP np _) = headNP np
headNP (GConjNP _ (GListNP (np:nps))) = headNP np
headNP (GDetNP det) = handleAnaphora det
headNP (GExtAdvNP np _) = headNP np
headNP (GGenModNP _ _ cn) = headCN cn
headNP (GPredetNP _ np) = headNP np
headNP (GRelNP np _) = headNP np
headNP _ = error "not implemented"

handleAnaphora :: GDet -> String
handleAnaphora = error "not implemented"

headCN :: GCN -> String
headCN (GUseN n) | Just (n', []) <- unApp (gf n) = headName n' -- organization
headCN (GAdjCN ap cn) = mashTogether (headAP ap) (headCN cn) -- publicAgency_N
headCN _ = error "not implemented"

headAP :: GAP -> String
headAP (GPositA a) | Just (a', []) <- unApp (gf a) = headName a'
headAP _ = error "not implemented"


mashTogether :: String -> String -> String
mashTogether a n = a ++ capitalize n

capitalize :: String  -> String
capitalize (a:as) = toUpper a : as
capitalize [] = error "empty string"

{-}
NP -> N
VP -> V
AP -> A

rootA_ exp -> extract xxx_A from exp
rootN_ exp -> extract xxx_N from exp

rootV_ exp -> extract xxx_V from exp
rootAdv_ exp -> try: extract xxx_Adv from exp
                try: extract PrepNP xxx_Prep yyy_NP from exp

    -}