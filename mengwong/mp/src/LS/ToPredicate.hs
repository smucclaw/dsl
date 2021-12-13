{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module LS.ToPredicate where


import LS.UDExt
import PGF hiding (Tree)
import Data.List.Split (splitOn)
import Data.Char (toUpper)


data Predicate = Not Predicate | Unary String | Binary String
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
findHead (GrootN_ x) = Unary $ headNP x
findHead (GrootV_ x) = Unary $ headVP x
findHead (Groot_only rt) = findHead rt
findHead (Groot_cop_advmod rt _cp Gnot_advmod) = Not $ findHead rt
findHead (Groot_mark_nsubj rt _ _) = findHead rt
findHead (Groot_nsubj rt _) = findHead rt
--findHead (Groot_xcomp rt xc) = mashTogether
findHead (Groot_xcomp_ccomp (GrootV_ vp) xc (Gccomp_ uds)) =
    Unary (headVP vp `combineName` headXC xc)
     `combinePredicate` findHead uds
findHead x = error $ "don't know how to find the head from " ++ showExpr [] (gf x)

headXC :: Gxcomp -> String
headXC (GxcompA_ a) = headAP a
headXC (GxcompAdv_ adv) = error "not implemented: headXC"

headVP :: GVP -> String
headVP (GUseV v) | Just (v', []) <- unApp (gf v) = headName v'
headVP _ = error "not implemented"

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
headCN (GAdjCN ap cn) = combineName (headAP ap) (headCN cn) -- publicAgency_N
headCN _ = error "not implemented"

headAP :: GAP -> String
headAP (GPositA a) | Just (a', []) <- unApp (gf a) = headName a'
headAP _ = error "not implemented"

infixr 2 `combineName`
infixr 2 `combinePredicate`

combineName :: String -> String -> String
combineName a n = a ++ capitalize n

combinePredicate :: Predicate -> Predicate -> Predicate
combinePredicate (Unary p1) (Unary p2) = Binary $ p1 `combineName` p2

capitalize :: String  -> String
capitalize (a:as) = toUpper a : as
capitalize [] = []

