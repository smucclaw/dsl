{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module LS.ToPredicate where


import LS.UDExt
import PGF hiding (Tree)
import Data.List.Split (splitOn)
import Data.Char (toUpper)
import LS.NLG (AnnotatedRule(..))
import Data.List (intercalate)
import Data.Bifunctor (first)
import Data.Maybe (mapMaybe)

newtype Formula = And [Predicate]
  deriving (Show, Eq)

convertToFormula :: AnnotatedRule -> Formula
convertToFormula rl@RegulativeA {subjA,whoA,uponA} = And $ mapMaybe (fmap convertToPredicate) [Just subjA, whoA, uponA]
convertToFormula _ = error "not implemented"

applyFormula :: Formula -> String -> String
applyFormula (And xs) subj = "\\forall " ++ subj ++ " . " ++ intercalate " && " (map (applyPredicate subj) xs)

applyPredicate :: String -> Predicate -> String
applyPredicate subj (Unary n) = n ++ "(" ++ subj ++ ")"
applyPredicate subj (Not pred) = "!" ++ applyPredicate subj pred
applyPredicate subj (Binary n arg) = n ++ "(" ++ intercalate ", " [subj, arg] ++ ")"
applyPredicate subj (Ternary n a1 a2) = n ++ "(" ++ intercalate ", " [subj, a1, a2] ++  ")"


type Name = String
type Arg = String
data Predicate = Not Predicate | Unary Name | Binary Name Arg | Ternary Name Arg Arg
  deriving (Show, Eq)

convertToPredicate :: Expr -> Predicate
convertToPredicate expr = findHead (fg expr :: GUDS)

removeType :: String -> String
removeType str | [prefix, _suffix] <- splitOn "_" str = prefix
 | otherwise = error $ "Can't remove type from: " ++ show str

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
findHead (Groot_xcomp_ccomp (GrootV_ vp) xc (Gccomp_ uds)) =
    Unary (headVP vp `combineName` headXC xc)
     `combinePredicate` findHeadAndArg uds
findHead (Groot_ccomp (GrootV_ vp) (Gccomp_ uds)) =
    Unary (headVP vp) `combinePredicate` findHeadAndArg uds
findHead (Groot_nsubj_ccomp rt subj cc) = undefined
findHead x = error $ "don't know how to find the head from " ++ showExpr [] (gf x)

findHeadAndArg :: Tree a -> (Predicate, String)
findHeadAndArg uds | [] <- findCcomp uds
                   , [root] <- findRoot uds
                   , [sub] <- findNsubj uds = (findHead root, headNP sub)
findHeadAndArg uds | [cc] <- findCcomp uds
                   , [knowRoot] <- findRoot uds
                   , [lawyerSubj] <- findNsubj uds
                   , (Unary occur, databreach) <- findHeadAndArg cc
                   , (Unary know, lawyer) <- (findHead knowRoot , headNP lawyerSubj)
                   = (Binary (know `combineName` occur) lawyer, databreach)

findHeadAndArg uds | roots <- findRoot uds, nps <- findNsubj uds = error $ "too many roots:\n "
  ++ intercalate ", " (map (showExpr [] . gf) roots)
  ++ "\n and nsubjs\n"
  ++ intercalate ", " (map (showExpr [] . gf) nps)

findNsubj :: Tree a -> [GNP]
findNsubj (Gnsubj_ np) = [np]
findNsubj c@(Gccomp_ uds) = []
findNsubj x = composOpMonoid findNsubj x

findCcomp :: Tree a -> [GUDS]
findCcomp (Gccomp_ uds) = [uds]
findCcomp x = composOpMonoid findCcomp x

findRoot :: Tree a -> [Groot]
findRoot rt@(GrootA_ ap) = [rt]
findRoot rt@(GrootN_ np) = [rt]
findRoot rt@(GrootV_ vp) = [rt]
findRoot c@(Gccomp_ uds) = []
findRoot x = composOpMonoid findRoot x


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

pattern Lexical :: Gf a => String -> a
pattern Lexical n <- (fmap (first headName) . unApp . gf -> Just (n, []))

headCN :: GCN -> String
headCN (GUseN (Lexical n)) = n -- organization
headCN (GUseN (GCompoundN (Lexical n1) (Lexical n2))) = n1 `combineName` n2 -- organization
headCN (GAdjCN ap cn) = combineName (headAP ap) (headCN cn) -- publicAgency_N
headCN _ = error "not implemented"

headAP :: GAP -> String
headAP (GPositA a) | Just (a', []) <- unApp (gf a) = headName a'
headAP _ = error "not implemented"

infixr 2 `combineName`
infixr 2 `combinePredicate`

combineName :: String -> String -> String
combineName a n = a ++ capitalize n

combinePredicate :: Predicate -> (Predicate, Arg) -> Predicate
combinePredicate (Unary p1) (Unary p2, arg) = Binary (p1 `combineName` p2) arg
combinePredicate (Unary p1) (Binary p2 arg1, arg2) = Ternary (p1 `combineName` p2) arg1 arg2

capitalize :: String  -> String
capitalize (a:as) = toUpper a : as
capitalize [] = []

