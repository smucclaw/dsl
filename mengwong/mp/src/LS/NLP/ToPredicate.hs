{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module LS.NLP.ToPredicate where


import LS.NLP.UDExt
import PGF hiding (Tree)
import Data.List.Split (splitOn)
import Data.Char (toUpper)
import LS.NLP.NLG (AnnotatedRule(..))
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
convertToPredicate expr = mkPredicate (fg expr :: GUDS)

removeType :: String -> String
removeType str | [prefix, _suffix] <- splitOn "_" str = prefix
 | otherwise = error $ "Can't remove type from: " ++ show str

headName :: CId -> String
headName = removeType . showCId

data SomeTree = forall b. SomeTree (Tree b)

mkPredicate :: Gf (Tree a) => Tree a -> Predicate
mkPredicate (GrootN_ x) = Unary $ headNP x
mkPredicate (GrootV_ x) = Unary $ headVP x
mkPredicate (Groot_cop_advmod root _ Gnot_advmod) = Not $ mkPredicate root -- TODO: use findNeg for more general solution?
mkPredicate (Groot_only rt) = mkPredicate rt
mkPredicate (Groot_mark_nsubj rt _ _) = mkPredicate rt
mkPredicate (Groot_nsubj rt _) = mkPredicate rt
mkPredicate (Groot_xcomp (GrootV_ vp) (GxcompA_ccomp_ ap (Gccomp_ uds))) =
    Unary (headVP vp `combineName` headAP ap)
     `combinePredicate` findHeadAndArg uds
mkPredicate (Groot_xcomp_ccomp (GrootV_ vp) xc (Gccomp_ uds)) =
    Unary (headVP vp `combineName` headXC xc)
     `combinePredicate` findHeadAndArg uds
mkPredicate (Groot_ccomp (GrootV_ vp) (Gccomp_ uds)) =
    Unary (headVP vp) `combinePredicate` findHeadAndArg uds
mkPredicate (Groot_nsubj_ccomp rt subj cc) = undefined
mkPredicate x = error $ "don't know how to find the head from " ++ showExpr [] (gf x)

-- Used when there are ccomp (clausal complement) present in the sentence
-- We need to find, not just the head, but also the argument of the ccomp
findHeadAndArg :: Tree a -> (Predicate, String)
findHeadAndArg uds | [] <- findCcomp uds  -- (org) becomes aware that breach occurs
                   , [root] <- findRoot uds
                   , [sub] <- findNsubj uds = (mkPredicate root, headNP sub)
findHeadAndArg uds | [cc] <- findCcomp uds -- (org) becomes aware that lawyer knows that breach occurs
                   , [knowRoot] <- findRoot uds
                   , [lawyerSubj] <- findNsubj uds
                   , (Unary occur, databreach) <- findHeadAndArg cc
                   , (Unary know, lawyer) <- (mkPredicate knowRoot, headNP lawyerSubj)
                   = (Binary (know `combineName` occur) lawyer, databreach)
-- Need to manually add support for each number of nested ccomps
findHeadAndArg uds | roots <- findRoot uds, nps <- findNsubj uds = error $ "too many roots:\n "
  ++ intercalate ", " (map (showExpr [] . gf) roots)
  ++ "\n and nsubjs\n"
  ++ intercalate ", " (map (showExpr [] . gf) nps)

findCcomp :: Tree a -> [GUDS]
findCcomp (Gccomp_ uds) = [uds]
findCcomp x = composOpMonoid findCcomp x

-- Finds a NP labelled with nsubj, but skips clausal complements (ccomp).
-- So for a sentence like "I know that you know", it will only return [i_NP].
-- The list is there just to not crash when there are no nsubjs,
-- we don't expect a sentence to have multiple nsubjs, aside from the excluded ccomp.
findNsubj :: Tree a -> [GNP]
findNsubj (Gnsubj_ np) = [np]
findNsubj (Gccomp_ _uds) = [] -- when finding ccomp, return empty list, don't look further for nsubjs
findNsubj x = composOpMonoid findNsubj x

-- Finds negation, but skips ccomp(s)
-- "I don't know that you know" -> returns negation
-- "I know that you don't know" -> returns empty list
findNeg :: Tree a -> [Gadvmod]
findNeg Gnot_advmod = [Gnot_advmod]
findNeg (Gccomp_ _) = []
findNeg x = composOpMonoid findNeg x

-- Finds root, but skips ccomp(s)
-- "I know that you sing" -> returns [know], not sing
findRoot :: Tree a -> [Groot]
findRoot rt@(GrootA_ _ap) = [rt]
findRoot rt@(GrootN_ _np) = [rt]
findRoot rt@(GrootV_ _vp) = [rt]
findRoot    (Gccomp_ _)   = []
findRoot x = composOpMonoid findRoot x


headXC :: Gxcomp -> String
headXC (GxcompA_ a) = headAP a
headXC (GxcompAdv_ adv) = headAdv adv
headXC (GxcompA_ccomp_ ap cc) = error "not implemented"

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
headCN (GUseN (GCompoundN (Lexical n1) (Lexical n2))) =
   n1 `combineName` n2         -- dataBreach
headCN (GAdjCN ap cn) =        -- publicAgency
   headAP ap `combineName` headCN cn
headCN _ = error "not implemented"

headAP :: GAP -> String
headAP (GPositA (Lexical a)) = a
headAP _ = error "not implemented"

headAdv :: GAdv -> String
headAdv (GPrepNP (LexPrep pr) np) = pr `combineName` headNP np
headAdv (LexAdv adv) = adv
headAdv _ = error "not implemented"

infixr 2 `combineName`
infixr 2 `combinePredicate`

combineName :: String -> String -> String
combineName a n = a ++ capitalize n

combinePredicate :: Predicate -> (Predicate, Arg) -> Predicate
combinePredicate (Unary p1) (Unary p2, arg) = Binary (p1 `combineName` p2) arg
combinePredicate (Unary p1) (Binary p2 arg1, arg2) = Ternary (p1 `combineName` p2) arg1 arg2
--combinePredicate (Unary p1) ((Not pred), arg) = _ -- TODO: put negation in the name only
combinePredicate _ _ = error "combinePredicate: not implemented"


capitalize :: String  -> String
capitalize (a:as) = toUpper a : as
capitalize [] = []

