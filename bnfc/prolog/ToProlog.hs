module ToProlog where

import Data.Char (isUpper, isLower, toTitle, toLower)
import AbsL
import Data.List (intercalate)

class ToProlog a where
  toProlog :: a -> String

instance ToProlog Rules where
  toProlog (Toplevel toplevels) =
    unlines $ toProlog <$> toplevels

instance ToProlog Toplevels where
  toProlog (ToplevelsRule rule) = toProlog rule
  toProlog (ToplevelsModule m)   = show m
  toProlog (ToplevelsImport i)   = show i
  toProlog (ToplevelsPragma p)   = show p

instance ToProlog Rule where
  toProlog (RuleStanza ruledef rulebody) =
    unlines [ unwords [ "%% ruleDef:",   show ruledef ]
            , unwords [ "%% ruleBody:",  show rulebody ]
            , unwords [ toProlog rulebody ]
            ]

instance ToProlog RuleBody where
  toProlog RBNoop = "%% NOOP lol"
  toProlog (RBDeem objattrands predexpr) = unlines $
    (\(ObjAttrAnd1 objAttr) -> unwords [ toProlog objAttr
                                       , ":-"
                                       , toProlog predexpr ++ "." ])
    <$> objattrands

instance ToProlog ObjAttr where 
  toProlog (OA_method oa commalist) = toProlog oa ++ "(" ++ toProlog commalist ++ ")"
  toProlog (OA_dots objAttrElems) =
    case asVar objAttrElems of
      (inside, outside, Just asvar) -> outside ++ "(" ++ intercalate "," [inside, asvar] ++ ")"
      (inside, outside, Nothing)    -> outside ++ "(" ++ inside ++ ")"

asVar :: [ObjAttrElem] -> (String, String, Maybe String)
asVar objAttrElems =
   let oalist = toProlog <$> objAttrElems
       outside = last oalist
       inside  = head oalist
    in
    if and [ length oalist == 2
           , isUpper . head $ outside
           , isUpper . head $ inside ]
    then (inside, lcfirst outside, Just $ titleCase outside)
    else (inside, lcfirst outside, Nothing)
    where titleCase "" = ""
          titleCase (x:xs) = toTitle x : xs
          lcfirst "" = ""
          lcfirst (x:xs) = toLower x : xs


instance ToProlog ObjAttrElem where 
  toProlog (ObjAttrElemIdent (Ident oaeii)) = oaeii

instance ToProlog PredExpr where 
  toProlog (PEOA oa)  = toProlog oa
  toProlog (PEAnd exp1 exp2) = toProlog exp1 ++ ", " ++ toProlog exp2
  toProlog (PEOr  exp1 exp2) = toProlog exp1 ++ ", " ++ toProlog exp2
  toProlog (PEME  matchexp)  = toProlog matchexp

instance ToProlog MatchExpr where
  toProlog (ME_OA_JL oa@(OA_dots oaes) jl) =
    case asVar oaes of
      (inside, outside, Just asvar) -> intercalate ", " [ toProlog oa, "match(" ++ asvar, toProlog jl ++ ")" ]
      (inside, outside, Nothing)    -> "%% not sure what to do here"
  toProlog (ME_OA_JL oa jl) = "%% unable to render a match against a junctionlist because the object-attribute doesn't look like Upper.lower"
  toProlog (ME_True)        = "true"
  toProlog (ME_False)       = "false"

instance ToProlog JunctionList where
  toProlog (JL_Comma l) = "plain, [" ++ toProlog l ++ "]"
  toProlog (JL_And   l) =   "and, [" ++ toProlog l ++ "]"
  toProlog (JL_Or    l) =    "or, [" ++ toProlog l ++ "]"
  toProlog (JL_Xor   (XorList l)) -- you can take out the plain bit when the xor match in prolog is working properly
    | length l == 1       = "plain, [" ++ toProlog (XorList l) ++ "]"
    | otherwise           =   "xor, [" ++ toProlog (XorList l) ++ "]"

instance ToProlog CommaList where
  toProlog (CommaList  l) = intercalate ", " $ toProlog <$> l
instance ToProlog   AndList where toProlog (  AndList l) = intercalate ", " $ toProlog <$> l
instance ToProlog    OrList where toProlog (   OrList l) = intercalate ", " $ toProlog <$> l
instance ToProlog   XorList where toProlog (  XorList l) = intercalate ", " $ toProlog <$> l

instance ToProlog CommaElem where
  toProlog (CommaElemObjAttr oa) = toProlog oa
  toProlog (CommaElemString  oa) = show oa
instance ToProlog   AndElem where
  toProlog (  AndElemObjAttr oa) = toProlog oa
  toProlog (  AndElemString  oa) = show oa
instance ToProlog    OrElem where
  toProlog (   OrElemObjAttr oa) = toProlog oa
  toProlog (   OrElemString  oa) = show oa
instance ToProlog   XorElem where
  toProlog (  XorElemObjAttr oa) = toProlog oa
  toProlog (  XorElemString  oa) = show oa
