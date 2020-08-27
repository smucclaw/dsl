module ToProlog where

import AbsL
import Data.List (intercalate)

class ToProlog a where
  toProlog :: a -> String

instance ToProlog Rules where
  toProlog (EModule rules) =
    unlines $ toProlog <$> rules

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
  toProlog (ObjAttr1 objAttrElems) =
    let outside = last (toProlog <$> objAttrElems)
        inside  = init (toProlog <$> objAttrElems)
    in outside ++ "(" ++ intercalate "." inside ++ ")"

instance ToProlog ObjAttrElem where 
  toProlog (ObjAttrElemIdent (Ident oaeii)) = oaeii

instance ToProlog PredExpr where 
  toProlog (PredExprObjAttr oa)  = toProlog oa
  toProlog (PredExpr1 and1 and2) = toProlog and1 ++ ", " ++ toProlog and2
