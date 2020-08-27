module ToProlog where

import AbsL

toProlog_Rules :: Rules -> String
toProlog_Rules (EModule rules) =
  unlines $ toProlog_Rule <$> rules

toProlog_Rule :: Rule -> String
toProlog_Rule (RuleStanza ruledef rulebody) =
  unlines [ unwords [ "// ruleDef: ", show ruledef ]
          , unwords [ "// ruleBody: " ++ show rulebody ] ]
