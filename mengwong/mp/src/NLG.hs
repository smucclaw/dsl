module NLG (
    nlg
    ) where

import Types ( Rule )
import PGF ( Expr )
import UD2GF ( getExprs )
import UDAnnotations ( UDEnv, getEnv, pgfGrammar )
import qualified Data.Text.Lazy as Text


myUDEnv :: IO UDEnv
myUDEnv = getEnv (path "UDApp") "Eng" "UDS"
  where path x = "grammars/" ++ x

nlg :: Rule -> IO Text.Text
nlg rl = do
   env <- myUDEnv
   let gr = pgfGrammar env
   allTrees <- parseCoNLLU env testCoNLLU
   let lins = []
   return $ Text.pack "hello world"


parseCoNLLU :: UDEnv -> String -> IO [[Expr]]
parseCoNLLU env string = do

  return $ getExprs [] env string


-- Test data, already in CoNLL
testCoNLLU :: String
testCoNLLU = unlines [
      "1\teveryone\teveryone\tPRON\tNN\tNumber=Sing\t11\tnsubj:pass\t_\tFUN=PRONNN"
    , "2\twho\twho\tPRON\tWP\tPronType=Rel\t4\tnsubj:pass\t_\tFUN=PRONWP"
    , "3\tis\tbe\tAUX\tVBZ\tMood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin\t4\taux:pass\t_\tFUN=UseComp"
    , "4\taffected\taffect\tVERB\tVBN\tTense=Past|VerbForm=Part|Voice=Pass\t1\tacl:relcl\t_\tFUN=affectVBN"
    , "5\tby\tby\tADP\tIN\t_\t8\tcase\t_\t_"
    , "6\tthe\tthe\tDET\tQuant\tFORM=0\t8\tdet\t_\tFUN=DefArt"
    , "7\tdata\tdata\tNOUN\tNN\tNumber=Sing\t8\tcompound\t_\tFUN=data_N"
    , "8\tbreach\tbreach\tNOUN\tNN\tNumber=Sing\t4\tobl\t_\tFUN=breach_N"
    , "9\tshould\tshould\tAUX\tMD\tVerbForm=Fin\t11\taux\t_\t_"
    , "10\tbe\tbe\tAUX\tVB\tVerbForm=Inf\t11\taux:pass\t_\tFUN=UseComp"
    , "11\tnotified\tnotify\tVERB\tVBN\tTense=Past|VerbForm=Part|Voice=Pass\t0\troot\t_\tFUN=notifyVBN"
    ]

-- data Rule = Regulative
--             { every    :: EntityType         -- every person
--             , who      :: Maybe BoolStruct         -- who walks and (eats or drinks)
--             , cond     :: Maybe BoolStruct         -- if it is a saturday
--             , deontic  :: Deontic            -- must
--             , action   :: ActionType         -- sing
--             , temporal :: Maybe (TemporalConstraint Text.Text) -- Before "midnight"
--             , hence    :: Maybe [Rule]
--             , lest     :: Maybe [Rule]
--             , rlabel   :: Maybe Text.Text
--             , lsource  :: Maybe Text.Text
--             , srcref   :: Maybe SrcRef
--             }
--           | Constitutive
--             { term     :: ConstitutiveTerm
--             , cond     :: Maybe BoolStruct
--             , rlabel   :: Maybe Text.Text
--             , lsource  :: Maybe Text.Text
--             , srcref   :: Maybe SrcRef
--             }
