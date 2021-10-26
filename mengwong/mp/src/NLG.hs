module NLG (
    nlg
    ) where

import Types
    ( Deontic(..),
      ActionType,
      EntityType,
      Rule(..) )
import PGF ( CId, Expr, linearize, mkApp, mkCId, startCat, parse, readType )
import UDAnnotations ( UDEnv(..), getEnv )
import qualified Data.Text.Lazy as Text
import Data.Maybe (fromMaybe)
import Data.Char (toLower)


myUDEnv :: IO UDEnv
myUDEnv = getEnv (path "RealSimple") "Eng" "UDS"
  where path x = "grammars/" ++ x

-- So far not going via UD, just raw GF parsing
nlg :: Rule -> IO Text.Text
nlg rl = do
   env <- myUDEnv
   let gr = pgfGrammar env
       lang = actLanguage env
       subject = parseEvery env (every rl)
       predicateRaw = parseAction env (action rl)
       deonticVV = parseDeontic (deontic rl)
       predicate = mkApp deonticVV [predicateRaw]
       newFancyTree = mkApp subjPred [subject, predicate]
       linText = Text.pack $ linearize gr lang newFancyTree
   return linText

  where
    parse' :: String -> UDEnv -> Text.Text -> Expr
    parse' cat env text =
      case trees of
        [] -> error $ "nlg: couldn't parse " ++ str
        x:_ -> x
      where
        gr = pgfGrammar env
        lang = actLanguage env
        str = map toLower $ Text.unpack text
        startcat = fromMaybe (startCat gr) $ readType cat
        trees = parse gr lang startcat str

    subjPred :: CId
    subjPred = mkCId "subjPred"

    -- These funs all just parse directly with PGF library
    -- In the future, there will be a UDpipe—ud2gf pipeline
    parseEvery :: UDEnv -> EntityType -> Expr
    parseEvery = parse' "NP"

    -- parseWho :: BoolStruct -> Expr
    -- parseWho = undefined

    -- parseCond :: BoolStruct -> Expr
    -- parseCond = undefined

    parseAction :: UDEnv -> ActionType -> Expr
    parseAction env at = parse' "VP" env (at2str at)

    parseDeontic :: Deontic -> CId
    parseDeontic d = case d of
        DMust  -> mkCId "must_Deontic"
        DMay   -> mkCId "may_Deontic"
        DShant -> mkCId "shant_Deontic"

    -- parseTemporal :: TemporalConstraint Text.Text -> Expr
    -- parseTemporal = undefined


at2str :: ActionType -> Text.Text
at2str (verb, []) = verb
at2str (verb, (_op, args):_) = Text.unwords [verb, Text.unwords args] -- TODO: rest of args

------------------------------------------------------------
-- Ignore everything below for now

-- parseCoNLLU :: UDEnv -> String -> [[Expr]]
-- parseCoNLLU = getExprs []
data AnnotatedRule = RegulativeA
            { everyA    :: Expr                      -- every person (NP)
            , whoA      :: Maybe Expr                -- who walks and (eats or drinks) (RS)
            , condA     :: Maybe Expr                -- if it is a saturday (Adv)
            , deonticA  :: Deontic                   -- must (VV)
            , actionA   :: Expr                      -- sing / pay the king $20 (VP)
            , temporalA :: Maybe Expr                -- before midnight (Adv)
            -- TODO later
            -- , henceA    :: Maybe [AnnotatedRule]     -- hence [UDS]
            -- , lestA     :: Maybe [AnnotatedRule]     -- lest [UDS]
            -- , rlabelA   :: Maybe Text.Text -- TODO what are these?
            -- , lsourceA  :: Maybe Text.Text
            -- , srcrefA   :: Maybe SrcRef
            }
          deriving (Eq, Show)


-- Test data, already in CoNLL
-- testCoNLLU :: String
-- testCoNLLU = unlines [
--       "1\teveryone\teveryone\tPRON\tNN\tNumber=Sing\t11\tnsubj:pass\t_\tFUN=PRONNN"
--     , "2\twho\twho\tPRON\tWP\tPronType=Rel\t4\tnsubj:pass\t_\tFUN=PRONWP"
--     , "3\tis\tbe\tAUX\tVBZ\tMood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin\t4\taux:pass\t_\tFUN=UseComp"
--     , "4\taffected\taffect\tVERB\tVBN\tTense=Past|VerbForm=Part|Voice=Pass\t1\tacl:relcl\t_\tFUN=affectVBN"
--     , "5\tby\tby\tADP\tIN\t_\t8\tcase\t_\t_"
--     , "6\tthe\tthe\tDET\tQuant\tFORM=0\t8\tdet\t_\tFUN=DefArt"
--     , "7\tdata\tdata\tNOUN\tNN\tNumber=Sing\t8\tcompound\t_\tFUN=data_N"
--     , "8\tbreach\tbreach\tNOUN\tNN\tNumber=Sing\t4\tobl\t_\tFUN=breach_N"
--     , "9\tshould\tshould\tAUX\tMD\tVerbForm=Fin\t11\taux\t_\t_"
--     , "10\tbe\tbe\tAUX\tVB\tVerbForm=Inf\t11\taux:pass\t_\tFUN=UseComp"
--     , "11\tnotified\tnotify\tVERB\tVBN\tTense=Past|VerbForm=Part|Voice=Pass\t0\troot\t_\tFUN=notifyVBN"
--     ]
