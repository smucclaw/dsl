module L4.NLG (
    nlg
    ) where

import L4.Types
    ( Deontic(..),
      ParamText,
      EntityType,
      ParamText,
      BoolStruct(..),
      Rule(..) )
import PGF ( CId, Expr, linearize, mkApp, mkCId, startCat, parse, readType, showExpr )
import UDAnnotations ( UDEnv(..), getEnv )
import qualified Data.Text.Lazy as Text
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Data.List.NonEmpty (toList)
import UD2GF (getExprs)
import AnyAll (Item(..), Label(..))
import Data.Maybe

myUDEnv :: IO UDEnv
myUDEnv = getEnv (path "RealSimple") "Eng" "UDS"
  where path x = "grammars/" ++ x

-- So far not going via UD, just raw GF parsing
nlg :: Rule -> IO Text.Text
nlg rl = do
   env <- myUDEnv
   let annotatedRule = parseFields env rl
       gr = pgfGrammar env
       lang = actLanguage env

       -- piecing together in a simple GF grammar
       -- TODO: check which parts are Nothing and which have a value, then use the ones that have a value
       -- For now we just use the fields that are not Maybe
       subject = everyA annotatedRule
       predicate = mkApp (deonticA annotatedRule) [actionA annotatedRule]
       newFancyTree = mkApp subjPred [subject, predicate]

       -- TODO: piecing together the parts in a more complex, UD-based grammar ???
       linText = linearize gr lang newFancyTree
       linTree = showExpr [] newFancyTree
   return (Text.pack (linText ++ "\n" ++ linTree))
  where
    subjPred :: CId
    subjPred = mkCId "subjPred"


parseFields :: UDEnv -> Rule -> AnnotatedRule
parseFields env rl@(Regulative {}) =
  RegulativeA { everyA  = parseEvery env (every rl)    ::  PGF.Expr
              , whoA    = Nothing
              , condA   = Nothing
              , deonticA = parseDeontic (deontic rl)   :: PGF.CId
              , actionA  = parseAction env (action rl) :: PGF.Expr
              , temporalA = Nothing
              , uponA = Nothing
              , givenA = fmap (parseGiven env) (given rl) :: Maybe PGF.Expr
                -- corresponds to     case given rl of
                        --               Just bs -> Just $ parseGiven env bs
                        --               _ -> Nothing
            }
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


    -- These funs all just parse directly with PGF library
    -- In the future, there will be a UDpipe—ud2gf pipeline
    parseEvery :: UDEnv -> EntityType -> Expr
    parseEvery = parse' "NP"

    -- parseWho :: UDEnv -> BoolStruct -> Expr
    -- parseWho = undefined

    -- parseCond :: UDEnv -> BoolStruct -> Expr
    -- parseCond = undefined

    parseGiven :: UDEnv -> BoolStruct -> Expr
    parseGiven env bs = parse' "S" env (bs2text bs)

    parseAction :: UDEnv -> ParamText -> Expr
    parseAction env at = parse' "VP" env (at2text at)

    parseDeontic :: Deontic -> CId
    parseDeontic d = case d of
        DMust  -> mkCId "must_Deontic"
        DMay   -> mkCId "may_Deontic"
        DShant -> mkCId "shant_Deontic"

    -- parseTemporal :: TemporalConstraint Text.Text -> Expr
    -- parseTemporal = undefined


at2text :: ParamText -> Text.Text
at2text = Text.unwords . concatMap toList . toList

-- BoolStruct is from Types, and Item is from AnyAll
bs2text :: BoolStruct -> Text.Text
bs2text (Leaf txt) = txt
bs2text (All _ _) = Text.pack "my hovercraft is full of eels"
bs2text (Any _ _) = Text.pack "my hovercraft is full of eels"
bs2text (Not _) = Text.pack "my hovercraft is full of eels"
------------------------------------------------------------
-- Ignore everything below for now

parseCoNLLU :: UDEnv -> String -> [[Expr]]
parseCoNLLU = getExprs []

data AnnotatedRule = RegulativeA
            { everyA    :: Expr                      -- every person (NP)
            , whoA      :: Maybe Expr                -- who walks and (eats or drinks) (RS)
            , condA     :: Maybe Expr                -- if it is a saturday (Adv)
            , deonticA  :: CId                       -- must (CId -- a hack, will change later)
            , actionA   :: Expr                      -- sing / pay the king $20 (VP)
            , temporalA :: Maybe Expr                -- before midnight (Adv)
            , uponA     :: Maybe Expr                -- UPON entering the club (event prereq trigger)
            , givenA    :: Maybe Expr                -- GIVEN an Entertainment flag was previously set in the history trace
            -- TODO later
            -- , henceA    :: Maybe [AnnotatedRule]     -- hence [UDS]
            -- , lestA     :: Maybe [AnnotatedRule]     -- lest [UDS]
            -- , rlabelA   :: Maybe Text.Text -- TODO what are these?
            -- , lsourceA  :: Maybe Text.Text
            -- , srcrefA   :: Maybe SrcRef
            }
          deriving (Eq, Show)


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
