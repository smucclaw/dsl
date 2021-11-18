module LS.NLG (
    nlg
    ) where

import LS.Types
    ( Deontic(..),
      EntityType,
      TemporalConstraint (..),
      BoolStruct,
      BoolStructP,
      Rule(..),
      pt2text, text2pt, ParamText, ruleName
      -- bsp2text
      )
import PGF ( CId, Expr, linearize, mkApp, mkCId, startCat, parse, readType, showExpr )
import UDAnnotations ( UDEnv(..), getEnv )
import qualified Data.Text.Lazy as Text
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Data.List.NonEmpty (toList)
import UD2GF (getExprs)
import AnyAll (Item(..))
import Data.Maybe
import qualified AnyAll as AA
-- import Llvm.AbsSyn (LlvmStatement(Expr))

myUDEnv :: IO UDEnv
myUDEnv = getEnv (path "RealSimple") "Eng" "S"
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
       subjectRaw = everyA annotatedRule
       subject = case whoA annotatedRule of
                   Nothing -> subjectRaw
                   Just vp -> mkRelClNP subjectRaw vp
       predicate = mkApp (deonticA annotatedRule) [actionA annotatedRule]
       predicateTemporal = case temporalA annotatedRule of
                          Nothing -> predicate
                          Just adv -> mkApp ( mkCId "AdvVP") [predicate, adv]
       newFancyTree = mkApp subjPred [subject, predicateTemporal]
       newFancyTreeCond = case condA annotatedRule of
                          Nothing -> newFancyTree
                          Just utt -> mkApp ( mkCId "addCond" ) [utt, newFancyTree]
       -- TODO: piecing together the parts in a more complex, UD-based grammar ???
       linText = linearize gr lang newFancyTreeCond
       linTree = showExpr [] newFancyTreeCond

   return (Text.pack (linText ++ "\n" ++ linTree))
  where
    subjPred :: CId
    subjPred = mkCId "subjPred"

    mkRelClNP :: Expr -> Expr -> Expr
    mkRelClNP np vp = mkApp (mkCId "addWho") [np,vp]



parseFields :: UDEnv -> Rule -> AnnotatedRule
parseFields env rl@(Regulative {}) =
  RegulativeA { everyA  = parseEvery env (ruleName rl)     ::  PGF.Expr
              , whoA    = fmap (parseWho env) (who rl)  :: Maybe PGF.Expr
              , condA   = parseCond env <$> cond rl     :: Maybe PGF.Expr
              , deonticA = parseDeontic (deontic rl)    :: PGF.CId
              , actionA  = parseAction env (action rl)  :: PGF.Expr
              , temporalA = fmap (parseTemporal env) (temporal rl)  :: Maybe PGF.Expr
              , uponA  = (parseUpon  env) <$> listToMaybe (upon rl)    :: Maybe PGF.Expr
              , givenA = (parseGiven env) <$> given rl :: Maybe PGF.Expr -- as a hack, we ignore all but the first element of a Given. TODO
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

    parseWho :: UDEnv -> BoolStructP -> Expr
    parseWho env bs = parse' "VP" env (bsp2text bs)

    parseCond :: UDEnv -> BoolStructP -> Expr
    parseCond env bs = parse' "S" env (bsp2text bs) -- was "Utt"

    parseGiven :: UDEnv -> ParamText -> Expr
    parseGiven env pt = parse' "S" env (pt2text pt)

    parseAction :: UDEnv -> BoolStructP -> Expr
    parseAction env bsp = parse' "VP" env (bsp2text bsp)

    parseDeontic :: Deontic -> CId
    parseDeontic d = case d of
        DMust  -> mkCId "must_Deontic"
        DMay   -> mkCId "may_Deontic"
        DShant -> mkCId "shant_Deontic"

    parseTemporal :: UDEnv -> TemporalConstraint Text.Text -> Expr
    parseTemporal env (TBefore event)  = parse' "Adv"  env (Text.unwords [Text.pack "before", event])
    parseTemporal env (TAfter event)  = parse' "Adv"  env (Text.unwords [Text.pack "after", event])
    parseTemporal env (TBy event)  = parse' "Adv"  env (Text.unwords [Text.pack "by", event])
    parseTemporal env (TOn event)  = parse' "Adv"  env (Text.unwords [Text.pack "on", event])

    parseUpon :: UDEnv -> BoolStructP -> Expr
    parseUpon env bs = parse' "Adv" env (Text.unwords [Text.pack "upon", bsp2text bs])

-- BoolStruct is from Types, and Item is from AnyAll
-- TODO: for now only return the first thing
-- later: BoolStruct -> PGF.Expr -- mimic the structure in GF grammar
bs2text :: BoolStruct -> Text.Text
bs2text (Leaf txt) = txt
bs2text (All _) = Text.pack "walk"
bs2text (Any _) = Text.pack "walk"
bs2text (Not _) = Text.pack "walk"


bsp2text :: BoolStructP -> Text.Text
bsp2text (AA.Leaf pt)  = pt2text pt
-- bsp2text (AA.Not  x)   = "not " <> bsp2text x
bsp2text (AA.Any xs) =  Text.unwords (bsp2text <$> xs)
bsp2text (AA.All xs) =  Text.unwords (bsp2text <$> xs)
-- and possibily we want to have interspersed BoolStructs along the way

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
