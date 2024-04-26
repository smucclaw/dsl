{-# OPTIONS_GHC -W #-}
-- {-# OPTIONS_GHC -foptimal-applicative-do #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, OverloadedLabels, UndecidableInstances, RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, DataKinds, TypeFamilies, FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}


module LS.XPile.MathLang.GenericMathLang.TranslateL4 where
-- TODO: Add export list

import Control.Arrow ((>>>))
import Control.Applicative (asum)
import Data.HashSet qualified as Set

import LS.XPile.MathLang.GenericMathLang.ArithOps ( allArithOps, arithOps )
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST -- TODO: Add import list
import LS.XPile.MathLang.Logging (LogConfig, defaultLogConfig)
-- TODO: Haven't actually finished setting up logging infra, unfortunately.
-- But it's also not really necessary for working on the transpiler

import AnyAll qualified as AA
import LS.Types as L4
  (SrcRef(..), RelationalPredicate(..), RPRel(..), MTExpr(..),
  HornClause (..), HornClause2, BoolStructR,
  TypeSig(..), TypedMulti,
  SimpleHlike(..), BaseHL(..), AtomicHC(..), HeadOnlyHC(..),
  MultiClauseHL(..), mkMultiClauseHL,
  HeadOnlyHC, mkHeadOnlyAtomicHC,
  HnBodHC(..),
  mtexpr2text, ParamText, pt2text
  )

import LS.Rule (
                -- Interpreted(..),
                getGiven,
                RuleLabel)
import LS.Rule qualified as L4 (Rule(..))

import Effectful (Eff, (:>), runPureEff)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Reader.Static (Reader, runReader, local, asks, ask)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Flow ((|>))
import Optics hiding ((|>))
-- import Data.Text.Optics (packed, unpacked)
import GHC.Generics (Generic)
import Data.List.NonEmpty qualified as NE
-- import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as T
-- import LS.Utils.TextUtils (int2Text, float2Text)
import Data.Foldable qualified as F (toList, foldrM)
import LS.Utils ((|$>))

-- for parsing expressions that are just strings inside MTExpr
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import Control.Monad.Trans (lift)
import Text.Megaparsec (ParsecT, runParserT, eof, (<?>), try, some, many, between, choice, satisfy, notFollowedBy)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1, char, string)
import Data.Char (isAlphaNum, isDigit)
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Void ( Void )
import Text.Regex.PCRE.Heavy qualified as PCRE
import Text.Read (readMaybe)
import Prelude hiding (exp)
import Debug.Trace (trace)
import Effectful.State.Dynamic qualified as EffState

import Language.Haskell.TH.Syntax qualified as TH
{- | Parse L4 into a Generic MathLang lambda calculus (and thence to Meng's Math Lang AST) -}

{-----------------------------------------------------
  Translating / parsing L4 to generic lamda calculus
------------------------------------------------------}

{-------------------------
the patterns in the head:
-------------------------
* RPMT MultiTerm

    * RPMT [ MTT "case 1 qualifies" ]
    * RPMT
        [ MTT "ind"
        , MTT "qualifies a la case 3"
        ]

* RPConstraint  MultiTerm RPRel MultiTerm

    [ MTT "incomeTaxRate" ] RPis [ MTF 1.0e-2 ]

* RPBoolStructR MultiTerm RPRel BoolStructR

    Hornlike
        { name =
            [ MTT "meets the property eligibility criteria for GSTV-Cash" ]
        , super = Nothing
        , keyword = Means
        , given = Nothing
        , giveth = Nothing
        , upon = Nothing
        , clauses =
            [ HC
                { hHead = RPBoolStructR
                    [ MTT "meets the property eligibility criteria for GSTV-Cash" ] RPis
                    ( Any Nothing
                        [ Not
                            ( Leaf
                                ( RPMT
                                    [ MTT "owns more than one property" ]
                                )
                            )
                        , Leaf
                            ( RPMT
                                [ MTT "owns 2 or more HDB flats and no other property" ]
                            )
                        ]
                    )
                , hBody = Nothing
                }
            ]
* RPnary RPRel [RelationalPredicate] --- potentially recursive

      RPnary RPis
        [ RPMT
            [ MTT "income tax component" ]
        , RPnary RPproduct
            [ RPMT
                [ MTT "annualIncome" ]
            , RPMT
                [ MTT "incomeTaxRate" ]
            ]
        ]

-----------------------------------
Another way of carving up the space
-----------------------------------

---- Set Var ----------------------------------------

-------- Set Var to ...

    A hc with no body, and where RPis in head

    , HC
        { hHead = RPnary RPis
            [ RPMT
                [ MTT "taxesPayableAlive" ]
            , RPnary RPsum
                [ RPMT
                    [ MTT "income tax component" ]
                , RPMT
                    [ MTT "asset tax component" ]
                ]
            ]
        , hBody = Nothing
        }

---- Fn app ----------------------------------------

    hBody = Just
                ( All Nothing
                    [ Leaf
                        ( RPConstraint
                            [ MTT "ind" ] RPis
                            [ MTT "Singapore citizen" ]
                        )


---------------------

Block of clauses examples

  * example of recursive sum / prod

      , HC
          { hHead = RPnary RPis
              [ RPMT
                  [ MTT "o3b" ]
              , RPnary RPsum
                  [ RPnary RPproduct
                      [ RPMT
                          [ MTT "o1" ]
                      , RPMT
                          [ MTF 1.0e-2 ]
                      ]
                  , RPnary RPproduct
                      [ RPMT
                          [ MTT "o2" ]
                      , RPMT
                          [ MTF 7.0e-2 ]
                      ]
                  ]
              ]
          , hBody = Nothing
          }

-}

{-
what im trying to think thru:

* should i have the compiler work thorugh the rules sequentially, rather than mapping over thme?
* shld i keep track of the global vars (including undeclared ones tt we can infer are vars)?


sat evening:
OK I think main thing is must have a way of keeping trakc of what the global vs local vars are
b/c in L4 it might be possible to declare and initalize a global var from, e.g., within an if block, whereas that may not be possible in other langs (in some other langs may need to decalre first)
-}

---------------- Errors related ---------------------------------------------------

type Msg = T.Text
type ShowReprOfData = T.Text
{- | TODO: Add SrcPositn;
     also would be nice to just do something like NotYetImplemented :: Show a -> a -> ErrorMsg -> ToLCError
-}
data ToLCError = NotYetImplemented ShowReprOfData Msg
               | ParserProblem ShowReprOfData Msg
               | NotSupported ShowReprOfData Msg
               | MiscError ShowReprOfData Msg
               | ErrImpossible ShowReprOfData Msg
  deriving stock (Eq, Show, Generic)
  deriving anyclass Hashable

stringifyToLCError :: ToLCError -> T.Text
stringifyToLCError = \case
  NotYetImplemented repr msg -> go "NotYetImplemented" repr msg
  ParserProblem repr msg -> go "ParserProblem" repr msg
  NotSupported repr msg -> go "NotSupported" repr msg
  MiscError repr msg -> go "MiscError" repr msg
  ErrImpossible repr msg -> go "ErrImpossible" repr msg
  where
    go (s :: T.Text) repr msg = [i|#{s}: #{repr} msg: #{msg}|]

---- Specialized error throwing convenience funcs ---------------------------------

-- | TODO: make this better with `pretty` and better structured errors later; see also `diagnose` package
throwErrorBase :: (Error e :> '[Error ToLCError], Show p) => (ShowReprOfData -> Msg -> e) -> p -> Msg -> ToLC a
throwErrorBase errorType l4ds =
  mkToLC . throwError . errorType (T.pack . show $ l4ds)

throwNotYetImplError :: Show a => a -> ToLC b
throwNotYetImplError l4ds = throwErrorBase NotYetImplemented l4ds ""

throwNotSupportedError :: Show a => a -> ToLC b
throwNotSupportedError l4ds = throwErrorBase NotSupported l4ds ""

throwNotSupportedWithMsgError :: Show a => a -> T.Text  -> ToLC b
throwNotSupportedWithMsgError = throwErrorBase NotSupported

throwParserProblemWithMsg :: Show a => a -> T.Text -> ToLC c
throwParserProblemWithMsg = throwErrorBase ParserProblem

throwErrorImpossibleWithMsg :: Show a => a -> T.Text -> ToLC c
throwErrorImpossibleWithMsg = throwErrorBase ErrImpossible

-------- Env -----------------------------------------------------------------------

type VarTypeDeclMap = HM.HashMap Var (Maybe L4EntType)
type RetVarInfo = [(Var, Maybe L4EntType)]
data UserDefinedFun = MkUserFun {
    getFunName :: Var
  , getFunDef :: Exp
  , getBoundVars :: [Var]
  , getOperMP :: Operator Parser BaseExp
  }

instance Show UserDefinedFun where
  show = showUserDefinedFun

showUserDefinedFun f = [i|#{fname f} #{args} = #{getFunDef f}|]
  where
    fname (getFunName -> MkVar v) = v
    args = T.unwords [v | MkVar v <- getBoundVars f]

data Env =
  MkEnv { localVars :: VarTypeDeclMap
        -- ^ vars declared in GIVENs and WHERE (but NOT including those declared in GIVETH)
        -- , retVarInfo :: RetVarInfo
          -- ^ not sure we need retVarInfo
        , userDefinedFuns :: [UserDefinedFun]
        , currSrcPos :: SrcPositn
        , logConfig :: LogConfig }
  deriving stock (Generic)

initialEnv :: Env
initialEnv = MkEnv { localVars = HM.empty
                   , userDefinedFuns = []
                   , currSrcPos = MkPositn 0 0
                   , logConfig = defaultLogConfig }

addCustomFuns :: [UserDefinedFun] -> Env -> Env
addCustomFuns funs env = env {userDefinedFuns = funs <> userDefinedFuns env}

--withCustomFuns :: [UserDefinedFun] -> a
withCustomFuns funs = over _ToLC (local $ addCustomFuns funs)

-- vars and vals
getTypeString :: MTExpr -> String
getTypeString = \case
  MTT _ -> "text"
  MTI _ -> "integer"
  MTF _ -> "double"
  MTB _ -> "boolean"

getVarVals :: L4.Rule -> HM.HashMap String String
getVarVals = \case
  L4.Hornlike { given = givens, clauses = hornClauses } ->
    let givensMap = extractFromGivens givens HM.empty
        clausesMap = extractFromHornClauses hornClauses HM.empty
    in givensMap `HM.union` clausesMap
  _ -> mempty

extractFromGivens :: Maybe L4.ParamText -> HM.HashMap String String -> HM.HashMap String String
extractFromGivens Nothing acc = acc
extractFromGivens (Just (givens)) acc =
  foldr (\(mtExpr NE.:| _, maybeTypeSig) hm ->
          case (mtExpr, maybeTypeSig) of
            (MTT t, Just (SimpleType _ ts)) -> HM.insert [i|#{t}|] [i|#{ts}|] hm
            (MTT t, Just (InlineEnum _ values)) -> HM.insert [i|#{t}|] [i|#{pt2text values}|] hm
            _ -> hm) acc givens

extractFromHornClauses :: [HornClause2] -> HM.HashMap String String -> HM.HashMap String String
extractFromHornClauses hornClauses hashmap =
  foldr extractFromHornClause hashmap hornClauses

extractFromHornClause :: HornClause2 -> HM.HashMap String String -> HM.HashMap String String
extractFromHornClause (hHead -> RPConstraint vars _ expr) hashmap =
  case expr of
    [MTF val] -> go $ show val
    [MTI val] -> go $ show val
    [MTT txt] -> go $ T.unpack txt
    _ -> hashmap
  where
    go str =
      foldr
        ( \case
            MTT var -> HM.insert (T.unpack var) str
            _ -> id
        )
        hashmap
        vars

extractFromHornClause _ hashmap = hashmap

------------------------------------------------------------------------------------

{- | TODO: Revise this when time permits --- this is not 'idiomatic'
something like the following more idiomatic:
  type ToLCEffs es =
    ( Reader Env :> es
    , State GlobalVars :> es
    , ValidateT (HS.HashSet ToLCError) :> es
    )

Resources (stuff on other effects libs also translates well to `Effectful`):
  https://haskell-explained.gitlab.io/blog/posts/2019/07/28/polysemy-is-cool-part-1/
  https://code.well-typed.com/cclaw/haskell-course/-/blob/main/error-handling/ErrorHandling.hs
-}
newtype ToLC a =
  ToLC (Eff '[Reader Env,
              EffState.State [UserDefinedFun],
              Error ToLCError] a
        )
  deriving newtype (Functor, Applicative, Monad)

_ToLC :: Iso' (ToLC a) (Eff '[Reader Env, EffState.State [UserDefinedFun], Error ToLCError] a)
_ToLC = coerced

mkToLC :: Eff [Reader Env, EffState.State [UserDefinedFun], Error ToLCError] a -> ToLC a
mkToLC = view (re _ToLC)

unToLC :: ToLC a -> Eff [Reader Env, EffState.State [UserDefinedFun], Error ToLCError] a
unToLC = view _ToLC

runToLC :: ToLC a -> Either ToLCError a
runToLC m = case runToLC' m of
              Right (res,_state) -> Right res
              Left err           -> Left err

execToLC :: ToLC a -> [UserDefinedFun]
execToLC m = case runToLC' m of
              Right (_res,state) -> state
              Left _             -> []


runToLC' :: ToLC a -> Either ToLCError (a, [UserDefinedFun])
runToLC' (unToLC -> m) =
  m
    |> runReader initialEnv
    |> EffState.runStateLocal []
    |> runErrorNoCallStack
    |> runPureEff
  -- where
    -- initialState = mkGlobalVars HM.empty

--------------------------------------------------------------------------------------------------


{- | Translate L4 program consisting of Hornlike rules to a LC Program -}
l4ToLCProgram :: Traversable t => t L4.Rule -> ToLC LCProgram
l4ToLCProgram rules = do
  l4HLs <- traverse simplifyL4Hlike rules
  let customOpers = execToLC $ traverse expifyHL l4HLs -- to fill env with user-defined functions
      customOpers' = execToLC $ withCustomFuns customOpers $ traverse (expifyHL) l4HLs

  lcProg <- withCustomFuns customOpers' $ traverse (expifyHL' True) l4HLs
  pure MkLCProgram { progMetadata = MkLCProgMdata "[TODO] Not Yet Implemented"
                   , lcProgram = programWithoutUserFuns lcProg
                   , globalVars = mkGlobalVars $ HM.unions $ shcGiven <$> F.toList l4HLs
                   , giveths = giveths
                   , userFuns = getUserFuns customOpers'}
  where
    giveths :: [T.Text]
    giveths = [pt2text pt | Just pt <- L4.giveth <$> F.toList rules]

    getUserFuns :: [UserDefinedFun] -> HM.HashMap String ([Var], Exp)
    getUserFuns opers = HM.fromList [
      (T.unpack var, (getBoundVars fun, getFunDef fun))
      | fun@(getFunName -> MkVar var) <- opers]
--      | fun@(getFunName -> MkVar var) e boundVars _ <- opers]

    -- Separate user functions from the body of the program (mostly because
    -- I don't want to handle them in ToMathLang the same way as the rest /Inari)
    programWithoutUserFuns prog = [e | e <- F.toList prog, notLambda e]
      where
        notLambda (exp -> ELam {}) = False
        notLambda _ = True

{-==============================================================================
  1. Simplify L4: massage L4.Rule (Hornlike) into the more convenient SimpleHL
===============================================================================-}

type SimpleHL = SimpleHlike VarTypeDeclMap RetVarInfo (Maybe RuleLabel)

simplifyL4Hlike :: L4.Rule -> ToLC SimpleHL
simplifyL4Hlike rule =
  case rule.srcref of
    Just srcref -> do
      baseHL <- extractBaseHL rule
      pure MkSimpleHL { shcSrcRef = srcref
                      , shcGiven = maybe HM.empty mkVarEntMap rule.given
                      , shcRet  = rule.giveth ^.. folded % folding mkL4VarTypeDeclAssocList
                      , baseHL = baseHL
                      , shRLabel = rule.rlabel
                      }
    Nothing -> throwParserProblemWithMsg rule "Parser should not be returning L4 rules with Nothing in src ref"
{- this always takes up more time than one expects:
given :: Maybe ParamText = Maybe (NonEmpty TypedMulti)
        = Maybe (NonEmpty
                    (NonEmpty MTExpr, Maybe TypeSig))
-}

l4HcToAtomicHC :: L4.HornClause2 -> AtomicHC
l4HcToAtomicHC hc =
  case hc.hBody of
    Just hbody -> HeadAndBody MkHnBHC {hbHead = hc.hHead, hbBody = hbody}
    Nothing -> mkHeadOnlyAtomicHC hc.hHead

extractBaseHL :: L4.Rule -> ToLC BaseHL
extractBaseHL rule =
  case rule.clauses of
    [] -> throwParserProblemWithMsg rule "Parser should not return L4 Hornlikes with no clauses"
    [hc] -> pure $ OneClause $ l4HcToAtomicHC hc
    hc : hcs ->
      (hc NE.:| hcs)
        |$> l4HcToAtomicHC |> mkMultiClauseHL |> MultiClause |> pure

--- Utils for dealing with 'Maybe ParamText' -------------------------------------
mkL4VarTypeDeclAssocList :: Foldable f => f TypedMulti -> [(Var, Maybe L4EntType)]
mkL4VarTypeDeclAssocList = convertL4Types . declaredVarsToAssocList
  where
    declaredVarsToAssocList :: Foldable f => f TypedMulti -> [(T.Text, Maybe TypeSig)]
    declaredVarsToAssocList dvars = dvars ^.. folded % to getGiven % folded

    convertL4Types :: [(T.Text, Maybe TypeSig)] -> [(Var, Maybe L4EntType)]
    convertL4Types = each % _1 %~ mkVar >>> each % _2 %~ fmap mkEntType

mkVarEntMap :: Foldable f => f TypedMulti -> VarTypeDeclMap
mkVarEntMap = HM.fromList . mkL4VarTypeDeclAssocList

{-================================================================================
  2. SimpleHL to LC Exp
=================================================================================-}

srcRefToSrcPos :: SrcRef -> SrcPositn
srcRefToSrcPos sr = MkPositn { row = sr.srcrow, col = sr.srccol }

-- | Convenience fn: For when there's no (additional) metadata to add to base exp
noExtraMdata :: BaseExp -> Exp
noExtraMdata baseexp = MkExp baseexp []

typeMdata :: SrcPositn -> T.Text -> BaseExp -> Exp
typeMdata pos typ bexp = MkExp bexp (inferredType pos [] typ)

inferredType :: SrcPositn -> MdGrp -> T.Text -> MdGrp
inferredType pos [] typ = [ MkExpMetadata
                              pos
                          (Just $ Inferred typ)
                          Nothing
                      ]
inferredType _pos mds _typ = mds -- md {typeLabel = Just $ Inferred typ}:mds


-- | Treat the seq of L4 rules as being a block of statements
l4sHLsToLCExp :: [SimpleHL] -> ToLC Exp
l4sHLsToLCExp rules = fmap mkExpFrSeqExp (l4sHLsToLCSeqExp rules)
  where
    mkExpFrSeqExp :: SeqExp -> Exp
    mkExpFrSeqExp seqExp = noExtraMdata (ESeq seqExp)
    -- TODO: Can add metadata here in the future if needed.
    -- Bear in mind already adding metadata at level of L4Prog and in `expifyHL`

{- | Right now I don't think the order in which we compile the HLs actually matters,
so just using a foldrM -}
l4sHLsToLCSeqExp ::  [SimpleHL] -> ToLC SeqExp
l4sHLsToLCSeqExp = F.foldrM go mempty
  where
    go :: SimpleHL -> SeqExp -> ToLC SeqExp
    go hornlike seqExp = consSE <$> expifyHL hornlike <*> pure seqExp

--------------------------------------------------------------------
expifyHL = expifyHL' False

expifyHL' :: Bool -> SimpleHL -> ToLC Exp
expifyHL' verbose hl = do
  userFuns <- ToLC $ asks userDefinedFuns
  bexp <- trace (if verbose then [i|\nexpifyHL: userFuns = #{getFunName <$> userFuns}\n          hl = #{hl}\n|] else "") $ baseExpify userFuns hl
  return $ MkExp bexp [] -- using mdata here puts it in weird place! but we don't care about types so much at this stage so leave it empty for now
--  return $ MkExp bexp [mdata]
   where
    returnType = case hl.shcRet of
      [(_, mReturnType)] -> mReturnType
      [] -> Nothing
      xs -> fail [i|expifyHL: the SimpleHL's shcRet has multiple return types, is this weird? #{xs}|]
      -- TODO: when would this list have more than 1 element? if there are multiple GIVETHs?

    _mdata = MkExpMetadata {
              srcPos = srcRefToSrcPos $ shcSrcRef hl
            , typeLabel = FromUser <$> returnType
            , explnAnnot = Nothing
            }

{- | My current understanding is that the LC Exps that a SimpleHL can be are:
1. If Then (currently converted into If Then Else in MathLang—maybe already do the transformation here?)
2. Lam Def
3. Block of statements / expressions (TODO)
-}
baseExpify :: [UserDefinedFun] -> SimpleHL -> ToLC BaseExp
baseExpify funs (runToLC . isLambda funs -> Right (operator, v:vs, bexp)) = do
  trace [i|baseExpify: got a fun #{operator}|] $
    mkToLC $ EffState.modify $ (operator :)
  ELam v <$> mkLambda vs bexp

baseExpify _funs hl@(shRLabel -> Just (_section, _number, rname)) = do
  bexp <- baseExpify _funs $ hl {shRLabel = Nothing}
  mkVarSetFromVar (MkVar rname) (noExtraMdata bexp)

baseExpify _ (isIf -> Just (hl, hc)) = toIfExp hl hc
baseExpify _ hl@(baseHL -> OneClause (HeadOnly hc)) = toHeadOnlyExp hl hc
baseExpify _ (isMultiClause -> hls@(_:_)) = ESeq <$> l4sHLsToLCSeqExp hls
baseExpify _ hornlike = throwNotYetImplError hornlike
-- for the future, remove the catch all `baseExpify hornlike` and add stuff like
-- baseExpify (isLamDef -> ...) = ...
-- baseExpify (isBlockOfExps -> ...)) = ...

mkLambda :: [Var] -> BaseExp -> ToLC Exp
mkLambda [] bexp = do
  pos <- mkToLC $ asks currSrcPos
  pure $ typeMdata pos "Function" bexp

mkLambda (v:vs) bexp = do
  pos <- mkToLC $ asks currSrcPos
  let funType = typeMdata pos "Function"
  funType . ELam v <$> mkLambda vs bexp

isIf :: SimpleHL -> Maybe (SimpleHL, HnBodHC)
isIf hl =
  case hl.baseHL of
    MultiClause _ -> Nothing
    OneClause atomicHC ->
      case atomicHC of
        HeadOnly _ -> Nothing
        HeadAndBody hc -> Just (hl, hc)

-- we try to parse the following forms:
-- (GIVEN x y) DECIDE,x,discounted by,y,IS,x * (1 - y)
-- (GIVEN x) DECIDE,prefixF,x,IS,x * (1 - y) -- y may be a global variable or whatever, we don't care
-- (GIVEN x) DECIDE,x,postfixF,IS,x * (1 - y) -- as above
isLambda :: [UserDefinedFun] -> SimpleHL -> ToLC (UserDefinedFun, [Var], BaseExp)
isLambda funs hl = case HM.keys hl.shcGiven of
  []   -> throwNotSupportedWithMsgError hl "not a function, because no given arguments"
  givens -> case hl.baseHL of
    OneClause (isConstraint -> Just (lhs, rhs)) -> do
      -- Step 1: is the LHS of the form `f x`, `x f y` or `x f`?
      -- This step throws error if not.
      (fname, boundVars, operMP) <- mkOperator lhs givens

      -- Step 2: parse RHS and see if all bound variables are in the body
      let funNames = fmap getFunName funs
      exprRHS <- trace [i|\nisLambda.funs = #{funNames}\n|] $
        withCustomFuns funs $ baseExpifyMTEs' "called from isLambda " rhs
      let varsRHS = MkExp exprRHS [] ^.. cosmosOf (gplate @Exp) % gplate @Var
          nonFunVarsRHS = [v | v <- varsRHS, v `notElem` funNames]
      if all (`elem` nonFunVarsRHS) givens
          then pure $ ( MkUserFun fname (noExtraMdata exprRHS) boundVars operMP
                      , boundVars
                      , exprRHS )
          else trace [i|\n!!! #{nonFunVarsRHS} != #{givens} !!!\n\n\n|] $ throwNotSupportedWithMsgError ([i|#{nonFunVarsRHS} != #{givens}|] :: T.Text) "not all varsInExprs defined"

    _ -> throwNotSupportedWithMsgError hl "only checking for lambdas in RPConstraints in single clauses"
  where
    isConstraint (HeadOnly (hcHead -> RPConstraint lhs RPis rhs)) = Just (lhs, rhs)
    isConstraint _ = Nothing

    -- Split into version with Maybe, so I can call it and check if it succeeds without the whole calling function erroring
    mkOperator :: [MTExpr] -> [Var] -> ToLC (Var, [Var], Operator Parser BaseExp)
    mkOperator mtes vars = do
      pos <- mkToLC $ asks currSrcPos
      case maybeOperator pos mtes vars of
        Just op -> pure op
        Nothing -> throwNotSupportedWithMsgError "isLambda:" [i|not a lambda expression: #{mtes}|]

    -- We require explicit arguments, otherwise impossible to distinguish from normal variable assignment
    maybeOperator :: SrcPositn -> [MTExpr] -> [Var] -> Maybe (Var, [Var], Operator Parser BaseExp)
    maybeOperator pos [MTT f, MTT x] [var]
      | MkVar f /= var && MkVar x == var =
      let mkBexp = customUnary (MkVar f) pos
      in Just (
        MkVar f,                   -- fun name
        [var],                     -- bound vars
        prefix f mkBexp)           -- megaparsec operator
    maybeOperator pos [MTT x, MTT f] [var]
      | MkVar f /= var && MkVar x == var =
      let mkBexp = customUnary (MkVar f) pos
      in Just (
        MkVar f,                   -- fun name
        [var],                     -- bound vars
        postfix f mkBexp)           -- megaparsec operator
    maybeOperator pos [MTT x, MTT f, MTT y] vs@[_, _]
      | all (`elem` vs) [vx, vy]
        && f `notElem` $(TH.lift allArithOps)
        = Just (
            MkVar f,                     -- fun name
            [vx, vy],                    -- bound vars
            binary f mkBexp)             -- megaparsec operator
      where
        mkBexp = customBinary (MkVar f) pos
        (vx, vy) = (MkVar x, MkVar y)
    maybeOperator _pos _mtes _vars = Nothing

isMultiClause :: SimpleHL -> [SimpleHL]
isMultiClause hl =
  case hl.baseHL of
    MultiClause (MkMultiClauseHL hls) -> keepOgHL <$> NE.toList hls
    _ -> []
  where
    keepOgHL :: AtomicHC -> SimpleHL
    keepOgHL hc = hl {baseHL = OneClause hc}

toIfExp :: SimpleHL -> HnBodHC -> ToLC BaseExp
toIfExp hl hc = do
  condE <- withLocalVarsAndSrcPos $ processHcBody hc.hbBody
  thenE <- withLocalVarsAndSrcPos $ processHcHeadForIf hc.hbHead
  case thenE.exp of
    EPredSet var (exp -> ELit EBoolTrue) -> pure $ EPredSet var condE
    _ -> pure $ EIfThen condE thenE
  where
    withLocalVarsAndSrcPos = over _ToLC (local $ setCurrSrcPos . setLocalVars)
    setCurrSrcPos, setLocalVars :: Env -> Env
    setCurrSrcPos = set #currSrcPos (srcRefToSrcPos hl.shcSrcRef)
    setLocalVars = set #localVars hl.shcGiven
-- TODO: Add metadata from hl
-- TODO: If Then with ELSE for v2

toHeadOnlyExp :: SimpleHL -> HeadOnlyHC -> ToLC BaseExp
toHeadOnlyExp hl hc =
  withLocalVarsAndSrcPos $ exp <$> processHcHeadForIf hc.hcHead
  where
    withLocalVarsAndSrcPos = over _ToLC (local $ setCurrSrcPos . setLocalVars)
    setCurrSrcPos, setLocalVars :: Env -> Env
    setCurrSrcPos = set #currSrcPos (srcRefToSrcPos hl.shcSrcRef)
    setLocalVars = set #localVars hl.shcGiven


--------------------- Head of HL and Var Set ------------------------------------------------------------

{- | Preconditions / assumptions: The L4 rule that's being processed corresponds to an EIf in our LC AST;
i.e., the RelationalPredicate comes from an L4 rule tt's been identified as corresponding to an EIf

What can be in hHead if the ambient L4 rule is an EIf?
  Set Var
    (i) Simple Set Var:
      * `RPConstraint [ MTT "n3c" ] RPis [ MTT "n1 + n2" ]`
    (ii) Set Var True (typically with an IF):
      * `RPMT [ MTT "case 1 qualifies" ]

Things like arithmetic constraints (<, >, etc)
don't appear here -- they appear in hBody
-}
processHcHeadForIf :: L4.RelationalPredicate -> ToLC Exp
processHcHeadForIf (isSetVarToTrue -> Just putativeVar) = do
  pos <- mkToLC $ asks currSrcPos
  pure $ noExtraMdata $
            EPredSet (mkVar . textifyMTEs $ putativeVar)
            (typeMdata pos "Boolean" $ ELit EBoolTrue)
  --noExtraMdata <$> mkSetVarTrue putativeVar
processHcHeadForIf rp = expifyHeadRP rp
-- processHcHeadForIf rp = throwNotSupportedError rp

{- Note that processing hcHead for *function definitions* would need to consider cases like the following,
   though do check if the conventions have changed --- this example might be outdated

      HC
        { hHead = RPMT
            [ MTT "ind"
            , MTT "meets the property eligibility criteria for GSTV-Cash"
            ]
        , hBody = Just
            ( Any Nothing
                [ Not
                    ( Leaf
                        ( RPMT
                            [ MTT "ind"
                            , MTT "owns more than one property"
                            ]
                        )
                    )
                , Leaf
                    ( RPMT
                        [ MTT "ind"
                        , MTT "owns 2 or more HDB flats and no other property"
                        ] ) ] ) }
-}

isSetVarToTrue :: L4.RelationalPredicate -> Maybe [MTExpr]
isSetVarToTrue = \case
  RPMT mtes  -> Just mtes
  _ -> Nothing
    -- TODO: think about how to handle metadata specifically for vars (and if that is even necessary)

{- | Is a Set Var that's NOT Set Var to True
      eg: `RPConstraint [ MTT "n3c" ] RPis [ MTT "n1 + n2" ]`
-}
isOtherSetVar :: L4.RelationalPredicate -> Maybe ([MTExpr], [MTExpr])
isOtherSetVar = \case
  RPConstraint lefts RPis rights -> Just (lefts, rights)
--  RPnary RPis [MTT mtes, rp] -- this is handled in expifyHeadRP
  _ -> Nothing

{- |
We want to handle things like
  * `[ MTT "n1 + n2" ]`, from `RPConstraint [ MTT "n3c" ] RPis [ MTT "n1 + n2" ]`

To handle arithmetic parsing, try Control.Monad.Combinators.Expr (https://github.com/mrkkrp/parser-combinators/Control/Monad/Combinators/Expr.hs)

One future complication I can see has to do with fun app (when it appears inline).
  EG: 'n1 + f n2'
If we cannot tell from the syntax alone whether
something is meant to be a func / 'in the func position of a func app',
we'd prob need to do a prelim pass to find all the function defns / declarations first

TODO: Think about what kind of validation we might want to do here

NOTE: If it seems like literals will appear here (e.g. number literals), see defn of `mteToLitExp` for how to translate to literals
-}

isFun :: [Var] -> MTExpr -> Bool
isFun funs mte =
  let ops = $(TH.lift allArithOps)
      allFuns = foldr (Set.insert . varAsTxt) ops funs
  in case mte of
    MTT t -> t `elem` allFuns
    _ -> False

baseExpifyMTEs = baseExpifyMTEs' ""

baseExpifyMTEs' :: String -> [MTExpr] -> ToLC BaseExp
baseExpifyMTEs' _ (splitGenitives -> (Just g, rest@(_:_))) = do
  userFuns <- mkToLC $ asks $ fmap getFunName . userDefinedFuns -- :: [Var]
  recname <- expifyMTEsNoMd [g]
  case break (isFun userFuns) rest of
  -- ind's parent's sibling's … income
    (_x:_xs,[]) -> do
      fieldname <- expifyMTEsNoMd rest -- income
      pure $ ERec fieldname recname

  -- ind's parent's sibling's … income, discountedBy, foo
    (x:xs,f:ys) -> do
      fieldname <- expifyMTEsNoMd (x:xs) -- income
      fExp <- expifyMTEsNoMd [f]
      arg2 <- expifyMTEsNoMd ys
      let arg1 = ERec fieldname recname
          fArg1 = noExtraMdata $ EApp fExp $ noExtraMdata arg1
      pure $ EApp fArg1 arg2
    _ -> throwErrorImpossibleWithMsg g "shouldn't happen because we matched that the stuff after genitives is not empty"

baseExpifyMTEs' msg mtes = do
  userFuns <- mkToLC $ asks $ fmap getFunName . userDefinedFuns -- :: [Var]
  case mtes of
    [mte] -> do
      -- Inari: assuming that the Var will be used for comparison
      -- because SetVar is handled elsewhere, by extracting it
      -- from RPConstraint
      mvar <- isDeclaredVar mte
      case mvar of
        Just var -> return $ EVar var
        Nothing -> parseExpr mte

    [mte1@(MTT _), mte2@(MTT _)] -> do
      mvar1 <- isDeclaredVar mte1
      mvar2 <- isDeclaredVar mte2
      case (mvar1, mvar2) of
        (Just var1, Nothing) -> do -- "ind","qualifies" = qualifies(ind)
          varWeAssumeToBePred <- varFromMTEs [mte2]
          fExp <- mkVarExp varWeAssumeToBePred
          pure $ EApp fExp $ noExtraMdata $ EVar var1

        (Nothing, Just var2) -> do -- "qualifies","ind" = qualifies(ind)
          varWeAssumeToBePred <- varFromMTEs [mte1]
          fExp <- mkVarExp varWeAssumeToBePred
          pure $ EApp fExp $ noExtraMdata $ EVar var2

        (Nothing, Nothing)
          -> throwNotSupportedWithMsgError (RPMT mtes)
                "baseExpifyMTEs: trying to apply non-function"

        (Just var1, Just var2) -> do
          case fmap (`elem` userFuns) [var1, var2] of
            [True, False] -> do
              let f = noExtraMdata $ EVar var1
                  x = noExtraMdata $ EVar var2
              pure $ EApp f x
            [False, True] -> do
              let f = noExtraMdata (EVar var2)
                  x = noExtraMdata (EVar var1)
              pure $ EApp f x
            [True, True] -> throwNotSupportedWithMsgError (RPMT mtes) "baseExpifyMTEs: trying to apply function to another function—we probably don't support that"
            _ -> throwNotSupportedWithMsgError (RPMT mtes) "baseExpifyMTEs: trying to apply non-function"

--    mtes -> do
    mtes -> trace (msg <> [i|userFuns = #{userFuns}|] ) $ do

      case break (isFun userFuns) mtes of
      -- function, rest
        ([],f:ys) -> do
          arg <- expifyMTEsNoMd ys
          assumedVar <- varFromMTEs [f]
          fExp <- mkVarExp assumedVar
          pure $ EApp fExp arg

      -- mte1 [, …, mteN], function, rest
        (xs,f:ys) -> do
          arg1 <- trace [i|baseExpifyMTEs: found userfun #{f}|] $ expifyMTEsNoMd xs
          arg2 <- expifyMTEsNoMd ys
          -- since f is in userfuns, we can parse it using parseExpr. replacing args to some dummy x and y (but will fail if there is a userfun called x or y ¯\_(ツ)_/¯)
          bexp <- parseExpr $ MTT $ T.unwords ["x", mtexpr2text f, "y"]
          case bexp of
            ENumOp {} -> pure $ ENumOp bexp.numOp arg1 arg2
            ECompOp {} -> pure $ ECompOp bexp.compOp arg1 arg2
            _ -> do assumedVar <- varFromMTEs [f]
                    fExp <- mkVarExp assumedVar
                    let fArg1 = noExtraMdata (EApp fExp arg1)
                    pure $ EApp fArg1 arg2

      -- no userfuns here
        (_xs,[]) -> do
          let parenExp = MTT $ textifyMTEs $ parenExps mtes
          expParsedAsText <- parseExpr parenExp
          case expParsedAsText of
            ELit _ -> trace [i|parseExpr returned this as a string literal: #{expParsedAsText}|] do
            -- TODO: this should definitely not be a sequence, what should it be instead???
              parsedExs <- traverse parseExpr mtes
              return $ ESeq $ foldr consSeqExp mempty parsedExs

            -- arithmetic expression like [MTT "m1",MTT "*",MTT "m2"]
            notStringLit -> return notStringLit
  where
    consSeqExp :: BaseExp -> SeqExp -> SeqExp
    consSeqExp = consSE . noExtraMdata

    parenExps :: [MTExpr] -> [MTExpr]
    parenExps mtes
      | any (isOp . mtexpr2text) mtes = parenNestedExprs <$> mtes
      | otherwise = mtes

    isOp :: T.Text -> Bool
    isOp = (`elem` $(TH.lift arithOps))

    -- don't parenthesize single variables or literals, like "taxesPayable" or "Singapore citizen"
    -- do parenthesize "(x + 6)"
    parenNestedExprs :: MTExpr -> MTExpr
    parenNestedExprs = mtexpr2text >>> parenNE >>> MTT
      where
        parenNE :: T.Text -> T.Text
        parenNE text
          | text PCRE.≈ [PCRE.re| |\+|\*|\-|\/|] = trace [i|added parentheses (#{text})|] [i|(#{text})|]
          | otherwise = text

parseExpr :: MTExpr -> ToLC BaseExp
parseExpr x@(MTT str) = do
  res <- runParserT pExpr "" str
  case res of
    -- if it's just a String literal, don't use the megaparsec version—it removes whitespace, e.g. "Singapore citizen" -> "Singapore"
    Right (EVar _) -> pure $ EVar $ MkVar str
    Right notStringLit -> pure notStringLit
    Left _error -> trace [i|can't parse with pExpr: #{x}|] return $ mteToLitExp x
parseExpr x = pure $ mteToLitExp x

splitGenitives :: [MTExpr] -> (Maybe MTExpr, [MTExpr])
splitGenitives [] = (Nothing, [])
splitGenitives (mte:mtes) = (isGenitive mte, mtes)
  where
    -- removes the genitive s if it is genitive
    isGenitive :: MTExpr -> Maybe MTExpr
    isGenitive (MTT text) =
      text
        |> T.stripSuffix "'s"
        |$> MTT
    isGenitive _ = Nothing

---------------- Expr parser ------------------------------------------------------
--type Parser = Parsec Void T.Text
type Parser = ParsecT Void T.Text ToLC

pExpr :: Parser BaseExp
pExpr = do
  pos <- lift $ mkToLC $ asks currSrcPos
  customOpers <- lift $ mkToLC $ asks userDefinedFuns
  --trace [i|pExpr: length customOpers = #{length customOpers}|]
  makeExprParser pTerm (fmap getOperMP customOpers : table pos) <?> "expression"

--pTerm = parens pExpr <|> pIdentifier <?> "term"
pTerm :: Parser BaseExp
pTerm = choice $ map try
  [ parens pExpr
  , pEnum
  , pMoney
  , pVariable
  , pFloat
  , pInteger
  ]

table :: SrcPositn -> [[Operator Parser BaseExp]]
table pos = [ [ binary  "*" (mul' pos)
            , binary  "/" (div' pos) ]
          , [ binary  "+" (plus' pos)
            , binary  "-" (minus' pos) ]
          , [ binary  "<" (lt' pos)
            , binary  "<=" (lte' pos)
            , binary  ">" (gt' pos)
            , binary  ">=" (gte' pos)
            , binary  "==" (numeq' pos)]
        ]

binary :: T.Text -> (BaseExp -> BaseExp -> BaseExp) -> Operator Parser BaseExp
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: T.Text -> (BaseExp -> BaseExp) -> Operator Parser BaseExp
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

mul', div', plus', minus', lt', lte', gt', gte', numeq' :: SrcPositn -> BaseExp -> BaseExp -> BaseExp
mul' pos x y = ENumOp OpMul (typeMdata pos "Number" x) (typeMdata pos "Number" y)
div' pos x y = ENumOp OpDiv (typeMdata pos "Number" x) (typeMdata pos "Number" y)
plus' pos x y = ENumOp OpPlus (typeMdata pos "Number" x) (typeMdata pos "Number" y)
minus' pos x y =  ENumOp OpMinus (typeMdata pos "Number" x) (typeMdata pos "Number" y)
lt' pos x y =  ECompOp OpLt (typeMdata pos "Number" x) (typeMdata pos "Number" y)
lte' pos x y =  ECompOp OpLte (typeMdata pos "Number" x) (typeMdata pos "Number" y)
gt' pos x y =  ECompOp OpGt (typeMdata pos "Number" x) (typeMdata pos "Number" y)
gte' pos x y =  ECompOp OpGte (typeMdata pos "Number" x) (typeMdata pos "Number" y)
numeq' pos x y = ECompOp OpNumEq (typeMdata pos "Number" x) (typeMdata pos "Number" y)

customUnary :: Var -> SrcPositn -> BaseExp -> BaseExp
customUnary fname pos x =
  let f = typeMdata pos "Function" (EVar fname)
  in EApp f (noExtraMdata x)

customBinary :: Var -> SrcPositn -> BaseExp -> BaseExp -> BaseExp
customBinary fname pos x y =
  let f = typeMdata pos "Function" (EVar fname)
  in EApp (noExtraMdata (EApp f (noExtraMdata x))) (noExtraMdata y)

-- TODO: should we identify already here whether things are Vars or Lits?
-- Or make everything by default Var, and later on correct if the Var is not set.

pVariable :: Parser BaseExp
pVariable = do
  putativeVar <- T.pack <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
  return $ EVar $ MkVar putativeVar

pEnum :: Parser BaseExp
pEnum = do
  localVars :: VarTypeDeclMap <- lift $ mkToLC $ asks localVars
  putativeEnumVal <- T.pack <$> lexeme (some alphaNumChar <?> "enum")
  case isDeclaredEnumTxt putativeEnumVal localVars of
    Just lit -> return $ ELit lit
    Nothing -> fail "not a declared enum"

pInteger :: Parser BaseExp
pInteger = ELit . EInteger <$> (lexeme L.decimal <* notFollowedBy (char '.')) <?> "integer"

pMoney :: Parser BaseExp
pMoney = do
  curr <- T.pack <$> lexeme pCurrency
  rest <- some $ satisfy (\x -> isDigit x || x `elem` ['.',','])
  _ <- eof
  let amount = readMaybe (filter (/= ',') rest) :: Maybe Double
  case amount of
    Just dbl -> pure $ ELit $ ECurrency curr dbl
    Nothing -> fail "unable to parse as currency"

pCurrency :: Parser String
pCurrency = choice $
  [ "USD"   <$ string "$"
  , "EUR"   <$ string "€"
  , "GBP"   <$ string "£"
  , "JPY"  <$ string "¥"
  ] <>
  [ cur <$ string (T.pack cur) | cur <- currencies]
  where
    currencies :: [String]
    currencies = [ "AUD", "BGN", "BRL", "CAD", "CHF", "CNY"
      , "CZK", "DKK", "EUR", "GBP", "HKD", "HRK", "HUF"
      , "IDR", "ILS", "INR", "JPY", "KRW", "MXN", "MYR"
      , "NOK", "NZD", "PHP", "PLN", "RON", "RUB", "SEK"
      , "SGD", "THB", "TRY", "USD", "ZAR" ]

pFloat :: Parser BaseExp
pFloat = ELit . EFloat <$> lexeme L.float <?> "float"

-- NB. assuming that a literal cannot be inside an arithmetic expression, that's why we try to consume the whole input
pLiteral :: Parser BaseExp
pLiteral = ELit . EString . T.pack <$> lexeme (some $ satisfy alphaNumOrSpaceChar <* eof)
  where
    alphaNumOrSpaceChar c = isAlphaNum c || c == ' '

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")       -- just copied and pasted this from the internet
  (L.skipBlockComment "/*" "*/") -- don't think L4 has this kind of comments but ¯\_(ツ)_/¯

expifyMTEsNoMd = expifyMTEsNoMd' "expifyMTEsNoMd "

expifyMTEsNoMd' :: String -> [MTExpr] -> ToLC Exp
expifyMTEsNoMd' msg mtes = addMetadataToVar =<< baseExpifyMTEs' (msg <> " via expifyMTEsNoMd ") mtes
 where
  addMetadataToVar :: BaseExp -> ToLC Exp
  -- This is supposed to work as follows:
  -- baseExpifyMTEs for a single [mte] only returns a Var if it is declared
  -- baseExpifyMTEs for [mte1, mte2] returns an EApp
  -- baseExpifyMTEs for [mte1, mte2, …] returns an arithmetic expression
  addMetadataToVar = \case
    EVar var -> mkVarExp var
    bexp -> return $ noExtraMdata bexp

----- Util funcs for looking up / annotating / making Vars -------------------------------------

lookupVar :: Var -> VarTypeDeclMap -> Maybe L4EntType
lookupVar var = preview (ix var % _Just)

{- | Returns (Just $ Var <txt>) iff input text (a T.Text!) corresponds to a local var (i.e., either a GIVEN or a type decl in WHERE)
(though the WHERE bit hasn't been implemented yet in rest of the code)
-}
isDeclaredVarTxt :: T.Text -> VarTypeDeclMap -> Maybe Var
isDeclaredVarTxt vartxt varTypeMap =
  let putativeVar = mkVar vartxt
  in if HM.member putativeVar varTypeMap then Just putativeVar else Nothing

isDeclaredEnumTxt :: T.Text -> VarTypeDeclMap -> Maybe Lit
isDeclaredEnumTxt valtxt varTypeMap = case enums of
  [] -> Nothing
  xs -> asum $ mkLitIfMatches valtxt <$> xs
  where
    getEnumVals (Just (L4Enum vals)) = Just vals
    getEnumVals _ = Nothing

    enums = HM.elems $ HM.mapMaybe getEnumVals varTypeMap

    mkLitIfMatches v vs
      | v `elem` vs = Just $ EENum v
      | otherwise   = Nothing

-- TODO: Look into trying to add Maybe to our ToLC transformer stack
-- | Use this to check if some MTE is a Var
isDeclaredVar :: MTExpr -> ToLC (Maybe Var)
isDeclaredVar = \case
  MTT mteTxt -> do
    localVars :: VarTypeDeclMap <- ToLC $ asks localVars
    return $ isDeclaredVarTxt mteTxt localVars
  _ -> return Nothing

-- | Annotate with TLabel metadata if available
mkVarExp :: Var -> ToLC Exp
mkVarExp var = do
  env :: Env <- ToLC ask
  let varExpMetadata = MkExpMetadata { srcPos = env.currSrcPos
                                     , typeLabel = FromUser <$> lookupVar var env.localVars
                                     , explnAnnot = Nothing --Temp plceholder; TODO
                                     }
  return $ MkExp (EVar var) [varExpMetadata]

-- | Make a var set exp, when you know it's a var
mkVarSetFromVar :: Var -> Exp -> ToLC BaseExp
mkVarSetFromVar var argE = EVarSet <$> mkVarExp var <*> pure argE

mkSetVarFromMTEsHelper :: [MTExpr] -> Exp -> ToLC BaseExp
mkSetVarFromMTEsHelper putativeVar argE = do
  var <- varFromMTEs putativeVar
  mkVarSetFromVar var argE

mkVarSetTrueFromVar :: Var
                    -> (BaseExp -> Exp)
                    -- ^ Func that augments base exp with metadata
                    -> ToLC BaseExp
mkVarSetTrueFromVar var mdFunc =
  mkVarSetFromVar var $ mdFunc $ ELit EBoolTrue

mkSetVarTrueExpFromVarNoMd :: Var -> ToLC Exp
mkSetVarTrueExpFromVarNoMd var =
  noExtraMdata <$> mkVarSetTrueFromVar var noExtraMdata

mkSetVarTrue :: [MTExpr] -> ToLC BaseExp
mkSetVarTrue putativeVar = do
  pos <- mkToLC $ asks currSrcPos
  mkSetVarFromMTEsHelper putativeVar $ typeMdata pos "Boolean" $ ELit EBoolTrue

mkOtherSetVar :: [MTExpr] -> [MTExpr] -> ToLC BaseExp
mkOtherSetVar putativeVar argMTEs = do
  arg <- noExtraMdata <$> baseExpifyMTEs' "coming from mkOtherSetVar " argMTEs
  mkSetVarFromMTEsHelper putativeVar arg

{- | TODO: Check that it meets the formatting etc requirements for a variable,
(e.g. prob don't want var names to start with numbers, and probably want to error if the MTE is a MTB)
and throw an error if not
-}
varFromMTEs :: [MTExpr] -> ToLC Var
varFromMTEs mtes = pure $
  mkVar . textifyMTEs $ mtes -- NOT doing any validation rn! TODO

textifyMTEs :: [MTExpr] -> T.Text
textifyMTEs = T.intercalate " " . fmap mtexpr2text

mteToLitExp :: MTExpr -> BaseExp
mteToLitExp = \case
  MTT txt -> ELit $ EString txt
  MTB True -> ELit EBoolTrue
  MTB False -> ELit EBoolFalse
  MTI integer -> ELit $ EInteger integer -- TODO: Can always change this if we want just one type for numbers
  MTF float -> ELit $ EFloat float

---------------- Processing HC Body --------------------------------------------

processHcBody :: L4.BoolStructR -> ToLC Exp
processHcBody bsr = do
  pos <- mkToLC $ asks currSrcPos
  case bsr of
    AA.Leaf rp -> expifyBodyRP rp
  -- TODO: Consider using the `mlbl` to augment with metadata
    AA.All _mlbl propns -> F.foldrM (makeOp pos EAnd) emptyExp propns
    AA.Any _mlbl propns -> F.foldrM (makeOp pos EOr) emptyExp propns
    AA.Not propn -> typeMdata pos "Boolean" . ENot <$> processHcBody propn
  where
    emptyExp :: Exp = noExtraMdata EEmpty

    -- TODO: Can try augmenting with `mlbl` here
    makeOp :: SrcPositn -> (Exp -> a -> BaseExp) -> L4.BoolStructR -> a -> ToLC Exp
    makeOp pos op bsr exp =
      noExtraMdata <$> ((op . toBoolEq pos <$> processHcBody bsr) <*> pure exp)

    toBoolEq pos e =
      e {exp = toBoolEqBE e.exp, md = inferredType pos e.md "Boolean"}
      where
        inferredBool = typeMdata pos "Boolean"
        toBoolEqBE _e@(ELit (EString str)) =
          let varExp = EVar (MkVar str)
          in ECompOp OpBoolEq (inferredBool varExp) $ inferredBool $ ELit EBoolTrue

        toBoolEqBE e@(EApp _ _) =
          ECompOp OpBoolEq (inferredBool e) $ inferredBool $ ELit EBoolTrue

        toBoolEqBE e = e

{- |
Helps to remember:
  * if it's in the hBody,
    with the impt exception of OTHERWISE,
    it evals to a Bool / is a Propn / Condn of some sort

So the things that can be part of hBody are:

  1. fn app

      Leaf
        ( RPConstraint
            [ MTT "ind's"
            , MTT "place of residence"
            ] RPis
            [ MTT "Singapore" ]
        )

  2. if just a var: shorthand for 'var == True'

        HC { hHead = RPConstraint
                  [ MTT "taxesPayable" ] RPis
                  [ MTT "taxesPayableAlive" ]
              , hBody = Just
                  ( Leaf
                      ( RPMT
                          [ MTT "vivacity" ]
                      ))}

  3. boolean propn

      e.g.: arithmetic comparisons
          , Leaf
              ( RPConstraint
                  [ MTT "ind's"
                  , MTT "age"
                  ] RPgte
                  [ MTI 21 ]
              )

  4. (edge case: OTHERWISE --- haven't thought too much abt this yet)
-}
expifyHeadRP :: RelationalPredicate -> ToLC Exp
expifyHeadRP = \case
  -- This is
  RPConstraint lefts RPis rights ->
    expifyHeadRP $ RPnary RPis [RPMT lefts, RPMT rights]
  RPnary RPis [RPMT [mte], rp]   -> do
    mvar <- isDeclaredVar mte
    exp <- expifyBodyRP rp
    case mvar of
      Just var -> do -- The var is given and has a type
        varExp <- mkVarExp var
        let valExp = inferTypeFromOtherExp exp varExp
        return $ noExtraMdata $ EVarSet varExp valExp

      Nothing -> do -- Var is not given, but we can try to infer it from the RHS
        pos <- ToLC $ asks currSrcPos
        varNoType <- noExtraMdata . EVar <$> varFromMTEs [mte]
        let valExp = inferTypeRHS pos exp
        let varExp = inferTypeFromOtherExp varNoType valExp
        return $ noExtraMdata $ EVarSet varExp valExp
  rp -> expifyBodyRP rp

expifyBodyRP :: RelationalPredicate -> ToLC Exp
expifyBodyRP = \case
  -- OTHERWISE
  RPMT (MTT "OTHERWISE" : _mtes) -> do
    pos <- mkToLC $ asks currSrcPos
    pure $ typeMdata pos "Bool" $ ELit EBoolTrue -- throwNotYetImplError "The IF ... OTHERWISE ... construct has not been implemented yet"

  _rp@(RPMT mtes) -> expifyMTEsNoMd' "coming from expifyBodyRP " mtes

  RPConstraint lefts rel rights ->
    expifyBodyRP $ RPnary rel [RPMT lefts, RPMT rights]

  RPnary RPis [rp1, rp2] -> do
    pos <- ToLC $ asks currSrcPos
    exp1maybeUntyped <- expifyBodyRP rp1
    exp2 <- inferTypeRHS pos <$> expifyBodyRP rp2
    let exp1 = inferTypeFromOtherExp exp1maybeUntyped exp2
    return $ noExtraMdata $ EIs exp1 exp2

  -- Numeric operations can have multiple arguments
  RPnary (rprel2numop -> Just op) rps -> do
   pos <- mkToLC $ asks currSrcPos
   exps <- mapM expifyBodyRP rps
   pure $ foldl1 (numOrCompOp pos "Number" ENumOp op) exps

  -- Comparison operations can only have two
  RPnary (rprel2compop -> Just op) rps@(_:_) -> do
   pos <- mkToLC $ asks currSrcPos
   exps <- mapM expifyBodyRP rps
   pure $ foldl1 (numOrCompOp pos "Boolean" ECompOp op) exps


  RPnary rprel _ -> throwNotSupportedWithMsgError "not implemented" [i|#{rprel}|]

  -- The other cases: Either not yet implemented or not supported, with hacky erorr msges
  rp@(RPBoolStructR {}) -> throwNotSupportedWithMsgError rp "RPBoolStructR {} case of expifyBodyRP"
  rp@(RPParamText _) -> throwNotSupportedWithMsgError rp "RPParamText _ case of expifyBodyRP"
  -- rp -> throwNotSupportedWithMsgError rp "unknown rp"
  where
    numOrCompOp :: SrcPositn -> T.Text -> (t -> Exp -> Exp -> BaseExp) -> t -> Exp -> Exp -> Exp
    numOrCompOp pos str ctor op x y =
      let coerceNumber = coerceType pos "Number"
      in typeMdata pos str $ ctor op (coerceNumber x) $ coerceNumber y

    rprel2numop :: RPRel -> Maybe NumOp
    rprel2numop = \case
      RPsum     -> Just OpPlus
      RPproduct -> Just OpMul
      RPminus   -> Just OpMinus
      RPdivide  -> Just OpDiv
      RPmodulo  -> Just OpModulo
      RPmax     -> Just OpMaxOf
      RPmin     -> Just OpMinOf
      _         -> Nothing

    rprel2compop :: RPRel -> Maybe CompOp
    rprel2compop = \case
      RPlt   -> Just OpLt
      RPlte  -> Just OpLte
      RPgt   -> Just OpGt
      RPgte  -> Just OpGte
      RPeq   -> Just OpNumEq
      _      -> Nothing

    -- inferTypeFromOtherExp already does the check whether target has empty typeLabel
    coerceType :: SrcPositn -> T.Text -> Exp -> Exp
    coerceType pos typ exp@(MkExp bexp _) =
      inferTypeFromOtherExp exp $ MkExp bexp $ inferredType pos [] typ

inferTypeFromOtherExp :: Exp -> Exp -> Exp
inferTypeFromOtherExp copyTarget copySource = case copyTarget.md of
  m:_ -> case m.typeLabel of
            Just _  -> copyTarget -- copyTarget has already a type, don't override
            Nothing -> copyTarget {md = copySource.md}
  [] -> copyTarget {md = copySource.md} -- copySource has potentially more reliable type info

inferTypeRHS :: SrcPositn -> Exp -> Exp
inferTypeRHS pos x@(MkExp bexp md) = case bexp of
  ELit (EString _) -> go "String"
  ELit (EInteger _) -> go "Number"
  ELit (EFloat _) -> go "Number"
  ENumOp{} -> go "Number"
  ECompOp{} -> go "Bool"
  _ -> x
  where
    go = MkExp bexp . inferredType pos md
--  EVar (MkVar t) -> ??? -- NB. a Var can be defined outside givens, like "incomeTaxRate,IS,0.01"
-- so when incomeTaxRate appears in another expression, we could check elsewhere in the program what its type is.