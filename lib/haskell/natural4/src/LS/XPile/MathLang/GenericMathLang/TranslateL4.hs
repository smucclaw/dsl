{-# OPTIONS_GHC -Wall -Wredundant-constraints #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, OverloadedLabels, RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications, DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}


module LS.XPile.MathLang.GenericMathLang.TranslateL4 (
  -- * Specialised Errors we may throw
  ToLCError(..),
  stringifyToLCError,
  throwNotYetImplError,
  throwErrorBase,
  throwNotSupportedWithMsgError,
  throwNotSupportedError,
  throwErrorImpossibleWithMsg,
  throwParserProblemWithMsg,
  -- * Transpilers
  l4ToLCProgram,
  -- * Helpers to run and test the transpiler
  runToLC,
  simplifyL4Hlike,
  baseExpifyMTEs,
  noExtraMetadata,
  -- * Exported for doctests, don't use them! They are private 😠
  splitGenitives,
)
where

import Control.Arrow ((>>>))
import Control.Applicative (asum)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set

import LS.XPile.MathLang.GenericMathLang.ArithOps ( allArithOps, arithOps )
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST -- TODO: Add import list
-- TODO: Haven't actually finished setting up logging infra, unfortunately.
-- But it's also not really necessary for working on the transpiler
import LS.XPile.MathLang.GenericMathLang.RPrel2opTable
    ( NumCompOp(..), rpRel2opTable )

import AnyAll qualified as AA
import LS.Types as L4
  (SrcRef(..), RelationalPredicate(..), RPRel(..), MTExpr(..),
  HornClause (..), HornClause2, BoolStructR,
  TypeSig(..), TypedMulti,
  SimpleHlike(..), BaseHL(..), AtomicHC(..), HeadOnlyHC(..),
  HeadOnlyHC, mkHeadOnlyAtomicHC,
  pattern OneClause,
  pattern MultiClause,
  HornBodyHeadClause(..),
  mtexpr2text, pt2text, pt2multiterm
  )

import LS.Rule (
                -- Interpreted(..),
                getGiven,
                RuleLabel)
import LS.Rule qualified as L4 (Rule(..))
import LS.Utils ((|$>))
import LS.XPile.Common

import Effectful (Eff, (:>), runPureEff)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Reader.Static (Reader, runReader, local, asks, ask)
import Effectful.State.Dynamic qualified as State
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Flow ((|>))
import Optics hiding ((|>))
import GHC.Generics (Generic)
import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Foldable qualified as F (toList, foldrM)
import Data.Bifunctor (bimap)

-- for parsing expressions that are just strings inside MTExpr
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import Control.Monad.Trans (lift)
import Text.Megaparsec (ParsecT, runParserT, eof, (<?>), try, some, many, between, choice, satisfy, notFollowedBy)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1, char, string)
import Data.Char (isDigit)
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Void ( Void )
import Text.Regex.PCRE.Heavy qualified as PCRE
import Text.Read (readMaybe)
import Prelude hiding (exp)
import Debug.Trace (trace)


-- $setup
-- >>> import Data.Text qualified as T
-- >>> import LS.Types
-- >>> :set -XOverloadedStrings

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
throwErrorBase :: (Error e :> es, Show p) => (ShowReprOfData -> Msg -> e) -> p -> Msg -> Eff es a
throwErrorBase errorType l4ds =
  throwError . errorType (T.pack . show $ l4ds)

throwNotYetImplError :: (Error ToLCError :> es, Show a) => a -> Eff es b
throwNotYetImplError l4ds = throwErrorBase NotYetImplemented l4ds ""

throwNotSupportedError :: (Error ToLCError :> es, Show a) => a -> Eff es b
throwNotSupportedError l4ds = throwErrorBase NotSupported l4ds ""

throwNotSupportedWithMsgError :: (Error ToLCError :> es, Show a) => a -> T.Text  -> Eff es b
throwNotSupportedWithMsgError = throwErrorBase NotSupported

throwParserProblemWithMsg :: (Error ToLCError :> es, Show a) => a -> T.Text -> Eff es c
throwParserProblemWithMsg = throwErrorBase ParserProblem

throwErrorImpossibleWithMsg :: (Error ToLCError :> es, Show a) => a -> T.Text -> Eff es c
throwErrorImpossibleWithMsg = throwErrorBase ErrImpossible

-------- Env -----------------------------------------------------------------------

type VarTypeDeclMap = HM.HashMap Var (Maybe L4EntityType)
type ReturnVarInfo = [(Var, Maybe L4EntityType)]
data UserDefinedFun = MkUserFun
  { getFunName :: Var
  , getFunDef :: Exp
  , getBoundVars :: [Var]
  , getOperMP :: Operator Parser BaseExp
  }

instance Eq UserDefinedFun where
  f == g = getFunName f == getFunName g &&
           getFunDef f == getFunDef g &&
           getBoundVars f == getBoundVars g

instance Ord UserDefinedFun where
  f `compare` g = getFunName f `compare` getFunName g <>
           getFunDef f `compare` getFunDef g <>
           getBoundVars f `compare` getBoundVars g

instance Show UserDefinedFun where
  show = showUserDefinedFun

showUserDefinedFun :: UserDefinedFun -> String
showUserDefinedFun f = [i|#{fname f} #{args} = #{getFunDef f}|]
  where
    fname (getFunName -> MkVar v) = v
    args = T.unwords [v | MkVar v <- getBoundVars f]

data Env =
  MkEnv { localVars :: VarTypeDeclMap
        -- ^ vars declared in GIVENs and WHERE (but NOT including those declared in GIVETH)
        -- , returnVarInfo :: ReturnVarInfo
          -- ^ not sure we need returnVarInfo
        , userDefinedFuns :: [UserDefinedFun]
        , currentSrcPosition :: SrcPosition }
  deriving stock (Generic)

initialEnv :: Env
initialEnv = MkEnv { localVars = HM.empty
                   , userDefinedFuns = []
                   , currentSrcPosition = MkPosition 0 0 }

addCustomFuns :: [UserDefinedFun] -> Env -> Env
addCustomFuns funs env = env {userDefinedFuns = funs <> userDefinedFuns env}

withCustomFuns :: Reader Env :> es => [UserDefinedFun] -> Eff es a -> Eff es a
withCustomFuns funs = local $ addCustomFuns funs

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
type ToLCEffs =
  [ Reader Env
  , State.State [UserDefinedFun]
  , Error ToLCError
  ]

type ParserEffs =
  '[ Reader Env ]

runToLC :: Eff ToLCEffs a -> Either ToLCError a
runToLC m = case runToLC' m of
              Right (res,_state) -> Right res
              Left err           -> Left err

execToLC :: Eff ToLCEffs a -> [UserDefinedFun]
execToLC m = case runToLC' m of
              Right (_res,state) -> state
              Left _             -> []

runToLC' :: Eff ToLCEffs a -> Either ToLCError (a, [UserDefinedFun])
runToLC' m =
  m
    |> runReader initialEnv
    |> State.runStateLocal []
    |> runErrorNoCallStack
    |> runPureEff

runParserInLCContext :: Reader Env :> es => Eff ParserEffs a -> Eff es a
runParserInLCContext act = do
  env <- ask
  act
    |> runReader env
    |> runPureEff
    |> pure

--------------------------------------------------------------------------------------------------


-- | Main GML translation function.
--
-- Expects a list of Hornlike rules, produces a translated GML program
-- in the form of an 'LCProgram'.
--
l4ToLCProgram ::
  (Error ToLCError :> es, Reader Env :> es, State.State [UserDefinedFun] :> es) =>
  [L4.Rule] ->
  Eff es LCProgram
l4ToLCProgram rules = do
  l4HLs <- traverse simplifyL4Hlike rules
  let customUserFuns = iterateFuns [] $ F.toList l4HLs
  lcProg <- withCustomFuns customUserFuns $ traverse expifyHL l4HLs
  pure MkLCProgram { lcProgram = programWithoutUserFuns lcProg
                   , globalVars = mkGlobalVars $ HM.unions $ shGiven <$> F.toList l4HLs
                   , giveths = giveths
                   , userFuns = mkUserFuns customUserFuns}
  where
    giveths :: [T.Text]
    giveths = [pt2text pt | Just pt <- L4.giveth <$> F.toList rules]

    iterateFuns :: [UserDefinedFun] -> [SimpleHL] -> [UserDefinedFun]
    iterateFuns firstPass l4HLs =
      if newFuns == firstPass
        then newFuns
        else iterateFuns newFuns l4HLs
      where
        newFuns = execToLC $ withCustomFuns firstPass $ traverse expifyHL l4HLs

    mkUserFuns :: [UserDefinedFun] -> HM.HashMap String ([Var], Exp)
    mkUserFuns opers = HM.fromList [
      (T.unpack var, (getBoundVars fun, getFunDef fun))
      | fun@(getFunName -> MkVar var) <- opers]

    -- Separate user functions from the body of the program (mostly because
    -- I don't want to handle them in ToMathLang the same way as the rest /Inari)
    programWithoutUserFuns prog = [e | e <- F.toList prog, notLambda e]
      where
        notLambda (exp -> ELam {}) = False
        notLambda _ = True

{-==============================================================================
  1. Simplify L4: massage L4.Rule (Hornlike) into the more convenient SimpleHL
===============================================================================-}

type SimpleHL = SimpleHlike VarTypeDeclMap ReturnVarInfo (Maybe RuleLabel)

simplifyL4Hlike :: Error ToLCError :> es => L4.Rule -> Eff es SimpleHL
simplifyL4Hlike rule =
  case rule.srcref of
    Just srcref -> do
      shClauses <- extractBaseHL rule
      pure MkSimpleHL { shSrcRef = srcref
                      , shGiven = maybe HM.empty mkVarEntityMap rule.given
                      , shReturnVariables  = rule.giveth ^.. folded % folding mkL4VarTypeDeclAssocList
                      , shClauses = shClauses
                      , shLabel = rule.rlabel
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
    Just hbody -> HeadAndBody MkHornBodyHeadClause {hbHead = hc.hHead, hbBody = hbody}
    Nothing -> mkHeadOnlyAtomicHC hc.hHead

extractBaseHL :: Error ToLCError :> es => L4.Rule -> Eff es BaseHL
extractBaseHL rule =
  case rule.clauses of
    [] -> throwParserProblemWithMsg rule "Parser should not return L4 Hornlikes with no clauses"
    [hc] -> pure $ OneClause $ l4HcToAtomicHC hc
    hc : hcs ->
      (hc NE.:| hcs)
        |$> l4HcToAtomicHC |> MultiClause |> pure

--- Utils for dealing with 'Maybe ParamText' -------------------------------------
mkL4VarTypeDeclAssocList :: Foldable f => f TypedMulti -> [(Var, Maybe L4EntityType)]
mkL4VarTypeDeclAssocList = convertL4Types . declaredVarsToAssocList
  where
    declaredVarsToAssocList :: Foldable f => f TypedMulti -> [(T.Text, Maybe TypeSig)]
    declaredVarsToAssocList dvars = dvars ^.. folded % to getGiven % folded

    convertL4Types :: [(T.Text, Maybe TypeSig)] -> [(Var, Maybe L4EntityType)]
    convertL4Types = fmap (bimap mkVar (fmap mkEntityType))

mkVarEntityMap :: Foldable f => f TypedMulti -> VarTypeDeclMap
mkVarEntityMap = HM.fromList . mkL4VarTypeDeclAssocList

{-================================================================================
  2. SimpleHL to LC Exp
=================================================================================-}

srcRefToSrcPos :: SrcRef -> SrcPosition
srcRefToSrcPos sr = MkPosition { row = sr.srcrow, col = sr.srccol }

-- | Convenience fn: For when there's no (additional) metadata to add to base exp
noExtraMetadata :: BaseExp -> Exp
noExtraMetadata baseexp = MkExp baseexp []

typeMetadata :: SrcPosition -> T.Text -> BaseExp -> Exp
typeMetadata pos typ bexp = MkExp bexp (inferredType pos typ [])

inferredType :: SrcPosition -> T.Text -> MdGrp -> MdGrp
inferredType pos typ = \case
  [] -> [ MkExpMetadata
            pos
            (Just $ Inferred typ)
            Nothing
        ]
  mds -> mds -- md {typeLabel = Just $ Inferred typ}:mds

addRuleName :: SrcPosition -> T.Text -> Exp -> Exp
addRuleName pos rname exp = case exp.md of
  []   -> exp {md = [MkExpMetadata pos Nothing annot]}
  m:ms -> exp {md = m {explnAnnot = annot}:ms}
  where
    annot = Just $ MkExplnAnnot rname Nothing Nothing

{- | Right now I don't think the order in which we compile the HLs actually matters,
so just using a foldrM -}
l4sHLsToLCSeqExp ::
  (Error ToLCError :> es, Reader Env :> es, State.State [UserDefinedFun] :> es) =>
  [SimpleHL] ->
  Eff es SeqExp
l4sHLsToLCSeqExp = F.foldrM go mempty
  where
    go hornlike seqExp = consSE <$> expifyHL hornlike <*> pure seqExp

--------------------------------------------------------------------
expifyHL ::
  (Error ToLCError :> es, Reader Env :> es, State.State [UserDefinedFun] :> es) =>
  SimpleHL ->
  Eff es Exp
expifyHL hl = do
  bexp <- baseExpify hl
  pure $ MkExp bexp [] -- using mdata here puts it in weird place! but we don't care about types so much at this stage so leave it empty for now
--  return $ MkExp bexp [mdata]
   where
    returnType = case hl.shReturnVariables of
      [(_, mReturnType)] -> mReturnType
      [] -> Nothing
      xs -> fail [i|expifyHL: the SimpleHL's shReturnVariables has multiple return types, is this weird? #{xs}|]
      -- TODO: when would this list have more than 1 element? if there are multiple GIVETHs?

    _mdata = MkExpMetadata {
              srcPos = srcRefToSrcPos $ shSrcRef hl
            , typeLabel = FromUser <$> returnType
            , explnAnnot = Nothing
            }

{- | My current understanding is that the LC Exps that a SimpleHL can be are:
1. If Then (currently converted into If Then Else in MathLang—maybe already do the transformation here?)
2. Lam Def
3. Block of statements / expressions (TODO)
-}
baseExpify ::
  (Error ToLCError :> es, Reader Env :> es, State.State [UserDefinedFun] :> es) =>
  SimpleHL ->
  Eff es BaseExp
baseExpify hl = case lhsLooksLikeLambda hl of
  -- We have verified the LHS looks like a lambda abstraction! Now we must get
  -- monadic in order to parse the RHS and check if the bound variables appear there
  Just (varsLHS@(v:vs), lhs, rhs) -> do
    (fname, operMP) <- mkOperator lhs varsLHS
    bexpRHS <- baseExpifyMTEs rhs
    varsRHS <- argVarsOnly bexpRHS
    if all (`elem` varsRHS) varsLHS -- FIXME: should also accept global variables
      then do
        let userFun = MkUserFun fname (noExtraMetadata bexpRHS) varsLHS operMP
        State.modify $ (userFun :)
        ELam v <$> mkLambda vs bexpRHS
      else baseExpify'
  _ -> baseExpify'
  where
    baseExpify' = case hl of
      -- A labelled function that can be called later.
      -- Thus, parse the function and assign the result to a variable.
      -- TODO: what happens with recursion?
      (shLabel -> Just (_section, _number, rname)) -> do
        bexp <- baseExpify $ hl {shLabel = Nothing}
        mkVarSetFromVar (MkVar rname) (noExtraMetadata bexp)
      (isIf -> Just hc) -> toIfExp hl hc
      (shClauses -> OneClause (HeadOnly hc)) -> toHeadOnlyExp hl hc
      (isMultiClause -> hls@(_:_)) -> ESeq <$> l4sHLsToLCSeqExp hls
      hornlike -> throwNotYetImplError hornlike

    argVarsOnly bexp = do
      funNames <- asks $ fmap getFunName . userDefinedFuns
      pure $ filter (`notElem` funNames) $
        MkExp bexp [] ^.. cosmosOf (gplate @Exp) % gplate @Var

mkLambda :: Reader Env :> es => [Var] -> BaseExp -> Eff es Exp
mkLambda [] bexp = do
  pos <- asks currentSrcPosition
  pure $ typeMetadata pos "Function" bexp
mkLambda (v:vs) bexp = do
  pos <- asks currentSrcPosition
  let funType = typeMetadata pos "Function"
  funType . ELam v <$> mkLambda vs bexp

-- | A 'SimpleHL' represents an 'If' expression, if it is a single clause
-- with a body.
isIf :: SimpleHL -> Maybe HornBodyHeadClause
isIf hl =
  case hl.shClauses of
    MultiClause _ -> Nothing
    OneClause atomicHC ->
      case atomicHC of
        HeadOnly _ -> Nothing
        HeadAndBody hc -> Just hc

-- | If this horn rule looks like a lambda, we can translate it into a lambda!
--
-- A horn rule looks like a lambda if it has givens and its head and body looks
-- like an 'Is' constraint. For example, @GIVEN foo DECIDE bar IS baz@ looks like a lambda.
--
-- In this case is @foo@ a parameter that can occur freely in both @bar@.
-- Why not also in @baz@? Can't answer that, yet.
lhsLooksLikeLambda :: SimpleHL -> Maybe ([Var], [MTExpr], [MTExpr])
lhsLooksLikeLambda hl = case (hasGivens hl, isConstraint hl) of
  (Just givens, Just (lhs, rhs)) -> (,lhs,rhs) <$> maybeBoundVars lhs givens
  _ -> Nothing -- not a function: LHS is none of `f x`, `x f y` or `x f`

isConstraint :: SimpleHL -> Maybe ([MTExpr], [MTExpr])
isConstraint hornlike = do
  OneClause (HeadOnly hornClauseHead) <- Just $ shClauses hornlike
  RPConstraint lhs RPis rhs <- Just $ hcHead hornClauseHead
  Just (lhs, rhs)

hasGivens :: SimpleHL -> Maybe [Var]
hasGivens hl = case HM.keys hl.shGiven of
                [] -> Nothing
                ks -> Just ks

mkOperator ::
  (Error ToLCError :> es, Reader Env :> es) =>
  [MTExpr] ->
  [Var] ->
  Eff es (Var, Operator Parser BaseExp)
mkOperator mtes vars = do
  pos <- asks currentSrcPosition
  case maybeOperator pos mtes vars of
    Just (fname, _vars, op) -> pure (fname, op)
    Nothing -> throwNotSupportedWithMsgError ("isLambda:" :: String) [i|not a lambda expression: #{mtes}|]

-- | Get the variables that are already bound if any.
--
maybeBoundVars :: [MTExpr] -> [Var] -> Maybe [Var]
maybeBoundVars mtes givens = getBoundVars <$> maybeOperator dummyPos mtes givens
  where
    getBoundVars (_fname, vars, _op) = vars
    dummyPos = MkPosition undefined undefined

-- We require explicit arguments, otherwise impossible to distinguish from normal variable assignment
maybeOperator :: SrcPosition -> [MTExpr] -> [Var] -> Maybe (Var, [Var], Operator Parser BaseExp)
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
    postfix f mkBexp)          -- megaparsec operator
maybeOperator pos [MTT x, MTT f, MTT y] vs@[_, _]
  | all (`elem` vs) [vx, vy]
    && not (f `Set.member` allArithOps)
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
  case hl.shClauses of
    MultiClause hls -> keepOgHL <$> NE.toList hls
    _ -> []
  where
    keepOgHL :: AtomicHC -> SimpleHL
    keepOgHL hc = hl {shClauses = OneClause hc}

toIfExp :: (Error ToLCError :> es, Reader Env :> es) => SimpleHL -> HornBodyHeadClause -> Eff es BaseExp
toIfExp hl hc = do
  condE <- withLocalVarsAndSrcPos $ processHcBody hc.hbBody
  thenE <- withLocalVarsAndSrcPos $ processHcHeadForIf hc.hbHead
  case thenE.exp of
    EPredSet var (exp -> ELit EBoolTrue) -> pure $ EPredSet var condE
    _ -> pure $ EIfThen condE thenE
  where
    withLocalVarsAndSrcPos = local $ setCurrSrcPos . setLocalVars
    setCurrSrcPos, setLocalVars :: Env -> Env
    setCurrSrcPos = set #currentSrcPosition (srcRefToSrcPos hl.shSrcRef)
    setLocalVars = set #localVars hl.shGiven
-- TODO: Add metadata from hl
-- TODO: If Then with ELSE for v2

toHeadOnlyExp :: (Error ToLCError :> es, Reader Env :> es) => SimpleHL -> HeadOnlyHC -> Eff es BaseExp
toHeadOnlyExp hl hc =
  withLocalVarsAndSrcPos $ exp <$> processHcHeadForIf hc.hcHead
  where
    withLocalVarsAndSrcPos = local $ setCurrSrcPos . setLocalVars
    setCurrSrcPos, setLocalVars :: Env -> Env
    setCurrSrcPos = set #currentSrcPosition (srcRefToSrcPos hl.shSrcRef)
    setLocalVars = set #localVars hl.shGiven


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
processHcHeadForIf :: (Error ToLCError :> es, Reader Env :> es) => L4.RelationalPredicate -> Eff es Exp
processHcHeadForIf (isSetVarToTrue -> Just putativeVar) = do
  pos <- asks currentSrcPosition
  pure $ noExtraMetadata $
            EPredSet (mkVar . textifyMTEs $ putativeVar)
            (typeMetadata pos "Boolean" $ ELit EBoolTrue)
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

{- |
We want to handle things like
  * @[ MTT "n1 + n2" ]@, from @RPConstraint [ MTT "n3c" ] RPis [ MTT "n1 + n2" ]@

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
  let ops = allArithOps
      allFuns = foldr (Set.insert . varAsTxt) ops funs
  in case mte of
    MTT t -> t `Set.member` allFuns
    _ -> False

-- | Interpret the given 'MTExpr' and its subsequent 'MTExpr's params
-- as a record selector. If all 'MTExpr's are record selectors, we simply
-- create one record selector.
--
-- However, often a record selector is passed
-- to a function application, for example @sibling's beverage discountedBy 20@, where
-- @discountedBy@ is a function.
-- If that's the case, we roughly translate this to @discountedBy (sibling.beverage) 20@.
genetivesMultiTermExpression ::
  (Error ToLCError :> es, Reader Env :> es) =>
  MTExpr ->
  NE.NonEmpty MTExpr ->
  Eff es BaseExp
genetivesMultiTermExpression g rest = do
  userFuns <- asks $ fmap getFunName . userDefinedFuns -- :: [Var]
  recname <- expifyMTEsNoMd [g]
  case NE.break (isFun userFuns) rest of
  -- ind's parent's sibling's … income
    (_x:_xs,[]) -> do
      fieldname <- expifyMTEsNoMd $ NE.toList rest -- income
      pure $ ERec fieldname recname

  -- ind's parent's sibling's … income, discountedBy, foo
    (x:xs,f:ys) -> do
      fieldname <- expifyMTEsNoMd (x:xs) -- income
      fExp <- expifyMTEsNoMd [f]
      arg2 <- expifyMTEsNoMd ys
      let arg1 = ERec fieldname recname
          fArg1 = noExtraMetadata $ EApp fExp $ noExtraMetadata arg1
      pure $ EApp fArg1 arg2

    _ -> throwErrorImpossibleWithMsg g "shouldn't happen because we matched that the stuff after genitives is not empty"

baseExpifyMTEs :: (Reader Env :> es, Error ToLCError :> es) => [MTExpr] -> Eff es BaseExp
baseExpifyMTEs mtes = do
  case splitGenitives mtes of
    Just (g, nonEmptyMtes) -> genetivesMultiTermExpression g nonEmptyMtes
    Nothing -> do
      userFuns <- asks $ fmap getFunName . userDefinedFuns -- :: [Var]
      -- Now, we differentiate the following cases:
      -- 1. It might be a variable
      -- 2. It might be a simple function aplication
      -- 3. It might be a more complex function application or expression
      --    that requires more analysis.
      case mtes of
        [mte] -> do
          -- Inari: assuming that the Var will be used for comparison
          -- because SetVar is handled elsewhere, by extracting it
          -- from RPConstraint
          mvar <- isDeclaredVar mte
          case mvar of
            Just var -> return $ EVar var
            Nothing -> parseExpr mte

        -- Exactly two elements, might be a function application like:
        --
        -- * f x
        -- * x f
        --
        -- Check whether 'f' and 'x' are functions or variables, etc...
        -- However, it seems we do not *really* care whether it is a user-defined
        -- function or simply a variable.
        [mte1@(MTT _), mte2@(MTT _)] -> do
          mvar1 <- isDeclaredVar mte1
          mvar2 <- isDeclaredVar mte2
          case (mvar1, mvar2) of
            (Just var1, Nothing) -> do -- "ind","qualifies" = qualifies(ind)
              varWeAssumeToBePred <- varFromMTEs [mte2]
              fExp <- mkVarExp varWeAssumeToBePred
              pure $ EApp fExp $ noExtraMetadata $ EVar var1

            (Nothing, Just var2) -> do -- "qualifies","ind" = qualifies(ind)
              varWeAssumeToBePred <- varFromMTEs [mte1]
              fExp <- mkVarExp varWeAssumeToBePred
              pure $ EApp fExp $ noExtraMetadata $ EVar var2

            (Nothing, Nothing)
              -> throwNotSupportedWithMsgError (RPMT mtes)
                    [i|baseExpifyMTEs: trying to apply non-function #{mtes}|]

            (Just var1, Just var2) -> do
              case (var1, var2) & both %~ (`elem` userFuns) of
                (True, False) -> do
                  let f = noExtraMetadata $ EVar var1
                      x = noExtraMetadata $ EVar var2
                  pure $ EApp f x
                (False, True) -> do
                  let f = noExtraMetadata (EVar var2)
                      x = noExtraMetadata (EVar var1)
                  pure $ EApp f x
                (True, True) -> throwNotSupportedWithMsgError (RPMT mtes) "baseExpifyMTEs: trying to apply function to another function—we probably don't support that"
                (False, False) -> throwNotSupportedWithMsgError (RPMT mtes) "baseExpifyMTEs: trying to apply non-function"

        _mtes ->
          case break (isFun userFuns) mtes of
          -- function, rest
            ([],f:ys) -> do
              arg <- expifyMTEsNoMd ys
              assumedVar <- varFromMTEs [f]
              fExp <- mkVarExp assumedVar
              pure $ EApp fExp arg

          -- mte1 [, …, mteN], function, rest
            (xs,f:ys) -> do
              arg1 <- expifyMTEsNoMd xs
              arg2 <- expifyMTEsNoMd ys
              -- since f is in userfuns, we can parse it using parseExpr. replacing args to some dummy x and y (but will fail if there is a userfun called x or y ¯\_(ツ)_/¯)
              bexp <- parseExpr $ MTT [i|x #{mtexpr2text f} y|]
              case bexp of
                ENumOp {} -> pure $ ENumOp bexp.numOp arg1 arg2
                ECompOp {} -> pure $ ECompOp bexp.compOp arg1 arg2
                _ -> do
                  assumedVar <- varFromMTEs [f]
                  fExp <- mkVarExp assumedVar
                  let fArg1 = noExtraMetadata $ EApp fExp arg1
                  pure $ EApp fArg1 arg2

          -- no userfuns here
            (_xs,[]) -> do
              let parenExp = MTT $ textifyMTEs $ parenExps mtes
              expParsedAsText <- parseExpr parenExp
              case expParsedAsText of
                ELit _ -> trace [i|parseExpr returned this as a string literal: #{expParsedAsText}|] do
                -- TODO: this should definitely not be a sequence, what should it be instead???
                  parsedExs <- traverse parseExpr mtes
                  pure $ ESeq $ foldr consSeqExp mempty parsedExs

                -- arithmetic expression like [MTT "m1",MTT "*",MTT "m2"]
                notStringLit -> pure notStringLit
  where
    consSeqExp :: BaseExp -> SeqExp -> SeqExp
    consSeqExp = consSE . noExtraMetadata

    parenExps :: [MTExpr] -> [MTExpr]
    parenExps mtexpr
      | any (isOp . mtexpr2text) mtexpr = parenNestedExprs <$> mtexpr
      | otherwise = mtexpr

    isOp :: T.Text -> Bool
    isOp = (`Set.member` arithOps)

    -- don't parenthesize single variables or literals, like "taxesPayable" or "Singapore citizen"
    -- do parenthesize "(x + 6)"
    parenNestedExprs :: MTExpr -> MTExpr
    parenNestedExprs = mtexpr2text >>> parenNE >>> MTT
      where
        parenNE :: T.Text -> T.Text
        parenNE text
          | text PCRE.≈ [PCRE.re| |\+|\*|\-|\/|] =
            trace [i|added parentheses (#{text})|] [i|(#{text})|]
          | otherwise = text

parseExpr :: Reader Env :> es => MTExpr -> Eff es BaseExp
parseExpr x@(MTT str) = do
  res <- runParserInLCContext $ runParserT pExpr "" str
  case res of
    -- if it's just a String literal, don't use the megaparsec version—it removes whitespace, e.g. "Singapore citizen" -> "Singapore"
    Right (EVar _) -> pure $ EVar $ MkVar str
    Right notStringLit -> pure notStringLit
    Left _error -> trace [i|can't parse with pExpr: #{x}|] pure $ mteToLitExp x
parseExpr x = pure $ mteToLitExp x

-- | Given a '[MTExpr]' that looks like:
--
-- @[MTT "parent's", MTT "sibling's", MTT "income"]@
--
-- strip away the suffix @"'s"@ from the first element of the list
-- if there is any.
--
-- >>> splitGenitives [MTT "parent's", MTT "sibling's", MTT "income"]
-- Just (MTT "parent",MTT "sibling's" :| [MTT "income"])
--
-- >>> splitGenitives [MTI 0, MTT "parent's", MTT "income"]
-- Nothing
--
splitGenitives :: [MTExpr] -> Maybe (MTExpr, NE.NonEmpty MTExpr)
splitGenitives [] = Nothing
splitGenitives (mte:mtes) = do
  mtesNE <- NE.nonEmpty mtes
  noun <- isGenitive mte
  pure $ (noun, mtesNE)
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
type Parser = ParsecT Void T.Text (Eff ParserEffs)

pExpr :: Parser BaseExp
pExpr = do
  pos <- lift $ asks currentSrcPosition
  customOpers <- lift $ asks userDefinedFuns
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

table :: SrcPosition -> [[Operator Parser BaseExp]]
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

mul', div', plus', minus', lt', lte', gt', gte', numeq' :: SrcPosition -> BaseExp -> BaseExp -> BaseExp
mul' pos x y = ENumOp OpMul (typeMetadata pos "Number" x) (typeMetadata pos "Number" y)
div' pos x y = ENumOp OpDiv (typeMetadata pos "Number" x) (typeMetadata pos "Number" y)
plus' pos x y = ENumOp OpPlus (typeMetadata pos "Number" x) (typeMetadata pos "Number" y)
minus' pos x y =  ENumOp OpMinus (typeMetadata pos "Number" x) (typeMetadata pos "Number" y)
lt' pos x y =  ECompOp OpLt (typeMetadata pos "Number" x) (typeMetadata pos "Number" y)
lte' pos x y =  ECompOp OpLte (typeMetadata pos "Number" x) (typeMetadata pos "Number" y)
gt' pos x y =  ECompOp OpGt (typeMetadata pos "Number" x) (typeMetadata pos "Number" y)
gte' pos x y =  ECompOp OpGte (typeMetadata pos "Number" x) (typeMetadata pos "Number" y)
numeq' pos x y = ECompOp OpNumEq (typeMetadata pos "Number" x) (typeMetadata pos "Number" y)

customUnary :: Var -> SrcPosition -> BaseExp -> BaseExp
customUnary fname pos x =
  let f = typeMetadata pos "Function" (EVar fname)
  in EApp f (noExtraMetadata x)

customBinary :: Var -> SrcPosition -> BaseExp -> BaseExp -> BaseExp
customBinary fname pos x y =
  let f = typeMetadata pos "Function" (EVar fname)
  in EApp (noExtraMetadata (EApp f (noExtraMetadata x))) (noExtraMetadata y)

-- TODO: should we identify already here whether things are Vars or Lits?
-- Or make everything by default Var, and later on correct if the Var is not set.

pVariable :: Parser BaseExp
pVariable = do
  putativeVar <- T.pack <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
  return $ EVar $ MkVar putativeVar

pEnum :: Parser BaseExp
pEnum = do
  localVars :: VarTypeDeclMap <- lift $ asks localVars
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

expifyMTEsNoMd :: forall es . (Error ToLCError :> es, Reader Env :> es) => [MTExpr] -> Eff es Exp
expifyMTEsNoMd mtes = addMetadataToVar =<< baseExpifyMTEs mtes
 where
  addMetadataToVar :: BaseExp -> Eff es Exp
  -- This is supposed to work as follows:
  -- baseExpifyMTEs for a single [mte] only returns a Var if it is declared
  -- baseExpifyMTEs for [mte1, mte2] returns an EApp
  -- baseExpifyMTEs for [mte1, mte2, …] returns an arithmetic expression
  addMetadataToVar = \case
    EVar var -> mkVarExp var
    bexp -> return $ noExtraMetadata bexp

----- Util funcs for looking up / annotating / making Vars -------------------------------------

lookupVar :: Var -> VarTypeDeclMap -> Maybe L4EntityType
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

-- TODO: Look into trying to add Maybe to our Eff es transformer stack
-- | Use this to check if some MTE is a Var
isDeclaredVar :: Reader Env :> es => MTExpr -> Eff es (Maybe Var)
isDeclaredVar = \case
  MTT mteTxt -> do
    localVars <- asks localVars
    return $ isDeclaredVarTxt mteTxt localVars
  _ -> return Nothing

-- | Annotate with TLabel metadata if available
mkVarExp :: Reader Env :> es => Var -> Eff es Exp
mkVarExp var = do
  env :: Env <- ask
  let varExpMetadata = MkExpMetadata { srcPos = env.currentSrcPosition
                                     , typeLabel = FromUser <$> lookupVar var env.localVars
                                     , explnAnnot = Nothing --Temp plceholder; TODO
                                     }
  return $ MkExp (EVar var) [varExpMetadata]

-- | Make a var set exp, when you know it's a var
mkVarSetFromVar :: Reader Env :> es => Var -> Exp -> Eff es BaseExp
mkVarSetFromVar var argE = EVarSet <$> mkVarExp var <*> pure argE

{- | TODO: Check that it meets the formatting etc requirements for a variable,
(e.g. prob don't want var names to start with numbers, and probably want to error if the MTE is a MTB)
and throw an error if not
-}
varFromMTEs :: [MTExpr] -> Eff es Var
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

processHcBody :: forall es . (Reader Env :> es, Error ToLCError :> es) => L4.BoolStructR -> Eff es Exp
processHcBody bsr = do
  pos <- asks currentSrcPosition
  case bsr of
    AA.Leaf rp -> expifyBodyRP rp
  -- If the label is Metadata, then we add it to MdGrp
    AA.All (Just (AA.Metadata lbl)) propns ->
      addRuleName pos lbl <$> F.foldrM (makeOp pos EAnd) emptyExp propns
    AA.Any (Just (AA.Metadata lbl)) propns ->
      addRuleName pos lbl <$> F.foldrM (makeOp pos EOr) emptyExp propns

  -- If there is a label and it's not Metadata, it might be part of the text.
  -- If you uncomment fmap addLabel below, then the possible text in mlbl will be added to the leaves.
    AA.All _mlbl propns -> F.foldrM (makeOp pos EAnd) emptyExp ({-fmap (addLabel mlbl) <$> -} propns)
    AA.Any _mlbl propns -> F.foldrM (makeOp pos EOr) emptyExp ({-fmap (addLabel mlbl) <$>-} propns)
    AA.Not propn -> typeMetadata pos "Boolean" . ENot <$> processHcBody propn
  where
    emptyExp = noExtraMetadata EEmpty

    makeOp :: SrcPosition -> (Exp -> a -> BaseExp) -> L4.BoolStructR -> a -> Eff es Exp
    makeOp pos op boolStructR exp =
      noExtraMetadata <$> ((op . toBoolEq pos <$> processHcBody boolStructR) <*> pure exp)


    toBoolEq pos expr =
      expr {exp = toBoolEqBE expr.exp, md = inferredType pos "Boolean" expr.md}
      where
        inferredBool = typeMetadata pos "Boolean"
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
expifyHeadRP :: (Error ToLCError :> es, Reader Env :> es) => RelationalPredicate -> Eff es Exp
expifyHeadRP = \case
  -- This is
  RPConstraint lefts RPis rights ->
    expifyHeadRP $ RPnary RPis [RPMT lefts, RPMT rights]

  RPnary RPis [RPMT [mte], rp]   -> do
    mvar <- isDeclaredVar mte
    exp <- expifyBodyRP rp
    (varExp, valExp) <- case mvar of
      Just var -> do -- The var is given and has a type
        varExp <- mkVarExp var
        let valExp = inferTypeFromOtherExp exp varExp
        pure (varExp, valExp)

      Nothing -> do -- Var is not given, but we can try to infer it from the RHS
        pos <- asks currentSrcPosition
        varNoType <- noExtraMetadata . EVar <$> varFromMTEs [mte]
        let valExp = inferTypeRHS pos exp
        let varExp = inferTypeFromOtherExp varNoType valExp
        pure (varExp, valExp)

    pure $ noExtraMetadata $ EVarSet varExp valExp

  rp -> expifyBodyRP rp

expifyBodyRP :: forall es . (Error ToLCError :> es, Reader Env :> es) => RelationalPredicate -> Eff es Exp
expifyBodyRP = \case
  -- OTHERWISE
  RPMT (MTT "OTHERWISE" : _mtes) -> do
    pos <- asks currentSrcPosition
    pure $ typeMetadata pos "Bool" $ ELit EBoolTrue

  _rp@(RPMT mtes) -> expifyMTEsNoMd mtes

  RPConstraint lefts rel rights ->
    expifyBodyRP $ RPnary rel [RPMT lefts, RPMT rights]

  RPnary RPis [rp1, rp2] -> do
    pos <- asks currentSrcPosition
    exp1maybeUntyped <- expifyBodyRP rp1
    exp2 <- inferTypeRHS pos <$> expifyBodyRP rp2
    let exp1 = inferTypeFromOtherExp exp1maybeUntyped exp2
    pure $ noExtraMetadata $ EIs exp1 exp2

  -- RPnary with logical relations: convert into BoolStruct and reuse processHcBody
  RPnary RPand rps -> processHcBody $ AA.All Nothing $ AA.Leaf <$> rps
  RPnary RPor rps -> processHcBody $ AA.Any Nothing $ AA.Leaf <$> rps
  RPnary RPnot [rp] -> processHcBody $ AA.Not $ AA.Leaf rp
  RPnary RPnot rps -> processHcBody $ AA.All Nothing $ AA.Not . AA.Leaf <$> rps

  -- RPnary with numeric or comparison op
  RPnary rprel rps -> case (rprel2op rprel, rps) of
    -- Numeric operations can have multiple arguments
    (Just (Num op), _) -> go op "Number" ENumOp
   -- Comparison operations can only have two
    (Just (Comp op), _:_) -> go op "Boolean" ECompOp
    _ -> throwNotSupportedWithMsgError ("not implemented" :: String) [i|#{rprel}|]
    where
      go :: a -> T.Text -> (a -> Exp -> Exp -> BaseExp) -> Eff es Exp
      go op str ctor = do
        pos <- asks currentSrcPosition
        exps <- traverse expifyBodyRP rps
        pure $ foldl1 (numOrCompOp pos str ctor op) exps

  -- ParamText is just a simpler MultiTerm
  RPParamText pt -> expifyBodyRP $ RPMT $ pt2multiterm pt

  -- RPBoolStructR only should work with IS
  RPBoolStructR lhs RPis bsr -> do
    lhsExp <- expifyMTEsNoMd lhs
    rhsExp <- processHcBody bsr
    pure $ noExtraMetadata $ EIs lhsExp rhsExp
  rp@(RPBoolStructR _lhs rprel _bsr) -> throwNotSupportedWithMsgError rp [i|#{rprel} not supported with RPBoolStructR|]

  where
    numOrCompOp :: SrcPosition -> T.Text -> (t -> Exp -> Exp -> BaseExp) -> t -> Exp -> Exp -> Exp
    numOrCompOp pos str ctor op x y =
      let coerceNumber = coerceType pos "Number"
      in typeMetadata pos str $ ctor op (coerceNumber x) $ coerceNumber y

    rprel2op :: RPRel -> Maybe NumCompOp
    rprel2op =  (`Map.lookup` rpRel2opTable)

    -- inferTypeFromOtherExp already does the check whether target has empty typeLabel
    coerceType :: SrcPosition -> T.Text -> Exp -> Exp
    coerceType pos typ exp@(MkExp bexp _) =
      inferTypeFromOtherExp exp $ MkExp bexp $ inferredType pos typ []

inferTypeFromOtherExp :: Exp -> Exp -> Exp
inferTypeFromOtherExp copyTarget copySource = case copyTarget.md of
  m:_ -> case m.typeLabel of
            Just _  -> copyTarget -- copyTarget has already a type, don't override
            Nothing -> copyTarget {md = copySource.md}
  [] -> copyTarget {md = copySource.md} -- copySource has potentially more reliable type info

inferTypeRHS :: SrcPosition -> Exp -> Exp
inferTypeRHS pos x@(MkExp bexp md) = case bexp of
  ELit (EString _) -> go "String"
  ELit (EInteger _) -> go "Number"
  ELit (EFloat _) -> go "Number"
  ENumOp{} -> go "Number"
  ECompOp{} -> go "Bool"
  _ -> x
  where
    go = MkExp bexp . flip (inferredType pos) md
--  EVar (MkVar t) -> ??? -- NB. a Var can be defined outside givens, like "incomeTaxRate,IS,0.01"
-- so when incomeTaxRate appears in another expression, we could check elsewhere in the program what its type is.
