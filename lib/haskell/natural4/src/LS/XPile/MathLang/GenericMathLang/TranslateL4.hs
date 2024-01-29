-- {-# OPTIONS_GHC -W #-}
-- {-# OPTIONS_GHC -foptimal-applicative-do #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, OverloadedLabels, UndecidableInstances, RecordWildCards #-}
{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, DataKinds, TypeFamilies, FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}


module LS.XPile.MathLang.GenericMathLang.TranslateL4 where
-- TODO: Add export list

import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST -- TODO: Add import list
import LS.XPile.MathLang.Logging (LogConfig, defaultLogConfig)

import AnyAll qualified as AA
import LS.Types as L4
  (SrcRef(..), RelationalPredicate(..), RPRel(..), MTExpr(..), EntityType,
  HornClause (..), HornClause2, BoolStructR,
  TypeSig(..), TypedMulti,
  SimpleHlike(..), BaseHL(..), AtomicHC(..), HeadOnlyHC(..),
  MultiClauseHL, _MkMultiClauseHL, mkMultiClauseHL,
  HeadOnlyHC, _MkHeadOnlyHC, mkHeadOnlyAtomicHC, mkHeadOnlyHC, headOnlyHLasMTEs,
  HnBodHC(..)
  )
-- import LS.Interpreter (qaHornsT)
import LS.Rule (
                -- Interpreted(..), 
                extractMTExprs, getGivenWithSimpleType,
                defaultHorn)
import LS.Rule qualified as L4 (Rule(..))

import Control.Monad (foldM, join)
import Effectful (Effect, Eff, runPureEff)
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (send, interpret, localSeqUnlift)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Reader.Static (runReader, local, Reader)
-- import Effectful.State.Static.Shared (State, runState)
-- experimenting with Effectful.Error rn
-- import Control.Monad.Validate qualified as V
--   ( ValidateT
--     , MonadValidate
--     -- , Validate
--     , refute
--     , dispute
--     , tolerate
--     )
import Data.HashSet qualified as HS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Optics
import Data.Text.Optics (packed, unpacked)
import GHC.Generics (Generic)
import Data.Generics.Sum.Constructors
-- import Data.Generics.Product.Types (types)
-- import Prettyprinter (Pretty)
import Data.String.Interpolate (__i)
import Data.String (IsString)
import Data.Text qualified as T
-- import LS.Utils.TextUtils (int2Text, float2Text)
import Data.Foldable qualified as F (toList)

import LS.XPile.MathLang.UtilsLCReplDev

{- | Parse L4 into a Generic MathLang lambda calculus (and thence to Meng's Math Lang AST)

If prototyping in GHCi / REPL, use these:

    :set -XTypeApplications
    :set -XDataKinds
    :set -XDeriveGeneric
    :set -XFlexibleContexts
    :set -XTypeFamilies
    import GHC.Generics
-}

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

-------- Set Var to True If ...
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

data ToLCError = NotYetImplemented T.Text -- SrcPositn
               | ParserProblem T.Text
               | MiscError T.Text
               | NotSupported T.Text
  deriving stock (Eq, Show, Generic)

instance Hashable ToLCError


stringifyToLCError :: ToLCError -> T.Text
stringifyToLCError = undefined

---- Specialized error throwing convenience funcs ---------------------------------
-- | TODO: make this better with `pretty` and better structured errors later
throwNotYetImplError :: Show a => a -> ToLC b
throwNotYetImplError l4ds = ToLC $ throwError $ NotYetImplemented (T.pack . show $ l4ds)

throwParserParserProblem :: (Show a) => a -> T.Text -> ToLC c
throwParserParserProblem l4ds msg = ToLC $ throwError $ ParserProblem $ (T.pack . show $ l4ds) <> msg

-------- Env -----------------------------------------------------------------------

type VarTypeDeclMap = HM.HashMap Var (Maybe L4EntType)
type RetVarInfo = [(Var, Maybe L4EntType)]

data Env =
  MkEnv { localVars :: VarTypeDeclMap
        -- ^ would *not* include 'global' GIVEN-declared vars --- just the local ones 
        -- , retVarInfo :: Maybe RetVarInfo
          -- ^ not sure we need retVarInfo
        , logConfig :: LogConfig }
  deriving stock (Show, Generic)

initialEnv :: Env
initialEnv = MkEnv { localVars = HM.empty
                  --  , retVarInfo = Nothing
                   , logConfig = defaultLogConfig }

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
        -- State GlobalVars,
        -- Might be better to just do a separate pass that finds the global vars --- not sure rn 
        Error ToLCError] a )
  deriving newtype (Functor, Applicative, Monad)

_ToLC :: Iso' (ToLC a) (Eff '[Reader Env, Error ToLCError] a)
_ToLC = coerced

unToLC :: ToLC a -> Eff [Reader Env, Error ToLCError] a
unToLC = view _ToLC


-- runToLC :: ToLC a -> Either ToLCError (a, GlobalVars)
runToLC :: ToLC a -> Either ToLCError a
runToLC (ToLC m) = runPureEff
                 . runErrorNoCallStack
                --  . runState initialState 
                 . runReader initialEnv $ m
  -- where
    -- initialState = mkGlobalVars HM.empty


--------------------------------------------------------------------------------------------------

isDeclaredVar :: MTExpr -> ToLC (Maybe Var)
isDeclaredVar = undefined

{- | Look for the global vars in a separate pass for now. 
May need Reader, but only going to think abt that when we get there -}
findGlobalVars :: Exp -> (Exp, GlobalVars)
findGlobalVars exp = (exp, placeholderTODO)
  where placeholderTODO = mkGlobalVars HM.empty

{- | Translate L4 program consisting of Hornlike rules to a LC Program -}
l4ToLCProgram :: (Foldable t, Traversable t) => t L4.Rule -> ToLC LCProgram
l4ToLCProgram rules = do
  l4HLs <- traverse simplifyL4Hlike rules
  (lcProg, globalVars) <- fmap findGlobalVars . l4sHLsToLCExp . F.toList $ l4HLs
  return $ MkLCProgram { progMetadata = MkLCProgMdata "[TODO] Not Yet Implemented"
                       , lcProgram = lcProg
                       , globalVars = globalVars}


{-==============================================================================
  1. Simplify L4: massage L4.Rule (Hornlike) into the more convenient SimpleHL 
===============================================================================-}

type SimpleHL = SimpleHlike VarTypeDeclMap RetVarInfo

simplifyL4Hlike :: L4.Rule -> ToLC SimpleHL
simplifyL4Hlike rule =
  case rule.srcref of
    Just srcref -> do
      baseHL <- extractBaseHL rule
      return $ MkSimpleHL { shcSrcRef = srcref
                          , shcGiven = maybe HM.empty mkVarEntMap rule.given
                          , shcRet  = rule.giveth ^.. folded % folding mkL4VarTypeDeclAssocList
                          , baseHL = baseHL
                          }
    Nothing -> throwParserParserProblem rule "Parser should not be returning L4 rules with Nothing in src ref"
{- this always takes up more time than one expects:
given :: Maybe ParamText = Maybe (NonEmpty TypedMulti) 
        = Maybe (NonEmpty 
                    (NonEmpty MTExpr, Maybe TypeSig))
-}

l4HcToAtomicHC :: L4.HornClause2 -> AtomicHC
l4HcToAtomicHC hc =
  case hc.hBody of
    Just hbody -> HeadAndBody $ MkHnBHC { hbHead = hc.hHead, hbBody = hbody }
    Nothing -> mkHeadOnlyAtomicHC hc.hHead

extractBaseHL :: L4.Rule -> ToLC BaseHL
extractBaseHL rule =
  case rule.clauses of
    [] -> throwParserParserProblem rule "Parser should not return L4 Hornlikes with no clauses"
    [hc] -> pure $ OneClause . l4HcToAtomicHC $ hc
    multipleHCs -> pure $ MultiClause . mkMultiClauseHL $ fmap l4HcToAtomicHC multipleHCs


--- Utils for dealing with 'Maybe ParamText' -------------------------------------
mkL4VarTypeDeclAssocList :: Foldable f => f TypedMulti -> [(Var, Maybe L4EntType)]
mkL4VarTypeDeclAssocList = convertL4Types . declaredVarsToAssocList
  where
    declaredVarsToAssocList :: Foldable f => f TypedMulti -> [(T.Text, Maybe L4.EntityType)]
    declaredVarsToAssocList dvars = dvars ^.. folded % to getGivenWithSimpleType % folded

    convertL4Types :: [(T.Text, Maybe L4.EntityType)] -> [(Var, Maybe L4EntType)]
    convertL4Types al =
      al
        & each % _1 %~ mkVar
        & each % _2 %~ fmap mkEntType

mkVarEntMap :: Foldable f => f TypedMulti -> VarTypeDeclMap
mkVarEntMap = HM.fromList . mkL4VarTypeDeclAssocList

{-================================================================================
  2. SimpleHL to Exp
=================================================================================-}

-- | Treat the seq of L4 rules as being a block of statements
l4sHLsToLCExp :: [SimpleHL] -> ToLC Exp
l4sHLsToLCExp rules = fmap mkExpFrSeqExp (l4sHLsToLCSeqExp rules)
  where
    mkExpFrSeqExp :: SeqExp -> Exp
    mkExpFrSeqExp seqExp = MkExp (ESeq seqExp) []

l4sHLsToLCSeqExp ::  [SimpleHL] -> ToLC SeqExp
l4sHLsToLCSeqExp = foldM go EmptySeqE
  where
    go :: SeqExp -> SimpleHL -> ToLC SeqExp
    go seqExp hornlike = ConsSE <$> expifyHL hornlike <*> pure seqExp

--------------------------------------------------------------------

expifyHL :: SimpleHL -> ToLC Exp
expifyHL hl = addMdataToBaseExp hl (baseExpify hl)
   where
    addMdataToBaseExp :: SimpleHL -> ToLC BaseExp -> ToLC Exp
    addMdataToBaseExp = undefined

baseExpify :: SimpleHL -> ToLC BaseExp
baseExpify (isIf -> Just (hl, hc)) = toIfExp hl hc
baseExpify hornlike = throwNotYetImplError hornlike
-- for the future, stuff like
-- baseExpify (isLamDef -> ...) = toLamDef ...

isIf :: SimpleHL -> Maybe (SimpleHL, HnBodHC)
isIf hl =
  case hl.baseHL of
    MultiClause _ -> Nothing
    OneClause atomicHC ->
      case atomicHC of
        HeadOnly _ -> Nothing
        HeadAndBody hc -> Just (hl, hc)

toIfExp :: SimpleHL -> HnBodHC -> ToLC BaseExp
toIfExp hl hc = do
  condE <- withinGivenAugmentedEnv $ processHcBody hc.hbBody
  thenE <- withinGivenAugmentedEnv $ processHchead hc.hbHead
  return $ EIfThen condE thenE
  where
    withinGivenAugmentedEnv = over _ToLC (local setLocalVars)
    setLocalVars :: Env -> Env
    setLocalVars = set #localVars hl.shcGiven

-- TODO: Add metadata from hl
-- TODO: If Then with ELSE for v2



--------------------------------------------------------------------

{- | What can be in hHead?
1. Set Var
    (i) Simple Set Var: 
      * `RPConstraint [ MTT "n3c" ] RPis [ MTT "n1 + n2" ]`
    (ii) Set Var True (typically with an IF):
      * `RPMT [ MTT "case 1 qualifies" ]
-}
processHchead :: L4.RelationalPredicate -> ToLC Exp
processHchead = undefined

processHcBody :: L4.BoolStructR -> ToLC Exp
processHcBody = undefined


---------------------------------------------------------

isL4BlockOfStatements :: L4.Rule -> Bool
isL4BlockOfStatements rule = length rule.clauses > 1
