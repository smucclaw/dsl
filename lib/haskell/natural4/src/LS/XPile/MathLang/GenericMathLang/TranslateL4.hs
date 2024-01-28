-- {-# OPTIONS_GHC -W #-}
-- {-# OPTIONS_GHC -foptimal-applicative-do #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, OverloadedLabels, UndecidableInstances #-}
{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, DataKinds, TypeFamilies, FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}


module LS.XPile.MathLang.GenericMathLang.TranslateL4 where
-- TODO: Add export list

import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST -- TODO: Add import list
import LS.XPile.MathLang.Logging (LogConfig, defaultLogConfig)

-- import AnyAll qualified as AA
-- import LS.Types qualified as L4
import LS.Types as L4 (RelationalPredicate(..), RPRel(..), MTExpr(..), EntityType, HornClause (..))
-- import LS.Interpreter (qaHornsT)
import LS.Rule (
                -- Interpreted(..), 
                extractMTExprs, getGivenWithSimpleType,
                defaultHorn)
import LS.Rule qualified as L4 (Rule(..))

import Control.Monad (foldM)
import Effectful (Effect, Eff, runPureEff)
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (send, interpret, localSeqUnlift)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Reader.Static (runReader, Reader)
import Effectful.State.Static.Shared (State, runState)
-- experimenting with Effectful.Error rn; will switch over to Control.Monad.Validate later
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
import GHC.Generics (Generic)
import Data.Generics.Sum.Constructors
-- import Data.Generics.Product.Types (types)
-- import Prettyprinter (Pretty)
import Data.String.Interpolate (__i)
import Data.String (IsString)
import Data.Text qualified as T
-- import LS.Utils.TextUtils (int2Text, float2Text)
import Data.Foldable qualified as F (toList)

import LS.Types (TypeSig(..), TypedMulti)
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


data ToLCError = NotYetImplemented T.Text -- SrcPositn
               | MiscError T.Text
  deriving stock (Eq, Show, Generic)

instance Hashable ToLCError

stringifyToLCError :: ToLCError -> T.Text
stringifyToLCError = undefined


-------- Abortive attempt at adapting Monad Validate for Effectful ---------- 

-- data EffValidate e :: Effect where
--   Refute :: e -> EffValidate e m a
--   Dispute :: e -> EffValidate e m ()
--   Tolerate :: m a -> EffValidate e m (Maybe a)
-- makeEffect ''EffValidate

-- type instance DispatchOf (EffValidate e) = Dynamic

-- instance (Semigroup e, V.MonadValidate e (Eff es), EffValidate e :> es) => V.MonadValidate e (Eff es) where
--   refute = send . Refute
--   dispute = send . Dispute
------------------------------------------------


data Env =
  MkEnv { localVars :: !(HashMap Var (Maybe L4EntType))
        -- ^ would *not* include 'global' GIVEN-declared vars --- just the local ones 
        , retVarInfo :: !(Maybe [(Var, Maybe L4EntType)])
          -- ^ not sure this is necessary
        , logConfig :: LogConfig }
  deriving stock (Show)
makeFieldLabelsNoPrefix ''Env

initialEnv :: Env
initialEnv = MkEnv { localVars = HM.empty
                   , retVarInfo = Nothing
                   , logConfig = defaultLogConfig }

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
newtype ToLC a = ToLC (Eff '[Reader Env, State GlobalVars, Error ToLCError] a)
  deriving newtype (Functor, Applicative, Monad)

-- | https://hackage.haskell.org/package/effectful-core-2.3.0.1/docs/Effectful-Dispatch-Dynamic.html#g:3
-- runValidate :: Eff (EffValidate e : es) a -> Eff es a
-- runValidate = interpret $ \localEnv -> \case
--   Refute errs -> localSeqUnlift localEnv $ \unlift -> do
--     V.refute errs


-- TODO: Adapting MonadValidate is prob going to be more complicated than in the WT presentation
runToLC :: ToLC a -> Either ToLCError (a, GlobalVars)
runToLC (ToLC m) = runPureEff . runErrorNoCallStack . runState initialState . runReader initialEnv $ m
  where
    initialState = mkGlobalVars HM.empty


--------------------------------------------------------------------

isDeclaredVar :: MTExpr -> ToLC (Maybe Var)
isDeclaredVar = undefined


{- | Translate L4 program consisting of Hornlike rules to a LC Program -}
l4ToLCProgram :: (Foldable seq, Traversable seq) => seq L4.Rule -> Either ToLCError LCProgram
l4ToLCProgram rules = do
  (lcProg, globalVars) <- runToLC . l4RulesToLCExp . F.toList $ rules
  return $ MkLCProgram { progMetadata = MkLCProgMdata "[TODO] Not Yet Implemented"
                       , lcProgram = lcProg
                       , globalVars = globalVars}



mkVarEntMap :: Foldable f => f TypedMulti -> HM.HashMap Var (Maybe L4EntType)
mkVarEntMap = processL4VarAssocList . declaredVarsToAssocList
  where
    declaredVarsToAssocList :: Foldable f => f TypedMulti -> [(T.Text, Maybe L4.EntityType)]
    declaredVarsToAssocList dvars = dvars ^.. folded % to getGivenWithSimpleType % folded

    processL4VarAssocList :: [(T.Text, Maybe L4.EntityType)] -> HM.HashMap Var (Maybe L4EntType)
    processL4VarAssocList al =
      al
        & each % _1 %~ mkVar
        & each % _2 %~ fmap mkEntType
        & HM.fromList

-- | Treat the seq of L4 rules as being a block of statements
l4RulesToLCExp :: [L4.Rule] -> ToLC Exp
l4RulesToLCExp rules = fmap mkExpFrSeqExp (l4RulesToLCSeqExp rules)
  where
    mkExpFrSeqExp :: SeqExp -> Exp
    mkExpFrSeqExp seqExp = MkExp (ESeq seqExp) []


l4RulesToLCSeqExp ::  [L4.Rule] -> ToLC SeqExp
l4RulesToLCSeqExp = foldM go EmptySeqE
  where
    go :: SeqExp -> L4.Rule -> ToLC SeqExp
    go seqExp rule = ConsSE <$> ruleToExp rule <*> pure seqExp

--------------------------------------------------------------------

ruleToExp :: L4.Rule -> ToLC Exp
ruleToExp rule = addMdataToBaseExp rule (ruleToBaseExp rule)
   where
    addMdataToBaseExp :: L4.Rule -> ToLC BaseExp -> ToLC Exp
    addMdataToBaseExp = undefined

ruleToBaseExp :: L4.Rule -> ToLC BaseExp
ruleToBaseExp (isIf -> baseIfExp) = undefined
ruleToBaseExp l4rule = undefined --not yet implemented or not supported
-- $ refute [(MiscError "not yet implemented or not supported")]
-- TODO: For future versions
-- ruleToBaseExp (isLamDef -> baseLamExp) = undefined
-- ruleToBaseExp (isBlockOfExps -> seqBaseExps) = undefined

isIf :: L4.Rule -> BaseExp
isIf rule = undefined

notL4BlockOfStatements :: L4.Rule -> Bool
notL4BlockOfStatements rule = length rule.clauses > 1

-- write this first as a way to help myself undrestand this better; can scrap it later
isIfHelper :: L4.Rule -> Bool
isIfHelper rule =
  length rule.clauses == 1 -- not sure abt this but go on for now
  && isn't _Nothing (rule.clauses ^? ix 0 % #hBody)

--------------------------------------------------------------------

{- | What can be in hHead?
* Set Var
    * Simple Set Var: 
      * `RPConstraint [ MTT "n3c" ] RPis [ MTT "n1 + n2" ]`
    * Set Var True IF ...:
      * `RPMT [ MTT "case 1 qualifies" ]
-}
processHChead = undefined



