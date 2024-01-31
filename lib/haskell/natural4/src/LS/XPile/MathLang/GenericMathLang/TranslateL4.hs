-- {-# OPTIONS_GHC -W #-}!
-- {-# OPTIONS_GHC -foptimal-applicative-do #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, OverloadedLabels, UndecidableInstances, RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
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
  HnBodHC(..),
  mtexpr2text
  )
-- import LS.Interpreter (qaHornsT)
import LS.Rule (
                -- Interpreted(..), 
                extractMTExprs, getGivenWithSimpleType,
                defaultHorn)
import LS.Rule qualified as L4 (Rule(..))

import Control.Monad (join)
import Effectful (Effect, Eff, (:>), runPureEff, raise)
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (send, interpret, localSeqUnlift)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Reader.Static (Reader, runReader, local, asks, ask)
-- import Control.Monad.Reader (MonadReader(..))
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
-- import Data.HashSet qualified as HS
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
import Data.Foldable qualified as F (toList, foldrM)

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

-- TODO: Add SrcPositn
data ToLCError = NotYetImplemented T.Text T.Text
               | ParserProblem T.Text T.Text
               | NotSupported T.Text T.Text
               | MiscError T.Text T.Text
  deriving stock (Eq, Show, Generic)

instance Hashable ToLCError


stringifyToLCError :: ToLCError -> T.Text
stringifyToLCError = undefined

---- Specialized error throwing convenience funcs ---------------------------------

-- | TODO: make this better with `pretty` and better structured errors later; see also `diagnose` package
throwErrorBase :: (Error e :> '[Error ToLCError], Show p) => (T.Text -> T.Text -> e) -> p -> T.Text -> ToLC a
throwErrorBase errorType l4ds msg = ToLC $ throwError $ errorType (T.pack . show $ l4ds) msg

throwNotYetImplError :: Show a => a -> ToLC b
throwNotYetImplError l4ds = throwErrorBase NotYetImplemented l4ds ""

throwNotSupportedError :: Show a => a -> ToLC b
throwNotSupportedError l4ds = throwErrorBase NotSupported l4ds ""

throwNotSupportedWithMsgError :: Show a => a -> T.Text  -> ToLC b
throwNotSupportedWithMsgError = throwErrorBase NotSupported

throwParserProblemWithMsg :: (Show a) => a -> T.Text -> ToLC c
throwParserProblemWithMsg = throwErrorBase ParserProblem

-------- Env -----------------------------------------------------------------------

type VarTypeDeclMap = HM.HashMap Var (Maybe L4EntType)
type RetVarInfo = [(Var, Maybe L4EntType)]

data Env =
  MkEnv { localVars :: VarTypeDeclMap
        -- ^ vars declared in GIVENs and WHERE (but NOT including those declared in GIVETH)
        -- , retVarInfo :: RetVarInfo
          -- ^ not sure we need retVarInfo
        , currSrcPos :: SrcPositn
        , logConfig :: LogConfig }
  deriving stock (Show, Generic)

initialEnv :: Env
initialEnv = MkEnv { localVars = HM.empty
                  --  , retVarInfo = []
                   , currSrcPos = undefined
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
    Nothing -> throwParserProblemWithMsg rule "Parser should not be returning L4 rules with Nothing in src ref"
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
    [] -> throwParserProblemWithMsg rule "Parser should not return L4 Hornlikes with no clauses"
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
  2. SimpleHL to LC Exp
=================================================================================-}

srcRefToSrcPos :: SrcRef -> SrcPositn
srcRefToSrcPos sr = MkPositn { row = sr.srcrow, col = sr.srccol }

-- | Convenience fn: For when there's no (additional) metadata to add to base exp
noExtraMdata :: BaseExp -> Exp
noExtraMdata baseexp = MkExp baseexp []

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
l4sHLsToLCSeqExp = F.foldrM go EmptySeqE
  where
    go :: SimpleHL -> SeqExp -> ToLC SeqExp
    go hornlike seqExp = ConsSE <$> expifyHL hornlike <*> pure seqExp

--------------------------------------------------------------------

expifyHL :: SimpleHL -> ToLC Exp
expifyHL hl = addMdataFromSimpleHL hl (baseExpify hl)
   where
    addMdataFromSimpleHL :: SimpleHL -> ToLC BaseExp -> ToLC Exp
    addMdataFromSimpleHL = undefined --TODO

{- | My current understanding is that the LC Exps that a SimpleHL can be are:
1. If Then (maybe also If Then Else; not sure offhand)
2. Lam Def
3. Block of statements / expressions
-}
baseExpify :: SimpleHL -> ToLC BaseExp
baseExpify (isIf -> Just (hl, hc)) = toIfExp hl hc
baseExpify hornlike = throwNotYetImplError hornlike
-- for the future, remove the catch all `baseExpify hornlike` and add stuff like
-- baseExpify (isLamDef -> ...) = ...
-- baseExpify (isBlockOfExps -> ...)) = ...

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
  condE <- withLocalVarsAndSrcPos $ processHcBody hc.hbBody
  thenE <- withLocalVarsAndSrcPos $ processHcHead hc.hbHead
  return $ EIfThen condE thenE
  where
    withLocalVarsAndSrcPos = over _ToLC (local $ setCurrSrcPos . setLocalVars)
    setCurrSrcPos, setLocalVars :: Env -> Env
    setCurrSrcPos = set #currSrcPos (srcRefToSrcPos hl.shcSrcRef)
    setLocalVars = set #localVars hl.shcGiven
-- TODO: Add metadata from hl
-- TODO: If Then with ELSE for v2

--------------------- Head of HL and Var Set ------------------------------------------------------------

{- | What can be in hHead?
Set Var
  (i) Simple Set Var: 
    * `RPConstraint [ MTT "n3c" ] RPis [ MTT "n1 + n2" ]`
  (ii) Set Var True (typically with an IF):
    * `RPMT [ MTT "case 1 qualifies" ]

Things like arithmetic constraints (<, >, etc) 
don't appear here -- they appear in hBody
-}
processHcHead :: L4.RelationalPredicate -> ToLC Exp
processHcHead (isSetVarToTrue -> Just putativeVar) = noExtraMdata <$> mkSetVarTrue putativeVar
processHcHead (isOtherSetVar -> Just (lefts, rights)) = noExtraMdata <$> mkOtherSetVar lefts rights
processHcHead rp = throwNotSupportedError rp

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
-}
expifyMTEs :: [MTExpr] -> ToLC BaseExp
expifyMTEs = undefined

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
mkVarSetTrueFromVar var mdFunc = mkVarSetFromVar var (mdFunc $ ELit EBoolTrue)

mkSetVarTrueExpFromVarNoMd :: Var -> ToLC Exp
mkSetVarTrueExpFromVarNoMd var = noExtraMdata <$> mkVarSetTrueFromVar var noExtraMdata


mkSetVarTrue :: [MTExpr] -> ToLC BaseExp
mkSetVarTrue putativeVar = mkSetVarFromMTEsHelper putativeVar (noExtraMdata $ ELit EBoolTrue)

mkOtherSetVar :: [MTExpr] -> [MTExpr] -> ToLC BaseExp
mkOtherSetVar putativeVar argMTEs = do
  arg <- noExtraMdata <$> expifyMTEs argMTEs
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
  MTT txt -> ELit . EString $ txt
  MTB True -> ELit EBoolTrue
  MTB False -> ELit EBoolFalse
  MTI integer -> ELit $ EInteger integer -- TODO: Can always change this if we want just one type for numbers
  MTF float -> ELit $ EFloat float

---------------------------------------------------------

processHcBody :: L4.BoolStructR -> ToLC Exp
processHcBody = \case
  AA.Leaf rp -> expifyBodyRP rp
  -- TODO: Consider using the `mlbl` to augment with metadata
  AA.All mlbl propns -> F.foldrM (makeOp EAnd) emptyExp propns
  AA.Any mlbl propns -> F.foldrM (makeOp EOr) emptyExp propns
  AA.Not propn -> noExtraMdata . ENot <$> processHcBody propn
  where
    emptyExp :: Exp = noExtraMdata EEmpty

    -- TODO: Can try augmenting with `mlbl` here
    makeOp :: (Exp -> a -> BaseExp) -> L4.BoolStructR -> a -> ToLC Exp
    makeOp op bsr exp = noExtraMdata <$> (op <$> processHcBody bsr <*> pure exp)


-- patterns for `expifyBodyRP`



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

This is where we might want to use pattern synonyms
-}
expifyBodyRP :: RelationalPredicate -> ToLC Exp
expifyBodyRP = \case

  -- 'var == True'
  rp@(RPMT [mte]) -> do
    mvar <- isDeclaredVar mte
    case mvar of
      Just var -> mkSetVarTrueExpFromVarNoMd var
      Nothing -> throwNotSupportedWithMsgError rp "Not sure if we can assume this means: 'check if <var> == True' --- would need to think through spec / conventions more"

  -- func app
  RPConstraint lefts RPis rights -> throwNotYetImplError "Func app not implemented / supported yet, but will hopefully be in next release"

  -- arithmetic comparisons
  RPConstraint lefts rel rights -> arithComparisons lefts rights rel


arithComparisons = undefined