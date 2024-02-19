{-# OPTIONS_GHC -W #-}
-- {-# OPTIONS_GHC -foptimal-applicative-do #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, OverloadedLabels, UndecidableInstances, RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, DataKinds, TypeFamilies, FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}


module LS.XPile.MathLang.GenericMathLang.TranslateL4 where
-- TODO: Add export list

import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST -- TODO: Add import list
import LS.XPile.MathLang.Logging (LogConfig, defaultLogConfig)
-- TODO: Haven't actually finished setting up logging infra, unfortunately.
-- But it's also not really necessary for working on the transpiler

import AnyAll qualified as AA
import LS.Types as L4
  (SrcRef(..), RelationalPredicate(..), RPRel(..), MTExpr(..), EntityType,
  HornClause (..), HornClause2, BoolStructR,
  TypeSig(..), TypedMulti,
  SimpleHlike(..), BaseHL(..), AtomicHC(..), HeadOnlyHC(..),
  MultiClauseHL(..), _MkMultiClauseHL, mkMultiClauseHL,
  HeadOnlyHC, _MkHeadOnlyHC, mkHeadOnlyAtomicHC, mkHeadOnlyHC, headOnlyHLasMTEs,
  HnBodHC(..),
  mtexpr2text
  )

import LS.Rule (
                -- Interpreted(..),
                extractMTExprs, getGivenWithSimpleType,
                defaultHorn)
import LS.Rule qualified as L4 (Rule(..))

import Effectful (Effect, Eff, (:>), runPureEff)
-- import Effectful.TH (makeEffect)
-- import Effectful.Dispatch.Dynamic (send, interpret, localSeqUnlift)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Reader.Static (Reader, runReader, local, asks, ask)
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
import Control.Arrow ((>>>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Flow ((|>))
import Optics hiding ((|>))
import Data.Text.Optics (packed, unpacked)
import GHC.Generics (Generic)
import Data.Generics.Sum.Constructors
import Data.List.NonEmpty qualified as NE
-- import Data.Generics.Product.Types (types)
-- import Prettyprinter (Pretty)
import Data.String.Interpolate (__i)
import Data.String (IsString)
import Data.Text qualified as T
-- import LS.Utils.TextUtils (int2Text, float2Text)
import Data.Foldable qualified as F (toList, foldrM)

import LS.XPile.MathLang.UtilsLCReplDev
import LS.Utils ((|$>))

import qualified Data.List.NonEmpty as NE

-- for parsing expressions that are just strings inside MTExpr
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import Control.Monad.Trans (lift)
import Text.Megaparsec (ParsecT, runParserT, eof, (<?>), (<|>), try, some, many, between, choice, satisfy, parseError, notFollowedBy)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1, char)
import Data.Char (isAlphaNum)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void ( Void )
import Data.List.HT (partitionMaybe)

import Debug.Trace (trace)


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
stringifyToLCError lce = case lce of
  NotYetImplemented repr msg -> "NotYetImplemented: " <> repr <> "msg: " <> msg
  ParserProblem repr msg -> "ParserProblem: " <> repr <> "msg: " <> msg
  NotSupported repr msg -> "NotSupported: " <> repr <> "msg: " <> msg
  MiscError repr msg -> "MiscError: " <> repr <> "msg: " <> msg
  ErrImpossible repr msg -> "ErrImpossible: " <> repr <> "msg: " <> msg

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
                   , currSrcPos = MkPositn 0 0
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

mkToLC :: Eff [Reader Env, Error ToLCError] a -> ToLC a
mkToLC = view (re _ToLC)

unToLC :: ToLC a -> Eff [Reader Env, Error ToLCError] a
unToLC = view _ToLC

-- runToLC :: ToLC a -> Either ToLCError (a, GlobalVars)
runToLC :: ToLC a -> Either ToLCError a
runToLC (unToLC -> m) =
  m
    |> runReader initialEnv
    -- |> runState initialState
    |> runErrorNoCallStack
    |> runPureEff
  -- where
    -- initialState = mkGlobalVars HM.empty

--------------------------------------------------------------------------------------------------

{- | Look for the global vars in a separate pass for now.
May need Reader, but only going to think abt that when we get there -}
findGlobalVars :: Exp -> (Exp, GlobalVars)
findGlobalVars exp = (exp, placeholderTODO)
  where placeholderTODO = mkGlobalVars HM.empty

{- | Translate L4 program consisting of Hornlike rules to a LC Program -}
l4ToLCProgram :: Traversable t => t L4.Rule -> ToLC LCProgram
l4ToLCProgram rules = do
  l4HLs <- traverse simplifyL4Hlike rules
  (lcProg, globalVars) <- fmap findGlobalVars . l4sHLsToLCExp . F.toList $ l4HLs
  pure MkLCProgram { progMetadata = MkLCProgMdata "[TODO] Not Yet Implemented"
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
      pure MkSimpleHL { shcSrcRef = srcref
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
    declaredVarsToAssocList :: Foldable f => f TypedMulti -> [(T.Text, Maybe L4.EntityType)]
    declaredVarsToAssocList dvars = dvars ^.. folded % to getGivenWithSimpleType % folded

    convertL4Types :: [(T.Text, Maybe L4.EntityType)] -> [(Var, Maybe L4EntType)]
    convertL4Types al =
      al
        |> each % _1 %~ mkVar
        |> each % _2 %~ fmap mkEntType

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

typeMdata :: T.Text -> BaseExp -> Exp
typeMdata typ bexp = MkExp bexp (inferredType [] typ)

inferredType :: MdGrp -> T.Text -> MdGrp
inferredType (md:mds) typ = md {typeLabel = Just $ Inferred typ}:mds
inferredType [] typ = [ MkExpMetadata
                          (MkPositn 0 0) -- TODO: inherit this from somewhere
                          (Just $ Inferred typ)
                          Nothing
                      ]

leaveTrace :: T.Text -> MdGrp -> MdGrp
leaveTrace str (md:mds) = md{typeLabel = (<> Inferred str) <$> md.typeLabel}:mds
leaveTrace str [] = inferredType [] str

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
expifyHL hl = do
  bexp <- baseExpify hl
  return $ MkExp bexp [mdata]
   where
    returnType = case hl.shcRet of
      [(_, mReturnType)] -> mReturnType
      [] -> Nothing
      xs -> error $ "expifyHL: the SimpleHL's shcRet has multiple return types, is this weird? " <> show xs
      -- TODO: when would this list have more than 1 element? if there are multiple GIVETHs?

    mdata = MkExpMetadata {
              srcPos = srcRefToSrcPos $ shcSrcRef hl
            , typeLabel = FromUser <$> returnType
            , explnAnnot = Nothing
            }

{- | My current understanding is that the LC Exps that a SimpleHL can be are:
1. If Then (maybe also If Then Else; not sure offhand)
2. Lam Def (TODO)
3. Block of statements / expressions (TODO)
-}
baseExpify :: SimpleHL -> ToLC BaseExp
baseExpify (isIf -> Just (hl, hc)) = toIfExp hl hc
baseExpify hl@(MkSimpleHL _ _ (OneClause (HeadOnly hc)) _) = toHeadOnlyExp hl hc
baseExpify (isMultiClause -> hls@(_:_)) = ESeq <$> l4sHLsToLCSeqExp hls
baseExpify hornlike = throwNotYetImplError hornlike
-- for the future, remove the catch all `baseExpify hornlike` and add stuff like
-- baseExpify (isLamDef -> ...) = ...
-- baseExpify (isBlockOfExps -> ...)) = ...


{- | TODO: Need to figure out (and decide) how to distinguish function definitions from IFs,
since right now this would consider something like the following to be an IF:
    GIVEN		ind		IS	A	Person
        t		IS	A	Timepoint
    DECIDE		ind	meets the property eligibility criteria for GSTV-Cash
    IF	NOT	ind	owns more than one property
    OR		ind	owns 2 or more HDB flats and no other property
-}
isIf :: SimpleHL -> Maybe (SimpleHL, HnBodHC)
isIf hl =
  case hl.baseHL of
    MultiClause _ -> Nothing
    OneClause atomicHC ->
      case atomicHC of
        HeadOnly _ -> Nothing
        HeadAndBody hc -> Just (hl, hc)

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
  return $ EIfThen condE thenE
  where
    withLocalVarsAndSrcPos = over _ToLC (local $ setCurrSrcPos . setLocalVars)
    setCurrSrcPos, setLocalVars :: Env -> Env
    setCurrSrcPos = set #currSrcPos (srcRefToSrcPos hl.shcSrcRef)
    setLocalVars = set #localVars hl.shcGiven
-- TODO: Add metadata from hl
-- TODO: If Then with ELSE for v2

toHeadOnlyExp :: SimpleHL -> HeadOnlyHC -> ToLC BaseExp
toHeadOnlyExp hl hc =
  withLocalVarsAndSrcPos $ LS.XPile.MathLang.GenericMathLang.GenericMathLangAST.exp <$> processHcHeadForIf hc.hcHead
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
processHcHeadForIf (isSetVarToTrue -> Just putativeVar) = noExtraMdata <$> mkSetVarTrue putativeVar
--processHcHeadForIf (isOtherSetVar -> Just (lefts, rights)) = noExtraMdata <$> mkOtherSetVar lefts rights
processHcHeadForIf rp = trace (show rp) $ expifyHeadRP rp
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
baseExpifyMTEs :: [MTExpr] -> ToLC BaseExp
baseExpifyMTEs (splitGenitives -> (genitives@(g:_), rest)) = do
  -- ind's parent's sibling's … income
  recname <- expifyMTEsNoMd [g] -- TODO fix later, just doing the first one now! Also check if it's a variable and if so, add metadata
  fieldname <- expifyMTEsNoMd rest -- income
  return $ ERec fieldname recname
baseExpifyMTEs mtes = case mtes of
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
        let litWeAssumeToBePred = noExtraMdata $ mteToLitExp mte2
        return $ EApp litWeAssumeToBePred var1
      (Nothing, Just var2) -> do -- "qualifies","ind" = qualifies(ind)
        let litWeAssumeToBePred = noExtraMdata $ mteToLitExp mte1
        return $ EApp litWeAssumeToBePred var2
      (Nothing, Nothing) -> throwNotSupportedWithMsgError (RPMT mtes) "Not sure if this is supported; not sure if spec is clear on this"

  _ -> trace ("baseExpifyMTEs: " <> show mtes) $ do
      expParsedAsText <- parseExpr $ MTT $ textifyMTEs mtes
      case expParsedAsText of
        ELit _ -> do
        -- TODO: this should definitely not be a sequence, what should it be instead???
          parsedExs <- mapM parseExpr mtes
          return $ ESeq $ foldr consSeqExp EmptySeqE parsedExs

        -- arithmetic expression like [MTT "m1",MTT "*",MTT "m2"]
        notStringLit -> return notStringLit

  where
    consSeqExp :: BaseExp -> SeqExp -> SeqExp
    consSeqExp be = ConsSE (noExtraMdata be)

    parseExpr :: MTExpr -> ToLC BaseExp
    parseExpr x@(MTT str) = do
      res <- runParserT pExpr "" str
--      res <- mres
      case res of
      -- case parse pExpr "dummy" str of
        Right exp@(EVar (MkVar str')) ->
          if str /= str'
            then return $ mteToLitExp x -- if it's just a String literal, don't use the megaparsec version—it removes whitespace, e.g. "Singapore citizen" -> "Singapore"
            else return $ exp
          -- TODO: find the right megaparsec way to fail if single term contains whitespace
        Right notStringLit -> return notStringLit
        Left error -> trace ("can't parse with pExpr: " <> show x) $ return $ mteToLitExp x
    parseExpr x = return $ mteToLitExp x


splitGenitives :: [MTExpr] -> ([MTExpr], [MTExpr])
splitGenitives = partitionMaybe isGenitive
  where
    -- removes the genitive s if it is genitive
    isGenitive :: MTExpr -> Maybe MTExpr
    isGenitive (MTT text) = case reverse $ T.unpack text of
      's':'\'':rest -> Just $ MTT $ T.pack $ reverse rest
      _ -> Nothing
    isGenitive x = Nothing

---------------- Expr parser ------------------------------------------------------
--type Parser = Parsec Void T.Text
type Parser = ParsecT Void T.Text ToLC

pExpr :: Parser BaseExp
pExpr = makeExprParser pTerm table <?> "expression"

--pTerm = parens pExpr <|> pIdentifier <?> "term"
pTerm :: Parser BaseExp
pTerm = choice $ map try
  [ parens pExpr
  , pVariable
  , pFloat
  , pInteger
  ]


table :: [[Operator Parser BaseExp]]
table = [ [ binary  "*" mul'
          , binary  "/" div' ]
        , [ binary  "+" plus'
          , binary  "-" minus' ]
        , [ binary  "<" lt'
          , binary  "<=" lte'
          , binary  ">" gt'
          , binary  ">=" gte'
          , binary  "==" numeq']
        ]

binary :: T.Text -> (BaseExp -> BaseExp -> BaseExp) -> Operator Parser BaseExp
binary name f = InfixL (f <$ symbol name)

mul', div', plus', minus', lt', lte', gt', gte', numeq' :: BaseExp -> BaseExp -> BaseExp
mul' x y = ENumOp OpMul (noExtraMdata x) (noExtraMdata y)
div' x y = ENumOp OpDiv (noExtraMdata x) (noExtraMdata y)
plus' x y = ENumOp OpPlus (noExtraMdata x) (noExtraMdata y)
minus' x y =  ENumOp OpMinus (noExtraMdata x) (noExtraMdata y)
lt' x y =  ECompOp OpLt (noExtraMdata x) (noExtraMdata y)
lte' x y =  ECompOp OpLte (noExtraMdata x) (noExtraMdata y)
gt' x y =  ECompOp OpGt (noExtraMdata x) (noExtraMdata y)
gte' x y =  ECompOp OpGte (noExtraMdata x) (noExtraMdata y)
numeq' x y = ECompOp OpNumEq (noExtraMdata x) (noExtraMdata y)

-- TODO: should we identify already here whether things are Vars or Lits?
-- Or make everything by default Var, and later on correct if the Var is not set.

pVariable :: Parser BaseExp
pVariable = do
  localVars :: VarTypeDeclMap <- lift $ ToLC $ asks localVars
  putativeVar <- T.pack <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
  case isDeclaredVarTxt putativeVar localVars of
    Just var -> return $ EVar var
    Nothing -> fail "not a declared variable"

pInteger :: Parser BaseExp
pInteger = ELit . EInteger <$> (lexeme L.decimal <* notFollowedBy (char '.')) <?> "integer"

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

expifyMTEsNoMd :: [MTExpr] -> ToLC Exp
expifyMTEsNoMd mtes = addMetadataToVar =<< baseExpifyMTEs mtes
 where
  -- TODO: here use the composOp style thing to do this transformation in all sub-BaseExps in the BaseExp!
  addMetadataToVar :: BaseExp -> ToLC Exp
  -- TODO: check that if isDeclaredVar, use the actual metadata, otherwise use inferred metadata
  addMetadataToVar bexp = case bexp of
                            EVar var -> mkVarExp var
                            _ -> return $ noExtraMdata bexp

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
  arg <- noExtraMdata <$> baseExpifyMTEs argMTEs
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
processHcBody = \case
  AA.Leaf rp -> expifyBodyRP rp
  -- TODO: Consider using the `mlbl` to augment with metadata
  AA.All _mlbl propns -> F.foldrM (makeOp EAnd) emptyExp propns
  AA.Any _mlbl propns -> F.foldrM (makeOp EOr) emptyExp propns
  AA.Not propn -> noExtraMdata . ENot <$> processHcBody propn
  where
    emptyExp :: Exp = noExtraMdata EEmpty

    -- TODO: Can try augmenting with `mlbl` here
    makeOp :: (Exp -> a -> BaseExp) -> L4.BoolStructR -> a -> ToLC Exp
    makeOp op bsr exp = noExtraMdata <$> (op <$> (toBoolEq <$> processHcBody bsr) <*> pure exp)

    toBoolEq e = e {exp = toBoolEqBE e.exp, md = inferredType e.md "Bool"}
    toBoolEqBE e@(ELit _) = ECompOp OpBoolEq (noExtraMdata e) (noExtraMdata (ELit EBoolTrue))
    toBoolEqBE e@(EApp _ _) = ECompOp OpBoolEq (noExtraMdata e) (noExtraMdata (ELit EBoolTrue))
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
  RPConstraint lefts RPis rights -> expifyHeadRP $ RPnary RPis [RPMT lefts, RPMT rights]
  RPnary RPis [RPMT [mte], rp]   -> do
    mvar <- isDeclaredVar mte
    exp <- expifyBodyRP rp
    varMd <- case mvar of
              Just var -> md <$> mkVarExp var -- only call mkVarExp to get the metadata
              Nothing -> return []
    varSet <- mkSetVarFromMTEsHelper [mte] exp -- metadata doesn't come here!
    return $ MkExp varSet varMd

  rp -> expifyBodyRP rp

expifyBodyRP :: RelationalPredicate -> ToLC Exp
expifyBodyRP = \case
  -- OTHERWISE
  RPMT (MTT "OTHERWISE" : _mtes) -> return $ typeMdata "Bool" (ELit EBoolTrue) -- throwNotYetImplError "The IF ... OTHERWISE ... construct has not been implemented yet"

  -- A single term could often be interpreted as 'var == True',
  -- but we choose to do that transformation later.
  rp@(RPMT [mte]) -> expifyMTEsNoMd [mte] -- adds metadata to Var, not others

  -- probably a good idea to merge with above, but keeping them separate just to remind myself to rethink
  rp@(RPMT mtes) -> expifyMTEsNoMd mtes

  rp@(RPMT _) -> throwNotSupportedWithMsgError rp "Not sure if this is supported; not sure if spec is clear on this"

  RPConstraint lefts rel rights -> expifyBodyRP $ RPnary rel [RPMT lefts, RPMT rights]
  RPnary RPis [rp1, rp2] -> do
    exp1maybeUntyped <- expifyBodyRP rp1
    exp2 <- inferTypeRHS <$> expifyBodyRP rp2
    let exp1 = inferTypeLHS exp1maybeUntyped exp2
    return $ noExtraMdata $ EIs exp1 exp2
  RPnary rel [rp1, rp2] -> do
    expLeft <- expifyBodyRP rp1
    expRight <- expifyBodyRP rp2
    numOrCompOp rel (numberType expLeft) (numberType expRight)

  -- The other cases: Either not yet implemented or not supported, with hacky erorr msges
  rp@(RPBoolStructR {}) -> throwNotSupportedWithMsgError rp "RPBoolStructR {} case of expifyBodyRP"
  rp@(RPParamText _) -> throwNotSupportedWithMsgError rp "RPParamText _ case of expifyBodyRP"
  rp -> throwNotSupportedWithMsgError rp "unknown rp"
  where
    numOrCompOp :: RPRel -> Exp -> Exp -> ToLC Exp
    numOrCompOp (rprel2compop -> Just compOp) = \x y -> return $ noExtraMdata $ ECompOp compOp x y
    numOrCompOp (rprel2numop -> Just numOp) = \x y -> return $  noExtraMdata $ ENumOp numOp x y
    numOrCompOp rprel = error $ "not implemented" <> show rprel

    rprel2numop :: RPRel -> Maybe NumOp
    rprel2numop rel = case rel of
      RPsum     -> Just OpPlus
      RPproduct -> Just OpMul
      RPmax     -> Just OpMaxOf
      RPmin     -> Just OpMinOf
      _         -> Nothing

    rprel2compop :: RPRel -> Maybe CompOp
    rprel2compop rel = case rel of
      RPlt   -> Just OpLt
      RPlte  -> Just OpLte
      RPgt   -> Just OpGt
      RPgte  -> Just OpGte
      RPeq   -> Just OpNumEq
      _      -> Nothing

    numberType :: Exp -> Exp
    numberType (MkExp bexp []) = MkExp bexp (inferredType [] "Number")
    numberType exp = exp

    inferTypeLHS :: Exp -> Exp -> Exp
    inferTypeLHS exp1 exp2 = case exp1.md of
      m:_ -> case m.typeLabel of
                Just _ -> exp1 -- exp1 has already a type, coming from localVars
                _ -> exp1 {md = leaveTrace (T.pack $ " copied over from " <> show exp2) exp2.md}
      [] -> exp1 {md = leaveTrace (T.pack $ " copied over from " <> show exp2) exp2.md} -- exp2 has potentially more reliable type info

    inferTypeRHS :: Exp -> Exp
    inferTypeRHS x@(MkExp bexp md) = case bexp of
      EVar (MkVar t)
        -> MkExp (ELit (EString t)) (inferredType md "String")
      ELit (EString _)
        -> MkExp bexp (inferredType md "String")
      ELit (EInteger _)
        -> MkExp bexp (inferredType md "Number")
      ELit (EFloat _)
        -> MkExp bexp (inferredType md "Number")
      _ -> x