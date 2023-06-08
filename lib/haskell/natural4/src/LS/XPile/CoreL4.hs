{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

{-| transpiler to CoreL4 (BabyL4). See the `baby-l4` repository. -}

module LS.XPile.CoreL4 where

import Prettyprinter

import AnyAll
import LS.PrettyPrinter
import L4.Syntax as L4 hiding (All, trueVNoType, falseVNoType) -- TODO, to be reconsidered
import qualified LS.XPile.ToASP as ASP
import qualified LS.XPile.ToEpilog_fm_nat as Epilog

import ToDMN.FromL4 (genXMLTreeNoType)

import L4.Annotation
import LS as SFL4

-- import Data.Function ( (&) )
import Data.Functor ( (<&>) )
-- import Control.Arrow ( (>>>) )

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe (catMaybes, fromMaybe, isJust, fromJust)
import qualified Data.List.NonEmpty as NE
-- import           Data.List.NonEmpty (NonEmpty((:|)))
-- import Text.Pretty.Simple (pShow, pShowNoColor)
-- import qualified Data.Text.Lazy as TL
import Control.Monad (guard, join)
import Data.Either (rights, isRight, fromRight)
import Control.Monad.Except ( MonadError(throwError) )
-- import qualified Data.Traversable as DT

import Text.Regex.TDFA
import Data.List (nub, intercalate, (\\), isPrefixOf, elemIndex)
import qualified Data.Foldable as DF
import Data.Map ((!))

-- TODO: the following is only for testing purposes, can be removed later
import L4.PrintProg (showL4, PrintSystem (L4Style), PrintConfig (PrintSystem))
import L4.SyntaxManipulation (applyVarsNoType, funArgsToAppNoType)
import LS.Tokens (undeepers)

import qualified Text.XML.HXT.Core as HXT

import Debug.Trace (trace)
import LS.XPile.Logging ( mutter, mutters, XPileLog, XPileLogE, xpReturn, xpError, xpLog )
type ExprM a = Either String (Expr a)

-- output to Core L4 for further transformation

-- TODO: could be removed: the result type of transpilation 
-- is now ... () (as in Program()) and not ... SRng (as in Program SRng)
sfl4Dummy :: SRng
sfl4Dummy = DummySRng "From spreadsheet"

sfl4ToBabyl4 :: Interpreted -> String
sfl4ToBabyl4 l4i = show $ sfl4ToCorel4Program l4i

sfl4ToASP :: [SFL4.Rule] -> XPileLogE String
sfl4ToASP rs = do
  let (rulesTransformed, errs) = xpLog $ mapM sfl4ToCorel4Rule rs
      prg = Program () (concat $ rights rulesTransformed)
      doc = ASP.astToDoc prg
                   -- trace ("asp" ++ (show $ showL4 [] prg)) $
  mutters errs
  xpReturn (show doc)

sfl4ToEpilog :: [SFL4.Rule] -> XPileLogE String
sfl4ToEpilog rs = do
  let (rulesTransformed, errs) = xpLog $ mapM sfl4ToCorel4Rule rs
      prg = Program () (concat $ rights rulesTransformed)
      doc = Epilog.astToDoc prg
  mutters errs
  xpReturn $ show doc

-- destructure (Rule t) from this
-- data TopLevelElement t = RuleTLE (Rule t) | ...
-- not actually necessary

-- sfl4ToDMN :: HXT.ArrowXml cat => [SFL4.Rule] -> cat a HXT.XmlTree
sfl4ToDMN :: [SFL4.Rule] -> XPileLog (HXT.IOSLA (HXT.XIOState ()) HXT.XmlTree HXT.XmlTree)
sfl4ToDMN rs = do
  let (rulesTransformed, errs) = xpLog $ mapM sfl4ToCorel4Rule rs
      prg = Program () (concat $ rights rulesTransformed)
  -- in trace ("dmn" ++ (show $ showL4 [] prg)) $ genXMLTree prg
  mutters errs
  return $ genXMLTreeNoType prg

sfl4ToCorel4 :: [SFL4.Rule] -> XPileLogE String
sfl4ToCorel4 rs =
  let interpreted = l4interpret (defaultInterpreterOptions { enums2decls = True }) rs
      -- sTable = scopetable interpreted
      cTable = classtable interpreted
      pclasses = myrender $ prettyClasses cTable
      pBoilerplate = myrender $ prettyBoilerplate cTable
      hardCoded = unlines [ "decl age: Number"
                          , "decl weight: Number"
                          , "decl height: Number"
                          , "decl years: Unit"
                          , "decl meters: Unit"
                          , "decl kg: Unit"
                          , "decl relLT: Number -> Number -> Unit -> Boolean"
                          , "decl relLTE: Number -> Number -> Unit -> Boolean"

                          , "class Unit"
                          , "decl tb_length: Number" -- [TODO] the hc2decls should be responsible for this.
                          , "decl wingspan: Number"
                          , ""
                          ]

  in xpReturn $ unlines $ nubstrings $ concatMap lines
  ( [ -- "#\n# outputted via L4.Program types\n#\n\n"
      -- , ppCorel4 . sfl4ToCorel4Program $ rs
      "\n#\n# outputted directly from XPile/CoreL4.hs\n#\n"
      -- some hardcoding while we debug the transpiler and babyl4 interpreter
    , hardCoded
    , "\n\n## classes\n",                   T.unpack pclasses
    , "\n\n## boilerplate\n",               T.unpack pBoilerplate

    , "\n\n## decls for predicates used in rules (and not defined above already)\n"
    , T.unpack . myrender $ prettyDecls (T.pack hardCoded <> pclasses <> pBoilerplate) rs

    -- honestly i think we can just live without these
    --               , "\n\n## facts\n",                     show $ prettyFacts   sTable
    --               , "\n\n## defn from decision rules\n",  show $ prettyDefns   rs

    , "\n# directToCore\n\n"
    ] ++
    [ T.unpack $ myrender (directToCore r)
    | r <- rs
    ]
  )
  where
    -- dedup input; if a line has previously been output, don't output it again.
    nubstrings :: [String] -> [String]
    nubstrings xs =
      let zipped = zip xs [1..]
          xMap2 = Map.fromList (reverse zipped)
          xtends = Map.fromListWith (<>) (T.breakOn " extends " . T.pack <$> xs)
      in
        [ -- show n ++ ": " ++ show (xMap2 Map.! l) ++ ": " ++
          l
        | (l,n) <- zipped
        , if "decl" `isPrefixOf` l || "class" `isPrefixOf` l
          then (n :: Int) <= xMap2 ! l
               && maybe True (== "") (Map.lookup (T.pack l) xtends)
          else True
        ]

-- | go directly from Natural L4 to Baby L4, bypassing the text-file step.
-- We need some amount of type inference here to upgrade natural l4 to be as rigorously typed as core l4.
-- [TODO] this has been planned for some time. LET'S DO THIS.
-- well, maybe next year, as Sondheim said.

sfl4ToCorel4Program :: Interpreted -> L4.Program ()
sfl4ToCorel4Program l4i
  = Program { annotOfProgram = ()
            , elementsOfProgram = [] }
-- [TODO] we could also go from the output of Interpreter, e.g. with qaHorns*

ppCorel4 :: L4.Program () -> String
ppCorel4 p =
  T.unpack $ myrender (vsep $ pptle <$> elementsOfProgram p)

pptle :: TopLevelElement () -> Doc ann
pptle (ClassDeclTLE cdcl) = "class" <+> snake_inner (MTT . T.pack . stringOfClassName . nameOfClassDecl $ cdcl)

pptle (RuleTLE Rule { nameOfRule }) =
  vsep [nameOfRule']
  where
    nameOfRule' = fromMaybe
      -- If the rule doesn't have a name, just use an empty string.
      "" $
      -- Otherwise if the rule has a name, we turn it into
      -- rule <RULE_NAME>
      nameOfRule
      <&> (\x -> ["rule <", x, ">"])
      <&> foldMap pretty

pptle tle                 = vsep ( "## pptle: UNIMPLEMENTED, showing Haskell source:"
                                   : (pretty . ("## " <>) <$> lines (show tle)) )

-- TODO: remove after import from BabyL4 works correctly
trueVNoType :: Expr ()
trueVNoType = ValE () (BoolV True)
falseVNoType :: Expr ()
falseVNoType = ValE () (BoolV False)

-- TODO: BEGIN helper functions
-- maybe move into BabyL4/SyntaxManipulations.hs

-- Convert variable name to global variable
-- TODO: should be refined to generate local/global variable 
-- depending on contextual information when available
-- ASP TODO: add env (var list) as a second arg, and look up varname in env
-- i.e varNameToVarNoType :: VarName -> [String] -> Var ()
varNameToVarNoType :: [String] -> VarName -> Var ()
varNameToVarNoType cont vn
  | null cont = GlobalVar (QVarName () vn)
  | vn ==  head cont = LocalVar (QVarName () vn) (fromMaybe 0 (elemIndex vn cont))
  | otherwise = varNameToVarNoType (tail cont) vn

varsToExprNoType :: [Var t] -> ExprM t
varsToExprNoType (v:vs) = pure $ applyVarsNoType v vs
varsToExprNoType [] = throwError "internal error (varsToExprNoType [])"

multiTermToExprNoType :: [String] -> MultiTerm -> ExprM ()
-- multiTermToExprNoType = varsToExprNoType . map (varNameToVarNoType . T.unpack . mtexpr2text)
multiTermToExprNoType cont mt = do
  boo <- mapM (mtExprToExprNoType cont) mt
  case boo of
    ((VarE t v) : args) -> pure $ funArgsToAppNoType (VarE t v) args
    [e] -> pure e
    _ -> throwError "non-variable name in function position"


mtExprToExprNoType :: [String] -> MTExpr -> ExprM ()
mtExprToExprNoType cont (MTT t) = pure $ VarE () (varNameToVarNoType cont (T.unpack t))
mtExprToExprNoType _ (MTI i) = pure $ ValE () (IntV i)
mtExprToExprNoType _ (MTF i) = pure $ ValE () (FloatV i)
mtExprToExprNoType _ (MTB i) = pure $ ValE () (BoolV i)


rpRelToBComparOp :: RPRel -> Either String BinOp
rpRelToBComparOp cop = case cop of
  RPis       -> throwError "rpRelToBComparOp: erroring on RPis"
  RPhas      -> throwError "rpRelToBComparOp: erroring on RPhas"
  RPeq       -> pure $ BCompar BCeq
  RPlt       -> pure $ BCompar BClt
  RPlte      -> pure $ BCompar BClte
  RPgt       -> pure $ BCompar BCgt
  RPgte      -> pure $ BCompar BCgte
  RPelem     -> throwError "rpRelToBComparOp: erroring on RPelem"
  RPnotElem  -> throwError "rpRelToBComparOp: erroring on RPnotElem"
  RPnot      -> throwError "rpRelToBComparOp: erroring on RPnot"
  RPTC _     -> throwError "rpRelToBComparOp: erroring on RPTC"

conjExprNoType :: Expr () -> Expr () -> Expr ()
conjExprNoType = BinOpE () (BBool BBand)

disjExprNoType :: Expr () -> Expr () -> Expr ()
disjExprNoType = BinOpE () (BBool BBor)

conjsExprNoType :: [Expr ()] -> Expr ()
conjsExprNoType [] = trueVNoType
conjsExprNoType [e] = e
conjsExprNoType (e:es) = conjExprNoType e (conjsExprNoType es)

disjsExprNoType :: [Expr ()] -> Expr ()
disjsExprNoType [] = falseVNoType
disjsExprNoType [e] = e
disjsExprNoType (e:es) = disjExprNoType e (disjsExprNoType es)
-- END helper functions

boolStructRToExpr :: [String] -> BoolStructR -> ExprM ()
boolStructRToExpr cont bs = case bs of
  Leaf rp -> relationalPredicateToExpr cont rp
  All _m_la bss -> conjsExprNoType <$> mapM (boolStructRToExpr cont) bss
  Any _m_la bss -> disjsExprNoType <$> mapM (boolStructRToExpr cont) bss
  Not bs' -> UnaOpE () (UBool UBnot) <$> boolStructRToExpr cont bs'

relationalPredicateToExpr :: [String] -> RelationalPredicate -> ExprM ()
relationalPredicateToExpr cont rp = case rp of
  -- [TODO] use throwError here
  RPParamText ne -> trace ("CoreL4: relationalPredicateToExpr: erroring on RPParamText " <> show ne) $
                    pure $
                    ValE () (StringV $ "ERROR relationalPredicateToExpr not implemented for " ++ show ne)

  RPMT mts -> multiTermToExprNoType cont mts
  RPConstraint mts RPis mts' -> multiTermToExprNoType cont (mts' ++ mts)
  RPConstraint mts rr mts' ->
    let bop = rpRelToBComparOp rr
    in
      bop >>=
      (\r -> BinOpE () r <$> multiTermToExprNoType cont mts <*> multiTermToExprNoType cont mts')

  RPBoolStructR mts rr bs ->
    -- TODO: translate bs
    rpRelToBComparOp rr >>= (\r -> BinOpE () r <$> multiTermToExprNoType cont mts <*> pure falseVNoType)
  RPnary rr rp' -> throwError "relationalPredicateToExpr: erroring on RPnary"


-- ASP TODO: add env as a second arg, where env is a list of locally declared var names extracted from given clause
-- i.e. precondOfHornClauses :: [HornClause2] -> [String] -> Expr ()
precondOfHornClauses :: [String] -> [HornClause2] -> ExprM ()
precondOfHornClauses cont [HC _hh (Just hb)] = boolStructRToExpr cont hb
precondOfHornClauses _ _ = pure trueVNoType

postcondOfHornClauses :: [String] -> [HornClause2] -> ExprM ()
postcondOfHornClauses cont [HC hh _hb] = relationalPredicateToExpr cont hh
postcondOfHornClauses _ _ = pure trueVNoType

sfl4ToCorel4Rule :: SFL4.Rule -> XPileLogE [TopLevelElement ()]
sfl4ToCorel4Rule Regulative{} = mutter "sfl4ToCorel4Rule: Regulative rule unsupported, returning empty" >> xpReturn []

sfl4ToCorel4Rule h@Hornlike{..} =
            -- pull any type annotations out of the "given" paramtext as ClassDeclarations
            -- we do not pull type annotations out of the "upon" paramtext because that's an event so we need a different kind of toplevel -- maybe a AutomatonTLE?
            -- TODO: the following produces an error: Prelude.tail: empty list
            -- has been temporarily commented out 
            -- given2classdecls given ++
  rule
  where
    cont = createContext h
    given2classdecls :: Maybe ParamText -> [TopLevelElement ()]
    given2classdecls Nothing = []
    given2classdecls (Just pt) =
      catMaybes [ case ts of
                    Just (SimpleType TOne s1) -> Just $ ClassDeclTLE (ClassDecl { annotOfClassDecl = ()
                                                                                , nameOfClassDecl =  ClsNm (T.unpack s1)
                                                                                , defOfClassDecl = ClassDef [] []
                                                                                } )
                    _                         -> Nothing
                | ts <- snd <$> NE.toList pt
                ]
    -- ASP TODO: localContext = extractLocalsFromGiven given
    -- account also for the case where there are no givens in horn clause
    rule = do
      let preCond = precondOfHornClauses cont clauses
          postCond = postcondOfHornClauses cont clauses
      if isRight preCond && isRight postCond
      then xpReturn $ pure $ RuleTLE Rule
             { annotOfRule    = ()
             , nameOfRule     = rlabel <&> rl2text <&> T.unpack
             , instrOfRule    = []
             , varDeclsOfRule = []
             , precondOfRule  = fromRight (error "no precond") preCond
             -- ASP TODO: , precondOfRule  = precondOfHornClauses localContext clauses
             , postcondOfRule = fromRight (error "no postcond") postCond
             }
      else mutter "sfl4ToCorel4Rule: preCond and postCond were problematic, returning empty" >> xpReturn []


sfl4ToCorel4Rule Constitutive{ } = mutter "sfl4ToCorel4Rule: erroring on Constitutive" >> xpReturn mempty
sfl4ToCorel4Rule TypeDecl{..} = xpReturn [ClassDeclTLE (ClassDecl { annotOfClassDecl = ()
                                                         , nameOfClassDecl  = ClsNm $ T.unpack (mt2text name)
                                                         , defOfClassDecl   = ClassDef [] []}) ]
sfl4ToCorel4Rule DefNameAlias {} = mutter  "sfl4ToCorel4Rule: not handling DefNameAlias"  >> xpReturn mempty
sfl4ToCorel4Rule (RuleAlias _)   = mutter  "sfl4ToCorel4Rule: not handling RuleAlias"     >> xpReturn mempty   -- internal softlink to a constitutive rule label = _
sfl4ToCorel4Rule RegFulfilled    = mutter  "sfl4ToCorel4Rule: not handling RegFulfilled"  >> xpReturn mempty
sfl4ToCorel4Rule RegBreach       = mutter  "sfl4ToCorel4Rule: not handling RegBreach"     >> xpReturn mempty
sfl4ToCorel4Rule Scenario {}     = mutter  "sfl4ToCorel4Rule: not handling Scenario"      >> xpReturn mempty
sfl4ToCorel4Rule DefTypically {} = mutter  "sfl4ToCorel4Rule: not handling DefTypically"  >> xpReturn mempty
sfl4ToCorel4Rule RuleGroup {}    = mutter  "sfl4ToCorel4Rule: not handling RuleGroup"  >> xpReturn mempty
sfl4ToCorel4Rule (NotARule _)    = mutter  "sfl4ToCorel4Rule: not handling NotARule"   >> xpReturn mempty

-- we need some function to convert a HornClause2 to an Expr
-- in practice, a BoolStructR to an Expr
-- where the RPMT elements of the BooLStructR are nullary, unary, or binary operators depending on how many elements are in the list


-- p :: Person
-- p.dependents           // javascript
-- | p | dependents |
-- | p's dependents |


-- every predicate in a Hornlike Declare rule needs to be typed.
-- let's walk through all the bodyNonEx

-- see also comments in Prettyprinter.hs around RP1

-- [TODO] we have a situation where we have class Incident attribute Vehicle that is typed Asset
-- and in a rule which speaks of Vehicle we need to know how to resolve that type intelligently.
-- i am guessing we need to quantify the entire class ancestry?

directToCore :: SFL4.Rule -> Doc ann
directToCore r@Hornlike{keyword}
  | keyword /= Define =
      vsep [
      case hBod of
        Just _ ->
          let (bodyEx, bodyNonEx) = partitionExistentials c
          in
          vsep
          [ "rule" <+> angles rname
          , maybe "# for-limb absent" (\x -> "for"  <+> prettyTypedMulti x) (given r <> bsr2pt bodyEx )
          ,                                  "if"   <+> haskellStyle (RP1 <$> bodyNonEx )
          ,                                  "then" <+> pretty (RP1  $  hHead c)
          , Prettyprinter.line
          ]
            -- [TODO] when testing for an optional boolean, add a hasAttrname test inside bodyNonEx
        Nothing -> vsep ( "#####" <+> rname : prettyDefnCs rname [ c ]) <> Prettyprinter.line
      | (c,cnum) <- zip (clauses r) [1..]
      , (HC _headRP hBod) <- [c]
      , let needClauseNumbering = length (clauses r) > 1
      , let rname = prettyRuleName cnum needClauseNumbering (ruleLabelName r)
      ]
  | otherwise = "# DEFINE rules unsupported at the moment"
-- fact <rulename> multiterm

-- [TODO] -- we can relate classes and attributes by saying in babyl4:
--                     for i: Incident.  a: Asset.  i.vehicle(a)
-- we can also say:    for i: Incident,  a: Asset  -- this one will work better for translation to Epilog.
--                     if (vehicle i a && ...)
--                     then ...

directToCore TypeDecl{} = ""
directToCore _ = ""


hc2decls :: SFL4.Rule -> Doc ann
hc2decls r
  | hasClauses r =
    vsep
    [ "decl" <+> pretty pf <> encloseSep ": " "" " -> " (declType ++ ["Boolean"])
  --    <> Prettyprinter.line
  --    <> "### headRP: " <> viaShow headRP <> Prettyprinter.line
  --    <> "### hBod: "   <> viaShow hBod   <> Prettyprinter.line
  --    <> "### xform 1 headRP:" <+> viaShow headRP <> Prettyprinter.line
  --    <> "### xform 1 hBod:"   <+> viaShow (maybe [] DF.toList hBod) <> Prettyprinter.line
  --    <> "### xform 2:"        <+> viaShow (inPredicateForm <$> headRP : maybe [] DF.toList hBod) <> Prettyprinter.line
  --    <> "### typemap:"        <+> viaShow typeMap <> Prettyprinter.line
    | c@(HC headRP hBod) <- clauses r
    , pf:pfs <- inPredicateForm <$> headRP : maybe [] DF.toList hBod
    , T.take 3 (mtexpr2text pf) /= "rel"
    , let (bodyEx, _bodyNonEx) = partitionExistentials c
          localEnv = given r <> bsr2pt bodyEx
          typeMap = Map.fromList [ (varName, fromJust varType) -- safe due to isJust test below
                                 | (varName, mtypesig) <- maybe [] (fmap (mapFst NE.head) . NE.toList) localEnv
                                 , let underlyingm = getUnderlyingType <$> mtypesig
                                 , isJust underlyingm
                                 , isRight $ fromJust underlyingm
                                 , let varType = rightToMaybe =<< underlyingm
                                 , isJust varType
                                 ]
          declType = fmap pretty $ catMaybes $ flip Map.lookup typeMap <$> pfs
    ]
  where
    mapFst f (x,y) = (f x,y)
    rightToMaybe (Left _) = Nothing
    rightToMaybe (Right x) = Just x
hc2decls _ = emptyDoc


prettyTypedMulti :: ParamText -> Doc ann
prettyTypedMulti pt = pretty $ PT3 pt

prettyRuleName :: Int -> Bool -> RuleName -> Doc ann
prettyRuleName cnum needed text = snake_case text <> (if needed then "_" <> pretty cnum else mempty)

-- deal with this properly rather than doing all this icky string manipulation
--- -- but we would have to refactor the output from hc2decls to not be a Doc, and it would be harder to insert manual overrides
prettyDecls :: T.Text -> [SFL4.Rule] -> Doc ann
prettyDecls previously rs =
  let previousDecls = Map.fromList $ (,""::String) . T.takeWhile (/= ':') <$> filter ("decl " `T.isPrefixOf`) (T.lines previously)
      predDecls = Map.fromList $ T.breakOn ":" <$> T.lines (myrender $ vsep (hc2decls <$> rs))
  in pretty $ T.unlines $ uncurry (<>) <$> Map.toList (predDecls Map.\\ previousDecls)


-- [TODO]
-- fact <helplimit>
-- for p : Policy
-- HelpLimit p 7






prettyFacts :: ScopeTabs -> Doc ann
prettyFacts sctabs =
  vsep $ concat
  [ -- global symtab as facts
    [ "fact" <+> angles (snake_case scopename)
    , commentShow "#" symtab'
    ]
  | (scopename , symtab') <- Map.toList sctabs
  , (_mt, (_symtype,_vals)) <- Map.toList symtab'
  ]

-- | enums are exhaustive and disjoint
prettyBoilerplate :: ClsTab -> Doc ann
prettyBoilerplate ct@(CT ch) =
  vsep $ concat [
  [ "fact" <+> angles (c_name <> "Exhaustive")
  , "for x:" <+> c_name
  , encloseSep "" "" " || " $ (\x -> parens ("x" <+> "==" <+> pretty x)) <$> enumList
  , ""
  , "fact" <+> angles (c_name <> "Disj")
  , encloseSep "" "" " && " $ (\(x,y) -> parens (snake_inner (MTT x) <+> "/=" <+> snake_inner (MTT y))) <$> pairwise enumList
  , ""
  ]
  | className <- getCTkeys ct
  , Just (ctype, _) <- [Map.lookup className ch]
  , (Just (InlineEnum TOne nelist),_) <- [ctype]
  , let c_name = snake_inner (MTT className)
        enumList = enumLabels_ nelist
  ]
  where
    pairwise :: [a] -> [(a, a)]
    pairwise [] = []
    pairwise (x:xs) = [(x, y) | y <- xs] ++ pairwise xs

-- | print arithmetic elements as defn
-- eg: defn minsavings : Integer -> Integer = \x : Integer ->         5000 * x
--     defn minincome  : Integer -> Integer = \x : Integer -> 15000 + 4000 * x

commentShow :: Show a => T.Text -> a -> Doc ann
commentShow c x = commentWith c (T.lines (T.pack (show x)))

prettyDefnCs :: Doc ann -> [SFL4.HornClause2] -> [Doc ann]
prettyDefnCs rname cs =
  [
    if null myterms
    then
      "fact" <+> angles rname <> Prettyprinter.line <>
      commentShow "#" cl <>
      pretty (RP1 clHead)
    else
      "defn" <+>
      -- we assume the lhs is "p something" so we get rid of the p
      pretty (mt2text (tail lhs)) <+> colon <+>
      -- rip out "p's dependents" and "dependents p" from the input rhs
      -- nub and zip map them to integer indices
      -- each integer index becomes an x y z a b c d etc
      -- perhaps wiser if we use x1 x2 x3 instead of x y z
      -- them we output it all back with the input terms rewritten to x1 x2 x3
      encloseSep "" "" " -> " (intypes ++ [ returntype ])
      <+> equals <+>
      encloseSep "" "" " -> " ([ "\\" <> idx <+> colon <+> typ
                               | (typ,idx) <- zip intypes x123
                               ] ++ [ pretty outstr ])
      <> Prettyprinter.line <> commentShow "#" cl
    -- defn aPlusB : Integer -> Integer -> Integer = \x : Integer -> \y : Integer -> x + y
  | cl <- cs
  , let clHead = hHead cl
        clBody = hBody cl
  , clBody == Nothing

  -- [TODO] we had some code that detected which word of (Foo IS Bar) was previously
  -- encountered, and which was new. The new word (suppose it's Bar) would be the
  -- predicate, so it would turn into (Bar Foo).
  -- And that would work whether the input was (Foo IS Bar) or (Bar IS Foo).

  -- [TODO] convert "age < 16 years" to "age_in_years < 16"
  -- OR just convert to "age < 16"

  , (RPConstraint lhs RPis rhs) <- [clHead]
  , let rhss = T.unpack (mt2text rhs)
  , let myterms = getAllTextMatches (rhss =~ (intercalate "|" ["\\<[[:alpha:]]+'s [[:alpha:]]+\\>"
                                                          ,"\\<[[:alpha:]]( +[[:alpha:]]+)*\\>"]
                                          :: String)) :: [String]
        intypes = replicate (length myterms) "Integer"
        replacements = [ T.replace (T.pack t) (T.pack $ show n)
                       | (t,n) <- zip (nub myterms) x123 ]
        outstr = chain replacements (mt2text rhs)
        returntype = "Integer"

  ]
  where
    chain :: [a -> a] -> a -> a
    chain = foldr (.) id
    x123 = (\n -> "x" <> pretty n) <$> ([1..] :: [Int])

prettyDefns :: [SFL4.Rule] -> Doc ann
prettyDefns rs =
  vsep $ concat [ prettyDefnCs "" (clauses r)
                | r <- rs
                , hasClauses r
                ]


{-
 a word or two about our type system.

we do have a notion of extension: sub extends super.

the novel element here is, we use the same annotation syntax for both extension and typing!

suppose we have: DECLARE Thing1
we know thing1 extends thing0 because thing1 is the direct subject of a DECLARE.
so we emit: class Thing1

we support extension.
suppose we have: DECLARE Thing1 IS A Thing0
we would emit: class Thing1 extends Thing0

supppose we have: DECLARE Thing1 HAS thing2 IS A Thing3
we know that thing2 is an attribute of type Thing3
so we emit: class Thing1
            decl thing2: Thing1 -> Thing3 -> Boolean

however:

suppose we have: DECLARE Thing1 HAS Thing2 IS A Thing3
                                    HAS thing4 IS a Thing5

we would want to treat Thing2 as a class as well, extending Thing3, with its own attribute Thing4.
the way to sense this is to see if a child has children; if it does, it's a class.

so we emit: class Thing1
            class Thing2 extends thing3
            decl thing2: Thing1 -> Thing2 -> Boolean
            decl thing4: Thing2 -> Thing5 -> Boolean

Note the automatic downcasing of thing2 from the class version to the decl

And there would be an automatic corresponding upcasing from the decl to the class version.

special case for enums:

suppose we have: DECLARE Thing 1 HAS Thing2 IS ONEOF enumA enumB enumC

we treat Thing2 as a class, and enumA, enumB, enumC as members of the class.

so we emit: class Thing1
            class Thing2
            decl thing2: Thing1 -> Thing2 -> Boolean
            decl enumA: Thing2
            decl enumB: Thing2
            decl enumC: Thing2

-}

prettyClasses :: ClsTab -> Doc ann
prettyClasses ct =
  vsep $
  ("## allCTkeys:" <+> hsep (pretty <$> allCTkeys ct)) :
  "### explicitly defined classes" :
  concat [
  [ if null mytype && Map.null (unCT children) && null enumDecls
    then "###" <+> "type annotation for" <+> c_name <+> "is blank, is not enum, and has no children; not emitting a class" <+> uc_name
    else vsep [ if Map.null (unCT children) && null enumDecls
                then "###" <+> lc_name <+> "is a" <+> viaShow mytype <> "; without children; we know it is a decl dealt with by my parent function call"
                else "class" <+> uc_name <> extends
              , "###" <+> "children length = " <> viaShow (Map.size (unCT children))
              , "###" <+> "c_name = " <> viaShow c_name
              , "###" <+> "dot_name = " <> viaShow dot_name
              , "###" <+> "ctype = " <> viaShow ctype
              , "###" <+> "mytype = " <> viaShow mytype
              ]
  , if null childDecls then emptyDoc else vsep (commentShow "### class attributes are typed using decl:" children : childDecls)
  , if null enumDecls  then emptyDoc else vsep ("### members of enums are typed using decl" : enumDecls)
  ]
  | (classpath, (ctype, children)) <- SFL4.classGraph ct []
  , let dot_name = encloseSep "" "" "." $ -- snake_inner <$> reverse classpath
                   snake_inner . MTT <$> reverse classpath
        c_name' = untaint $ head classpath
        c_name = pretty c_name'
        uc_name = pretty $ ucfirst c_name'
        lc_name = pretty $ lcfirst c_name'
        mytype = case getUnderlyingType <$> getSymType ctype of
                   Just (Right s1) -> Just s1
                   _               -> Nothing
        extends = maybe emptyDoc ((" extends" <+>) . pretty) mytype
        enumDecls = [ "decl" <+> pretty member <> ":" <+> uc_name
                    | (Just (InlineEnum TOne nelist), _) <- [ctype]
                    , member <- enumLabels_ nelist
                    ]


        childDecls = [
          "###" <+> uc_name <+> "has a child attribute" <+> dquotes (pretty attrname) <> Prettyprinter.line <>
          "decl" <+> lc_childname <>
          case getSymType attrtype of
            -- if it's a boolean, we're done. if not, en-predicate it by having it take type and output bool
            Just (InlineEnum _ptype _pt) -> -- " #" <+> "ERROR: inline enums not supported for CoreL4; use a top-level enum instead."
              encloseSep ": " "" " -> " [ uc_name
                                        , uc_childname
                                        , "Boolean"]
            Nothing ->
              encloseSep ": " "" " -> " [ uc_name
                                        , uc_childname
                                        , "Boolean"]
            Just (SimpleType ptype pt) ->
              encloseSep ": " "" " -> " ([ uc_name
                                         , child_simpletype
                                         ] ++ case pt of
                                                "Boolean" -> []
                                                _         -> ["Boolean"]
                                        )
              <> if ptype == TOptional
                 then Prettyprinter.line <>
                      "decl" <+> ("has" <> uc_childname) <>
                      encloseSep ": " "" " -> " [ uc_name , "Boolean" ]
                      <+> "# auto-generated by CoreL4.hs, optional " <> snake_inner (MTT attrname)
                 else emptyDoc

          <> if childIsClass
             then Prettyprinter.line <> "class" <+> uc_childname
             else Prettyprinter.line <> "   # " <> lc_childname <+> "is an attribute, not a class." <>
                  if isJust child_ts && show child_simpletype `notElem` ["Boolean", "Number"]
                  then Prettyprinter.line <> "   # let's print its type as a class anyway." <>
                       Prettyprinter.line <> "class" <+> child_simpletype
                  else emptyDoc


          | (attrname, (attrtype, attrchildren)) <- Map.toList (unCT children)
          , let childIsClass = not $ Map.null (unCT attrchildren)
                lc_childname = pretty $ lcfirst $ untaint attrname
                uc_childname = pretty $ ucfirst $ untaint attrname
                child_ts = getSymType attrtype
                child_simpletype = maybe emptyDoc (prettySimpleType "corel4" (snake_inner . MTT)) child_ts
         ]
    -- guard to exclude certain forms which should not appear in the output
  , case (ctype,children) of
      ((Nothing, []),                 CT m) | m == Map.empty -> False | otherwise -> True
      ((Just (SimpleType TOne _), []),CT m) | m == Map.empty -> False | otherwise -> True
      _                                   -> True -- commentShow "# ctype:" ctype
      -- [TODO] and how do we treat enum types?
  ]
  ++ [ superClassesNotExplicitlyDefined
     , typesNotExplicitlyDefined ]

  where -- [TODO] -- move this to the Interpreter

    superClassesNotExplicitlyDefined :: Doc ann
    superClassesNotExplicitlyDefined =
      let
        knownClasses = getCTkeys ct
        superClasses = nub $ catMaybes $ clsParent ct <$> knownClasses
      in vsep $ ("### superclasses not explicitly defined" :
                 ( ("class" <+>) . pretty <$> (superClasses \\ knownClasses) ))
         ++ ["###"]

    typesNotExplicitlyDefined :: Doc ann
    typesNotExplicitlyDefined =
      let
        foundTypes = rights $ getUnderlyingType <$> concatMap (getAttrTypesIn ct) (getCTkeys ct)
        knownClasses = getCTkeys ct
      in vsep $ ("### types not explicitly defined" :
                 ( ("class" <+>) . pretty <$> ((foundTypes \\ knownClasses) \\ ["Object", "Number"]) ))
         ++ ["###"]

    ucfirst "" = ""
    ucfirst x = T.toUpper (T.singleton $ T.head x) <> T.tail x
    lcfirst "" = ""
    lcfirst x = T.toLower (T.singleton $ T.head x) <> T.tail x



-- runTestrules :: IO()
runTestrules :: [Doc ann]
runTestrules =
  let rls = testrules
      (rulesTransformed, errs) = xpLog $ mapM sfl4ToCorel4Rule rls
  in map (showL4 [PrintSystem L4Style]) (concat $ rights rulesTransformed)

  {-
>>> runTestrules
[rule <Rule_exceeds1>

if (((numberOfAffectedIndividuals db) >= 500) && ((numberOfAffectedIndividuals db) && ((numberOfAffectedIndividuals db) >= (foobars db))))
then (exceedsPrescrNumberOfIndividuals db),rule 

if ((green Bar) && (blue Baz))
then Foo,rule 

if ((green Bloo) && (red Blubs))
then Foo]


-}


{-
§	Rule_exceeds1								
GIVEN		db	IS	A	DataBreach				
DECIDE		exceedsPrescrNumberOfIndividuals					db		
WHEN		numberOfAffectedIndividuals					db	>=	500
-}
r1 :: SFL4.Rule
r1 = defaultHorn
  { name = [ MTT "savings account" ]
    , super = Nothing
    , keyword = Decide
    , given = Nothing
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = RPConstraint [ MTT "savings account" ] RPis [ MTT "inadequate" ]
            , hBody = Just
                ( Leaf
                    ( mkRpmt ["OTHERWISE"] )
                )
            }
        ]
    , rlabel = Nothing
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "test/tobb1.csv"
            , short = "test/tobb1.csv"
            , srcrow = 2
            , srccol = 19
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }


{-
DECIDE		Foo		
WHEN		Bar	IS	green
AND		Baz	IS	blue
-}
r2 :: SFL4.Rule
r2 = defaultHorn
  { name = [ MTT "Foo" ]
    , super = Nothing
    , keyword = Decide
    , given = Nothing
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = mkRpmt ["Foo"]
            , hBody = Just
                ( All Nothing
                    [ Leaf
                        ( RPConstraint [ MTT "Bar" ] RPis [ MTT "green" ] )
                    , Leaf
                        ( RPConstraint [ MTT "Baz" ] RPis [ MTT "blue" ] )
                    ]
                )
            }
        ]
    , rlabel = Nothing
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "test/tobb1.csv"
            , short = "test/tobb1.csv"
            , srcrow = 1
            , srccol = 6
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }

testrules :: [SFL4.Rule]
testrules = [ defaultHorn
    { name =
        [ MTT "exceedsPrescrNumberOfIndividuals"
        , MTT "db"
        ]
    , super = Nothing
    , keyword = Decide
    , given = Just
        (
            ( pure (MTT "db")
            , Just
                ( SimpleType TOne "DataBreach" )
            ) NE.:| []
        )
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = RPMT
                [ MTT "exceedsPrescrNumberOfIndividuals"
                , MTT "db"
                ]
            , hBody = Just
                ( All Nothing
                    [ Leaf
                        ( RPConstraint
                            [ MTT "numberOfAffectedIndividuals"
                            , MTT "db"
                            ] RPgte [ MTI 500 ]
                        )
                    , Leaf
                        ( RPMT
                            [ MTT "numberOfAffectedIndividuals"
                            , MTT "db"
                            ]
                        )
                    , Leaf
                        ( RPConstraint
                            [ MTT "numberOfAffectedIndividuals"
                            , MTT "db"
                            ] RPgte
                            [ MTT "foobars"
                            , MTT "db"
                            ]
                        )
                    ]
                )
            }
        ]
    , rlabel = Just
        ( "§"
        , 1
        , "Rule_exceeds1"
        )
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "test/tobb1.csv"
            , short = "test/tobb1.csv"
            , srcrow = 1
            , srccol = 1
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
  , defaultHorn
    { name = [ MTT "Foo" ]
    , super = Nothing
    , keyword = Decide
    , given = Nothing
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = mkRpmt ["Foo"]
            , hBody = Just
                ( All Nothing
                    [ Leaf
                        ( RPConstraint [ MTT "Bar" ] RPis [ MTT "green" ] )
                    , Leaf
                        ( RPConstraint [ MTT "Baz" ] RPis [ MTT "blue" ] )
                    ]
                )
            }
        ]
    , rlabel = Nothing
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "test/tobb1.csv"
            , short = "test/tobb1.csv"
            , srcrow = 1
            , srccol = 8
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
  , defaultHorn
    { name = [ MTT "Foo" ]
    , super = Nothing
    , keyword = Decide
    , given = Nothing
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = mkRpmt ["Foo"]
            , hBody = Just
                ( All Nothing
                    [ Leaf
                        ( RPConstraint [ MTT "Bloo" ] RPis [ MTT "green" ] )
                    , Leaf
                        ( RPConstraint [ MTT "Blubs" ] RPis [ MTT "red" ] )
                    ]
                )
            }
        ]
    , rlabel = Nothing
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "test/tobb1.csv"
            , short = "test/tobb1.csv"
            , srcrow = 1
            , srccol = 13
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }

  ]

-- New stuff

-- For easier reference
-- given :: Maybe ParamText
-- given :: Maybe (NonEmpty (NonEmpty MTExpr, Maybe TypeSig))

-- extractGiven :: SFL4.Rule -> [TypedMulti]
extractGiven :: SFL4.Rule -> [(NE.NonEmpty MTExpr, Maybe TypeSig)]
extractGiven Hornlike{given=Nothing}        = []
extractGiven Hornlike{given=Just paramtext} = NE.toList paramtext
extractGiven _                              = trace "not a Hornlike rule, not extracting given" mempty

-- typedMultitoMTExprs :: TypedMulti -> MultiTerm
typedMultitoMTExprs :: (NE.NonEmpty MTExpr, Maybe TypeSig) -> [MTExpr]
typedMultitoMTExprs (mtexprs, _) = NE.toList mtexprs

destructMTT :: MTExpr -> String
destructMTT (MTT x) = T.unpack x
destructMTT _       = error "nothing to destructure; not an MTT"

createContext :: SFL4.Rule -> [String]
createContext hlike = map destructMTT (concatMap typedMultitoMTExprs (extractGiven hlike))
