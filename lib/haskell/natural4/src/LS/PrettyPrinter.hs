{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- the job of this module is to create orphan instances

{-|
This module instantiates a number of LS types into the Pretty typeclass used by Prettyprinter.
This is similar to instantiating into Show, but it all happens within Prettyprinter's "Doc ann" rather than String.
The pretty-printing then gets used by the transpilers.
-}

module LS.PrettyPrinter where

import AnyAll qualified as AA
import Data.Foldable qualified as DF
import Data.List (intersperse)
-- import qualified Data.Map as Map
import Data.List.NonEmpty as NE (NonEmpty ((:|)), head, tail, toList)
import Data.Text qualified as T
import Data.Traversable qualified as DT
import Debug.Trace
import LS.Rule
import LS.Types
import Prettyprinter
import Prettyprinter.Render.Text
import Text.Pretty.Simple qualified as TPS
import qualified Data.Text.Internal.Lazy

-- | Pretty MTExpr
instance Pretty MTExpr where
  pretty (MTT t) = pretty t
  pretty (MTI i) = pretty i
  pretty (MTF n) = pretty n
  pretty (MTB b) = pretty b

-- | Pretty MTExpr with quoting of string values; to use this, just wrap the MTExpr in an MT1 and pretty it as usual
data MTQ = MT1 MTExpr
         | MT2 MTExpr
instance Pretty MTQ where
  pretty (MT1 o@(MTT t)) = dquotes (pretty o) -- ^ double quotes
  pretty (MT1 o)         = pretty o
  pretty (MT2 o@(MTT t)) = squotes (pretty o) -- ^ single quotes
  pretty (MT2 o)         = pretty o

-- | Pretty RelationalPredicate: recurse
instance Pretty RelationalPredicate where
  pretty (RPParamText   pt)            = pretty $ pt2text pt
  pretty (RPMT          mt)            = snake_join mt
  pretty (RPConstraint  mt1 rprel mt2) = hsep [ snake_join mt1, pretty (rel2op rprel), snake_join mt2 ]
  pretty (RPBoolStructR mt1 rprel bsr) = hsep [ snake_join mt1, pretty rprel, pretty bsr ]
  pretty (RPnary rprel rps)            = hsep [ pretty rprel, pretty rps ]

-- Hornlike rule transformations -- these form HC2 situations
-- 1   p investment IS savings                     WHEN blah => if blah then investment p savings
-- 2   p investment IS savings                     OTHERWISE => if True then investment p savings
-- 3   p minSavings IS p's dependents * 5000  ( NO WHEN )    => defn p minsavings : \dependents -> dependents * 5000
-- 4   p dependents IS 5                      ( NO WHEN )    => fact <dependentsAdam> dependents adam 5

-- | RPCore is used ONLY by the CoreL4 transpiler to produce CoreL4-style function predicates, which are space-separated.
-- if we want to generalize this we can just have RP2 etc for other targets
-- or rename to RPcore or RPbabyl4
-- or add a string argument to RP1 String RelationalPredicate, so we can say RP1 "corel4" ....
newtype RP1 = RP1 RelationalPredicate
instance Pretty RP1 where
  pretty (RP1   (RPMT     [MTT "OTHERWISE"])     )   = "TRUE # default case"
  pretty (RP1 o@(RPConstraint  _mt1 RPis  [MTT "No"]))    = hsep $ "not" : (pretty <$> inPredicateForm o)
  pretty (RP1 o@(RPConstraint  _mt1 RPis  [MTB False]))   = hsep $ "not" : (pretty <$> inPredicateForm o)
  pretty (RP1 o@(RPConstraint  _mt1 RPis  _mt2))     = hsep $ pretty <$> inPredicateForm o
  pretty (RP1 o@(RPConstraint  _mt1 RPhas _mt2))     = hsep $ pretty <$> inPredicateForm o
  pretty (RP1   (RPConstraint   mt1 rprel  mt2))     = hsep [ pretty rprel, pred_snake mt1, hsep $ pretty . untaint . mtexpr2text <$> mt2 ]
  pretty (RP1   (RPBoolStructR  mt1 rprel  bsr))     = hsep [ pred_snake mt1, pretty rprel, AA.haskellStyle (RP1 <$> bsr) ]
                                               -- [TODO] confirm RP1 <$> bsr is the right thing to do
  pretty (RP1 o) = hsep $ pretty <$> inPredicateForm o

-- | [TODO] this is lossy -- preserve the original cell type information from the underlying multiterm in the relationalpredicate
inPredicateForm :: RelationalPredicate -> MultiTerm
inPredicateForm (RPParamText   pt)                = toList . (fst =<<) $ pt
inPredicateForm (RPMT     [MTT "OTHERWISE"])      = mempty
inPredicateForm (RPMT          mt)                = pred_flip mt
inPredicateForm (RPConstraint  mt  RPis  [MTT "Yes"]) = pred_flip mt
inPredicateForm (RPConstraint  mt  RPis  [MTB True] ) = pred_flip mt
inPredicateForm (RPConstraint  mt  RPis  [MTT "No"])  = pred_flip mt
inPredicateForm (RPConstraint  mt  RPis  [MTB False]) = pred_flip mt
inPredicateForm (RPConstraint  mt1 RPis  mt2)     = pred_flip mt2 ++ pred_flip mt1
inPredicateForm (RPConstraint  mt1 RPhas mt2)     = addHas (pred_flip mt2) ++ mt1
  where
    addHas (    x:xs) = MTT ("has"<>mtexpr2text x) : xs
    addHas [] = []
inPredicateForm (RPConstraint  mt1 rprel mt2)     = MTT (rel2txt rprel) : mt1 ++ mt2
inPredicateForm (RPBoolStructR mt1 _rprel bsr)    = mt1 ++ concatMap DF.toList (DT.traverse inPredicateForm bsr)
inPredicateForm (RPnary        rprel rps)         = MTT (rel2txt rprel) : concatMap inPredicateForm rps

pred_flip :: [a] -> [a]
pred_flip xs = last xs : init xs

{-
GIVEN p IS A Person
DECIDE p investment IS savings WHEN p savingsAccount IS inadequate
becomes
for p: Person
if savingsAccount p inadequate
then investment p savings
-}

-- --> bob's nephew
--     bob.nephew
--     nephew(bob,N) -- N is bob's nephew
-- --> nephew bob  -- bob is a nephew?
possessiveToObject :: T.Text -> T.Text
possessiveToObject str = T.intercalate " " $ reverse $ T.splitOn "'s " str


-- jack and jill are married. we use (jack x jill) to represent the relationship between jack and jill.
-- that relation can itself possess things:
-- (jack x jill)'s daughter
-- daughter of jack with jill
-- daughter (jack x jill)

-- multi-level:
-- john's mom's dad
-- dad of mom of john
-- dad(mom(john)) -- "function call"
-- dad(mom john)

-- mom john: john is a mom
-- mom(john): john's mother?

-- mom(john, M): M is john's mom


-- | pred_snake
-- "foo bar" "baz" --> "baz foo_bar
-- "fooBar" "baz"  --> "baz fooBar"
pred_snake :: MultiTerm -> Doc ann
pred_snake xs = encloseSep "" "" " " (snake_inner <$> last xs : init xs)

-- generalize the elimination of the units, across all the {pred,space}_{snake,join} functions.


-- | pred_join
-- "foo bar" "baz" --> "baz foo bar
-- "fooBar" "baz"  --> "baz fooBar"
pred_join :: MultiTerm -> Doc ann
pred_join (x:[MTT "years"]) = pred_join [x]
pred_join (x:[MTT "meters"]) = pred_join [x]
pred_join (x:[MTT "kg"]) = pred_join [x]
pred_join xs = encloseSep "" "" " " (pretty <$> last xs : init xs)


-- | space_join
-- "foo bar" "baz" --> "foo bar baz"
-- "fooBar" "baz"  --> "fooBar baz"
space_join :: MultiTerm -> Doc ann
space_join xs = encloseSep "" "" " " (pretty <$> xs)

-- | dot_join
-- "foo bar" "baz" --> "foo bar.baz"
-- "fooBar" "baz"  --> "fooBar.baz"
dot_join :: MultiTerm -> Doc ann
dot_join xs = encloseSep "" "" "." (pretty <$> xs)

-- | snake_join
-- "foo bar" "baz" --> "foo bar_baz"
-- "fooBar" "baz"  --> "fooBar_baz"
snake_join :: MultiTerm -> Doc ann
snake_join xs = encloseSep "" "" "_" (pretty <$> xs)

-- | snake_case
-- "foo bar" "baz" --> "foo_bar_baz"
-- "fooBar" "baz"  --> "fooBar_baz"
snake_case :: MultiTerm -> Doc ann
snake_case xs = encloseSep "" "" "_" (snake_inner <$> xs)

-- | snake_inner
-- "foo bar" --> "foo_bar"
-- "fooBar"" --> "fooBar"
snake_inner :: MTExpr -> Doc ann
snake_inner = pretty . untaint . mtexpr2text

untaint :: T.Text -> T.Text
untaint = T.replace " " "_" .
          T.replace "," "_" .
          T.replace "'" "_" .
          T.replace "%" "pct" .
          T.replace "(" "_" .
          T.replace ")" "_" .
          T.replace "-" "_" .
          T.replace "–" "_"
--          T.replace "\’" "_" -- [TODO] we need to replace all non-low-unicode characters with underscores.


instance Pretty RPRel where
  pretty rpr = pretty $ rel2txt rpr

newtype ParamText2 = PT2 ParamText

-- | ParamText2: the first line appears on the first line, subsequent lines appear on subsequent lines
-- if you want different ways of rendering a ParamText, go ahead and create newtypes for it.
-- because a ParamText is a type alias.
-- so it makes sense to marshal to a newtype PT2, PT3, PT4 etc that has the appropriate rendering instance.
instance Pretty ParamText2 where
  pretty (PT2 (x1 :| xs)) = nest 4 ( vsep ( hsep (pretty <$> NE.toList ( fst x1) )
                                           : (hsep . fmap pretty . toList . fst <$> xs) ) )

-- | ParamText3 is used by the CoreL4 transpiler to produce colon-annotated paramtexts.
newtype ParamText3 = PT3 ParamText
instance Pretty ParamText3 where
  pretty (PT3 pt) = hcat (intersperse ", " (toList $ typedOrNot "_" <$> pt))

-- | ParamText4 is used to approximate a recursive record. currently we can only go 2 deep.
-- in future we will have to upgrade ParamText to a full Tree type which can nest arbitrarily deep.
data ParamText4 = PT4 ParamText Interpreted -- VarPath
                | PT5 ParamText Interpreted
  deriving (Eq, Show)
instance Pretty ParamText4 where
  pretty (PT5 orig@(line1 :| line2s) l4i)
    | null line2s = word1 line1 <+> equals <+> quoteBoT l4i line1
    | otherwise   = word1 line1 <+> colon  <+> quoteBoT l4i line1
    
  pretty (PT4 orig@(line1 :| line2s) l4i) -- varpath)
    | null line2s = -- "//" <+> "208:" <+> viaShow line1 <//>
                     quoteRHS line1 <+> colon <+> pretty (MT1 (NE.head (fst line1)))
                     -- we should be in a DEFINE, printing a value; if we're not, we may be dumping values with the wrong order, so we need to create a PT5.
                     --    | line2s == [] = "-- " <> viaShow line1 <//> "199: " <> word1 line1 <> colon <+> quoteBoT line1
                     
    | otherwise    = "//" <+> "213: " <+> viaShow orig <//> -- [TODO] need to fix this -- test by considering a DEFINE with nested records.
                     quoteRHS line1 <> equals <+> lbrace <+> "--" <+> quoteBoT l4i line1 <> Prettyprinter.line
                     <> nest 2 (vsep [ word1 l2 <+> colon <+> dquotes (lrest l2) <> comma | l2 <- line2s ])
                     <> Prettyprinter.line
                     <> rbrace

word1,lrest :: TypedMulti -> Doc ann
word1 l = typedOrNot "_"      ((NE.head . fst $ l) :| [], snd l)
lrest l = hsep $ pretty . T.replace "\n" "\\n" <$> (NE.tail . fmap mtexpr2text . fst $ l)
-- quote based on type.
quoteBoT :: Interpreted -> TypedMulti -> Doc ann
quoteBoT l4i l@(net, _mts) =
  let unquoted = hsep $ pretty <$> NE.tail net
  in case typeOfTerm l4i {-varpath-} l of
    Just (SimpleType _ s1)  -> case s1 of
                                 "string" -> dquotes $ lrest l
                                 "number" -> unquoted
                                 "num"    -> unquoted
                                 _        -> snake_case (NE.tail net)
    Just (InlineEnum _p _s) -> dquotes $ lrest l
    Nothing                 -> unquoted
-- quote for a DEFINE value, where we are saying X IS THE Y
-- "Y" = X;
quoteRHS :: TypedMulti -> Doc ann
quoteRHS l@(val, mts) =
  case mts of
    Just (SimpleType _ s1) -> snake_case [MTT s1]
    unexpected             -> trace ("ERROR: PrettyPrinter PT4 quoteRHS: surprised to see the type annotation " ++ show unexpected) $
                              dquotes "ERRORTYPE_PrettyPrinter_quoteRHS"

-- what is the (L4) type of a given term?
-- the term in question is given as a TypedMulti, which often works out to be a key/value together in a TypedMulti.
-- if the TypedMulti already contains an explicit type, we'll return that.
-- otherwise, we need to interrogate the class hierarchy and the "varpath" breadcrumb by which we arrived at the current term.
-- for example, if we know that a Corporation extends Party with a representative : Natural Person, and we are now considering
-- a representative: Bob inside of a const Company : US_Company which extends Corporation, we have enough information to know that
-- the RHS Bob is a variable name, not a string, so we shoudn't double-quote it.

typeOfTerm :: Interpreted {- -> VarPath -} -> TypedMulti -> Maybe TypeSig
typeOfTerm l4i _tm =
  let (CT _ch, _scopetabs) = (classtable l4i, scopetable l4i)
  in
    Nothing

-- -- we are given a long varpath representing the traversal through class (anonymous) attribute space -- i.e. multiple levels of HAS
-- walk :: Interpreted -> Maybe ClsTab -> VarPath -> TypedMulti -> Maybe TypeSig
-- walk l4i Nothing xs     tm = trace ("walk 1: Maybe ClsTab argument is Nothing") $ Nothing
-- walk l4i _       []     tm = trace ("walk 2: varpath exhausted") $ Nothing
-- walk l4i (Just ct0) (x:xs) ot@(l:|r,mts) =
--   trace ("walk 3: x = " ++ show x ++ "; xs = " ++ show xs ++ "; l = " ++ show l ++ "; r = " ++ show r) $
--       case mts of
--         Just ts -> case getUnderlyingType ts of
--                      Right t1 -> case Map.lookup t1 (unCT $ classtable l4i) of
--                                    Nothing -> trace ("underlying type " ++ show t1 ++ " not found in toplevel classtable!") $
--                                               Just ts
--                                    Just (its, ct1) -> trace ("underlying type " ++ show t1 ++ " with inferrable typesig " ++ show its ++ " found in toplevel classtable, walking") $
--                                                       trace ("attempting to obtain extended attributes first tho") $
--                                                       let ct2 = extendedAttributes ct1 t1
--                                                       in 
--                                                       walk l4i ct2 xs ot
--                      Left err -> trace ("underlying type returned error: " ++ err) $
--                                  Nothing
--         Nothing -> trace ("no explicit type given, so attempting to walk current clstab " ++ show ct0) $
--                    trace ("does the current clstab have an attribute matching our varname " ++ show l ++ "?") $
--                    case Map.lookup l (unCT ct0) of
--                      Nothing -> trace ("no it does not.") $
--                                 Nothing
--                      Just (its, ct1) -> trace ("yes it does! we got back ct2 " ++ show ct1 ++ " so we will recurse, dropping " ++ show x) $
--                                         walk l4i (Just ct1) xs ot

typedOrNot :: String -> TypedMulti -> Doc ann
typedOrNot        _ (multiterm, Nothing)                        = snake_case (toList multiterm) <> ":"  <+> "Object"
typedOrNot        _ (multiterm, Just (SimpleType TOne      s1)) = snake_case (toList multiterm) <> ":"  <+> snake_case [MTT s1]
typedOrNot "corel4" (multiterm, Just (SimpleType TOptional s1)) = snake_case (toList multiterm) <> ":"  <+> pretty [MTT s1]
typedOrNot        _ (multiterm, Just (SimpleType TOptional s1)) = snake_case (toList multiterm) <> ":?" <+> snake_case [MTT s1]
typedOrNot        _ (multiterm, Just (SimpleType TList0    s1)) = snake_case (toList multiterm) <> ":"  <+> brackets (pretty s1)
typedOrNot        _ (multiterm, Just (SimpleType TList1    s1)) = snake_case (toList multiterm) <> ":"  <+> brackets (pretty s1)
typedOrNot        _ (multiterm, Just (InlineEnum pt1       s1)) = snake_case (toList multiterm) <> "# :"  <+> "InlineEnum unsupported:" <+> viaShow pt1 <+> parens (pretty $ PT2 s1)

prettySimpleType :: String -> (T.Text -> Doc ann) -> TypeSig -> Doc ann
prettySimpleType _        prty (SimpleType TOne      s1) = prty s1
prettySimpleType "corel4" prty (SimpleType TOptional s1) = prty s1
prettySimpleType "ts"     prty (SimpleType TOptional s1) = prty s1
prettySimpleType _        prty (SimpleType TOptional s1) = prty s1 <> "?"
prettySimpleType "ts"     prty (SimpleType TList0    s1) = prty s1 <> brackets ""
prettySimpleType "ts"     prty (SimpleType TList1    s1) = prty s1 <> brackets ""
prettySimpleType _        prty (SimpleType TList0    s1) = brackets (prty s1)
prettySimpleType _        prty (SimpleType TList1    s1) = brackets (prty s1)
prettySimpleType _       _prty (InlineEnum pt1       s1) = "# InlineEnum unsupported:" <+> viaShow pt1 <+> parens (pretty $ PT2 s1)

prettyMaybeType :: String -> (T.Text -> Doc ann) -> (Maybe TypeSig) -> Doc ann
prettyMaybeType _ _inner Nothing   = ""
prettyMaybeType t inner (Just ts) = colon <+> prettySimpleType t inner ts


-- | comment a block of lines
commentWith :: T.Text -> [T.Text] -> Doc ann
commentWith c xs = vsep ((\x -> pretty c <+> pretty x) <$> xs) <> line

-- | pretty print output without folding
myrender :: Doc ann -> T.Text
myrender = renderStrict . layoutPretty (defaultLayoutOptions { layoutPageWidth = Unbounded })

-- | utility function to add newlines between vsep output lines
vvsep :: [Doc ann] -> Doc ann
vvsep = vsep . Data.List.intersperse ""

-- | utility function similar to `brackets` or `parens` but with tildes, useful for org-mode
tildes :: Doc ann -> Doc ann
tildes x = "~" <> x <> "~"

-- | similar to ... <> Prettyprinter.line <> ...
(<//>), (</>) :: Doc ann -> Doc ann -> Doc ann
a </>  b = vvsep [ a, b ]
a <//> b = vsep  [ a, b ]
infixr 5 </>, <//>

-- | print haskell source in a way Org prefers
srchs :: (Show a) => a -> Doc ann
srchs = orgsrc "haskell" . pretty . pShowNoColor
orgsrc lang x = vsep [ "#+begin_src" <+> lang, x, "#+end_src" ]
orgexample  x = vsep [ "#+begin_example", x, "#+end_example" ]


pShowNoColor :: (Show a) => a -> Data.Text.Internal.Lazy.Text
pShowNoColor = TPS.pShowNoColor
