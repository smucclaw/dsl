{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase, TupleSections #-}

module LS.XPile.CoreL4 where

import Prettyprinter.Render.Text
import Prettyprinter

import AnyAll
import LS.PrettyPrinter
import L4.Syntax as CoreL4

import L4.Annotation
import LS as SFL4

-- import Data.Function ( (&) )
import Data.Functor ( (<&>) )
-- import Control.Arrow ( (>>>) )

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe (catMaybes, fromMaybe, isJust, fromJust)
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty((:|)))
import Text.Pretty.Simple (pShow, pShowNoColor)
import qualified Data.Text.Lazy as TL
import Control.Monad (guard, join)

import Text.Regex.TDFA
import Data.List (nub, intercalate, (\\), isPrefixOf)
import Data.Either (rights, isRight, fromRight)
import qualified Data.Traversable as DT
import qualified Data.Foldable as DF

-- output to Core L4 for further transformation

sfl4Dummy :: SRng
sfl4Dummy = DummySRng "From spreadsheet"

sfl4ToCorel4 :: [SFL4.Rule] -> String
sfl4ToCorel4 rs =
  let interpreted = l4interpret (defaultInterpreterOptions { enums2decls = True }) rs
      sTable = scopetable interpreted
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
                          , "class Tire" -- [TODO] get this out by recursing into the type hierarchy
                          , "class Address"
                          , "decl tb_length: Number" -- [TODO] the hc2decls should be responsible for this.
                          , "decl wingspan: Number"
                          , ""
                          ]

  in unlines $ nubstrings $ concatMap lines
  ( [ -- "#\n# outputted via CoreL4.Program types\n#\n\n"
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
      in
        [ l
        | (l,n) <- zipped
        , or [ l == ""
             , "for " `isPrefixOf` l
             , "#" `isPrefixOf` l
             , (n :: Int) <= xMap2 Map.! l
             ]
        ]
  

sfl4ToCorel4Program :: [SFL4.Rule] -> CoreL4.Program SRng
sfl4ToCorel4Program rus
  = Program {annotOfProgram = sfl4Dummy, elementsOfProgram = concatMap sfl4ToCorel4Rule rus}

ppCorel4 :: CoreL4.Program SRng -> String
ppCorel4 p =
  T.unpack $ myrender (vsep $ pptle <$> elementsOfProgram p)

pptle :: TopLevelElement SRng -> Doc ann
pptle (ClassDeclTLE cdcl) = "class" <+> snake_inner (T.pack . stringOfClassName . nameOfClassDecl $ cdcl)

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

sfl4ToCorel4Rule :: SFL4.Rule -> [TopLevelElement SRng]
sfl4ToCorel4Rule Regulative
            { subj     -- every person
            , rkeyword  -- every / party / all
            , who      -- who walks and (eats or drinks)
            , cond     -- if it is a saturday
            , deontic  -- must
            , action   -- fart loudly AND run away
            , temporal -- Before "midnight"
            , hence
            , lest
            , rlabel
            , lsource
            , srcref
            , upon     -- UPON entering the club (event prereq trigger)
            , given
            , having   -- HAVING sung...
            } = undefined

sfl4ToCorel4Rule hornlike@Hornlike
            { name     -- :: RuleName           -- colour
            , keyword  -- :: MyToken            -- decide / define / means
            , given    -- :: Maybe ParamText    -- applicant has submitted fee
            , upon     -- :: Maybe ParamText    -- second request occurs
            , clauses  -- :: [HornClause2]      -- colour IS blue WHEN fee > $10 ; colour IS green WHEN fee > $20 AND approver IS happy
            , rlabel   -- :: Maybe RuleLabel
            , lsource  -- :: Maybe Text.Text
            , srcref   -- :: Maybe SrcRef
            , defaults -- :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            , symtab   -- :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            } =
            -- pull any type annotations out of the "given" paramtext as ClassDeclarations
            -- we do not pull type annotations out of the "upon" paramtext because that's an event so we need a different kind of toplevel -- maybe a AutomatonTLE?
            given2classdecls given ++
            [rule]
  where
    given2classdecls :: Maybe ParamText -> [TopLevelElement SRng]
    given2classdecls Nothing = []
    given2classdecls (Just pt) =
      catMaybes [ case ts of
                    Just (SimpleType TOne s1) -> Just $ ClassDeclTLE (ClassDecl { annotOfClassDecl = sfl4Dummy
                                                                                , nameOfClassDecl =  ClsNm (T.unpack s1)
                                                                                , defOfClassDecl = ClassDef [] []
                                                                                } )
                    _                         -> Nothing
                | ts <- snd <$> NE.toList pt
                ]
    rule = RuleTLE Rule
      { annotOfRule = undefined
      , nameOfRule = rlabel <&> rl2text <&> T.unpack
      , instrOfRule = undefined
      , varDeclsOfRule = undefined
      , precondOfRule = undefined -- gonna need more time to figure out how to convert an L4 Rule to the Expr type. in the meantime there's directToCore
      , postcondOfRule = undefined
      }


sfl4ToCorel4Rule Constitutive
            { name     -- the thing we are defining
            , keyword  -- Means, Includes, Is, Deem
            , letbind  -- might be just a bunch of words to be parsed downstream
            , cond     -- a boolstruct set of conditions representing When/If/Unless
            , given
            , rlabel
            , lsource
            , srcref
            } = undefined
sfl4ToCorel4Rule TypeDecl
            { name     --      DEFINE Sign
            , super    --                 
            , has      -- HAS foo :: List Hand \n bar :: Optional Restaurant
            , enums    -- ONE OF rock, paper, scissors (basically, disjoint subtypes)
            , rlabel
            , lsource
            , srcref
            } = [ClassDeclTLE (ClassDecl { annotOfClassDecl = sfl4Dummy
                                         , nameOfClassDecl  = ClsNm $ T.unpack (T.unwords name)
                                         , defOfClassDecl   = ClassDef [] []}) ]
sfl4ToCorel4Rule DefNameAlias -- inline alias, like     some thing AKA Thing
            { name   -- "Thing"
            , detail -- "some thing"
            , nlhint -- "lang=en number=singular"
            , srcref
            } = undefined
sfl4ToCorel4Rule (RuleAlias t) = undefined -- internal softlink to a constitutive rule label = _
sfl4ToCorel4Rule RegFulfilled = undefined -- trivial top = _
sfl4ToCorel4Rule RegBreach    = undefined -- trivial bottom
sfl4ToCorel4Rule _    = undefined -- [TODO] Hornlike

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
      , (HC2 _headRP hBod) <- [c]
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
    | c@(HC2 headRP hBod) <- clauses r
    , pf:pfs <- inPredicateForm <$> headRP : maybe [] DF.toList hBod
    , T.take 3 pf /= "rel" 
    , let predname = ""
          (bodyEx, bodyNonEx) = partitionExistentials c
          localEnv = given r <> bsr2pt bodyEx
          typeMap = Map.fromList [ (varName, fromJust varType)
                                 | (varName, mtypesig) <- maybe [] (fmap (mapFst NE.head) . NE.toList) localEnv
                                 , let underlyingm = getUnderlyingType <$> mtypesig
                                         , isJust underlyingm
                                         , isRight $ fromJust underlyingm
                                         , let varType = rightToMaybe =<< underlyingm
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
  let previousDecls = Map.fromList $ (,"") . T.takeWhile (/= ':') <$> filter ("decl " `T.isPrefixOf`) (T.lines previously)
      predDecls = Map.fromList $ T.breakOn ":" <$> T.lines (myrender $ vsep (hc2decls <$> rs))
  in pretty $ T.unlines $ uncurry (<>) <$> Map.toList (predDecls Map.\\ previousDecls)


myrender :: Doc ann -> T.Text
myrender = renderStrict . layoutPretty (defaultLayoutOptions { layoutPageWidth = Unbounded })

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
  , (mt, (symtype,_vals)) <- Map.toList symtab'
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
  , encloseSep "" "" " && " $ (\(x,y) -> parens (snake_inner x <+> "/=" <+> snake_inner y)) <$> pairwise enumList
  , ""
  ]
  | className <- getCTkeys ct
  , Just (ctype, _) <- [Map.lookup className ch]
  , (Just (InlineEnum TOne nelist),_) <- [ctype]
  , let c_name = snake_inner className
        enumList = enumLabels_ nelist
  ]
  where
    pairwise :: [a] -> [(a, a)]
    pairwise [] = []
    pairwise (x:xs) = [(x, y) | y <- xs] ++ pairwise xs

-- | print arithmetic elements as defn
-- eg: defn minsavings : Integer -> Integer = \x : Integer ->         5000 * x
--     defn minincome  : Integer -> Integer = \x : Integer -> 15000 + 4000 * x

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
      pretty (T.unwords (tail lhs)) <+> colon <+>
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
  , let rhss = T.unpack (T.unwords rhs)
  , let myterms = getAllTextMatches (rhss =~ (intercalate "|" ["\\<[[:alpha:]]+'s [[:alpha:]]+\\>"
                                                          ,"\\<[[:alpha:]]( +[[:alpha:]]+)*\\>"]
                                          :: String)) :: [String]
        intypes = replicate (length myterms) "Integer"
        replacements = [ T.replace (T.pack t) (T.pack $ show n)
                       | (t,n) <- zip (nub myterms) x123 ]
        outstr = chain replacements (T.unwords rhs)
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

-- | redraw the class hierarchy as a rooted graph, where the fst in the pair contains all the breadcrumbs to the current node. root to the right.
classGraph :: ClsTab -> [EntityType] -> [([EntityType], TypedClass)]
classGraph ct@(CT ch) ancestors = concat
  [ (nodePath, (_itypesig, childct)) : classGraph childct nodePath
  | (childname, (_itypesig, childct)) <- Map.toList ch
  , let nodePath = childname : ancestors
  ]
  

prettyClasses :: ClsTab -> Doc ann
prettyClasses ct@(CT ch) =
  vsep $
  ("## allCTkeys:" <+> hsep (pretty <$> allCTkeys ct)) :
  superClassesNotExplicitlyDefined :
  typesNotExplicitlyDefined :
  "### explicitly defined classes" :
  concat [
  [ if isJust mytype
    then vsep [ "###" <+> "class" <+> c_name <+> "is a" <+> viaShow mytype
              , "###" <+> "dot_name = " <> viaShow dot_name
              , "###" <+> "ctype = " <> viaShow ctype
              , "###" <+> "mytype = " <> viaShow mytype
              ]
    else "class" <+> c_name <> extends
  , if null childDecls then emptyDoc else vsep (commentShow "### class attributes are typed using decl:" children : childDecls)
  , if null enumDecls  then emptyDoc else vsep ("### members of enums are typed using decl" : enumDecls)
  ]
  | (classpath, (ctype, children)) <- classGraph ct []
  , let dot_name = encloseSep "" "" "." $ -- snake_inner <$> reverse classpath
                   snake_inner <$> reverse classpath
        c_name = pretty . untaint $ head classpath
        mytype = case getUnderlyingType <$> getSymType ctype of
                   Just (Right s1) -> Just s1
                   _               -> Nothing
        extends = maybe emptyDoc ((" extends" <+>) . pretty) mytype
        enumDecls = [ "decl" <+> pretty member <> ":" <+> c_name
                    | (Just (InlineEnum TOne nelist), _) <- [ctype]
                    , member <- enumLabels_ nelist
                    ]
        childDecls = [
          "decl" <+> snake_inner attrname <>
          case attrType children attrname of
            -- if it's a boolean, we're done. if not, en-predicate it by having it take type and output bool
            Just t@(SimpleType ptype pt) ->
              encloseSep ": " "" " -> " ([ -- c_name
                                           lhstype
                                         , prettySimpleType "corel4" snake_inner t
                                         ] ++ case pt of
                                                "Boolean" -> []
                                                _         -> ["Boolean"]
                                        )
              <> if ptype == TOptional
                 then Prettyprinter.line <>
                      "decl" <+> snake_inner ("has" <> attrname) <>
                      encloseSep ": " "" " -> " [ lhstype , "Boolean" ]
                      <+> "# auto-generated by CoreL4.hs, optional " <> snake_inner attrname
                 else emptyDoc

            Just (InlineEnum _ptype _pt) -> " #" <+> "ERROR: inline enums not supported for CoreL4; use a top-level enum instead."
            Nothing   -> " ##" <+> "not typed"
          | attrname <- getCTkeys children
          , let lhstype = maybe c_name pretty mytype
         ]
    -- guard to exclude certain forms which should not appear in the output
  , case (ctype,children) of
      ((Nothing, []),                 CT m) | m == Map.empty -> False | otherwise -> True
      ((Just (SimpleType TOne _), []),CT m) | m == Map.empty -> False | otherwise -> True
      _                                   -> True -- commentShow "# ctype:" ctype
      -- [TODO] and how do we treat enum types?
  ]
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




