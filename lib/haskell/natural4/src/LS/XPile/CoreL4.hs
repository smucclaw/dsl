{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module LS.XPile.CoreL4 where

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
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.String.Utils as DSU
import Text.Pretty.Simple (pShow, pShowNoColor)
import qualified Data.Text.Lazy as TL

-- output to Core L4 for further transformation

sfl4Dummy :: SRng
sfl4Dummy = DummySRng "From spreadsheet"

sfl4ToCorel4 :: [SFL4.Rule] -> String
sfl4ToCorel4 rs =
  let interpreted = l4interpret (defaultInterpreterOptions { enums2decls = True }) rs
      sTable = scopetable interpreted
      cTable = classtable interpreted
  in unlines ( [ -- "#\n# outputted via CoreL4.Program types\n#\n\n"
                 -- , ppCorel4 . sfl4ToCorel4Program $ rs
               "\n#\n# outputted directly from XPile/CoreL4.hs\n#\n"
               , show $ prettyClasses cTable ,     "\n\n-- boilerplate\n"
               , show $ prettyBoilerplate cTable , "\n\n-- decls\n"
               , show $ prettyDecls   sTable ,     "\n\n-- facts\n"
               , show $ prettyFacts   sTable , ""
               , "\n# directToCore\n\n"
               ] ++
               [ show (directToCore r)
               | r <- rs
               ]
             )

sfl4ToCorel4Program :: [SFL4.Rule] -> CoreL4.Program SRng
sfl4ToCorel4Program rus
  = Program {annotOfProgram = sfl4Dummy, elementsOfProgram = concatMap sfl4ToCorel4Rule rus}

ppCorel4 :: CoreL4.Program SRng -> String
ppCorel4 p =
  show (vsep $ pptle <$> elementsOfProgram p)

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

pptle tle                 = vsep ( "-- pptle: UNIMPLEMENTED, showing Haskell source:"
                                   : (pretty . ("-- " <>) <$> lines (show tle)) )

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
            given2classdecls given ++ [rule]
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

directToCore :: SFL4.Rule -> Doc ann
directToCore r@Hornlike{keyword}
  | keyword /= Define =
    let needClauseNumbering = length (clauses r) > 1
    in
      vsep [ vsep [ maybe "# no rulename"   (\x -> "rule" <+> angles (prettyRuleName cnum needClauseNumbering x)) (Just $ ruleLabelName r)
                  , maybe "# no for"        (\x -> "for"  <+> prettyTypedMulti x)                                   (given r)
                  ,                                "if"   <+> cStyle (hc2preds c)
                  ,                                "then" <+> pretty (hHead c)
                  , Prettyprinter.line]
           | (c,cnum) <- zip (clauses r) [1..]
           ]
  | otherwise = ""

-- fact <rulename> multiterm

directToCore r@TypeDecl{} = ""
directToCore _ = ""

prettyTypedMulti :: ParamText -> Doc ann
prettyTypedMulti pt = pretty $ PT3 pt

prettyRuleName :: Int -> Bool -> RuleName -> Doc ann
prettyRuleName cnum needed text = snake_case text <> (if needed then "_" <> pretty cnum else mempty)

prettyDecls :: ScopeTabs -> Doc ann
prettyDecls sctabs =
  vsep [ "--" <+> pretty scopename <> Prettyprinter.line <>
         "decl" <+> typedOrNot (NE.fromList mt, getSymType symtype)
       | (scopename , symtab') <- Map.toList sctabs
       , (mt, (symtype,_vals)) <- Map.toList symtab'
       ]

prettyFacts :: ScopeTabs -> Doc ann
prettyFacts sctabs =
  vsep
  [ -- global symtab as facts
    "fact" <+> angles (snake_case scopename) <+> viaShow symtab'
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
  , encloseSep "" "" " && " $ (\(x,y) -> parens (pretty x <+> "/=" <+> pretty y)) <$> pairwise enumList
  , ""
  ]
  | className <- getCTkeys ct
  , Just (ctype, _) <- [Map.lookup className ch]
  , (Just (InlineEnum TOne (( nelist, _ ) :| _)), _) <- [ctype]
  , let c_name = snake_inner className
        enumList = NE.toList nelist
  ]
  where
    pairwise :: [a] -> [(a, a)]
    pairwise [] = []
    pairwise (x:xs) = [(x, y) | y <- xs] ++ pairwise xs

prettyClasses :: ClsTab -> Doc ann
prettyClasses ct@(CT ch) =
  vsep $ concat [
  [ "class" <+> snake_inner className <>
    case clsParent ct className of
      Nothing       -> mempty
      (Just parent) -> " extends" <+> pretty parent
      -- attributes of the class are shown as decls

      -- there are a couple scenarios here to consider.
      -- one, a top-level DECLARE Something IS ONE OF Enum1 Enum2
      -- corel4 wants that in the format of 
      -- # Enumeration of members of Something
      -- decl Enum1: Something
      -- decl Enum2: Something
  , if ctype == (Nothing, []) then Prettyprinter.emptyDoc else commentWith "#" ("ctype:"    : T.lines (T.pack (show ctype)))
  , vsep [ "decl" <+> pretty member <> ":" <+> pretty className
         | (Just (InlineEnum TOne (( nelist, _ ) :| _)), _) <- [ctype]
         , member <- NE.toList nelist
         ]

    -- two, a top-level DECLARE Someclass HAS Attr1 IS ONE OF enum1 enum2
    -- the correct representation would be something like
    -- decl Enum1 : Someclass -> Attr1 -> Boolean
    -- [TODO] however, we do not have a sensible treatment of recursive class declarations, so we need to think about that.
  , if children == CT Map.empty then Prettyprinter.emptyDoc else commentWith "#" ("children:" : T.lines (T.pack (show children)))
  , vsep [ "decl" <+> pretty attrname <>
           case attrType children attrname of
             -- if it's a boolean, we're done. if not, en-predicate it by having it take type and output bool
             Just t@(SimpleType _ptype pt) ->
               encloseSep ": " "" " -> " ([ snake_inner className
                                          , prettySimpleType snake_inner t
                                          ] ++ case pt of
                                                 "Boolean" -> []
                                                 _         -> ["Boolean"]
                                         )
             Just (InlineEnum _ptype _pt) -> " #" <+> "ERROR: inline enums not supported for CoreL4; use a top-level enum instead."
             Nothing   -> " #" <+> pretty attrname <+> "--" <+> "not typed"
         | attrname <- getCTkeys children
         -- [TODO] finish out the attribute definition -- particularly tricky if it's a DECIDE
         ]
  , ""
  ]
  | className <- getCTkeys ct
  , Just (ctype, children) <- [Map.lookup className ch]
  ]

