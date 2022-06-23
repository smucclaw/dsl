{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module LS.XPile.CoreL4 where

import Prettyprinter

import AnyAll
import LS.PrettyPrinter 
import L4.Syntax as CoreL4

import LS.Types as SFL4
import L4.Annotation
import LS.Interpreter

-- import Data.Function ( (&) )
import Data.Functor ( (<&>) )
-- import Control.Arrow ( (>>>) )

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.List.NonEmpty as NE

-- output to Core L4 for further transformation

sfl4Dummy :: SRng
sfl4Dummy = DummySRng "From spreadsheet"

sfl4ToCorel4 :: [SFL4.Rule] -> String
sfl4ToCorel4 rs =
  let sTable = symbolTable rs
      cTable = classHierarchy rs
  in unlines ( [ --"#\n# outputted via CoreL4.Program types\n#\n\n"
                 -- , ppCorel4 . sfl4ToCorel4Program $ rs
               "\n#\n# outputted directly from XPile/CoreL4.hs\n#\n"
               , (show $ prettyClasses cTable)
               , ""
               , (show $ prettyDecls   sTable)
               , ""
               ]
               ++ (show . directToCore <$> rs)
             )

sfl4ToCorel4Program :: [SFL4.Rule] -> CoreL4.Program SRng
sfl4ToCorel4Program rus
  = Program {annotOfProgram = sfl4Dummy, elementsOfProgram = concatMap sfl4ToCorel4Rule rus}

ppCorel4 :: CoreL4.Program SRng -> String
ppCorel4 p =
  show (vsep $ pptle <$> elementsOfProgram p)

pptle :: TopLevelElement SRng -> Doc ann
pptle (ClassDeclTLE cdcl) = "class" <+> pretty (stringOfClassName . nameOfClassDecl $ cdcl)

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
            } = ClassDeclTLE (ClassDecl {annotOfClassDecl = sfl4Dummy, nameOfClassDecl =  ClsNm $ T.unpack (T.unwords name), defOfClassDecl = ClassDef [] []}) : []
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
directToCore r@Hornlike{} =
  let needClauseNumbering = length (clauses r) > 1
  in
  vsep [ vsep [ maybe "# no rulename"   (\x -> "rule" <+> angles (prettyRuleLabel cnum needClauseNumbering x)) (rlabel r)
              , maybe "# no for"        (\x -> "for"  <+> prettyTypedMulti x)                                   (given r)
              ,                                "if"   <+> cStyle (hc2preds c)
              ,                                "then" <+> pretty (hHead c)
              , Prettyprinter.line]
       | (c,cnum) <- zip (clauses r) [1..]
       ]

directToCore r@TypeDecl{} = ""
directToCore _ = ""

prettyTypedMulti :: ParamText -> Doc ann
prettyTypedMulti pt = pretty $ PT3 pt
    
prettyRuleLabel :: Int -> Bool -> RuleLabel -> Doc ann
prettyRuleLabel cnum needed (_, _, text) = pretty text <> (if needed then "_" <> pretty cnum else mempty)

prettyDecls :: ScopeTabs -> Doc ann
prettyDecls sctabs = vsep [ "decl" <+> typedOrNot (NE.fromList mt, getSymType symtype)
                          | (_ , symtab') <- Map.toList sctabs
                          , (mt, symtype) <- Map.toList symtab'
                          ]

prettyClasses :: ClsTab -> Doc ann
prettyClasses ct@(CT ch) =
  vsep [ "class" <+> pretty className <>
         case clsParent ct className of
           Nothing       -> mempty
           (Just parent) -> " extends" <+> pretty parent
         -- attributes of the class are shown as decls
         <> Prettyprinter.line
         <> vsep [ "decl" <+> pretty attrname <>
                   case attrType children attrname of
                     Just t -> " :" <+> pretty className <+>
                               "->" <+> prettySimpleType t
                     Nothing -> ""
                 | attrname <- getCTkeys children
                 -- [TODO] finish out the attribute definition -- particularly tricky if it's a DECIDE
                 ]
       | className <- getCTkeys ct
       , let (Just (ctype, children)) = Map.lookup className ch
       ]

