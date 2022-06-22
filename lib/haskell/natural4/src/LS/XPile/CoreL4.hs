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

-- import Data.Function ( (&) )
import Data.Functor ( (<&>) )
-- import Control.Arrow ( (>>>) )
import Debug.Trace (trace)

import Data.Text (unpack, unwords, pack)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Data.List.NonEmpty (toList)
import Data.List (intersperse)

-- output to Core L4 for further transformation

sfl4Dummy :: SRng
sfl4Dummy = DummySRng "From spreadsheet"

sfl4ToCorel4 :: [SFL4.Rule] -> String
sfl4ToCorel4 rs = unlines ( ["#\n# outputted via CoreL4.Program types\n#\n\n"
                            , ppCorel4 . sfl4ToCorel4Program $ rs
                            , "\n#\n# outputted directly from XPile/CoreL4.hs\n#\n" ]
                            ++ (show . directToCore <$> rs) )

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
                                                                                , nameOfClassDecl =  ClsNm (unpack s1)
                                                                                , defOfClassDecl = ClassDef [] []
                                                                                } )
                    _                         -> Nothing
                | ts <- snd <$> toList pt
                ]
    rule = RuleTLE Rule
      { annotOfRule = undefined
      , nameOfRule = rlabel <&> rl2text <&> unpack 
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
            } = ClassDeclTLE (ClassDecl {annotOfClassDecl = sfl4Dummy, nameOfClassDecl =  ClsNm $ unpack (Data.Text.unwords name), defOfClassDecl = ClassDef [] []}) : []
sfl4ToCorel4Rule DefNameAlias -- inline alias, like     some thing AKA Thing
            { name   -- "Thing"
            , detail -- "some thing"
            , nlhint -- "lang=en number=singular"
            , srcref
            } = undefined
sfl4ToCorel4Rule (RuleAlias t) = undefined -- internal softlink to a constitutive rule label = _
sfl4ToCorel4Rule RegFulfilled = undefined -- trivial top = _
sfl4ToCorel4Rule RegBreach    = undefined -- trivial bottom


-- we need some function to convert a HornClause2 to an Expr
-- in practice, a BoolStructR to an Expr
-- where the RPMT elements of the BooLStructR are nullary, unary, or binary operators depending on how many elements are in the list

directToCore :: SFL4.Rule -> Doc ann
directToCore r@Hornlike{} =
  vsep [ vsep [ maybe "# no rulename"   (\x -> "rule" <+> angles (prettyRuleLabel x)) (rlabel r)
              , maybe "# no for"        (\x -> "for"  <+> prettyGivens x)             (given r)
              ,                                "if"   <+> cStyle (hc2preds c)
              ,                                "then" <+> pretty (hHead c)
              , Prettyprinter.line]
       | c <- clauses r
       ]

prettyGivens :: ParamText -> Doc ann
prettyGivens pt = hcat (intersperse "," (toList $ typedOrNot <$> pt)) -- they call this "marshalling" to a newtype, because ParamText is jbbust a type alias
  where
    typedOrNot :: TypedMulti -> Doc ann
    typedOrNot (multitext, Nothing) = snakeCase (toList multitext)
    typedOrNot (multitext, Just (SimpleType TOne      s1)) = snakeCase (toList multitext) <> ":"  <+> pretty s1
    typedOrNot (multitext, Just (SimpleType TOptional s1)) = snakeCase (toList multitext) <> ":?" <+> pretty s1
    typedOrNot (multitext, Just (SimpleType TList0    s1)) = snakeCase (toList multitext) <> ":"  <+> brackets (pretty s1)
    typedOrNot (multitext, Just (SimpleType TList1    s1)) = snakeCase (toList multitext) <> ":"  <+> brackets (pretty s1)
    
prettyRuleLabel :: RuleLabel -> Doc ann
prettyRuleLabel (_, _, text) = pretty text
