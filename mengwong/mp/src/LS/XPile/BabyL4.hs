{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module LS.XPile.BabyL4 where

import L4.Syntax as BabyL4

import LS.Types as SFL4
import L4.Annotation
import Data.Text.Lazy (unpack)

-- output to Baby L4 for further transformation

sfl4Dummy :: SRng
sfl4Dummy = DummySRng "From spreadsheet"

sfl4ToBabyl4 :: [SFL4.Rule] -> BabyL4.Program SRng
sfl4ToBabyl4 rus
  = Program {annotOfProgram = sfl4Dummy, elementsOfProgram = map sfl4ToBabyl4Rule rus}

sfl4ToBabyl4Rule :: SFL4.Rule -> TopLevelElement SRng
sfl4ToBabyl4Rule Regulative
            { name    -- every person
            , keyword  -- every / party / all
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
            , orig
            } = undefined
sfl4ToBabyl4Rule Constitutive
            { name     -- the thing we are defining
            , keyword  -- Means, Includes, Is, Deem
            , letbind  -- might be just a bunch of words to be parsed downstream
            , cond     -- a boolstruct set of conditions representing When/If/Unless
            , given
            , rlabel
            , lsource
            , srcref
            , orig
            } = undefined
sfl4ToBabyl4Rule TypeDecl
            { name     --      DEFINE Sign
            , super    --                 
            , has      -- HAS foo :: List Hand \n bar :: Optional Restaurant
            , enums    -- ONE OF rock, paper, scissors (basically, disjoint subtypes)
            , rlabel
            , lsource
            , srcref
            } = ClassDeclTLE (ClassDecl {annotOfClassDecl = sfl4Dummy, nameOfClassDecl =  ClsNm $ unpack (bsp2text name), defOfClassDecl = ClassDef [] []})
sfl4ToBabyl4Rule DefNameAlias -- inline alias, like     some thing AKA Thing
            { name   -- "Thing"
            , detail -- "some thing"
            , nlhint -- "lang=en number=singular"
            , srcref
            } = undefined
sfl4ToBabyl4Rule (RuleAlias t) = undefined -- internal softlink to a constitutive rule label = _
sfl4ToBabyl4Rule RegFulfilled = undefined -- trivial top = _
sfl4ToBabyl4Rule RegBreach    = undefined -- trivial bottom
