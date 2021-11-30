{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module LS.XPile.CoreL4 where

import L4.Syntax as CoreL4

import LS.Types as SFL4
import L4.Annotation
import Data.Text.Lazy (unpack)

-- output to Core L4 for further transformation

sfl4Dummy :: SRng
sfl4Dummy = DummySRng "From spreadsheet"

sfl4ToCorel4 :: [SFL4.Rule] -> CoreL4.Program SRng
sfl4ToCorel4 rus
  = Program {annotOfProgram = sfl4Dummy, elementsOfProgram = map sfl4ToCorel4Rule rus}

sfl4ToCorel4Rule :: SFL4.Rule -> TopLevelElement SRng
sfl4ToCorel4Rule Regulative
            { subj     -- every person
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
            } = undefined
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
            } = ClassDeclTLE (ClassDecl {annotOfClassDecl = sfl4Dummy, nameOfClassDecl =  ClsNm $ unpack name, defOfClassDecl = ClassDef [] []})
sfl4ToCorel4Rule DefNameAlias -- inline alias, like     some thing AKA Thing
            { name   -- "Thing"
            , detail -- "some thing"
            , nlhint -- "lang=en number=singular"
            , srcref
            } = undefined
sfl4ToCorel4Rule (RuleAlias t) = undefined -- internal softlink to a constitutive rule label = _
sfl4ToCorel4Rule RegFulfilled = undefined -- trivial top = _
sfl4ToCorel4Rule RegBreach    = undefined -- trivial bottom
