module ToGF where

import AbsL
--import L4
import PGF
import Top

-- BNFC to GF tree

bnfc2gf :: AbsL.Rule -> PGF.Tree
bnfc2gf (Rule _ _ _ _ rulebody) = gf $ body2gf rulebody

body2gf :: RuleBody -> GSentence
body2gf (RModal givenl modall whenhencewhere) = modal2gf modall
body2gf _ = undefined

modal2gf :: ModalLimb -> GSentence
modal2gf (MD1 party deonticl dl) = GMAction (party2gf party) (deontic2gf deonticl)

party2gf :: PartyLimb -> GParty
party2gf (PartyLimb partydef _) = partydef2gf partydef

partydef2gf :: PartyDef -> GParty
partydef2gf (PEvery _) = GEverybody
partydef2gf (PNobody _) = GNobody
partydef2gf (PSome oa) = GStrParty (GString $ oa2str oa)

oa2str :: ObjAttr -> String
oa2str (OA_dots elems) = unwords $ map elem2str elems
  where
    elem2str elem = case elem of
      ObjAttrElemIdent (Ident str) -> str
      ObjAttrElemUIdent (UIdent str) -> str

deontic2gf :: DeonticLimb -> GDeontic
deontic2gf (DeonticLimb1 deonexpr langstrs actionl) = case deonexpr of
  DEMust -> GMust (action2gf actionl)
  DEMay -> GMay (action2gf actionl)
  DEShant -> GShant (action2gf actionl)

action2gf :: ActionLimb -> GActionAlias
action2gf (ActionSingle om blahs alias) = GAAlias (GAComplDir GSend (GTDet GTheSg GOrder))
