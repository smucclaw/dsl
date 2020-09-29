-- concrete RuleEng of RuleEng = RuleEngBase  with
--   (Syntax=SyntaxEng),
--   (Extend=ExtendEng) ** {} ;
concrete RuleEng of Rule = ActionEng ** open
  Prelude,
  SyntaxEng,
  ExtendEng in {
  lincat
    Move = Utt ;

    Deontic = actionAlias ** {modal : PModal} ;

    Party = PN ;
    PartyAlias = {party : PN ; alias : NP} ; -- Meng Wong, "the Farmer"

    ActionAlias = LinActionAlias ;
    Deontic = LinDeontic ;

  oper
    LinDeontic : Type = LinActionAlias ** {
      modal : PModal
      } ;

    LinActionAlias : Type = {
      action : LinAction ; -- VP: "sell a potato"
      alias : NP           -- NP: "the sale"
      } ;

  param
    PModal = PMay | PMust | PShant ;

  lin



}

{-
IMPORT ContractLaw


No person shall sell a cabbage (an item with species Brassica chinensis or Brassica oleracea) except on the day of a full moon, unless the seller has an exemption granted by the Director of Agriculture.

RULE 1 SaleRestricted
PARTY NOBODY AS seller
MAY sell Item AS sale
WHEN Item IS cabbage
UNLESS sale IS onLegalDate // to be defined below
OR UNLIKELY seller HAS Exemption.from ~ [DirectorOfAgriculture]
HENCE ReturnPolicy
LEST VIOLATION // next-state transitions
WHERE Item IS cabbage // "where" syntax borrowed from Haskell
        WHEN Item.species ~ ["Brassica chinensis"|"Brassica oleracea"]
      sale IS onLegalDate
        WHEN sale.date ~ LegalDates
        WHERE LegalDates = external(url="https://www.almanac.com/
                astronomy/moon/full/")
              :en: "on the day of the full moon"


-- purchase
A buyer may return their purchase within three weeks for a refund.

RULE 2 ReturnPolicy
GIVEN sale
PARTY Buyer
MAY return Item
BEFORE sale.date + 3 weeks
HENCE Net3

The refund amount is 90% of the sale price. The seller must issue the refund within 3 days.

RULE 3 Net3
GIVEN return
PARTY Seller
MUST refund Amount
BEFORE return.date + 3 days
WHERE Amount = $return.sale.price * 90%
-}
