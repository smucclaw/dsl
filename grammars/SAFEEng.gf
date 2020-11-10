concrete SAFEEng of SAFE = ActionEng ** open
  ParadigmsEng,
  SyntaxEng,
  (R=ResEng),
  (N=NounEng),
  (C=ConjunctionEng),
  (WN=WordNetEng),
  Prelude,
  ExtendEng in {
  lin
    -------------
    -- Actions --
    -------------
    -- Direct object
    Raise = mkDir raise_V2 raise_VP ;
    Issue = mkDir issue_V2 issue_VP ;
    Sell  = mkDir sell_V2 sell_VP ;
    Offer = mkDir offer_V2 offer_VP ;
    Transfer = mkDir transfer_V2 transfer_VP ;
    Pledge = mkDir pledge_V2 pledge_VP ;
    Hypothecate = mkDir hypothecate_V2 ;

    -- Indirect object
    -- IssueAt = mkDirIndir issue_at_V3 whether_at_Prep ;
    -- SellAt = mkDirIndir sell_at_V3 whether_at_Prep ;

    -- Increase valency: make the Action open for more arguments.
    -- : Action -> Action_Indir
    PursuantTo a = a ** {
      intrans = a.s ; -- weird results if combined with ANoComplIndir
      indir = pursuant_to_Prep ;
      dir = \\_ => emptyAdv
      } ;
    At a = a ** {
      intrans = a.s ;
      indir = whether_at_Prep ;
      dir = \\_ => emptyAdv
      } ;

  oper
    pursuant_to_Prep : PrepPol = prepPol "pursuant to" ;

  lin

    ----------------
    -- Properties --
    ----------------
    Fixed = prop "fixed" ;
    PreMoney = prop "pre-money" ;
    PostMoney = prop "post-money" ;
    BonaFide = prop "bona fide" ;
    Voluntary = prop "voluntary" "involuntary" ;

    -- : Term -> Property ; -- for the benefit of the Company's creditors
    ForBenefit t =
      let for_b : Adv = adv for_Prep (mkNP the_Det (mkCN benefit_N2 (np t))) ;
          not_for_b : Adv = for_b ** ss2 "not" for_b.s ;
      in table {
        Pos => adv2ap for_b ;
        Neg => adv2ap not_for_b
      } ;

    -- : ActionGerund -> Property ; -- with the purpose of raising capital
    WithPurpose gerund =
      let s : Str = gerund.s ;
          gerund_NP : NP = mkNP (mkN s s s s) ;
          purpose_of_NP : NP = mkNP the_Det (mkCN purpose_N2 gerund_NP) ;
       in table {
         Pos => adv2ap (adv with_Prep purpose_of_NP) ;
         Neg => adv2ap (adv without_Prep purpose_of_NP)
      } ;

    -----------
    -- Terms --
    -----------
    -- : Term
    Company = mkNP theSg_Det (mkN "Company") ;
    Investor = mkNP theSg_Det (mkN "Investor") ;

    -- : Term -> Term ;
    Creditors t =   -- the company's creditors
      mkNP (mkDet (ExtendEng.GenNP (np t)) pluralNum) creditor_N ;

    RightTo t = -- the right to the shares
      mkNP theSg_Det (mkCN (mkN2 WN.right_1_N to_Prep) t) ;

    -- : Determiner -> Kind -> Term -> Term ;
    TExcluding the valuation t =
      let exclAdv : Adv = parenss (adv excluding_Prep (np t)) ; -- The adv "excluding post-money"
          valuation_excl : Kind = valuation ** {
            cn = mkCN valuation.cn exclAdv  -- first layer: "valuation excluding post-money"
            } ; -- Potential postmodifier is in valuation's adv field
      in term the valuation_excl ;

    -- : Determiner -> Kind -> Term -> Term ;
    TIncluding the valuation t = -- fixed valuation, including a pre-money or post-money valuation
      let inclAdv : Adv = adv including_Prep (np t) ; -- The adv "including pre-money"
          valuation_incl : Kind = valuation ** {
            cn = ExtAdvCN valuation.cn inclAdv  -- first layer: "valuation including pre-money"
            } ; -- Potential postmodifier is in valuation's adv field
      in term the valuation_incl ;

    -----------
    -- Kinds --
    -----------
    Event = kind "event" ;
    Capital = kind "capital" ** {k = Mass} ;

    DissolutionEvent = adjkind "dissolution" "event" ;
    Termination = ofkind "termination" "operations" ;
    GeneralAssignment = adjkind "general" "assignment" ;

    LiquidityEvent = adjkind "liquidity" "event" | kind "liquidity" ;
    ChangeOfControl = ofkind "change" "control" ;
    DirectListing = adjkind "direct" "listing" ;
    InitialPublicOffering = linkind (mkCN (mkA "initial") (adjkind "public" "offering").cn) ;

    EquityFinancing = adjkind "equity" "financing" ** {k = Mass} ;
    Transaction = kind "transaction" ;
    PreferredStock = adjkind "preferred" "stock" ** {k = Mass};
    ConversionPrice = kind "conversion price" ;
    PurchaseAmount = kind "purchase amount" ;
    Valuation = kind "valuation" ;
    Security = kind "security" ;
    DiscountRate = kind "discount rate" ;

    -- These should be used together with ComplKind : Kind -> Term -> Kind
    -- to get "liquidation of the company" etc.
    -- TODO: see if we need to split this into a new category
    Shares = kind "share" ** {k = Plural} ;
    Liquidation = kind "liquidation" ;
    Dissolution = kind "dissolution" ;
    WindingUp = mkKind (mkN "winding up" "windings up") ;


    -- : [Property] -> Kind -> Kind  -- dissolution event, whether voluntary or involuntary
    KWhetherOr props kind =
      let prop : Adv = ap2adv (mkAP whether_or_Conj (props ! Pos)) ;
      in kind ** {
        adv = cc2 kind.adv prop } ;

    -- : Kind -> Kind ;
    SingleOrSeries kind = kind ** {
      cn = C.ConjCN
        or_Conj
        (C.BaseCN
           (merge kind) --kind.cn
           (mkCN series_N2 (mkNP aPl_Det (merge kind) )) -- kind.cn))
        ) ;
      -- merged to avoid ambiguity: only "(X or series of X) with Y" allowed
      -- the other possibility becomes "(X with Y) or series of (X with Y)"
      adv = emptyAdv
      } ;

  oper

    ExtAdvCN : CN -> Adv -> CN = \cn,ad -> cn ** {   -- RGL fun AdvCN doesn't put comma
      s = \\n,c => cn.s ! n ! c ++ "," ++ ad.s
      } ;

    -------------
    -- Lexicon --
    -------------
    series_Det : LinDet = \\_ => aPl_Det ** {s = "series of"} ;

    raise_V2 : V2 = WN.raise_4_V2 ;
    sell_V2 : V2 = WN.sell_1_V2 ;
    issue_V2 : V2 = WN.issue_1_V2 ;
    offer_V2 : V2 = WN.offer_1_V2 ;
    transfer_V2 : V2 = mkV2 WN.transfer_2_V ;
    pledge_V2 : V2 = WN.pledge_2_V2 ;
    hypothecate_V2 : V2 = WN.hypothecate_1_V2 ;

    -- intransitive versions of transitive verbs
    -- very artificial language, rethink this design
    sell_VP : VP = mkVP WN.perform_1_V2 (mkNP theSg_Det WN.sale_1_N) ;
    raise_VP : VP = mkVP raise_V2  (mkNP aPl_Det WN.fund_1_N) ;
    issue_VP : VP = mkVP WN.perform_1_V2 (mkNP theSg_Det (mkN "issuance")) ;
    offer_VP : VP = mkVP WN.make_3_V2 (mkNP aSg_Det WN.offer_1_N) ;
    transfer_VP : VP = mkVP WN.make_3_V2 (mkNP aSg_Det WN.transfer_1_N) ;
    pledge_VP : VP = mkVP WN.make_3_V2 (mkNP aSg_Det WN.pledge_1_N) ;

    -- TODO: do ditransitive verbs also need intransitive version?
    sell_at_V3 : V3 = mkV3 (mkV "sell") noPrep at_Prep ;
    issue_at_V3 : V3 = mkV3 (mkV "issue") noPrep at_Prep ;

    benefit_N2 : N2 = mkN2 (mkN "benefit") ;
    purpose_N2 : N2 = mkN2 (mkN ("purpose"|"principal purpose")) ;
    series_N2 : N2 = mkN2 (mkN "series" "series") ;
    creditor_N : N = mkN "creditor" ;

    whether_or_Conj : Conj = or_Conj ** {s1 = ", whether"} ;

    whether_at_Prep : PrepPol =
     prepPol
      {s = "at" ; post = [] ; redupl = False}
      {s = ", whether at" ; post = "or without" ; redupl = True} ;

    at_Prep : Prep = mkPrep "at" ;
    excluding_Prep : Prep = mkPrep "excluding" ;
    including_Prep : Prep =  -- endComma: disappears in front of other punctuation
      mkPrep ("including" ++ strOpt ("but not limited to" ++ Prelude.endComma)) ;
    pursuant_to_Prep : PrepPol = prepPol "pursuant to" ;

}
