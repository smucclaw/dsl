

concrete UDAppEng of UDApp =
  UDCatEng, JustWordsWordNetEng - [some_Quant, some_Det, any_Det] **
  open Prelude, SyntaxEng, IrregEng, ExtendEng, SymbolicEng, (SyE=SymbolEng),
    (PE=ParseExtendEng), -- from WordNet
    (N=NounEng), (P=ParadigmsEng) in {

lin

  StrPN str = {s = \\_ => str.s ; g = P.human} ;
  StrN str = {s = \\_,_  => str.s ; g = P.human} ;
	StrA str = <P.mkA "dummy" : A> ** {s = \\_ => str.s ; isMost=True};
  StrAP str = <mkAP (P.mkA "dummy") : AP> ** {s = \\_ => str.s};
	StrCard str = symb (mkSymb str.s) ;
	StrNum str = N.NumPl ** {s,sp = \\_,_ => str.s} ;
  StrSymb = SyE.MkSymb ; -- String -> Symb
  SymbNP x = symb x ;

  DefPN pn = N.PredetNP (lin Predet {s= "the"}) (N.UsePN pn) ;
  IndefPN pn = N.PredetNP (lin Predet {s= "a"}) (N.UsePN pn) ;

-- The concrete syntax is sketchy on purpose.
-- One could say that it doesn't even have to bother to linearise properly,
-- but we do it for the sake of ud2gf: checking linearisations against the original.
-- At a later tree manipulation stage, we can always construct a different kind of tree.
-- So if you look at the concrete and think the attachment of some argument is wrong
-- in the linearisation, that shouldn't be a problem. The important thing is that we can look at the
-- AST and determine what the attachment actually should be.

-- Most of this file is a rather mechanical effort of cutting and pasting the internal trees together.

-----------------------------------------------------------------------------
-- Variations on root_nsubj_*

    -- : root -> nsubj -> UDS ;  -- the cat sleeps
    root_nsubj rt sub = mkUDS sub rt ;

    -- : root -> nsubj -> obj -> UDS ; -- the cat sees us
    root_nsubj_obj rt sub ob = mkUDS sub (dObjRoot rt ob) ;

    -- : root -> nsubj -> cop -> UDS ; -- the cat is small
    root_nsubj_cop rt sub _cp = root_nsubj rt sub ;

    -- UD has many names for Adv
    -- : root -> nsubj -> cop -> nmod/obl -> UDS ;
    root_nsubj_cop_obl,
    root_nsubj_cop_nmod = \rt,sub,cop,adv -> root_nsubj_obl rt sub adv ;

    -- : root -> nsubj -> cop -> advmod -> UDS ;
    root_nsubj_cop_advmod rt sub cop advm = root_nsubj_advmod rt sub advm ;

    {- The lincat of advmod is {adv : Adv ; isNot : Bool} (from UDCatEng.gf)
       The two constructors for advmod are: (found in UDCat.gf)
         1) advmod_    : Adv -> advmod
         2) not_advmod : advmod
       They create the following kinds of advmods: (found in UDCatEng.gf)
         1) advmod_ adv = {adv = adv ; isNot = False} ;
         2) not_advmod  = {adv = lin Adv (ss "not") ; isNot = True} ;
    -}
    root_nsubj_advmod rt sub advm =
      -- We pattern match the advmod's isNot field:
      case advm.isNot of {
        True =>  -- the advmod is not_advmod: the only one where isNot=True
          mkUDS sub (applyNeg rt) ; -- so we make the whole UDS into negative

        False => -- the advmod comes from advmod_ : Adv -> advmod
          root_nsubj_obl rt sub advm.adv -- so we use the advmod's adv field
          -- we can reuse the function root_nsubj_obl,
          -- because obl's lincat is Adv (found in UDCatEng.gf)
          -- and that's the same type that is in advmod's adv field (also in UDCatEng.gf)
      } ;

    -- : root -> nsubj -> obl -> UDS
    root_nsubj_obl rt sub adv = mkUDS sub (advRoot rt adv) ;

    -- : root -> nsubj -> aux -> UDS ; --a data breach may occur
		root_nsubj_aux occur breach may = mkUDS breach (applyAux positivePol may occur.vp) ;

    -- : root -> nsubj -> aux -> aux -> UDS ; --a data breach may have occurred
		root_nsubj_aux_aux occur breach may have = mkUDS breach (applyAux2 positivePol may have occur.vp) ;

    -- root_nsubj_* structure in a subordinate clause
    root_mark_nsubj rt mark sub = addMark mark (root_nsubj rt sub) ;

    -- : root -> mark -> nsubjPass -> auxPass -> UDS ; -- that the device that contains personal data is lost
    -- root_mark_nsubjPass_auxPass rt mark sub au = addMark mark (root_nsubjPass_auxPass rt sub au);

	  -- : root -> mark -> nsubj -> aux -> UDS ;--if device has gone
    root_mark_nsubj_aux rt mark sub au = addMark mark (root_nsubj_aux rt sub au);

    -- : root -> mark -> nsubj -> aux -> aux -> UDS ; --that a data breach may have occurred
		root_mark_nsubj_aux_aux rt mark sub may have = addMark mark (root_nsubj_aux_aux rt sub may have) ;

    -- : root -> mark -> nsubj -> xcomp -> UDS ;
	  root_mark_nsubj_xcomp goes if device missing =
      let device_goes_missing : UDS = root_nsubj_xcomp goes device missing ;
       in addMark if device_goes_missing ;

-----------------------------------------------------------------------------
-- Variations on root_obl_* / root_nmod_*

    -- : root -> obl -> UDS ;
  	-- subject to X ;
    root_nmod,
		root_obl = \subject, to_X -> onlyPred (mkVP subject.vp to_X) ;

    -- : root -> nmod -> acl -> UDS ;
    -- [any]:root [of the personal data]:nmod [relating to the individual]:acl
		root_nmod_acl rt nm acl =  root_acl (advRoot rt nm) acl;

    -- : root -> nmod -> aclRelcl -> UDS ;
    -- [any manner]:root [in these circumstances]:nmod [that is reasonable]:acl:rcl ;
    root_nmod_aclRelcl rt nmod rs = root_aclRelcl (advRoot rt nmod) rs;


		-- root -> nmod -> nmod -> UDS ;
	  -- [service]:root [from provider]:nmod [to payee]:nmod
    root_nmod_nmod rt nmod1 nmod2 =  root_nmod (advRoot rt nmod1) nmod2 ;

    root_advmod rt adv = root_obl rt adv.adv ;

    -- : root -> advmod -> advmod -> obl -> UDS ;
    -- [publicly] available [solely] because of any data [breach]. ;
  	root_advmod_advmod_obl available publicly solely because_of_breach =
      root_obl  -- TODO is this too overfitting? "publicly available solely …"
        (mkRoot (mkVP <publicly.adv : AdV> (mkVP available.vp solely.adv)))
        because_of_breach ;

    -- : root -> obl -> appos -> UDS ;
  	-- mentioned in [sub-paragraph] [(a)] -- this UDpipe output has wrong attachment
		root_obl_appos root obl appos = root_obl (mkRoot (mkVP root.vp <appos : Adv>)) obl ;

    -- : root -> obl -> aux -> UDS ;
  	--as the case may be -- TODO: this is definitely wrong parse
  	-- root_obl_aux

    -- : root -> obl -> case_ -> UDS ;
    --together with justification for why ; -- TODO: also wrong parse by UDpipe
		-- root_obl_case

		-- : root -> obl -> obj -> UDS ;
	  --provide to the PDPC an explanation for why your notification was late ;
    root_obl_obj provide to_pdpc explanation =
      let provide_to_PDPC : VP = mkVP provide.vp to_pdpc ;
       in root_obj (mkRoot provide_to_PDPC) explanation ;

    -- : root -> obl -> obl -> UDS ;
	  -- mentioned in paragraph 11 on behalf of the individual. ;
		root_obl_obl mentioned in_paragraph on_behalf =
      onlyPred (mkVP (mkVP mentioned.vp in_paragraph) on_behalf) ;

		-- : root -> obl -> obl -> obl -> UDS ;
	  --authorised in that behalf in writing by the Director-General
    root_obl_obl_obl_cc mentioned in_paragraph on_behalf by_director =
      onlyPred (mkVP (mkVP (mkVP mentioned.vp in_paragraph) on_behalf) by_director) ;

    --  : root -> obl -> xcomp -> UDS ;
	  -- for an adoption order to be made -- TODO: wrong UD parse
		-- root_obl_xcomp

-----------------------------------------------------------------------------
-- csubj

  -- "[it]:expl [is]:cop [critical]:root [to do an assessment]:csubj"
  -- : root -> expl -> cop -> csubj -> UDS ;
  root_expl_cop_csubj critical it _is to_do_assesment =
    let critical_to_assess_Root : Root = scRoot critical to_do_assesment ;
        it_NP : NP = mkNP it ; -- lincat of expl is Pron, so we need to make it into NP
     in root_nsubj critical_to_assess_Root it_NP ;
     -- we can call root_nsubj on these, because lincat of nsubj is NP

-----------------------------------------------------------------------------
-- No subject, only root
   -- : root -> UDS ;  -- sing ;
    root_only rt = onlyPred rt ;

   -- : root -> amod -> UDS ; -- significant breach
   root_amod rt ap = root_nsubj rt (ExtendEng.AdjAsNP ap) ; -- TODO: currently prints out "significant is a breach"
-- Variations on root_obj_*

		-- : root -> obj -> UDS ;
    --eat potato ;
    root_obj rt obj = onlyPred (mkVP (root2vpslash rt) obj) ;

    -- : root -> obj -> ccomp -> UDS ;
    --includes a [number] assigned to any account the individual [has]:ccomp with an organisation that is a bank or finance company. ;
    -- root_obj_ccomp

    -- : root -> obj -> nmod -> UDS ;
    --processes personal data on behalf of and for the purposes of a public agency ;
    root_obj_nmod processes personal_data for_purposes =
      root_obj processes (mkNP personal_data for_purposes) ;

-----------------------------------------------------------------------------
    -- We don't care that addRcl is a hack. For later applications, we can always attach the aclRelcl differently.
    -- : root -> nsubj -> cop -> aclRelcl -> UDS ;
    -- a data [intermediary]:nsubj is [one]:root that is [processing]:acl:relcl personal data  ;
    root_nsubj_cop_aclRelcl rt sub cop rcl = root_nsubj_cop (addRcl rt rcl) sub cop ;

    root_nsubj_aux_aclRelcl rt sub aux rcl = root_nsubj_cop_aclRelcl rt sub be_cop rcl ;

    -- : root -> nsubj -> cop -> aclRelcl -> obl -> UDS ;  -- the person whose personal data is affected by the breach
    root_nsubj_cop_aclRelcl_obl rt sub cop rcl obl =
      let root_obl : Root = advRoot rt obl ;
       in root_nsubj_cop_aclRelcl root_obl sub cop rcl ;

    -- : root -> nsubjPass -> auxPass -> UDS ; -- everyone is notified
    root_nsubjPass_auxPass rt nsubj _auxpass =
      let passVP : VP = ExtendEng.PassVPSlash (root2vpslash rt) -- TODO: are we doing this here or calling BareRG.PassV in auxfuns?
       in mkUDS nsubj (mkUDSPred passVP) ;

  --   -- : root -> nsubjPass -> auxPass -> advmod -> UDS; -- the updates are reviewed regularly
  --   -- step 1: attach the adverbial to root OR if the advm is negation, apply applyNeg to the root
  --  -- step 2; finad fun without the advmod part
  root_nsubjPass_auxPass_advmod rt subj aux advm = case advm.isNot of {
    False =>
      let rootAdvm : Root = advRoot rt advm.adv ; -- new root: attached the advmod into the old root
        in root_nsubjPass_auxPass rootAdvm subj aux;

    True => -- advmod is the negation, so the adv field is just a dummy --- the real info is in that it's NEG
            -- NB. auxPass is not a real Aux, it's just a sign to passivate the verb, so we don't call applyAux
      let passVP : VP = ExtendEng.PassVPSlash (root2vpslash rt) ; -- TODO: are we doing this here or calling BareRG.PassV in auxfuns?
          pred : UDSPred = applyNeg (mkRoot passVP) ;
        in mkUDS subj pred
  } ;

  -- : root -> nsubjPass -> aux -> auxPass -> UDS ; -- everyone should be notified
  root_nsubjPass_aux_auxPass notified pdpa should auxpass =
    let passVP : VP = ExtendEng.PassVPSlash (root2vpslash notified)
    in mkUDS pdpa (applyAux positivePol should passVP) ;  -- TODO: are we doing this here or calling BareRG.PassV in auxfuns?

  -- : root -> nsubjPass -> aux -> auxPass -> UDS ; -- garage should be considered a building
  root_nsubjPass_aux_auxPass_xcomp considered garage should auxpass building =
    let considered_building : Root = advRoot considered building ;
    in root_nsubjPass_aux_auxPass considered_building garage should auxpass ;

  -- : root -> nsubjPass -> aux -> auxPass -> obl -> UDS ;
  root_nsubjPass_aux_auxPass_obl notified pdpa should auxpass inAccWithSec10 =
    let notified_inAccWithSec10 : Root = advRoot notified inAccWithSec10 ;
      in root_nsubjPass_aux_auxPass notified_inAccWithSec10 pdpa should auxpass ;

  -- : root -> nsubj -> ccomp -> UDS ;
  root_nsubj_ccomp rt ns cc =
    let rootCcomp : UDS = root_ccomp rt cc ;
     in rootCcomp ** {subj = ns ; hasSubj = True} ;

  root_nsubj_xcomp rt ns xc =
    let rootXcomp : UDS = root_xcomp rt xc ;
     in rootXcomp ** {subj = ns ; hasSubj = True} ;

	-- : root -> xcomp ->  UDS ;	-- render unlikely ; go missing
	root_xcomp render unlikely =
	  let render_unlikely : VP = mkVP render.vp <unlikely : Adv> ;
	   in onlyPred render_unlikely ;

	-- : root -> xcomp -> ccomp -> UDS ;	--[render] it [unlikely] that the notifiable data breach will [result] in significant [harm] to the individual ;
	root_xcomp_ccomp render unlikely result_harm =
	  let render_unlikely : VP = mkVP render.vp <unlikely : Adv> ;
	      that_result_harm : Adv = mkAdv emptySubj result_harm
	   in onlyPred (mkVP render_unlikely that_result_harm) ;

  -- : root -> ccomp -> UDS -- unlikely that X
  root_ccomp unlikely result_harm =
    let that_result_harm : Adv = mkAdv emptySubj result_harm ;
     in onlyPred (advRoot unlikely that_result_harm) ;

	-- root_nsubj_aux_obl : root -> nsubj -> aux -> obl -> UDS ;
	--the notifiable data [breach] will [result] in significant [harm] to the individual ;
	root_nsubj_aux_obl result breach will in_harm =
    let result_in_harm : Root = advRoot result in_harm
    in root_nsubj_aux result_in_harm breach will ;

	--: root -> advmod -> nsubj -> aux -> advmod -> obj -> UDS ;-- when in doubt, one should actively seek clarification
  -- root_advmod_nsubj_aux_advmod_obj rt advm0 sub aux advm1 =
  --  case <advm0.isNot, advm1.isNot> of {
  --  }
  -- case am1.isNot of {
  --   True => mkUDS sub (applyNeg rt);
  --   False => root_
  -- }; TODO: Need to creat aux fun to handle the aux argument and the different combi of the 2 advmod first

  -- : root -> mark -> nsubj -> cop -> UDS ; -- if it is a breach
  root_advmod_nsubj_cop breach if it is = root_mark_nsubj_cop breach if.adv it is ; -- TODO: check if advmod is negation
  root_mark_nsubj_cop breach if it is =
    let if_Predet : Predet = lin Predet if ; -- hack
        if_it : NP = mkNP if_Predet it ;
     in root_nsubj_cop breach if_it is ;


  -- Two identical structures, just different cat but should be same lincat
	-- : root -> mark -> nsubj -> cop -> obl -> UDS ;
	--"when an organisation is aware of a data breach ;

  -- : root -> advmod -> nsubj -> cop -> obl -> UDS ;
  -- [once]:advmod an [organisation]:nsubj is [aware]:root of a data [breach]:obl ;
  -- hack: we just put "once" in subject. TODO investigate how we want to use these fragments
	  root_mark_nsubj_cop_obl aware once organisation is of_breach =
    let once_Predet : Predet = lin Predet once ; -- hack
        once_organisation : NP = mkNP once_Predet organisation ;
     in root_nsubj_cop_obl aware once_organisation is of_breach ;

    root_advmod_nsubj_cop_obl rt am sub cp obl = root_mark_nsubj_cop_obl rt am.adv sub cp obl ;

	-- : root -> cop -> advmod -> UDS ; -- is not beer
  root_cop_advmod rt cp am = case am.isNot of {
    True => onlyPred (applyNeg rt) ;
    False => onlyPred (advRoot rt am.adv) -- TODO or should it be AdV instead of Adv? Does word order matter?
  } ;

---------------------------------------------------------------------------
-- acl, advcl

  -- : root -> acl -> UDS ;	--a message obeying a certain format ;
  -- : root -> advcl -> UDS ; --assess if it is a breach ;
	root_advcl, root_acl = \rt,acl -> onlyPred (advRoot rt acl) ;

  -- : root -> acl -> nmod -> UDS ;
  -- [policy]:root (called in this item the applicable policy):acl [of the company]:nmod ;
	root_acl_nmod rt acl nm = root_nmod (advRoot rt acl) nm ;

  -- : root -> advmod -> acl -> UDS ; -- heuristic: advmod is actually AdV
	root_advmod_advcl rt am acl = root_acl (mkRoot (mkVP <am.adv : AdV> rt.vp)) acl ;

  -- : root -> aclRelcl -> UDS ; --any manner that is reasonable ;
	root_aclRelcl rt rs = root_only (addRcl rt rs) ;

  -- : root -> aclRelcl -> nmod -> UDS ; -- org for which you act as a DI ;
	root_aclRelcl_nmod rt rs nmod = root_nmod (addRcl rt rs) nmod ;

-- -- root -> advcl -> nsubj -> cop -> nsubj -> UDS ; where go it is warm
  root_advcl_nsubj_cop_nsubj rt acl ns1 cop ns2 = root_acl rt acl ;


---------------------------------------------------------------------------
-- Cases where GF and UD structures map less neatly to each other
-- Needed to add special funs that are not the top-level/application layer

    -- Some internal hammering to get relatives correct
    -- : root -> RP -> auxPass -> aclRelcl ; -- [whose data]:RP is affected
    passRelcl_ rt rp auxPass =
      let is_affected : VP = ExtendEng.PassVPSlash (root2vpslash rt) ;
       in mkRS (mkRCl rp is_affected) ;

  oper
    root2vpslash : Root -> VPSlash = \root -> slashV root.vp ;

    -- unstable hack, TODO fixme
    addRcl : Root -> RS -> Root = \rt,rs ->
      let dummyNP : NP = mkNP emptyNP rs ;
          RSasAdv : Adv = lin Adv (mkUtt dummyNP) ;
       in advRoot rt RSasAdv ;

    -- the lincat of mark is Subj (an RGL cat, short for subjunction—not "subject")
    -- this is for stuff like "if", "that"… that goes before the subject
    -- so this is a hack: we add it into the subj field of the LinUDS
    addMark : Subj -> LinUDS -> LinUDS = \if,breachOccurs ->
      let if_Predet : Predet = lin Predet if ; -- hack: Predet and Subj have the same lincat in English RG
          breach_NP : NP = breachOccurs.subj ;
       in breachOccurs ** {subj = mkNP if_Predet breach_NP} ;
      {- The subj field is now "if breach", and the pred field is still "occurs".
         Equivalent to writing
          { subj = mkNP if_Predet breachOccurs.subj ;
            pred = breachOccurs.pred } ;
      -}

	onlyPred = overload {
    onlyPred : VP -> LinUDS = \vp ->
      let uds : LinUDS = mkUDS it_NP (mkRoot vp)
       in uds ** {hasSubj = False} ;
    onlyPred : Root -> LinUDS = \rt ->
      let uds : LinUDS = mkUDS it_NP rt
       in uds ** {hasSubj = False} ;
    onlyPred : UDSPred -> LinUDS = \pr ->
      let uds : LinUDS = mkUDS it_NP pr
       in uds ** {hasSubj = False}
    } ;

  applyNeg : Root -> UDSPred = \root -> mkUDSPred presentTense simultaneousAnt negativePol root.vp ;

  applyAux : CatEng.Pol -> LinAux -> VP -> UDSPred = \pol,will,sleep ->
    case will.auxType of {
         RealAux => mkUDSPred presentTense simultaneousAnt pol (mkVP will.vv sleep) ; -- may, must, –
         Will => mkUDSPred futureTense simultaneousAnt pol sleep ;
			   Have => mkUDSPred presentTense anteriorAnt pol sleep ;
         Be => mkUDSPred presentTense simultaneousAnt pol sleep } ;

  applyAux2 : CatEng.Pol -> (may, have : LinAux) -> VP -> UDSPred = \pol,may,have,sleep ->
    case <may.auxType, have.auxType> of {
      <Be,x> => applyAux pol have sleep ;
      <x,Be> => applyAux pol may  sleep ;
      <RealAux,Have> => mkUDSPred presentTense simultaneousAnt pol (ParseExtendComplVV may.vv anteriorAnt positivePol sleep) ; -- for may have slept
      <Have,RealAux> => mkUDSPred presentTense simultaneousAnt pol (ParseExtendComplVV have.vv anteriorAnt positivePol sleep) ;
      <RealAux,Will> => applyAux pol have (mkVP may.vv sleep) ;
      <Will,RealAux> => applyAux pol may (mkVP have.vv sleep) ;
      <Will,Have>|<Have,Will>
        => let regPP : UDSPred = mkUDSPred sleep
            in regPP ** {fin = MkVPS (mkTemp futureTense anteriorAnt) positivePol sleep} ;

      _ => applyAux pol have sleep -- TODO: other combos?
      } ;


}
