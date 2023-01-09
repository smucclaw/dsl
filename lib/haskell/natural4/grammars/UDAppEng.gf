

concrete UDAppEng of UDApp =
  UDCatEng, JustWordsWordNetEng - [some_Quant, some_Det, any_Det] **
  open Prelude, SyntaxEng, IrregEng, ExtendEng,
    SymbolicEng,
    (PE=ParseExtendEng) -- from WordNet
    in {

lin
-- The concrete syntax is sketchy on purpose.
-- One could say that it doesn't even have to bother to linearise properly,
-- but we do it for the sake of ud2gf: checking linearisations against the original.
-- At a later tree manipulation stage, we can always construct a different kind of tree.
-- So if you look at the concrete and think the attachment of some argument is wrong
-- in the linearisation, that shouldn't be a problem. The important thing is that we can look at the
-- AST and determine what the attachment actually should be.

-- Most of this file is a rather mechanical effort of cutting and pasting the internal trees together.

  -- : mark -> UDS -> UDS
  -- addMark if it_is_ndb =

	-- : NP -> aclRelcl -> NP ; -- RelNP but for aclRelcl instead. Too annoying to build RS here, instead going via UDS.
  RelclNP np rcl = mkNP np rcl ;
  -- : NP -> acl -> NP ;        -- same but for gerunds etc
	AclNP np acl = mkNP np acl ;

-----------------------------------------------------------------------------
-- Variations on root_nsubj_*

    -- : root -> nsubj -> UDS ;  -- the cat sleeps
    root_nsubj rt sub = mkUDS sub rt ;

    -- : root -> nsubj -> obj -> UDS ; -- the cat sees us
    root_nsubj_obj rt sub ob = mkUDS sub (dObjRoot rt ob) ;

    root_nsubj_obj_obl,
    root_nsubj_obj_advcl = \rt,ns,obj,adv -> root_nsubj_obl (dObjRoot rt obj) ns adv ;

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
		root_nsubj_aux occur breach may = mkUDS breach (applyAux may occur) ;

    -- : root -> nsubj -> aux -> aux -> UDS ; --a data breach may have occurred
		root_nsubj_aux_aux occur breach may have = mkUDS breach (applyAux may occur) ;

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
		root_obl_appos root obl appos = root_obl (advRoot root <appos : Adv>) obl ;

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
    let critical_to_assess_Root : LinRoot = scRoot critical to_do_assesment ;
        it_NP : NP = mkNP it ; -- lincat of expl is Pron, so we need to make it into NP
     in root_nsubj critical_to_assess_Root it_NP ;
     -- we can call root_nsubj on these, because lincat of nsubj is NP

  -- this is probably a misparse, tailoring it for the particular case of
  -- [becoming aware a breach]:csubj [may have occurred]:root
  root_csubj rt cs = root_nsubj rt (symb cs) ;

-----------------------------------------------------------------------------
-- No subject, only root
   -- : root -> UDS ;  -- sing ;
    root_only rt = onlyPred rt ;

-- Variations on root_obj_*

		-- : root -> obj -> UDS ;
    --eat potato ;
    root_obj rt obj = onlyPred (dObjRoot rt obj) ;

    -- : root -> obj -> ccomp -> UDS ;
    --includes a [number] assigned to any account the individual [has]:ccomp with an organisation that is a bank or finance company. ;
    -- root_obj_ccomp

    -- : root -> obj -> nmod -> UDS ;
    --processes personal data on behalf of and for the purposes of a public agency ;
    root_obj_nmod processes personal_data for_purposes =
      root_obj processes (mkNP personal_data for_purposes) ;

	--	root_mark_obj_obl_advcl : root -> mark -> obj -> obl -> advcl -> UDS ;
	-- [to]:mark [do]:root [an assessment]:obj [upon discovery]:obl [to see if it's NDB]:advcl

    -- : root -> obj -> obl -> advcl -> UDS ;
		root_obj_obl_advcl rt obj obl adv = root_obl_obl (dObjRoot rt obj) obl adv ;
-----------------------------------------------------------------------------
  -- We don't care that addRcl is a hack. For later applications, we can always attach the aclRelcl differently.
  -- : root -> nsubj -> cop -> aclRelcl -> UDS ;
  -- a data [intermediary]:nsubj is [one]:root that is [processing]:acl:relcl personal data  ;
  root_nsubj_cop_aclRelcl rt sub cop rcl = root_nsubj_cop (addRcl rt rcl) sub cop ;

  root_nsubj_aux_aclRelcl rt sub aux rcl = root_nsubj_cop_aclRelcl rt sub be_cop rcl ;

  -- : root -> nsubj -> cop -> aclRelcl -> obl -> UDS ;  -- the person whose personal data is affected by the breach
  root_nsubj_cop_aclRelcl_obl rt sub cop rcl obl =
    let root_obl : LinRoot = advRoot rt obl ;
      in root_nsubj_cop_aclRelcl root_obl sub cop rcl ;

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

  -- : root -> obj -> xcomp -> UDS ;
  root_obj_xcomp rt obj xc = root_xcomp (dObjRoot rt obj) xc ;

	-- : root -> xcomp -> ccomp -> UDS ;	--[render] it [unlikely] that the notifiable data breach will [result] in significant [harm] to the individual ;
	root_xcomp_ccomp render unlikely result_harm =
	  let render_unlikely : VP = mkVP render.vp <unlikely : Adv> ;
	      that_result_harm : Adv = mkAdv emptySubj result_harm
	   in onlyPred (mkVP render_unlikely that_result_harm) ;

  -- : root -> ccomp -> UDS -- unlikely that X
  root_ccomp unlikely result_harm =
    let that_result_harm : Adv = mkAdv emptySubj result_harm ;
     in onlyPred (advRoot unlikely that_result_harm) ;

  -- root_nsubj_aux_xcomp : root -> nsubj -> aux -> xcomp -> UDS ;
	-- root_nsubj_aux_obl : root -> nsubj -> aux -> obl -> UDS ;
	--the notifiable data [breach] will [result] in significant [harm] to the individual ;
	root_nsubj_aux_obl,
  root_nsubj_aux_xcomp = \result,breach,will,in_harm ->
    let result_in_harm : LinRoot = advRoot result in_harm
    in root_nsubj_aux result_in_harm breach will ;

	--: root -> advmod -> nsubj -> aux -> advmod -> obj -> UDS ;-- when in doubt, one should actively seek clarification
  -- root_advmod_nsubj_aux_advmod_obj rt advm0 sub aux advm1 =
  --  case <advm0.isNot, advm1.isNot> of {
  --  }
  -- case am1.isNot of {
  --   True => mkUDS sub (applyNeg rt);
  --   False => root_
  -- }; TODO: Need to creat aux fun to handle the aux argument and the different combi of the 2 advmod first

  -- word order tells me this is likely an AdV or Predet
 root_advmod_nsubj_cop rt am sub _cop =
  root_nsubj (mkRoot (mkVP <am.adv : AdV> rt.vp)) sub ;


  -- Two identical structures, just different cat but should be same lincat
	-- : root -> mark -> nsubj -> cop -> obl -> UDS ;
	--"when an organisation is aware of a data breach ;

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

  oper
    -- unstable hack, TODO fixme
    addRcl : LinRoot -> Adv -> LinRoot = \rt,rs ->
      let dummyNP : NP = mkNP emptyNP rs ;
          RSasAdv : Adv = lin Adv (mkUtt dummyNP) ;
       in advRoot rt rs ;

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
    onlyPred : LinRoot -> LinUDS = \rt ->
      let uds : LinUDS = mkUDS it_NP rt
       in uds ** {hasSubj = False} ;
    onlyPred : UDSPred -> LinUDS = \pr ->
      let uds : LinUDS = mkUDS it_NP pr
       in uds ** {hasSubj = False}
    } ;

  applyNeg : LinRoot -> UDSPred = \root -> mkUDSPred presSimulTemp negativePol root.vp ;

  applyAux : LinAux -> LinRoot -> UDSPred = \will,sleep ->
    case will.auxType of {
         RealAux => mkUDSPred sleep.temp sleep.pol (mkVP will.vv sleep.vp) ; -- may, must, –
         _ => mkUDSPred sleep } ;

{-
  applyAux : CatEng.Pol -> LinAux -> VP -> UDSPred = \pol,will,sleep ->
    case will.auxType of {
         RealAux => mkUDSPred presSimulTemp pol (mkVP will.vv sleep) ; -- may, must, –
        --  Will => mkUDSPred (mkTemp futureTense simultaneousAnt) pol sleep ;
			  --  Have => mkUDSPred (mkTemp presentTense anteriorAnt) pol sleep ;
         Be => mkUDSPred presSimulTemp pol sleep } ;

  applyAux2 : CatEng.Pol -> (may, have : LinAux) -> VP -> UDSPred = \pol,may,have,sleep ->
    case <may.auxType, have.auxType> of {
      <Be,x> => applyAux pol have sleep ;
      <x,Be> => applyAux pol may  sleep ;
      <RealAux,Have> => mkUDSPred presSimulTemp pol (ParseExtendComplVV may.vv anteriorAnt positivePol sleep) ; -- for may have slept
      <Have,RealAux> => mkUDSPred presSimulTemp pol (ParseExtendComplVV have.vv anteriorAnt positivePol sleep) ;
      <RealAux,Will> => applyAux pol have (mkVP may.vv sleep) ;
      <Will,RealAux> => applyAux pol may (mkVP have.vv sleep) ;
      <Will,Have>|<Have,Will>
        => let regPP : UDSPred = mkUDSPred sleep
            in regPP ** {fin = MkVPS (mkTemp futureTense anteriorAnt) positivePol sleep} ;

      _ => applyAux pol have sleep -- TODO: other combos?
      } ;
-}

}
