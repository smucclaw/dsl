

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
    root_nsubj_cop rt sub cp = root_nsubj rt sub ;

    -- UD has many names for Adv
    -- : root -> nsubj -> cop -> nmod/advmod/obl -> UDS ;
    root_nsubj_cop_advmod rt sub cop am =
      case am.isNot of {
        True => applyNeg rt sub ;
        False => root_nsubj_cop_obl rt sub cop am.adv
      } ;

    root_nsubj_cop_obl,
    root_nsubj_cop_nmod = \rt,sub,cop,adv -> mkUDS sub (advRoot rt adv) ;

    root_nsubj_obl rt sub adv = mkUDS sub (advRoot rt adv) ;

    -- : root -> nsubj -> aux -> UDS ; --a data breach may occur
		root_nsubj_aux occur breach may = {
      subj = breach ;
      pred = applyAux may occur.vp
      } ;

    -- : root -> nsubj -> aux -> aux -> UDS ; --a data breach may have occurred
		root_nsubj_aux_aux occur breach may have = {
      subj = breach ;
      pred = applyAux2 may have occur.vp
      } ;

    -- root_nsubj_* structure in a subordinate clause
    -- We assume this doesn't appear in top level, so ignore mark
    root_mark_nsubj rt _mark sub = root_nsubj rt sub ;

    -- : root -> mark -> nsubj -> aux -> aux -> UDS ; --that a data breach may have occurred
		root_mark_nsubj_aux_aux rt _mark sub may have = root_nsubj_aux_aux rt sub may have ;


-----------------------------------------------------------------------------
-- Variations on root_obl_*

    -- : root -> obl -> UDS ;
  	-- subject to X ;
    root_nmod,
		root_obl = \subject, to_X -> onlyPred (mkVP subject.vp to_X) ;

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
      let root_obl : Root = rt ** {vp = mkVP rt.vp obl} ;
       in root_nsubj_cop_aclRelcl root_obl sub cop rcl ;

    -- : root -> nsubjPass -> auxPass -> UDS ; -- everyone is notified
    root_nsubjPass_auxPass rt nsubj aux = {
      subj = nsubj ;
      pred = myVPS (ExtendEng.PassVPSlash (root2vpslash rt)) ;
    } ;

    -- : root -> nsubjPass -> aux -> auxPass -> UDS ; -- everyone should be notified
    root_nsubjPass_aux_auxPass notified pdpa should auxpass = {
      subj = pdpa ;
      pred = applyAux should notified.vp -- TODO: has PassVP already been applied?
    } ;

    -- : root -> nsubjPass -> aux -> auxPass -> UDS ; -- garage should be considered a building
    root_nsubjPass_aux_auxPass_xcomp considered garage should auxpass building = {
      subj = garage ;
      pred = applyAux should (mkVP considered.vp building) -- TODO: has PassVP already been applied?
    } ;

    -- : root -> nsubjPass -> aux -> auxPass -> obl -> UDS ;
    root_nsubjPass_aux_auxPass_obl notified pdpa should auxpass inAccWithSec10 = {
      subj = pdpa ;
      pred = applyAux should (mkVP notified.vp inAccWithSec10) -- TODO: has PassVP already been applied?
    } ;

	-- : root -> xcomp -> ccomp -> UDS ;	--[render] it [unlikely] that the notifiable data breach will [result] in significant [harm] to the individual ;
	root_xcomp_ccomp render unlikely result_harm =
	  let render_unlikely : VP = mkVP render.vp <unlikely : Adv> ;
	      that_result_harm : Adv = mkAdv that_Subj result_harm
	   in onlyPred (mkVP render_unlikely that_result_harm) ;

  -- : root -> ccomp -> UDS -- unlikely that X
  root_ccomp unlikely result_harm =
    let that_result_harm : Adv = mkAdv that_Subj result_harm ;
     in onlyPred (advRoot unlikely that_result_harm) ;

	-- root_nsubj_aux_obl : root -> nsubj -> aux -> obl -> UDS ;
	--the notifiable data [breach] will [result] in significant [harm] to the individual ;
	root_nsubj_aux_obl result breach will in_harm = {
      subj = breach ;
      pred = applyAux will (mkVP result.vp in_harm) };

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
    True => applyNeg rt emptyNP ;
    False => onlyPred (advRoot rt am.adv) -- TODO or should it be AdV instead of Adv? Does word order matter?
  } ;

---------------------------------------------------------------------------
-- acl, advcl

  -- : root -> acl -> UDS ;	--a message obeying a certain format ;
	root_acl rt acl = onlyPred (advRoot rt acl) ;

  	-- onlyPred : VP -> UDS = \vp -> mkUDS it_NP (mkRoot vp) ;


  -- : root -> acl -> nmod -> UDS ;
	root_acl_nmod rt acl nm = root_acl (mkRoot (mkVP rt.vp nm)) acl ;

  -- : root -> advcl -> UDS ; --assess if it is a breach ;
	root_advcl rt adv = root_acl rt adv ;

  -- : root -> aclRelcl -> UDS ; --any manner that is reasonable ;
	root_aclRelcl rt rs = root_only (addRcl rt rs) ;

  -- : root -> aclRelcl -> nmod -> UDS ; -- org for which you act as a DI ;
	root_aclRelcl_nmod rt rs nmod = root_nmod (addRcl rt rs) nmod ;

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
      --     dummyVPSlash : VPSlash = root2vpslash rt ;
          RSasAdv : Adv = lin Adv (mkUtt dummyNP) ;
       in rt ** {
            vp = mkVP rt.vp RSasAdv
          } ;

	onlyPred = overload {
    onlyPred : VP -> UDS = \vp -> mkUDS it_NP (mkRoot vp) ;
    onlyPred : Root -> UDS = \rt -> mkUDS it_NP rt
  };

  applyNeg : Root -> NP -> LinUDS = \root,subj -> {
    subj = subj ;
    pred = {fin = MkVPS (mkTemp presentTense simultaneousAnt) negativePol root.vp ;
            pp =
              let pp' : AP = BareRGEng.PastPartAP root.vp
               in pp' ** {s = \\x => "not" ++ pp'.s ! x} ;
            presp =
              let pp' : AP = BareRGEng.PresPartAP root.vp
               in pp' ** {s = \\x => "not" ++ pp'.s ! x} ;
            inf =
              let inf' : VPI = MkVPI root.vp
               in inf' ** {s = \\typ,agr => "not" ++ inf'.s ! typ ! agr}
           } ** defaultUDSPred;
    } ;


  applyAux : LinAux -> VP -> UDSPred = \will,sleep ->
    case will.auxType of {
         RealAux => myVPS (mkVP will.vv sleep) ; -- may, must, –
         Will => myVPS futureTense sleep ;
			   Have => myVPS anteriorAnt sleep ;
         Be => myVPS sleep } ;

  applyAux2 : (may, have : LinAux) -> VP -> UDSPred = \may,have,sleep ->
    case <may.auxType, have.auxType> of {
      <Be,x> => applyAux have sleep ;
      <x,Be> => applyAux may  sleep ;
      <RealAux,Have> => myVPS (ParseExtendComplVV may.vv anteriorAnt positivePol sleep) ; -- for may have slept
      <Have,RealAux> => myVPS (ParseExtendComplVV have.vv anteriorAnt positivePol sleep) ;
      <RealAux,Will> => applyAux have (mkVP may.vv sleep) ;
      <Will,RealAux> => applyAux may (mkVP have.vv sleep) ;
      <Will,Have>|<Have,Will>
        => let regPP : UDSPred = myVPS sleep
            in regPP ** {fin = MkVPS (mkTemp futureTense anteriorAnt) positivePol sleep} ;

      _ => applyAux have sleep -- TODO: andra combos?
      } ;


}
