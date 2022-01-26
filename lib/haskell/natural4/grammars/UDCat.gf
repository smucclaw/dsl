

abstract UDCat = BareRG - [Deontic,may_Deontic,must_Deontic,should_Deontic,shall_Deontic,shant_Deontic] ** {
	cat
        UDS ;
        acl ;
        aclRelcl ;
        advcl ;
        advmod ;
        advmodEmph ;
        advmodLmod ;
        amod ;
        appos ;
        aux ;
        auxPass ;
        case_ ;
        cc ;
        ccPreconj ;
        ccomp ;
        clf ;
        compound ;
        compoundLvc ;
        compoundPrt ;
        compoundRedup ;
        compoundSvc ;
        conj ;
        cop ;
        csubj ;
        csubjPass ;
        dep ;
        det ;
        detNumgov ;
        detNummod ;
        detPoss ;
        discourse ;
        dislocated ;
        expl ;
        explImpers ;
        explPass ;
        explPv ;
        fixed ;
        flat ;
        flatForeign ;
        flatName ;
        goeswith ;
        iobj ;
        list ;
        mark ;
        nmod ;
        nmodPoss ;
        nmodTmod ;
        nsubj ;
        nsubjPass ;
        nummod ;
        nummodGov ;
        obj ;
        obl ;
        oblAgent ;
        oblArg ;
        oblLmod ;
        oblTmod ;
        orphan ;
        parataxis ;
        punct ;
        reparandum ;
        root ;
        vocative ;
        xcomp ;

	 -- coercion funs

	cat X ; -- later: replace the Xs with actual cats
	fun
        acl_ : X -> acl ;
        advcl_ : X -> advcl ;
        advmodEmph_ : X -> advmodEmph ;
        advmodLmod_ : X -> advmodLmod ;
        appos_ : X -> appos ;
        aux_ : X -> aux ;
        ccPreconj_ : X -> ccPreconj ;
        clf_ : X -> clf ;
        compound_ : X -> compound ;
        compoundLvc_ : X -> compoundLvc ;
        compoundPrt_ : X -> compoundPrt ;
        compoundRedup_ : X -> compoundRedup ;
        compoundSvc_ : X -> compoundSvc ;
        conj_ : X -> conj ;
        csubj_ : X -> csubj ;
        csubjPass_ : X -> csubjPass ;
        dep_ : X -> dep ;
        detNumgov_ : X -> detNumgov ;
        detNummod_ : X -> detNummod ;
        detPoss_ : X -> detPoss ;
        discourse_ : X -> discourse ;
        dislocated_ : X -> dislocated ;
        explImpers_ : X -> explImpers ;
        explPass_ : X -> explPass ;
        explPv_ : X -> explPv ;
        fixed_ : X -> fixed ;
        flat_ : X -> flat ;
        flatForeign_ : X -> flatForeign ;
        flatName_ : X -> flatName ;
        goeswith_ : X -> goeswith ;
        list_ : X -> list ;
        nmodPoss_ : X -> nmodPoss ;
        nmodTmod_ : X -> nmodTmod ;
        nummod_ : X -> nummod ;
        nummodGov_ : X -> nummodGov ;
        oblAgent_ : X -> oblAgent ;
        oblArg_ : X -> oblArg ;
        oblLmod_ : X -> oblLmod ;
        oblTmod_ : X -> oblTmod ;
        orphan_ : X -> orphan ;
        parataxis_ : X -> parataxis ;
        punct_ : X -> punct ;
        reparandum_ : X -> reparandum ;

        --
        xcompAdv_ : Adv -> xcomp ;
        xcompA_ : AP -> xcomp ; -- become [aware]:
        xcompA_ccomp_ : AP -> ccomp -> xcomp ; -- become [aware [that a data breach occurred]]

        ccomp_ : UDS -> ccomp ; -- just missing a complementiser, like "that"

        expl_ : Pron -> expl ;
        det_ : Det -> det ;
        vocative_ : NP -> vocative ;
        mark_ : Subj -> mark ;

        --------
        -- Lexicon
        -- in UD, syncategorematics (copula, tenses) are all AUX
        -- It's a bit clumsy to have e.g. be in 3 cats (aux, cop, auxPass), but it's good because ud2gf is parsing the string in the primary category
		be_aux,
        may_aux,
        have_aux,
        will_aux,
        can_aux,
        must_aux,
        should_aux : aux ;

        be_auxPass : auxPass ;

        -- genS_case : case_ ; -- 's is treated as a separate token in UD

        -------
    fun
    -- UD roots can be many GF cats
        rootV_ : VP -> root ;
        rootA_ : AP -> root ;
        rootN_ : NP -> root ;
        rootAdv_ : Adv -> root ; -- within 30 days
        rootDet_ : Det -> root ; -- some
        rootQuant_ : Quant -> root ; -- the customer's

    -- GF NPs can have many UD labels
        nsubj_ : NP -> nsubj ; -- lexical cat can be NOUN, DET, PRON, …
        obj_   : NP -> obj ;   -- but all become eventually NPs in GF
        iobj_  : NP -> iobj ;

    -- Some UD words are syncategorematic in GF
        be_cop : cop ;
        is_cop : cop ;
        it_expl : expl ; -- render [it] unlikely that …
        not_advmod : advmod ;


        conjA_ : AP -> conj ;
        conjN_ : NP -> conj ;
        conjAdv_ : Adv -> conj ;

		amod_ : AP -> amod ;

        cc_ : Conj -> cc ;
--        aclRelcl_ : RS -> aclRelcl ; -- whose personal data is/was/has been affected
		aclRelclUDS_ : UDS -> aclRelcl ; -- TODO figure out how this works properly, maybe needs some more auxfuns
		aclRelclRS_ : RS -> aclRelcl ;
        aclUDS_ : UDS -> acl ; -- (the issues) as he sees them -- TODO: how is "as" tagged?
        aclUDSpastpart_ : UDS -> acl ; -- (an individual) affected by the breach
        aclUDSgerund_ : UDS -> acl ; -- (a message) obeying a certain format
        advclUDS_ : UDS -> advcl ;

        obl_    : Adv -> obl ;
	    oblPrep_ : Prep -> obl ; -- sometimes empty preps are analysed as obl, e.g. "subject to"
        advmod_ : Adv -> advmod ;
        nmod_ : Prep -> NP -> nmod ; -- UD-specific version of PrepNP
        nsubjPass_ : NP -> nsubjPass ;

}
