

abstract UDApp = UDCat, JustWordsWordNet - [some_Quant, some_Det, any_Det] ** {

	flags
		startcat = UDS ;

	fun

	-- from the first test corpus, full sentences
	root_cop_advmod          : root -> cop -> advmod -> UDS ; -- is not a breach
	passRelcl_               : root -> RP -> auxPass -> aclRelcl ; -- [whose data]:RP is affected
	root_nsubj_cop_advmod    : root -> nsubj -> cop -> advmod -> UDS ;
    root_nsubj_cop_nmod      : root -> nsubj -> cop -> nmod -> UDS ;
    root_nsubj_aux_aclRelcl  : root -> nsubj -> aux -> aclRelcl -> UDS ; -- [nsubj] is [root] that [aux] [aclRelcl]ed (aux for tense etc.)
    root_nsubj_aux_aclRelcl_obl : root -> nsubj -> aux -> aclRelcl -> obl -> UDS ;  -- the person whose personal data [will]:aux disappear in the breach
    root_nsubj_cop_aclRelcl_obl : root -> nsubj -> cop -> aclRelcl -> obl -> UDS ;  -- the person whose personal data [is]:cop affected by the breach
    root_nsubjPass_aux_auxPass : root -> nsubjPass -> aux -> auxPass -> UDS ; -- everyone should be notified
	root_nsubjPass_aux_auxPass_obl :  root -> nsubjPass -> aux -> auxPass -> obl -> UDS ; -- everyone should be notified in accordance with Section 10

	-- from the text fragments in Legal Spreadsheets PDPA mockup

	root_only : root -> UDS ;
	--sing ;

	root_acl : root -> acl -> UDS ;
	--day of mourning ;

	root_acl_nmod : root -> acl -> nmod -> UDS ;
	--policy (called in this item the applicable policy) of the company ;

		root_advcl : root -> advcl -> UDS ;
	--mourning forbids singing ;

		root_advmod : root -> advmod -> UDS ;
	--y == Scissors ;

		root_advmod_advcl : root -> advmod -> advcl -> UDS ;

		root_advmod_advmod_obl : root -> advmod -> advmod -> obl -> UDS ; -- VP -> Adv -> Adv -> Adv -> VP
	--publicly available solely because of any data breach. ;

		root_advmod_amod : root -> advmod -> amod -> UDS ;
	--not legal advice." ;

		root_advmod_nsubj_aux_advmod_obj : root -> advmod -> nsubj -> aux -> advmod -> obj -> UDS ;
	-- when in doubt, one should actively seek clarification

		root_advmod_nsubj_cop : root -> advmod -> nsubj -> cop -> UDS ;
		root_advmod_nsubj_cop_obl : root -> advmod -> nsubj -> cop -> obl -> UDS ;
	--"[once]:advmod an [organisation]:nsubj is [aware]:root of a data [breach]:obl ;

	-- 	root_amod : root -> amod -> UDS ;
	-- --green potato ;
	-- For linearisation, it's easier if we parse a NP like "green potato" as root_only (AdjCN green_AP potato_CN),
	-- instead of splitting the AP and the CN in separate RGL trees, root_amod (rootN_ potato_CN) (amod_ green_AP).
	-- But if we want to split them for some other application, e.g. to easier produce "potato is green", then root_amod makes that easier.


		root_amod_nmod : root -> amod -> nmod -> UDS ;
	--significant harm to an affected individual" ;

		root_appos : root -> appos -> UDS ;
	--(d) the identity of any applicant for an adoption order; ;

		root_appos_advmod : root -> appos -> advmod -> UDS ;
	--(Regs §3.1.b.ii) ;

		root_auxPass : root -> auxPass -> UDS ;
	--(b) or (c) is placed ;

		root_case : root -> case_ -> UDS ;
	--ONE OF ;

		root_case_amod : root -> case_ -> amod -> UDS ;
	--without undue delay ;

		root_case_amod_amod : root -> case_ -> amod -> amod -> UDS ;
	--in other prescribed circumstances. ;

		root_case_compound : root -> case_ -> compound -> UDS ;
	--including drug addiction and" ;

		root_case_nummod : root -> case_ -> nummod -> UDS ;
	--within 30 days ;

		root_case_nummod_acl : root -> case_ -> nummod -> acl -> UDS ;
	--within 3 days of determining that it was a NDB ;

		root_case_nummod_nummod : root -> case_ -> nummod -> nummod -> UDS ;
	--on or after 1 July 2020 ;

		root_cc : root -> cc -> UDS ;
	--or is" ;

		root_cc_cop_xcomp : root -> cc -> cop -> xcomp -> UDS ;
	--or is likely to result in ;

		root_cc_nmod : root -> cc -> nmod -> UDS ;
	--or transfer of any property ;

		root_cc_obj : root -> cc -> obj -> UDS ;
	--and amount due or outstanding ;

		root_ccomp : root -> ccomp -> UDS ;
	--unlikely that the notifiable data breach will result in significant harm to the affected individual ;

		root_compound : root -> compound -> UDS ;
	--(Regs §3.1.a) ;

		root_compoundPrt_compoundPrt : root -> compoundPrt -> compoundPrt -> UDS ;
	--set out in ;

		root_compound_acl : root -> compound -> acl -> UDS ;
	--"adoption order made under the Adoption of Children Act (Cap. 4) ;

		root_compound_amod : root -> compound -> amod -> UDS ;
	--scope quantification slightly different vs 4 ;

		root_compound_appos : root -> compound -> appos -> UDS ;
	--Advisory Guidelines part 20 ;

		root_compound_compound : root -> compound -> compound -> UDS ;
	--(Regs §3.1.b.i) ;

		root_compound_compound_appos : root -> compound -> compound -> appos -> UDS ;
	--Personal Data Protection Act — Data Breach Notification Obligation ;

		root_compound_flat : root -> compound -> flat -> UDS ;
	--(b) Human Immunodeficiency Virus Infection; ;

		root_cop : root -> cop -> UDS ;
	--"(c) is committed ;

		root_csubj : root -> csubj -> UDS ;
	--(a) to create a secure electronic record or secure electronic signature; ;

		root_csubj_aux_aux : root -> csubj -> aux -> aux -> UDS ;
	--becoming aware a data breach may have occurred ;

		root_nsubj_aux : root -> nsubj -> aux -> UDS ;
	--a data breach may occur ;

		root_nsubj_aux_aux : root -> nsubj -> aux -> aux -> UDS ;
	--a data breach may have occurred ;

		root_mark_nsubj_aux : root -> mark -> nsubj -> aux -> UDS ;
	--if device has gone

		root_mark_nsubj_aux_aux : root -> mark -> nsubj -> aux -> aux -> UDS ;
	--that a data breach may have occurred

	    root_mark_nsubj_xcomp : root -> mark -> nsubj -> xcomp -> UDS ;
    -- if the device goes missing

		root_aclRelcl : root -> aclRelcl -> UDS ;
	-- [any manner]:root [that is reasonable]:acl:rcl ;

		root_aclRelcl_nmod : root -> aclRelcl -> nmod -> UDS ;
	--the Organisation for which you act as a DI ;

		root_discourse : root -> discourse -> UDS ;
	--(AdvG §20.4) ;

		root_fixed : root -> fixed -> UDS ;
	--all of ;

		root_goeswith : root -> goeswith -> UDS ;
	--(Act 1) ;


		root_goeswith_goeswith : root -> goeswith -> goeswith -> UDS ;
	--(Act §26B.3.b) ;

		root_mark : root -> mark -> UDS ;
	--IF NOT ;

		root_mark_cc_mark_obj : root -> mark -> cc -> mark -> obj -> UDS ;
	--"on or after notifying the Commission ;

		root_mark_expl_cop_xcomp : root -> mark -> expl -> cop -> xcomp -> UDS ;
	--"once it is likely to be of significant scale ;

		root_mark_expl_nsubj : root -> mark -> expl -> nsubj -> UDS ;
	--"when there is a data breach ;

		root_expl_cop_csubj	: root -> expl -> cop -> csubj -> UDS ;
	--"[it]:expl [is]:cop [critical]:root [to do an assessment]:csubj"

		root_mark_nsubj : root -> mark -> nsubj -> UDS ;
	--whether or not the court has" ;

		root_mark_nsubjPass_auxPass: root -> mark -> nsubjPass -> auxPass -> UDS ;
	-- that the device that contains personal data is lost

		root_mark_nsubjPass_auxPass_obl : root -> mark -> nsubjPass -> auxPass -> obl -> UDS ;
	--"when an organisation is notified of a data breach ;

		root_mark_nsubj_aux_advmod_obj : root -> mark -> nsubj -> aux -> advmod -> obj -> UDS ;
	--"although the organisation must still notify the Commission ;

		root_mark_nsubj_cop : root -> mark -> nsubj -> cop -> UDS ;
	--"if it is significant ;

		root_mark_nsubj_cop_obl : root -> mark -> nsubj -> cop -> obl -> UDS ;
	--"when an organisation is aware of a data breach ;

		root_nsubj_cop_obl : root -> nsubj -> cop -> obl -> UDS ;
	--"an organisation is aware of a data breach ;


		root_mark_nsubj_obj : root -> mark -> nsubj -> obj -> UDS ;
	--"if it harms the affected individual ;

		root_mark_nsubj_obl : root -> mark -> nsubj -> obl -> UDS ;
	--"when an organisation knows of a data breach ;

		root_mark_nummod : root -> mark -> nummod -> UDS ;
	--"if the data breach affects at least 500 people ;

		root_nmod : root -> nmod -> UDS ;
	--day of silence ;

		root_nmod_nmod : root -> nmod -> nmod -> UDS ;
	--service from provider to payee

		root_nmod_acl : root -> nmod -> acl -> UDS ;
	--[any] [of the personal data] [relating to the individual] ;

		root_nmod_aclRelcl : root -> nmod -> aclRelcl -> UDS ;
	-- [any manner]:root [in these circumstances]:nmod [that is reasonable]:acl:rcl ;

		root_nsubj : root -> nsubj -> UDS ;
	--"the organisation must ;

		root_nsubjPass_auxPass : root -> nsubjPass -> auxPass -> UDS ;
	--Bob is estranged ;

		root_nsubjPass_auxPass_advmod : root -> nsubjPass -> auxPass -> advmod -> UDS;
	-- the updates are reviewed regularly

		root_nsubjPass_auxPass_advmod_advcl : root -> nsubjPass -> auxPass -> advmod -> advcl -> UDS ;
	--Notification is not required if the data breach is within an organisation ;

		root_nsubjPass_auxPass_advmod_xcomp : root -> nsubjPass -> auxPass -> advmod -> xcomp -> UDS ;
	--a data breach within an organisation is deemed not to be a notifiable data breach ;

		root_nsubjPass_auxPass_xcomp : root -> nsubjPass -> auxPass -> xcomp -> UDS ;
	--data breach of prescribed personal data is deemed to result in significant harm ;

		root_nsubjPass_aux_auxPass : root -> nsubjPass -> aux -> auxPass -> UDS ;
	--every individual affected by the data breach should be notified ;

		root_nsubjPass_aux_auxPass_obl_advmod : root -> nsubjPass -> aux -> auxPass -> obl -> advmod -> UDS ;
	--the Commission must be notified of the data breach as soon as practicable once the organisation assesses it to be a notifiable breach ;

		root_nsubjPass_aux_auxPass_obl_obl_advcl : root -> nsubjPass -> aux -> auxPass -> obl -> obl -> advcl -> UDS ;
	--the Commission must be notified of the data breach within 3 days once the organisation assesses it to be a notifiable breach ;

		root_nsubjPass_aux_auxPass_obl_obl_advmod : root -> nsubjPass -> aux -> auxPass -> obl -> obl -> advmod -> UDS ;
	--the Commission must be notified of the data breach within 3 days or as soon as practicable once the organisation assesses it to be a notifiable breach ;

		root_nsubj_advmod : root -> nsubj -> advmod -> UDS ;
	--the King so desires ;

		root_nsubj_advmod_obj : root -> nsubj -> advmod -> obj -> UDS ;
	--the organisation already implemented any technological measure ;

		root_nsubj_aux_advmod : root -> nsubj -> aux -> advmod -> UDS ;
	--the Queen is not looking ;

		root_nsubj_aux_advmod_obj_advcl : root -> nsubj -> aux -> advmod -> obj -> advcl -> UDS ;
	--the organisation must not notify the affected individual if a prescribed law enforcement agency so instructs ;

		root_nsubj_aux_obj : root -> nsubj -> aux -> obj -> UDS ;
	--the organisation has taken any action ;

		root_nsubj_aux_obj_obl : root -> nsubj -> aux -> obj -> obl -> UDS ;
	--you are processing personal data on behalf of and for the purposes of a public agency ;

		root_nsubj_aux_obj_obl_advmod_advcl : root -> nsubj -> aux -> obj -> obl -> advmod -> advcl -> UDS ;
	--an organisation must report the notifiable data breach to the Commission as soon as practicable ;

		root_nsubj_aux_obj_obl_obl : root -> nsubj -> aux -> obj -> obl -> obl -> UDS ;
	--an organisation must report the notifiable data breach to the Commission within 3 days of the assessment ;

		root_nsubj_ccomp : root -> nsubj -> ccomp -> UDS ;
	--your Organisation determined it was an NDB ;

		root_nsubj_cop : root -> nsubj -> cop -> UDS ;
	--observance is mandatory ;

		root_nsubj_cop_aclRelcl : root -> nsubj -> cop -> aclRelcl -> UDS ;
	--a data intermediary is one that is processing personal data on behalf of and for the purposes of another organisation ;

		root_nsubj_cop_advcl : root -> nsubj -> cop -> advcl -> UDS ;
	--"A data breach is notifiable if it results in ;

		root_nsubj_cop_case_nmod_acl : root -> nsubj -> cop -> case_ -> nmod -> acl -> UDS ;
	--the data breach is in relation to any prescribed personal data or class of personal data relating to the individual ;

		root_nsubj_cop_nmodPoss : root -> nsubj -> cop -> nmodPoss -> UDS ;
	--Bob is your mother's brother ;


		root_nsubj_obj : root -> nsubj -> obj -> UDS ;
	--Data breach includes the loss of any storage medium or device with personal data. ;

		root_nsubj_obj_xcomp : root -> nsubj -> obj -> xcomp -> UDS ;
	--PDPC instructs you not to notify them ;

		root_nsubj_obl : root -> nsubj -> obl -> UDS ;
	--significant harm refers to data breach of any prescribed personal data or class of personal data of the individual ;

		root_nsubj_obl_obl : root -> nsubj -> obl -> obl -> UDS ;
	--you act as DI for org

		root_nsubj_xcomp : root -> nsubj -> xcomp -> UDS ;
	--the data breach relates to ;

		root_nummod : root -> nummod -> UDS ;
	--(Act §26C.2) ;

		root_nummod_appos : root -> nummod -> appos -> UDS ;
	--$10 * songLength ;

		root_nummod_auxPass_cc_aux_auxPass_obl_obl : root -> nummod -> auxPass -> cc -> aux -> auxPass -> obl -> obl -> UDS ;
	--(c) is or had been taken into care or custody by the Director-General of ;


		root_nummod_mark_obj : root -> nummod -> mark -> obj -> UDS ;
	--(c) to verify the authenticity or integrity of a secure electronic signature. ;

		root_nummod_mark_obj_cc : root -> nummod -> mark -> obj -> cc -> UDS ;
	--(b) to verify the integrity of a secure electronic record; or ;

		root_nummod_nmod : root -> nummod -> nmod -> UDS ;
	--Part 2 of the Schedule ;

		root_nummod_nsubjPass_nsubjPass_auxPass_cc : root -> nummod -> nsubjPass -> nsubjPass -> auxPass -> cc -> UDS ;
	--(b) any personal data that is disclosed to the extent that is required or ;

		root_nummod_obl : root -> nummod -> obl -> UDS ;
	--(b) owed by an organisation to the individual. ;

		root_nummod_obl_cc : root -> nummod -> obl -> cc -> UDS ;
	--(a) owed by the individual to an organisation; or ;

		root_obj : root -> obj -> UDS ;
	--eat potato ;

		root_obj_ccomp : root -> obj -> ccomp -> UDS ;
	--includes a number assigned to any account the individual has with an organisation that is a bank or finance company. ;

		root_obj_nmod : root -> obj -> nmod -> UDS ;
	--processes personal data on behalf of and for the purposes of a public agency ;

		root_obl : root -> obl -> UDS ;
	--subject to ;

		root_obl_appos : root -> obl -> appos -> UDS ;
	--"mentioned in sub-paragraph (a) ;

		root_obl_aux : root -> obl -> aux -> UDS ;
	--as the case may be." ;

		root_obl_case : root -> obl -> case_ -> UDS ;
	--together with justification for why ;

		root_obl_obj : root -> obl -> obj -> UDS ;
	--provide to the PDPC an explanation for why your notification was late ;

		root_obl_obl : root -> obl -> obl -> UDS ;
	--mentioned in paragraph 11 on behalf of the individual. ;

		root_obl_obl_obl_cc : root -> obl -> obl -> obl -> UDS ;
	--authorised in that behalf in writing by the Director-General

		root_obl_xcomp : root -> obl -> xcomp -> UDS ;
	--"for an adoption order to be made ;

		root_parataxis : root -> parataxis -> UDS ;
	--tasty(potato) ;

	    root_xcomp : root -> xcomp -> UDS ;
	-- [becoming]:root [aware that db occurs]:xcomp

	    root_advmod_xcomp : root -> advmod -> xcomp -> UDS ;
	-- [not]:advmod [becoming]:root [aware that db occurs]:xcomp

		root_xcomp_ccomp : root -> xcomp -> ccomp -> UDS ;
	--render it [unlikely] that the notifiable data breach will [result] in significant [harm] to the individual ;

		root_nsubj_aux_obl : root -> nsubj -> aux -> obl -> UDS ;
	--the notifiable data [breach] will [result] in significant [harm] to the individual ;


	fun root_advcl_nsubjPass_auxPass : root -> advcl -> nsubjPass -> auxPass -> UDS ;
	fun root_advcl_nsubj_aux_advcl : root -> advcl -> nsubj -> aux -> advcl -> UDS ;
	fun root_advcl_nsubj_aux_advmod_obj : root -> advcl -> nsubj -> aux -> advmod -> obj -> UDS ;
	fun root_advcl_nsubj_aux_ccomp : root -> advcl -> nsubj -> aux -> ccomp -> UDS ;
	fun root_advcl_nsubj_aux_obl_obj : root -> advcl -> nsubj -> aux -> obl -> obj -> UDS ;
	fun root_advcl_nsubj_cop : root -> advcl -> nsubj -> cop -> UDS ;
	fun root_advcl_nsubj_cop_case_amod_nmod : root -> advcl -> nsubj -> cop -> case_ -> amod -> nmod -> UDS ;
	fun root_advcl_nsubj_xcomp : root -> advcl -> nsubj -> xcomp -> UDS ;
	fun root_mark_nsubj_nsubj_xcomp : root -> mark -> nsubj -> nsubj -> xcomp -> UDS ;
	fun root_nsubj_cop_obl_parataxis : root -> nsubj -> cop -> obl -> parataxis -> UDS ;
	fun root_nsubj_obj_advcl : root -> nsubj -> obj -> advcl -> UDS ;

	-- Maryam's annotations
	fun root_advcl_nsubjPass_aux_auxPass_xcomp : root -> advcl -> nsubjPass -> aux -> auxPass -> xcomp -> UDS ;
	fun root_nsubjPass_aux_auxPass_advcl_advcl : root -> nsubjPass -> aux -> auxPass -> advcl -> advcl -> UDS ;
	fun root_nsubjPass_aux_auxPass_advmod : root -> nsubjPass -> aux -> auxPass -> advmod -> UDS ;
	fun root_nsubjPass_aux_auxPass_obl : root -> nsubjPass -> aux -> auxPass -> obl -> UDS ;
	fun root_nsubjPass_aux_auxPass_xcomp : root -> nsubjPass -> aux -> auxPass -> xcomp -> UDS ;
	fun root_nsubj_aux_cop_nmod : root -> nsubj -> aux -> cop -> nmod -> UDS ;
	fun root_nsubj_nsubj_aux_advmod_obj : root -> nsubj -> nsubj -> aux -> advmod -> obj -> UDS ;
  fun root_advcl_nsubj_cop_nsubj : root -> advcl -> nsubj -> cop -> nsubj -> UDS ;

}
