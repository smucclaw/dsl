module LS.UDExt where

import PGF hiding (Tree)

----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

newtype GString = GString String deriving Show

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int deriving Show

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double deriving Show

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

data GA =
   GStrA GString
 | LexA String
  deriving Show

data GACard =
   LexACard String
  deriving Show

data GAP =
   GAdAP GAdA GAP
 | GAdjOrd GOrd
 | GConjAP GConj GListAP
 | GPastPartAP GVP
 | GPositA GA
 | GPresPartAP GVP
 | GStrAP GString
 | GUseComparA GA
  deriving Show

data GAdA =
   LexAdA String
  deriving Show

data GAdN =
   GAdnCAdv GCAdv
 | LexAdN String
  deriving Show

data GAdV =
   GConjAdV GConj GListAdV
 | LexAdV String
  deriving Show

data GAdv =
   GComparAdvAdj GCAdv GA GNP
 | GComparAdvAdjS GCAdv GA GS
 | GConjAdv GConj GListAdv
 | GPositAdvAdj GA
 | GPrepNP GPrep GNP
 | GSubjS GSubj GS
 | LexAdv String
  deriving Show

data GAnt =
   GAAnter
 | GASimul
  deriving Show

data GCAdv =
   LexCAdv String
  deriving Show

data GCN =
   GAdjCN GAP GCN
 | GAdvCN GCN GAdv
 | GComplN2 GN2 GNP
 | GConjCN GConj GListCN
 | GPossNP GCN GNP
 | GRelCN GCN GRS
 | GSentCN GCN GSC
 | GUseN GN
 | Gday_CN
 | Ghigher_CN
 | Gleave_CN
 | Gtricyclic_CN
  deriving Show

data GCard =
   GAdNum GAdN GCard
 | GNumDigits GDigits
 | GNumNumeral GNumeral
 | GStrCard GString
 | LexCard String
  deriving Show

data GConj =
   LexConj String
  deriving Show

data GDAP =
   GAdjDAP GDAP GAP
 | GDetDAP GDet
  deriving Show

data GDet =
   GConjDet GConj GListDAP
 | GDetQuant GQuant GNum
 | GDetQuantOrd GQuant GNum GOrd
 | LexDet String
  deriving Show

data GDig =
   GD_0
 | GD_1
 | GD_2
 | GD_3
 | GD_4
 | GD_5
 | GD_6
 | GD_7
 | GD_8
 | GD_9
  deriving Show

data GDigit =
   G500_Digit
 | Gn2
 | Gn3
 | Gn4
 | Gn5
 | Gn6
 | Gn7
 | Gn8
 | Gn9
  deriving Show

data GDigits =
   GIDig GDig
 | GIIDig GDig GDigits
  deriving Show

data GIAdv =
   GAdvIAdv GIAdv GAdv
 | GConjIAdv GConj GListIAdv
 | GPrepIP GPrep GIP
 | Ghow_IAdv
 | Gwhen_IAdv
 | Gwhere_IAdv
 | Gwherein_IAdv
 | Gwhy_IAdv
  deriving Show

data GIComp =
   GCompIAdv GIAdv
 | GCompIP GIP
  deriving Show

data GIDet = GIdetQuant GIQuant GNum
  deriving Show

data GIP =
   GAdvIP GIP GAdv
 | GIdetCN GIDet GCN
 | GIdetIP GIDet
 | Gwhat_IP
 | Gwho_IP
  deriving Show

data GIQuant = Gwhich_IQuant
  deriving Show

data GImp = GImpVP GVP
  deriving Show

data GInterj =
   LexInterj String
  deriving Show

newtype GListAP = GListAP [GAP] deriving Show

newtype GListAdV = GListAdV [GAdV] deriving Show

newtype GListAdv = GListAdv [GAdv] deriving Show

newtype GListCN = GListCN [GCN] deriving Show

newtype GListDAP = GListDAP [GDAP] deriving Show

newtype GListIAdv = GListIAdv [GIAdv] deriving Show

newtype GListNP = GListNP [GNP] deriving Show

newtype GListRS = GListRS [GRS] deriving Show

newtype GListS = GListS [GS] deriving Show

data GN =
   GCompoundN GN GN
 | GStrN GString
 | LexN String
  deriving Show

data GN2 =
   GComplN3 GN3 GNP
 | GUse3N3 GN3
 | LexN2 String
  deriving Show

data GNP =
   GAdvNP GNP GAdv
 | GConjNP GConj GListNP
 | GDetCN GDet GCN
 | GDetNP GDet
 | GExtAdvNP GNP GAdv
 | GGenModNP GNum GNP GCN
 | GMassNP GCN
 | GPredetNP GPredet GNP
 | GRelNP GNP GRS
 | GUsePN GPN
 | GUsePron GPron
 | Geuropean_NP
 | Gnone_NP
 | Gwhoever_NP
  deriving Show

data GNum =
   GNumCard GCard
 | GNumPl
 | GNumSg
 | GStrNum GString
  deriving Show

data GNumeral = Gnum GSub1000000
  deriving Show

data GOrd =
   GOrdDigits GDigits
 | GOrdNumeral GNumeral
 | GOrdNumeralSuperl GNumeral GA
 | GOrdSuperl GA
  deriving Show

data GPConj =
   Gbut_PConj
 | Gfor_PConj
 | Gso_PConj
  deriving Show

data GPN =
   GStrPN GString
 | LexPN String
  deriving Show

data GPol =
   GPNeg
 | GPPos
  deriving Show

data GPredet =
   LexPredet String
  deriving Show

data GPrep =
   LexPrep String
  deriving Show

data GPron =
   LexPron String
  deriving Show

data GQCl =
   GQuestCl GCl
 | GQuestIAdv GIAdv GCl
 | GQuestIComp GIComp GNP
 | GQuestQVP GIP GQVP
 | GQuestSlash GIP GClSlash
 | GQuestVP GIP GVP
  deriving Show

data GQVP =
   GAddAdvQVP GQVP GIAdv
 | GAdvQVP GVP GIAdv
 | GComplSlashIP GVPSlash GIP
  deriving Show

data GQuant =
   GGenNP GNP
 | GPossPron GPron
 | LexQuant String
  deriving Show

data GRCl =
   GRelCl GCl
 | GRelSlash GRP GClSlash
 | GRelVP GRP GVP
  deriving Show

data GRP =
   GFunRP GPrep GNP GRP
 | GIdRP
 | Gthat_RP
 | Gwho_RP
  deriving Show

data GRS =
   GConjRS GConj GListRS
 | GUseRCl GTemp GPol GRCl
  deriving Show

data GS =
   GAdvS GAdv GS
 | GConjS GConj GListS
 | GExistS GTemp GPol GNP
 | GExtAdvS GAdv GS
 | GUseCl GTemp GPol GCl
  deriving Show

data GSub10 =
   Gpot0 GDigit
 | Gpot01
  deriving Show

data GSub100 =
   Gpot0as1 GSub10
 | Gpot1 GDigit
 | Gpot110
 | Gpot111
 | Gpot1plus GDigit GSub10
 | Gpot1to19 GDigit
  deriving Show

data GSub1000 =
   Gpot1as2 GSub100
 | Gpot2 GSub10
 | Gpot2plus GSub10 GSub100
  deriving Show

data GSub1000000 =
   Gpot2as3 GSub1000
 | Gpot3 GSub1000
 | Gpot3plus GSub1000 GSub1000
  deriving Show

data GSubj =
   LexSubj String
  deriving Show

data GTemp = GTTAnt GTense GAnt
  deriving Show

data GTense =
   GTCond
 | GTFut
 | GTPast
 | GTPres
  deriving Show

data GUDFragment =
   GAfter GUDS
 | GBefore GUDS
 | GBy GUDS
 | GOn GUDS
 | GUpon GUDS
 | GVaguely GUDS
 | GsubjAction GNP GUDS
  deriving Show

data GUDS =
   Groot_acl Groot Gacl
 | Groot_acl_nmod Groot Gacl Gnmod
 | Groot_advcl Groot Gadvcl
 | Groot_advmod Groot Gadvmod
 | Groot_advmod_advmod_obl Groot Gadvmod Gadvmod Gobl
 | Groot_advmod_amod Groot Gadvmod Gamod
 | Groot_advmod_nsubj_cop_obl Groot Gadvmod Gnsubj Gcop Gobl
 | Groot_amod Groot Gamod
 | Groot_amod_nmod Groot Gamod Gnmod
 | Groot_appos Groot Gappos
 | Groot_appos_advmod Groot Gappos Gadvmod
 | Groot_auxPass Groot GauxPass
 | Groot_case Groot Gcase_
 | Groot_case_amod Groot Gcase_ Gamod
 | Groot_case_amod_amod Groot Gcase_ Gamod Gamod
 | Groot_case_amod_conj_conj Groot Gcase_ Gamod Gconj Gconj
 | Groot_case_compound Groot Gcase_ Gcompound
 | Groot_case_det Groot Gcase_ Gdet
 | Groot_case_det_amod Groot Gcase_ Gdet Gamod
 | Groot_case_det_compound_conj Groot Gcase_ Gdet Gcompound Gconj
 | Groot_case_det_nmod Groot Gcase_ Gdet Gnmod
 | Groot_case_nummod Groot Gcase_ Gnummod
 | Groot_case_nummod_acl Groot Gcase_ Gnummod Gacl
 | Groot_case_nummod_nummod Groot Gcase_ Gnummod Gnummod
 | Groot_cc Groot Gcc
 | Groot_cc_aux_cop_det_nmod Groot Gcc Gaux Gcop Gdet Gnmod
 | Groot_cc_conj Groot Gcc Gconj
 | Groot_cc_cop_xcomp Groot Gcc Gcop Gxcomp
 | Groot_cc_det_nmod Groot Gcc Gdet Gnmod
 | Groot_cc_nmod Groot Gcc Gnmod
 | Groot_cc_obj Groot Gcc Gobj
 | Groot_ccomp Groot Gccomp
 | Groot_compound Groot Gcompound
 | Groot_compoundPrt_compoundPrt Groot GcompoundPrt GcompoundPrt
 | Groot_compound_acl Groot Gcompound Gacl
 | Groot_compound_amod Groot Gcompound Gamod
 | Groot_compound_appos Groot Gcompound Gappos
 | Groot_compound_compound Groot Gcompound Gcompound
 | Groot_compound_compound_appos Groot Gcompound Gcompound Gappos
 | Groot_compound_compound_conj Groot Gcompound Gcompound Gconj
 | Groot_compound_conj_acl Groot Gcompound Gconj Gacl
 | Groot_compound_flat Groot Gcompound Gflat
 | Groot_conj Groot Gconj
 | Groot_conj_acl Groot Gconj Gacl
 | Groot_conj_appos Groot Gconj Gappos
 | Groot_conj_case Groot Gconj Gcase_
 | Groot_conj_nmod Groot Gconj Gnmod
 | Groot_conj_parataxis Groot Gconj Gparataxis
 | Groot_cop Groot Gcop
 | Groot_cop_advmod Groot Gcop Gadvmod
 | Groot_cop_conj_conj Groot Gcop Gconj Gconj
 | Groot_cop_det_compound_amod Groot Gcop Gdet Gcompound Gamod
 | Groot_cop_det_nmod Groot Gcop Gdet Gnmod
 | Groot_csubj Groot Gcsubj
 | Groot_csubj_aux_aux Groot Gcsubj Gaux Gaux
 | Groot_det Groot Gdet
 | Groot_det_acl Groot Gdet Gacl
 | Groot_det_aclRelcl Groot Gdet GaclRelcl
 | Groot_det_aclRelcl_nmod Groot Gdet GaclRelcl Gnmod
 | Groot_det_advmod Groot Gdet Gadvmod
 | Groot_det_amod Groot Gdet Gamod
 | Groot_det_amod_aclRelcl Groot Gdet Gamod GaclRelcl
 | Groot_det_amod_aclRelcl_nmod Groot Gdet Gamod GaclRelcl Gnmod
 | Groot_det_amod_amod_acl_nmod Groot Gdet Gamod Gamod Gacl Gnmod
 | Groot_det_amod_nmod Groot Gdet Gamod Gnmod
 | Groot_det_amod_obl Groot Gdet Gamod Gobl
 | Groot_det_case Groot Gdet Gcase_
 | Groot_det_compound Groot Gdet Gcompound
 | Groot_det_compound_compound Groot Gdet Gcompound Gcompound
 | Groot_det_compound_compound_nmod_appos Groot Gdet Gcompound Gcompound Gnmod Gappos
 | Groot_det_conj_acl Groot Gdet Gconj Gacl
 | Groot_det_conj_nmod Groot Gdet Gconj Gnmod
 | Groot_det_conj_obj Groot Gdet Gconj Gobj
 | Groot_det_nmod Groot Gdet Gnmod
 | Groot_det_nmodPoss Groot Gdet GnmodPoss
 | Groot_det_nmodPoss_compound Groot Gdet GnmodPoss Gcompound
 | Groot_discourse Groot Gdiscourse
 | Groot_fixed Groot Gfixed
 | Groot_goeswith Groot Ggoeswith
 | Groot_goeswith_det_amod_nmod Groot Ggoeswith Gdet Gamod Gnmod
 | Groot_goeswith_goeswith Groot Ggoeswith Ggoeswith
 | Groot_mark Groot Gmark
 | Groot_mark_case_det_nmod Groot Gmark Gcase_ Gdet Gnmod
 | Groot_mark_cc_mark_obj Groot Gmark Gcc Gmark Gobj
 | Groot_mark_det_obj Groot Gmark Gdet Gobj
 | Groot_mark_expl_cop_xcomp Groot Gmark Gexpl Gcop Gxcomp
 | Groot_mark_expl_nsubj Groot Gmark Gexpl Gnsubj
 | Groot_mark_nsubj Groot Gmark Gnsubj
 | Groot_mark_nsubjPass_auxPass_obl Groot Gmark GnsubjPass GauxPass Gobl
 | Groot_mark_nsubj_aux_advmod_obj Groot Gmark Gnsubj Gaux Gadvmod Gobj
 | Groot_mark_nsubj_aux_aux Groot Gmark Gnsubj Gaux Gaux
 | Groot_mark_nsubj_cop Groot Gmark Gnsubj Gcop
 | Groot_mark_nsubj_cop_case_det Groot Gmark Gnsubj Gcop Gcase_ Gdet
 | Groot_mark_nsubj_cop_det_amod_compound_conj Groot Gmark Gnsubj Gcop Gdet Gamod Gcompound Gconj
 | Groot_mark_nsubj_cop_det_case Groot Gmark Gnsubj Gcop Gdet Gcase_
 | Groot_mark_nsubj_cop_det_compound_compound Groot Gmark Gnsubj Gcop Gdet Gcompound Gcompound
 | Groot_mark_nsubj_cop_obl Groot Gmark Gnsubj Gcop Gobl
 | Groot_mark_nsubj_obj Groot Gmark Gnsubj Gobj
 | Groot_mark_nsubj_obl Groot Gmark Gnsubj Gobl
 | Groot_mark_nummod Groot Gmark Gnummod
 | Groot_nmod Groot Gnmod
 | Groot_nmodPoss_advmod Groot GnmodPoss Gadvmod
 | Groot_nmodPoss_nmodPoss Groot GnmodPoss GnmodPoss
 | Groot_nmod_acl Groot Gnmod Gacl
 | Groot_nsubj Groot Gnsubj
 | Groot_nsubjPass_auxPass Groot GnsubjPass GauxPass
 | Groot_nsubjPass_auxPass_advmod_advcl Groot GnsubjPass GauxPass Gadvmod Gadvcl
 | Groot_nsubjPass_auxPass_advmod_xcomp Groot GnsubjPass GauxPass Gadvmod Gxcomp
 | Groot_nsubjPass_auxPass_xcomp Groot GnsubjPass GauxPass Gxcomp
 | Groot_nsubjPass_aux_auxPass Groot GnsubjPass Gaux GauxPass
 | Groot_nsubjPass_aux_auxPass_obl_advmod Groot GnsubjPass Gaux GauxPass Gobl Gadvmod
 | Groot_nsubjPass_aux_auxPass_obl_conj Groot GnsubjPass Gaux GauxPass Gobl Gconj
 | Groot_nsubjPass_aux_auxPass_obl_obl_advcl Groot GnsubjPass Gaux GauxPass Gobl Gobl Gadvcl
 | Groot_nsubjPass_aux_auxPass_obl_obl_advmod Groot GnsubjPass Gaux GauxPass Gobl Gobl Gadvmod
 | Groot_nsubj_advmod Groot Gnsubj Gadvmod
 | Groot_nsubj_advmod_case_det Groot Gnsubj Gadvmod Gcase_ Gdet
 | Groot_nsubj_advmod_obj Groot Gnsubj Gadvmod Gobj
 | Groot_nsubj_aux Groot Gnsubj Gaux
 | Groot_nsubj_aux_aclRelcl Groot Gnsubj Gaux GaclRelcl
 | Groot_nsubj_aux_aclRelcl_obl Groot Gnsubj Gaux GaclRelcl Gobl
 | Groot_nsubj_aux_advmod Groot Gnsubj Gaux Gadvmod
 | Groot_nsubj_aux_advmod_obj_advcl Groot Gnsubj Gaux Gadvmod Gobj Gadvcl
 | Groot_nsubj_aux_aux Groot Gnsubj Gaux Gaux
 | Groot_nsubj_aux_conj Groot Gnsubj Gaux Gconj
 | Groot_nsubj_aux_conj_obl Groot Gnsubj Gaux Gconj Gobl
 | Groot_nsubj_aux_obj Groot Gnsubj Gaux Gobj
 | Groot_nsubj_aux_obj_conj_conj Groot Gnsubj Gaux Gobj Gconj Gconj
 | Groot_nsubj_aux_obj_obl Groot Gnsubj Gaux Gobj Gobl
 | Groot_nsubj_aux_obj_obl_advmod_advcl Groot Gnsubj Gaux Gobj Gobl Gadvmod Gadvcl
 | Groot_nsubj_aux_obj_obl_obl Groot Gnsubj Gaux Gobj Gobl Gobl
 | Groot_nsubj_aux_obl Groot Gnsubj Gaux Gobl
 | Groot_nsubj_ccomp Groot Gnsubj Gccomp
 | Groot_nsubj_conj Groot Gnsubj Gconj
 | Groot_nsubj_conj_obl Groot Gnsubj Gconj Gobl
 | Groot_nsubj_cop Groot Gnsubj Gcop
 | Groot_nsubj_cop_aclRelcl Groot Gnsubj Gcop GaclRelcl
 | Groot_nsubj_cop_aclRelcl_obl Groot Gnsubj Gcop GaclRelcl Gobl
 | Groot_nsubj_cop_advcl Groot Gnsubj Gcop Gadvcl
 | Groot_nsubj_cop_advmod Groot Gnsubj Gcop Gadvmod
 | Groot_nsubj_cop_case_nmod_acl Groot Gnsubj Gcop Gcase_ Gnmod Gacl
 | Groot_nsubj_cop_cc_conj Groot Gnsubj Gcop Gcc Gconj
 | Groot_nsubj_cop_det_amod_advcl Groot Gnsubj Gcop Gdet Gamod Gadvcl
 | Groot_nsubj_cop_det_amod_compound Groot Gnsubj Gcop Gdet Gamod Gcompound
 | Groot_nsubj_cop_det_compound Groot Gnsubj Gcop Gdet Gcompound
 | Groot_nsubj_cop_det_compound_conj Groot Gnsubj Gcop Gdet Gcompound Gconj
 | Groot_nsubj_cop_det_conj Groot Gnsubj Gcop Gdet Gconj
 | Groot_nsubj_cop_det_nmod Groot Gnsubj Gcop Gdet Gnmod
 | Groot_nsubj_cop_nmod Groot Gnsubj Gcop Gnmod
 | Groot_nsubj_cop_nmodPoss Groot Gnsubj Gcop GnmodPoss
 | Groot_nsubj_cop_obl Groot Gnsubj Gcop Gobl
 | Groot_nsubj_det Groot Gnsubj Gdet
 | Groot_nsubj_det_nmod_nmod Groot Gnsubj Gdet Gnmod Gnmod
 | Groot_nsubj_obj Groot Gnsubj Gobj
 | Groot_nsubj_obj_xcomp Groot Gnsubj Gobj Gxcomp
 | Groot_nsubj_obl Groot Gnsubj Gobl
 | Groot_nsubj_xcomp Groot Gnsubj Gxcomp
 | Groot_nummod Groot Gnummod
 | Groot_nummod_appos Groot Gnummod Gappos
 | Groot_nummod_auxPass_cc_aux_auxPass_obl_obl Groot Gnummod GauxPass Gcc Gaux GauxPass Gobl Gobl
 | Groot_nummod_conj Groot Gnummod Gconj
 | Groot_nummod_cop_cc_aux_cop_det_nmod Groot Gnummod Gcop Gcc Gaux Gcop Gdet Gnmod
 | Groot_nummod_det_acl Groot Gnummod Gdet Gacl
 | Groot_nummod_det_aclRelcl Groot Gnummod Gdet GaclRelcl
 | Groot_nummod_det_amod Groot Gnummod Gdet Gamod
 | Groot_nummod_det_amod_conj_conj Groot Gnummod Gdet Gamod Gconj Gconj
 | Groot_nummod_det_conj_nmod Groot Gnummod Gdet Gconj Gnmod
 | Groot_nummod_det_conj_nmod_cc Groot Gnummod Gdet Gconj Gnmod Gcc
 | Groot_nummod_det_nmod Groot Gnummod Gdet Gnmod
 | Groot_nummod_mark_obj Groot Gnummod Gmark Gobj
 | Groot_nummod_mark_obj_cc Groot Gnummod Gmark Gobj Gcc
 | Groot_nummod_nmod Groot Gnummod Gnmod
 | Groot_nummod_nsubjPass_nsubjPass_auxPass_cc Groot Gnummod GnsubjPass GnsubjPass GauxPass Gcc
 | Groot_nummod_obl Groot Gnummod Gobl
 | Groot_nummod_obl_cc Groot Gnummod Gobl Gcc
 | Groot_obj Groot Gobj
 | Groot_obj_ccomp Groot Gobj Gccomp
 | Groot_obj_nmod Groot Gobj Gnmod
 | Groot_obl Groot Gobl
 | Groot_obl_appos Groot Gobl Gappos
 | Groot_obl_aux Groot Gobl Gaux
 | Groot_obl_case Groot Gobl Gcase_
 | Groot_obl_obj Groot Gobl Gobj
 | Groot_obl_obl Groot Gobl Gobl
 | Groot_obl_obl_obl_cc Groot Gobl Gobl Gobl
 | Groot_obl_xcomp Groot Gobl Gxcomp
 | Groot_only Groot
 | Groot_parataxis Groot Gparataxis
 | Groot_xcomp_ccomp Groot Gxcomp Gccomp
  deriving Show

data GV =
   LexV String
  deriving Show

data GVP =
   GAdVVP GAdV GVP
 | GAdvVP GVP GAdv
 | GComplV GV GNP
 | GPassV GV
 | GPassVAgent GV GNP
 | GProgrVP GVP
 | GUseV GV
  deriving Show

data Gacl =
   GaclUDS_ GUDS
 | Gacl_ GX
  deriving Show

data GaclRelcl =
   GaclRelclRS_ GRS
 | GaclRelclUDS_ GUDS
 | GpassRelcl_ Groot GRP GauxPass
  deriving Show

data Gadvcl =
   GadvclUDS_ GUDS
 | Gadvcl_ GX
  deriving Show

data Gadvmod =
   Gadvmod_ GAdv
 | Gnot_advmod
  deriving Show

data GadvmodEmph = GadvmodEmph_ GX
  deriving Show

data GadvmodLmod = GadvmodLmod_ GX
  deriving Show

data Gamod = Gamod_ GAP
  deriving Show

data Gappos = Gappos_ GX
  deriving Show

data Gaux =
   Gaux_ GX
 | Gbe_aux
 | Gcan_aux
 | Ghave_aux
 | Gmay_aux
 | Gmust_aux
 | Gshould_aux
 | Gwill_aux
  deriving Show

data GauxPass = Gbe_auxPass
  deriving Show

data Gcase_ = Gcase__ GX
  deriving Show

data Gcc = Gcc_ GConj
  deriving Show

data GccPreconj = GccPreconj_ GX
  deriving Show

data Gccomp = Gccomp_ GUDS
  deriving Show

data Gclf = Gclf_ GX
  deriving Show

data Gcompound = Gcompound_ GX
  deriving Show

data GcompoundLvc = GcompoundLvc_ GX
  deriving Show

data GcompoundPrt = GcompoundPrt_ GX
  deriving Show

data GcompoundRedup = GcompoundRedup_ GX
  deriving Show

data GcompoundSvc = GcompoundSvc_ GX
  deriving Show

data Gconj =
   GconjA_ GAP
 | GconjAdv_ GAdv
 | GconjN_ GNP
 | Gconj_ GX
  deriving Show

data Gcop = Gbe_cop
  deriving Show

data Gcsubj = Gcsubj_ GX
  deriving Show

data GcsubjPass = GcsubjPass_ GX
  deriving Show

data Gdep = Gdep_ GX
  deriving Show

data Gdet = Gdet_ GDet
  deriving Show

data GdetNumgov = GdetNumgov_ GX
  deriving Show

data GdetNummod = GdetNummod_ GX
  deriving Show

data GdetPoss = GdetPoss_ GX
  deriving Show

data Gdiscourse = Gdiscourse_ GX
  deriving Show

data Gdislocated = Gdislocated_ GX
  deriving Show

data Gexpl =
   Gexpl_ GPron
 | Git_expl
  deriving Show

data GexplImpers = GexplImpers_ GX
  deriving Show

data GexplPass = GexplPass_ GX
  deriving Show

data GexplPv = GexplPv_ GX
  deriving Show

data Gfixed = Gfixed_ GX
  deriving Show

data Gflat = Gflat_ GX
  deriving Show

data GflatForeign = GflatForeign_ GX
  deriving Show

data GflatName = GflatName_ GX
  deriving Show

data Ggoeswith = Ggoeswith_ GX
  deriving Show

data Giobj = Giobj_ GNP
  deriving Show

data Glist = Glist_ GX
  deriving Show

data Gmark = Gmark_ GSubj
  deriving Show

data Gnmod = Gnmod_ GPrep GNP
  deriving Show

data GnmodPoss = GnmodPoss_ GX
  deriving Show

data GnmodTmod = GnmodTmod_ GX
  deriving Show

data Gnsubj = Gnsubj_ GNP
  deriving Show

data GnsubjPass = GnsubjPass_ GNP
  deriving Show

data Gnummod = Gnummod_ GX
  deriving Show

data GnummodGov = GnummodGov_ GX
  deriving Show

data Gobj = Gobj_ GNP
  deriving Show

data Gobl =
   GoblPrep_ GPrep
 | Gobl_ GAdv
  deriving Show

data GoblAgent = GoblAgent_ GX
  deriving Show

data GoblArg = GoblArg_ GX
  deriving Show

data GoblLmod = GoblLmod_ GX
  deriving Show

data GoblTmod = GoblTmod_ GX
  deriving Show

data Gorphan = Gorphan_ GX
  deriving Show

data Gparataxis = Gparataxis_ GX
  deriving Show

data Gpunct = Gpunct_ GX
  deriving Show

data Greparandum = Greparandum_ GX
  deriving Show

data Groot =
   GrootA_ GAP
 | GrootAdv_ GAdv
 | GrootN_ GNP
 | GrootV_ GVP
  deriving Show

data Gvocative = Gvocative_ GNP
  deriving Show

data Gxcomp =
   GxcompA_ GAP
 | GxcompAdv_ GAdv
  deriving Show

data GA2

data GCl

data GClSlash

data GComp

data GN3

data GPhr

data GQS

data GSC

data GSSlash

data GText

data GUtt

data GV2

data GV2A

data GV2Q

data GV2S

data GV2V

data GV3

data GVA

data GVPSlash

data GVQ

data GVS

data GVV

data GVoc

data GX


instance Gf GA where
  gf (GStrA x1) = mkApp (mkCId "StrA") [gf x1]
  gf (LexA x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "StrA" -> GStrA (fg x1)

      Just (i,[]) -> LexA (showCId i)
      _ -> error ("no A " ++ show t)

instance Gf GACard where
  gf (LexACard x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexACard (showCId i)
      _ -> error ("no ACard " ++ show t)

instance Gf GAP where
  gf (GAdAP x1 x2) = mkApp (mkCId "AdAP") [gf x1, gf x2]
  gf (GAdjOrd x1) = mkApp (mkCId "AdjOrd") [gf x1]
  gf (GConjAP x1 x2) = mkApp (mkCId "ConjAP") [gf x1, gf x2]
  gf (GPastPartAP x1) = mkApp (mkCId "PastPartAP") [gf x1]
  gf (GPositA x1) = mkApp (mkCId "PositA") [gf x1]
  gf (GPresPartAP x1) = mkApp (mkCId "PresPartAP") [gf x1]
  gf (GStrAP x1) = mkApp (mkCId "StrAP") [gf x1]
  gf (GUseComparA x1) = mkApp (mkCId "UseComparA") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdAP" -> GAdAP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "AdjOrd" -> GAdjOrd (fg x1)
      Just (i,[x1,x2]) | i == mkCId "ConjAP" -> GConjAP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "PastPartAP" -> GPastPartAP (fg x1)
      Just (i,[x1]) | i == mkCId "PositA" -> GPositA (fg x1)
      Just (i,[x1]) | i == mkCId "PresPartAP" -> GPresPartAP (fg x1)
      Just (i,[x1]) | i == mkCId "StrAP" -> GStrAP (fg x1)
      Just (i,[x1]) | i == mkCId "UseComparA" -> GUseComparA (fg x1)


      _ -> error ("no AP " ++ show t)

instance Gf GAdA where
  gf (LexAdA x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexAdA (showCId i)
      _ -> error ("no AdA " ++ show t)

instance Gf GAdN where
  gf (GAdnCAdv x1) = mkApp (mkCId "AdnCAdv") [gf x1]
  gf (LexAdN x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "AdnCAdv" -> GAdnCAdv (fg x1)

      Just (i,[]) -> LexAdN (showCId i)
      _ -> error ("no AdN " ++ show t)

instance Gf GAdV where
  gf (GConjAdV x1 x2) = mkApp (mkCId "ConjAdV") [gf x1, gf x2]
  gf (LexAdV x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjAdV" -> GConjAdV (fg x1) (fg x2)

      Just (i,[]) -> LexAdV (showCId i)
      _ -> error ("no AdV " ++ show t)

instance Gf GAdv where
  gf (GComparAdvAdj x1 x2 x3) = mkApp (mkCId "ComparAdvAdj") [gf x1, gf x2, gf x3]
  gf (GComparAdvAdjS x1 x2 x3) = mkApp (mkCId "ComparAdvAdjS") [gf x1, gf x2, gf x3]
  gf (GConjAdv x1 x2) = mkApp (mkCId "ConjAdv") [gf x1, gf x2]
  gf (GPositAdvAdj x1) = mkApp (mkCId "PositAdvAdj") [gf x1]
  gf (GPrepNP x1 x2) = mkApp (mkCId "PrepNP") [gf x1, gf x2]
  gf (GSubjS x1 x2) = mkApp (mkCId "SubjS") [gf x1, gf x2]
  gf (LexAdv x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "ComparAdvAdj" -> GComparAdvAdj (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "ComparAdvAdjS" -> GComparAdvAdjS (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "ConjAdv" -> GConjAdv (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "PositAdvAdj" -> GPositAdvAdj (fg x1)
      Just (i,[x1,x2]) | i == mkCId "PrepNP" -> GPrepNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SubjS" -> GSubjS (fg x1) (fg x2)

      Just (i,[]) -> LexAdv (showCId i)
      _ -> error ("no Adv " ++ show t)

instance Gf GAnt where
  gf GAAnter = mkApp (mkCId "AAnter") []
  gf GASimul = mkApp (mkCId "ASimul") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "AAnter" -> GAAnter
      Just (i,[]) | i == mkCId "ASimul" -> GASimul


      _ -> error ("no Ant " ++ show t)

instance Gf GCAdv where
  gf (LexCAdv x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexCAdv (showCId i)
      _ -> error ("no CAdv " ++ show t)

instance Gf GCN where
  gf (GAdjCN x1 x2) = mkApp (mkCId "AdjCN") [gf x1, gf x2]
  gf (GAdvCN x1 x2) = mkApp (mkCId "AdvCN") [gf x1, gf x2]
  gf (GComplN2 x1 x2) = mkApp (mkCId "ComplN2") [gf x1, gf x2]
  gf (GConjCN x1 x2) = mkApp (mkCId "ConjCN") [gf x1, gf x2]
  gf (GPossNP x1 x2) = mkApp (mkCId "PossNP") [gf x1, gf x2]
  gf (GRelCN x1 x2) = mkApp (mkCId "RelCN") [gf x1, gf x2]
  gf (GSentCN x1 x2) = mkApp (mkCId "SentCN") [gf x1, gf x2]
  gf (GUseN x1) = mkApp (mkCId "UseN") [gf x1]
  gf Gday_CN = mkApp (mkCId "day_CN") []
  gf Ghigher_CN = mkApp (mkCId "higher_CN") []
  gf Gleave_CN = mkApp (mkCId "leave_CN") []
  gf Gtricyclic_CN = mkApp (mkCId "tricyclic_CN") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdjCN" -> GAdjCN (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AdvCN" -> GAdvCN (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplN2" -> GComplN2 (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjCN" -> GConjCN (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PossNP" -> GPossNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RelCN" -> GRelCN (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SentCN" -> GSentCN (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "UseN" -> GUseN (fg x1)
      Just (i,[]) | i == mkCId "day_CN" -> Gday_CN
      Just (i,[]) | i == mkCId "higher_CN" -> Ghigher_CN
      Just (i,[]) | i == mkCId "leave_CN" -> Gleave_CN
      Just (i,[]) | i == mkCId "tricyclic_CN" -> Gtricyclic_CN


      _ -> error ("no CN " ++ show t)

instance Gf GCard where
  gf (GAdNum x1 x2) = mkApp (mkCId "AdNum") [gf x1, gf x2]
  gf (GNumDigits x1) = mkApp (mkCId "NumDigits") [gf x1]
  gf (GNumNumeral x1) = mkApp (mkCId "NumNumeral") [gf x1]
  gf (GStrCard x1) = mkApp (mkCId "StrCard") [gf x1]
  gf (LexCard x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdNum" -> GAdNum (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "NumDigits" -> GNumDigits (fg x1)
      Just (i,[x1]) | i == mkCId "NumNumeral" -> GNumNumeral (fg x1)
      Just (i,[x1]) | i == mkCId "StrCard" -> GStrCard (fg x1)

      Just (i,[]) -> LexCard (showCId i)
      _ -> error ("no Card " ++ show t)

instance Gf GConj where
  gf (LexConj x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexConj (showCId i)
      _ -> error ("no Conj " ++ show t)

instance Gf GDAP where
  gf (GAdjDAP x1 x2) = mkApp (mkCId "AdjDAP") [gf x1, gf x2]
  gf (GDetDAP x1) = mkApp (mkCId "DetDAP") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdjDAP" -> GAdjDAP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "DetDAP" -> GDetDAP (fg x1)


      _ -> error ("no DAP " ++ show t)

instance Gf GDet where
  gf (GConjDet x1 x2) = mkApp (mkCId "ConjDet") [gf x1, gf x2]
  gf (GDetQuant x1 x2) = mkApp (mkCId "DetQuant") [gf x1, gf x2]
  gf (GDetQuantOrd x1 x2 x3) = mkApp (mkCId "DetQuantOrd") [gf x1, gf x2, gf x3]
  gf (LexDet x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjDet" -> GConjDet (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "DetQuant" -> GDetQuant (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "DetQuantOrd" -> GDetQuantOrd (fg x1) (fg x2) (fg x3)

      Just (i,[]) -> LexDet (showCId i)
      _ -> error ("no Det " ++ show t)

instance Gf GDig where
  gf GD_0 = mkApp (mkCId "D_0") []
  gf GD_1 = mkApp (mkCId "D_1") []
  gf GD_2 = mkApp (mkCId "D_2") []
  gf GD_3 = mkApp (mkCId "D_3") []
  gf GD_4 = mkApp (mkCId "D_4") []
  gf GD_5 = mkApp (mkCId "D_5") []
  gf GD_6 = mkApp (mkCId "D_6") []
  gf GD_7 = mkApp (mkCId "D_7") []
  gf GD_8 = mkApp (mkCId "D_8") []
  gf GD_9 = mkApp (mkCId "D_9") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "D_0" -> GD_0
      Just (i,[]) | i == mkCId "D_1" -> GD_1
      Just (i,[]) | i == mkCId "D_2" -> GD_2
      Just (i,[]) | i == mkCId "D_3" -> GD_3
      Just (i,[]) | i == mkCId "D_4" -> GD_4
      Just (i,[]) | i == mkCId "D_5" -> GD_5
      Just (i,[]) | i == mkCId "D_6" -> GD_6
      Just (i,[]) | i == mkCId "D_7" -> GD_7
      Just (i,[]) | i == mkCId "D_8" -> GD_8
      Just (i,[]) | i == mkCId "D_9" -> GD_9


      _ -> error ("no Dig " ++ show t)

instance Gf GDigit where
  gf G500_Digit = mkApp (mkCId "'500_Digit'") []
  gf Gn2 = mkApp (mkCId "n2") []
  gf Gn3 = mkApp (mkCId "n3") []
  gf Gn4 = mkApp (mkCId "n4") []
  gf Gn5 = mkApp (mkCId "n5") []
  gf Gn6 = mkApp (mkCId "n6") []
  gf Gn7 = mkApp (mkCId "n7") []
  gf Gn8 = mkApp (mkCId "n8") []
  gf Gn9 = mkApp (mkCId "n9") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "'500_Digit'" -> G500_Digit
      Just (i,[]) | i == mkCId "n2" -> Gn2
      Just (i,[]) | i == mkCId "n3" -> Gn3
      Just (i,[]) | i == mkCId "n4" -> Gn4
      Just (i,[]) | i == mkCId "n5" -> Gn5
      Just (i,[]) | i == mkCId "n6" -> Gn6
      Just (i,[]) | i == mkCId "n7" -> Gn7
      Just (i,[]) | i == mkCId "n8" -> Gn8
      Just (i,[]) | i == mkCId "n9" -> Gn9


      _ -> error ("no Digit " ++ show t)

instance Gf GDigits where
  gf (GIDig x1) = mkApp (mkCId "IDig") [gf x1]
  gf (GIIDig x1 x2) = mkApp (mkCId "IIDig") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "IDig" -> GIDig (fg x1)
      Just (i,[x1,x2]) | i == mkCId "IIDig" -> GIIDig (fg x1) (fg x2)


      _ -> error ("no Digits " ++ show t)

instance Gf GIAdv where
  gf (GAdvIAdv x1 x2) = mkApp (mkCId "AdvIAdv") [gf x1, gf x2]
  gf (GConjIAdv x1 x2) = mkApp (mkCId "ConjIAdv") [gf x1, gf x2]
  gf (GPrepIP x1 x2) = mkApp (mkCId "PrepIP") [gf x1, gf x2]
  gf Ghow_IAdv = mkApp (mkCId "how_IAdv") []
  gf Gwhen_IAdv = mkApp (mkCId "when_IAdv") []
  gf Gwhere_IAdv = mkApp (mkCId "where_IAdv") []
  gf Gwherein_IAdv = mkApp (mkCId "wherein_IAdv") []
  gf Gwhy_IAdv = mkApp (mkCId "why_IAdv") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvIAdv" -> GAdvIAdv (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjIAdv" -> GConjIAdv (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PrepIP" -> GPrepIP (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "how_IAdv" -> Ghow_IAdv
      Just (i,[]) | i == mkCId "when_IAdv" -> Gwhen_IAdv
      Just (i,[]) | i == mkCId "where_IAdv" -> Gwhere_IAdv
      Just (i,[]) | i == mkCId "wherein_IAdv" -> Gwherein_IAdv
      Just (i,[]) | i == mkCId "why_IAdv" -> Gwhy_IAdv


      _ -> error ("no IAdv " ++ show t)

instance Gf GIComp where
  gf (GCompIAdv x1) = mkApp (mkCId "CompIAdv") [gf x1]
  gf (GCompIP x1) = mkApp (mkCId "CompIP") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "CompIAdv" -> GCompIAdv (fg x1)
      Just (i,[x1]) | i == mkCId "CompIP" -> GCompIP (fg x1)


      _ -> error ("no IComp " ++ show t)

instance Gf GIDet where
  gf (GIdetQuant x1 x2) = mkApp (mkCId "IdetQuant") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "IdetQuant" -> GIdetQuant (fg x1) (fg x2)


      _ -> error ("no IDet " ++ show t)

instance Gf GIP where
  gf (GAdvIP x1 x2) = mkApp (mkCId "AdvIP") [gf x1, gf x2]
  gf (GIdetCN x1 x2) = mkApp (mkCId "IdetCN") [gf x1, gf x2]
  gf (GIdetIP x1) = mkApp (mkCId "IdetIP") [gf x1]
  gf Gwhat_IP = mkApp (mkCId "what_IP") []
  gf Gwho_IP = mkApp (mkCId "who_IP") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvIP" -> GAdvIP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "IdetCN" -> GIdetCN (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "IdetIP" -> GIdetIP (fg x1)
      Just (i,[]) | i == mkCId "what_IP" -> Gwhat_IP
      Just (i,[]) | i == mkCId "who_IP" -> Gwho_IP


      _ -> error ("no IP " ++ show t)

instance Gf GIQuant where
  gf Gwhich_IQuant = mkApp (mkCId "which_IQuant") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "which_IQuant" -> Gwhich_IQuant


      _ -> error ("no IQuant " ++ show t)

instance Gf GImp where
  gf (GImpVP x1) = mkApp (mkCId "ImpVP") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "ImpVP" -> GImpVP (fg x1)


      _ -> error ("no Imp " ++ show t)

instance Gf GInterj where
  gf (LexInterj x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexInterj (showCId i)
      _ -> error ("no Interj " ++ show t)

instance Gf GListAP where
  gf (GListAP [x1,x2]) = mkApp (mkCId "BaseAP") [gf x1, gf x2]
  gf (GListAP (x:xs)) = mkApp (mkCId "ConsAP") [gf x, gf (GListAP xs)]
  fg t =
    GListAP (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseAP" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsAP" -> fg x1 : fgs x2


      _ -> error ("no ListAP " ++ show t)

instance Gf GListAdV where
  gf (GListAdV [x1,x2]) = mkApp (mkCId "BaseAdV") [gf x1, gf x2]
  gf (GListAdV (x:xs)) = mkApp (mkCId "ConsAdV") [gf x, gf (GListAdV xs)]
  fg t =
    GListAdV (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseAdV" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsAdV" -> fg x1 : fgs x2


      _ -> error ("no ListAdV " ++ show t)

instance Gf GListAdv where
  gf (GListAdv [x1,x2]) = mkApp (mkCId "BaseAdv") [gf x1, gf x2]
  gf (GListAdv (x:xs)) = mkApp (mkCId "ConsAdv") [gf x, gf (GListAdv xs)]
  fg t =
    GListAdv (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseAdv" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsAdv" -> fg x1 : fgs x2


      _ -> error ("no ListAdv " ++ show t)

instance Gf GListCN where
  gf (GListCN [x1,x2]) = mkApp (mkCId "BaseCN") [gf x1, gf x2]
  gf (GListCN (x:xs)) = mkApp (mkCId "ConsCN") [gf x, gf (GListCN xs)]
  fg t =
    GListCN (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseCN" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsCN" -> fg x1 : fgs x2


      _ -> error ("no ListCN " ++ show t)

instance Gf GListDAP where
  gf (GListDAP [x1,x2]) = mkApp (mkCId "BaseDAP") [gf x1, gf x2]
  gf (GListDAP (x:xs)) = mkApp (mkCId "ConsDAP") [gf x, gf (GListDAP xs)]
  fg t =
    GListDAP (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseDAP" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsDAP" -> fg x1 : fgs x2


      _ -> error ("no ListDAP " ++ show t)

instance Gf GListIAdv where
  gf (GListIAdv [x1,x2]) = mkApp (mkCId "BaseIAdv") [gf x1, gf x2]
  gf (GListIAdv (x:xs)) = mkApp (mkCId "ConsIAdv") [gf x, gf (GListIAdv xs)]
  fg t =
    GListIAdv (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseIAdv" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsIAdv" -> fg x1 : fgs x2


      _ -> error ("no ListIAdv " ++ show t)

instance Gf GListNP where
  gf (GListNP [x1,x2]) = mkApp (mkCId "BaseNP") [gf x1, gf x2]
  gf (GListNP (x:xs)) = mkApp (mkCId "ConsNP") [gf x, gf (GListNP xs)]
  fg t =
    GListNP (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseNP" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsNP" -> fg x1 : fgs x2


      _ -> error ("no ListNP " ++ show t)

instance Gf GListRS where
  gf (GListRS [x1,x2]) = mkApp (mkCId "BaseRS") [gf x1, gf x2]
  gf (GListRS (x:xs)) = mkApp (mkCId "ConsRS") [gf x, gf (GListRS xs)]
  fg t =
    GListRS (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseRS" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsRS" -> fg x1 : fgs x2


      _ -> error ("no ListRS " ++ show t)

instance Gf GListS where
  gf (GListS [x1,x2]) = mkApp (mkCId "BaseS") [gf x1, gf x2]
  gf (GListS (x:xs)) = mkApp (mkCId "ConsS") [gf x, gf (GListS xs)]
  fg t =
    GListS (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseS" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsS" -> fg x1 : fgs x2


      _ -> error ("no ListS " ++ show t)

instance Gf GN where
  gf (GCompoundN x1 x2) = mkApp (mkCId "CompoundN") [gf x1, gf x2]
  gf (GStrN x1) = mkApp (mkCId "StrN") [gf x1]
  gf (LexN x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "CompoundN" -> GCompoundN (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "StrN" -> GStrN (fg x1)

      Just (i,[]) -> LexN (showCId i)
      _ -> error ("no N " ++ show t)

instance Gf GN2 where
  gf (GComplN3 x1 x2) = mkApp (mkCId "ComplN3") [gf x1, gf x2]
  gf (GUse3N3 x1) = mkApp (mkCId "Use3N3") [gf x1]
  gf (LexN2 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ComplN3" -> GComplN3 (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "Use3N3" -> GUse3N3 (fg x1)

      Just (i,[]) -> LexN2 (showCId i)
      _ -> error ("no N2 " ++ show t)

instance Gf GNP where
  gf (GAdvNP x1 x2) = mkApp (mkCId "AdvNP") [gf x1, gf x2]
  gf (GConjNP x1 x2) = mkApp (mkCId "ConjNP") [gf x1, gf x2]
  gf (GDetCN x1 x2) = mkApp (mkCId "DetCN") [gf x1, gf x2]
  gf (GDetNP x1) = mkApp (mkCId "DetNP") [gf x1]
  gf (GExtAdvNP x1 x2) = mkApp (mkCId "ExtAdvNP") [gf x1, gf x2]
  gf (GGenModNP x1 x2 x3) = mkApp (mkCId "GenModNP") [gf x1, gf x2, gf x3]
  gf (GMassNP x1) = mkApp (mkCId "MassNP") [gf x1]
  gf (GPredetNP x1 x2) = mkApp (mkCId "PredetNP") [gf x1, gf x2]
  gf (GRelNP x1 x2) = mkApp (mkCId "RelNP") [gf x1, gf x2]
  gf (GUsePN x1) = mkApp (mkCId "UsePN") [gf x1]
  gf (GUsePron x1) = mkApp (mkCId "UsePron") [gf x1]
  gf Geuropean_NP = mkApp (mkCId "european_NP") []
  gf Gnone_NP = mkApp (mkCId "none_NP") []
  gf Gwhoever_NP = mkApp (mkCId "whoever_NP") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvNP" -> GAdvNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjNP" -> GConjNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "DetCN" -> GDetCN (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "DetNP" -> GDetNP (fg x1)
      Just (i,[x1,x2]) | i == mkCId "ExtAdvNP" -> GExtAdvNP (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "GenModNP" -> GGenModNP (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "MassNP" -> GMassNP (fg x1)
      Just (i,[x1,x2]) | i == mkCId "PredetNP" -> GPredetNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RelNP" -> GRelNP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "UsePN" -> GUsePN (fg x1)
      Just (i,[x1]) | i == mkCId "UsePron" -> GUsePron (fg x1)
      Just (i,[]) | i == mkCId "european_NP" -> Geuropean_NP
      Just (i,[]) | i == mkCId "none_NP" -> Gnone_NP
      Just (i,[]) | i == mkCId "whoever_NP" -> Gwhoever_NP


      _ -> error ("no NP " ++ show t)

instance Gf GNum where
  gf (GNumCard x1) = mkApp (mkCId "NumCard") [gf x1]
  gf GNumPl = mkApp (mkCId "NumPl") []
  gf GNumSg = mkApp (mkCId "NumSg") []
  gf (GStrNum x1) = mkApp (mkCId "StrNum") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "NumCard" -> GNumCard (fg x1)
      Just (i,[]) | i == mkCId "NumPl" -> GNumPl
      Just (i,[]) | i == mkCId "NumSg" -> GNumSg
      Just (i,[x1]) | i == mkCId "StrNum" -> GStrNum (fg x1)


      _ -> error ("no Num " ++ show t)

instance Gf GNumeral where
  gf (Gnum x1) = mkApp (mkCId "num") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "num" -> Gnum (fg x1)


      _ -> error ("no Numeral " ++ show t)

instance Gf GOrd where
  gf (GOrdDigits x1) = mkApp (mkCId "OrdDigits") [gf x1]
  gf (GOrdNumeral x1) = mkApp (mkCId "OrdNumeral") [gf x1]
  gf (GOrdNumeralSuperl x1 x2) = mkApp (mkCId "OrdNumeralSuperl") [gf x1, gf x2]
  gf (GOrdSuperl x1) = mkApp (mkCId "OrdSuperl") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "OrdDigits" -> GOrdDigits (fg x1)
      Just (i,[x1]) | i == mkCId "OrdNumeral" -> GOrdNumeral (fg x1)
      Just (i,[x1,x2]) | i == mkCId "OrdNumeralSuperl" -> GOrdNumeralSuperl (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "OrdSuperl" -> GOrdSuperl (fg x1)


      _ -> error ("no Ord " ++ show t)

instance Gf GPConj where
  gf Gbut_PConj = mkApp (mkCId "but_PConj") []
  gf Gfor_PConj = mkApp (mkCId "for_PConj") []
  gf Gso_PConj = mkApp (mkCId "so_PConj") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "but_PConj" -> Gbut_PConj
      Just (i,[]) | i == mkCId "for_PConj" -> Gfor_PConj
      Just (i,[]) | i == mkCId "so_PConj" -> Gso_PConj


      _ -> error ("no PConj " ++ show t)

instance Gf GPN where
  gf (GStrPN x1) = mkApp (mkCId "StrPN") [gf x1]
  gf (LexPN x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "StrPN" -> GStrPN (fg x1)

      Just (i,[]) -> LexPN (showCId i)
      _ -> error ("no PN " ++ show t)

instance Gf GPol where
  gf GPNeg = mkApp (mkCId "PNeg") []
  gf GPPos = mkApp (mkCId "PPos") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "PNeg" -> GPNeg
      Just (i,[]) | i == mkCId "PPos" -> GPPos


      _ -> error ("no Pol " ++ show t)

instance Gf GPredet where
  gf (LexPredet x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexPredet (showCId i)
      _ -> error ("no Predet " ++ show t)

instance Gf GPrep where
  gf (LexPrep x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexPrep (showCId i)
      _ -> error ("no Prep " ++ show t)

instance Gf GPron where
  gf (LexPron x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexPron (showCId i)
      _ -> error ("no Pron " ++ show t)

instance Gf GQCl where
  gf (GQuestCl x1) = mkApp (mkCId "QuestCl") [gf x1]
  gf (GQuestIAdv x1 x2) = mkApp (mkCId "QuestIAdv") [gf x1, gf x2]
  gf (GQuestIComp x1 x2) = mkApp (mkCId "QuestIComp") [gf x1, gf x2]
  gf (GQuestQVP x1 x2) = mkApp (mkCId "QuestQVP") [gf x1, gf x2]
  gf (GQuestSlash x1 x2) = mkApp (mkCId "QuestSlash") [gf x1, gf x2]
  gf (GQuestVP x1 x2) = mkApp (mkCId "QuestVP") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "QuestCl" -> GQuestCl (fg x1)
      Just (i,[x1,x2]) | i == mkCId "QuestIAdv" -> GQuestIAdv (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QuestIComp" -> GQuestIComp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QuestQVP" -> GQuestQVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QuestSlash" -> GQuestSlash (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QuestVP" -> GQuestVP (fg x1) (fg x2)


      _ -> error ("no QCl " ++ show t)

instance Gf GQVP where
  gf (GAddAdvQVP x1 x2) = mkApp (mkCId "AddAdvQVP") [gf x1, gf x2]
  gf (GAdvQVP x1 x2) = mkApp (mkCId "AdvQVP") [gf x1, gf x2]
  gf (GComplSlashIP x1 x2) = mkApp (mkCId "ComplSlashIP") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AddAdvQVP" -> GAddAdvQVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AdvQVP" -> GAdvQVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplSlashIP" -> GComplSlashIP (fg x1) (fg x2)


      _ -> error ("no QVP " ++ show t)

instance Gf GQuant where
  gf (GGenNP x1) = mkApp (mkCId "GenNP") [gf x1]
  gf (GPossPron x1) = mkApp (mkCId "PossPron") [gf x1]
  gf (LexQuant x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "GenNP" -> GGenNP (fg x1)
      Just (i,[x1]) | i == mkCId "PossPron" -> GPossPron (fg x1)

      Just (i,[]) -> LexQuant (showCId i)
      _ -> error ("no Quant " ++ show t)

instance Gf GRCl where
  gf (GRelCl x1) = mkApp (mkCId "RelCl") [gf x1]
  gf (GRelSlash x1 x2) = mkApp (mkCId "RelSlash") [gf x1, gf x2]
  gf (GRelVP x1 x2) = mkApp (mkCId "RelVP") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "RelCl" -> GRelCl (fg x1)
      Just (i,[x1,x2]) | i == mkCId "RelSlash" -> GRelSlash (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RelVP" -> GRelVP (fg x1) (fg x2)


      _ -> error ("no RCl " ++ show t)

instance Gf GRP where
  gf (GFunRP x1 x2 x3) = mkApp (mkCId "FunRP") [gf x1, gf x2, gf x3]
  gf GIdRP = mkApp (mkCId "IdRP") []
  gf Gthat_RP = mkApp (mkCId "that_RP") []
  gf Gwho_RP = mkApp (mkCId "who_RP") []

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "FunRP" -> GFunRP (fg x1) (fg x2) (fg x3)
      Just (i,[]) | i == mkCId "IdRP" -> GIdRP
      Just (i,[]) | i == mkCId "that_RP" -> Gthat_RP
      Just (i,[]) | i == mkCId "who_RP" -> Gwho_RP


      _ -> error ("no RP " ++ show t)

instance Gf GRS where
  gf (GConjRS x1 x2) = mkApp (mkCId "ConjRS") [gf x1, gf x2]
  gf (GUseRCl x1 x2 x3) = mkApp (mkCId "UseRCl") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjRS" -> GConjRS (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "UseRCl" -> GUseRCl (fg x1) (fg x2) (fg x3)


      _ -> error ("no RS " ++ show t)

instance Gf GS where
  gf (GAdvS x1 x2) = mkApp (mkCId "AdvS") [gf x1, gf x2]
  gf (GConjS x1 x2) = mkApp (mkCId "ConjS") [gf x1, gf x2]
  gf (GExistS x1 x2 x3) = mkApp (mkCId "ExistS") [gf x1, gf x2, gf x3]
  gf (GExtAdvS x1 x2) = mkApp (mkCId "ExtAdvS") [gf x1, gf x2]
  gf (GUseCl x1 x2 x3) = mkApp (mkCId "UseCl") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvS" -> GAdvS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjS" -> GConjS (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "ExistS" -> GExistS (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "ExtAdvS" -> GExtAdvS (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "UseCl" -> GUseCl (fg x1) (fg x2) (fg x3)


      _ -> error ("no S " ++ show t)

instance Gf GSub10 where
  gf (Gpot0 x1) = mkApp (mkCId "pot0") [gf x1]
  gf Gpot01 = mkApp (mkCId "pot01") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "pot0" -> Gpot0 (fg x1)
      Just (i,[]) | i == mkCId "pot01" -> Gpot01


      _ -> error ("no Sub10 " ++ show t)

instance Gf GSub100 where
  gf (Gpot0as1 x1) = mkApp (mkCId "pot0as1") [gf x1]
  gf (Gpot1 x1) = mkApp (mkCId "pot1") [gf x1]
  gf Gpot110 = mkApp (mkCId "pot110") []
  gf Gpot111 = mkApp (mkCId "pot111") []
  gf (Gpot1plus x1 x2) = mkApp (mkCId "pot1plus") [gf x1, gf x2]
  gf (Gpot1to19 x1) = mkApp (mkCId "pot1to19") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "pot0as1" -> Gpot0as1 (fg x1)
      Just (i,[x1]) | i == mkCId "pot1" -> Gpot1 (fg x1)
      Just (i,[]) | i == mkCId "pot110" -> Gpot110
      Just (i,[]) | i == mkCId "pot111" -> Gpot111
      Just (i,[x1,x2]) | i == mkCId "pot1plus" -> Gpot1plus (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "pot1to19" -> Gpot1to19 (fg x1)


      _ -> error ("no Sub100 " ++ show t)

instance Gf GSub1000 where
  gf (Gpot1as2 x1) = mkApp (mkCId "pot1as2") [gf x1]
  gf (Gpot2 x1) = mkApp (mkCId "pot2") [gf x1]
  gf (Gpot2plus x1 x2) = mkApp (mkCId "pot2plus") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "pot1as2" -> Gpot1as2 (fg x1)
      Just (i,[x1]) | i == mkCId "pot2" -> Gpot2 (fg x1)
      Just (i,[x1,x2]) | i == mkCId "pot2plus" -> Gpot2plus (fg x1) (fg x2)


      _ -> error ("no Sub1000 " ++ show t)

instance Gf GSub1000000 where
  gf (Gpot2as3 x1) = mkApp (mkCId "pot2as3") [gf x1]
  gf (Gpot3 x1) = mkApp (mkCId "pot3") [gf x1]
  gf (Gpot3plus x1 x2) = mkApp (mkCId "pot3plus") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "pot2as3" -> Gpot2as3 (fg x1)
      Just (i,[x1]) | i == mkCId "pot3" -> Gpot3 (fg x1)
      Just (i,[x1,x2]) | i == mkCId "pot3plus" -> Gpot3plus (fg x1) (fg x2)


      _ -> error ("no Sub1000000 " ++ show t)

instance Gf GSubj where
  gf (LexSubj x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexSubj (showCId i)
      _ -> error ("no Subj " ++ show t)

instance Gf GTemp where
  gf (GTTAnt x1 x2) = mkApp (mkCId "TTAnt") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "TTAnt" -> GTTAnt (fg x1) (fg x2)


      _ -> error ("no Temp " ++ show t)

instance Gf GTense where
  gf GTCond = mkApp (mkCId "TCond") []
  gf GTFut = mkApp (mkCId "TFut") []
  gf GTPast = mkApp (mkCId "TPast") []
  gf GTPres = mkApp (mkCId "TPres") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "TCond" -> GTCond
      Just (i,[]) | i == mkCId "TFut" -> GTFut
      Just (i,[]) | i == mkCId "TPast" -> GTPast
      Just (i,[]) | i == mkCId "TPres" -> GTPres


      _ -> error ("no Tense " ++ show t)

instance Gf GUDFragment where
  gf (GAfter x1) = mkApp (mkCId "After") [gf x1]
  gf (GBefore x1) = mkApp (mkCId "Before") [gf x1]
  gf (GBy x1) = mkApp (mkCId "By") [gf x1]
  gf (GOn x1) = mkApp (mkCId "On") [gf x1]
  gf (GUpon x1) = mkApp (mkCId "Upon") [gf x1]
  gf (GVaguely x1) = mkApp (mkCId "Vaguely") [gf x1]
  gf (GsubjAction x1 x2) = mkApp (mkCId "subjAction") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "After" -> GAfter (fg x1)
      Just (i,[x1]) | i == mkCId "Before" -> GBefore (fg x1)
      Just (i,[x1]) | i == mkCId "By" -> GBy (fg x1)
      Just (i,[x1]) | i == mkCId "On" -> GOn (fg x1)
      Just (i,[x1]) | i == mkCId "Upon" -> GUpon (fg x1)
      Just (i,[x1]) | i == mkCId "Vaguely" -> GVaguely (fg x1)
      Just (i,[x1,x2]) | i == mkCId "subjAction" -> GsubjAction (fg x1) (fg x2)


      _ -> error ("no UDFragment " ++ show t)

instance Gf GUDS where
  gf (Groot_acl x1 x2) = mkApp (mkCId "root_acl") [gf x1, gf x2]
  gf (Groot_acl_nmod x1 x2 x3) = mkApp (mkCId "root_acl_nmod") [gf x1, gf x2, gf x3]
  gf (Groot_advcl x1 x2) = mkApp (mkCId "root_advcl") [gf x1, gf x2]
  gf (Groot_advmod x1 x2) = mkApp (mkCId "root_advmod") [gf x1, gf x2]
  gf (Groot_advmod_advmod_obl x1 x2 x3 x4) = mkApp (mkCId "root_advmod_advmod_obl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_advmod_amod x1 x2 x3) = mkApp (mkCId "root_advmod_amod") [gf x1, gf x2, gf x3]
  gf (Groot_advmod_nsubj_cop_obl x1 x2 x3 x4 x5) = mkApp (mkCId "root_advmod_nsubj_cop_obl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_amod x1 x2) = mkApp (mkCId "root_amod") [gf x1, gf x2]
  gf (Groot_amod_nmod x1 x2 x3) = mkApp (mkCId "root_amod_nmod") [gf x1, gf x2, gf x3]
  gf (Groot_appos x1 x2) = mkApp (mkCId "root_appos") [gf x1, gf x2]
  gf (Groot_appos_advmod x1 x2 x3) = mkApp (mkCId "root_appos_advmod") [gf x1, gf x2, gf x3]
  gf (Groot_auxPass x1 x2) = mkApp (mkCId "root_auxPass") [gf x1, gf x2]
  gf (Groot_case x1 x2) = mkApp (mkCId "root_case") [gf x1, gf x2]
  gf (Groot_case_amod x1 x2 x3) = mkApp (mkCId "root_case_amod") [gf x1, gf x2, gf x3]
  gf (Groot_case_amod_amod x1 x2 x3 x4) = mkApp (mkCId "root_case_amod_amod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_case_amod_conj_conj x1 x2 x3 x4 x5) = mkApp (mkCId "root_case_amod_conj_conj") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_case_compound x1 x2 x3) = mkApp (mkCId "root_case_compound") [gf x1, gf x2, gf x3]
  gf (Groot_case_det x1 x2 x3) = mkApp (mkCId "root_case_det") [gf x1, gf x2, gf x3]
  gf (Groot_case_det_amod x1 x2 x3 x4) = mkApp (mkCId "root_case_det_amod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_case_det_compound_conj x1 x2 x3 x4 x5) = mkApp (mkCId "root_case_det_compound_conj") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_case_det_nmod x1 x2 x3 x4) = mkApp (mkCId "root_case_det_nmod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_case_nummod x1 x2 x3) = mkApp (mkCId "root_case_nummod") [gf x1, gf x2, gf x3]
  gf (Groot_case_nummod_acl x1 x2 x3 x4) = mkApp (mkCId "root_case_nummod_acl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_case_nummod_nummod x1 x2 x3 x4) = mkApp (mkCId "root_case_nummod_nummod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_cc x1 x2) = mkApp (mkCId "root_cc") [gf x1, gf x2]
  gf (Groot_cc_aux_cop_det_nmod x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_cc_aux_cop_det_nmod") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_cc_conj x1 x2 x3) = mkApp (mkCId "root_cc_conj") [gf x1, gf x2, gf x3]
  gf (Groot_cc_cop_xcomp x1 x2 x3 x4) = mkApp (mkCId "root_cc_cop_xcomp") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_cc_det_nmod x1 x2 x3 x4) = mkApp (mkCId "root_cc_det_nmod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_cc_nmod x1 x2 x3) = mkApp (mkCId "root_cc_nmod") [gf x1, gf x2, gf x3]
  gf (Groot_cc_obj x1 x2 x3) = mkApp (mkCId "root_cc_obj") [gf x1, gf x2, gf x3]
  gf (Groot_ccomp x1 x2) = mkApp (mkCId "root_ccomp") [gf x1, gf x2]
  gf (Groot_compound x1 x2) = mkApp (mkCId "root_compound") [gf x1, gf x2]
  gf (Groot_compoundPrt_compoundPrt x1 x2 x3) = mkApp (mkCId "root_compoundPrt_compoundPrt") [gf x1, gf x2, gf x3]
  gf (Groot_compound_acl x1 x2 x3) = mkApp (mkCId "root_compound_acl") [gf x1, gf x2, gf x3]
  gf (Groot_compound_amod x1 x2 x3) = mkApp (mkCId "root_compound_amod") [gf x1, gf x2, gf x3]
  gf (Groot_compound_appos x1 x2 x3) = mkApp (mkCId "root_compound_appos") [gf x1, gf x2, gf x3]
  gf (Groot_compound_compound x1 x2 x3) = mkApp (mkCId "root_compound_compound") [gf x1, gf x2, gf x3]
  gf (Groot_compound_compound_appos x1 x2 x3 x4) = mkApp (mkCId "root_compound_compound_appos") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_compound_compound_conj x1 x2 x3 x4) = mkApp (mkCId "root_compound_compound_conj") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_compound_conj_acl x1 x2 x3 x4) = mkApp (mkCId "root_compound_conj_acl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_compound_flat x1 x2 x3) = mkApp (mkCId "root_compound_flat") [gf x1, gf x2, gf x3]
  gf (Groot_conj x1 x2) = mkApp (mkCId "root_conj") [gf x1, gf x2]
  gf (Groot_conj_acl x1 x2 x3) = mkApp (mkCId "root_conj_acl") [gf x1, gf x2, gf x3]
  gf (Groot_conj_appos x1 x2 x3) = mkApp (mkCId "root_conj_appos") [gf x1, gf x2, gf x3]
  gf (Groot_conj_case x1 x2 x3) = mkApp (mkCId "root_conj_case") [gf x1, gf x2, gf x3]
  gf (Groot_conj_nmod x1 x2 x3) = mkApp (mkCId "root_conj_nmod") [gf x1, gf x2, gf x3]
  gf (Groot_conj_parataxis x1 x2 x3) = mkApp (mkCId "root_conj_parataxis") [gf x1, gf x2, gf x3]
  gf (Groot_cop x1 x2) = mkApp (mkCId "root_cop") [gf x1, gf x2]
  gf (Groot_cop_advmod x1 x2 x3) = mkApp (mkCId "root_cop_advmod") [gf x1, gf x2, gf x3]
  gf (Groot_cop_conj_conj x1 x2 x3 x4) = mkApp (mkCId "root_cop_conj_conj") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_cop_det_compound_amod x1 x2 x3 x4 x5) = mkApp (mkCId "root_cop_det_compound_amod") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_cop_det_nmod x1 x2 x3 x4) = mkApp (mkCId "root_cop_det_nmod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_csubj x1 x2) = mkApp (mkCId "root_csubj") [gf x1, gf x2]
  gf (Groot_csubj_aux_aux x1 x2 x3 x4) = mkApp (mkCId "root_csubj_aux_aux") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_det x1 x2) = mkApp (mkCId "root_det") [gf x1, gf x2]
  gf (Groot_det_acl x1 x2 x3) = mkApp (mkCId "root_det_acl") [gf x1, gf x2, gf x3]
  gf (Groot_det_aclRelcl x1 x2 x3) = mkApp (mkCId "root_det_aclRelcl") [gf x1, gf x2, gf x3]
  gf (Groot_det_aclRelcl_nmod x1 x2 x3 x4) = mkApp (mkCId "root_det_aclRelcl_nmod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_det_advmod x1 x2 x3) = mkApp (mkCId "root_det_advmod") [gf x1, gf x2, gf x3]
  gf (Groot_det_amod x1 x2 x3) = mkApp (mkCId "root_det_amod") [gf x1, gf x2, gf x3]
  gf (Groot_det_amod_aclRelcl x1 x2 x3 x4) = mkApp (mkCId "root_det_amod_aclRelcl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_det_amod_aclRelcl_nmod x1 x2 x3 x4 x5) = mkApp (mkCId "root_det_amod_aclRelcl_nmod") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_det_amod_amod_acl_nmod x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_det_amod_amod_acl_nmod") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_det_amod_nmod x1 x2 x3 x4) = mkApp (mkCId "root_det_amod_nmod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_det_amod_obl x1 x2 x3 x4) = mkApp (mkCId "root_det_amod_obl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_det_case x1 x2 x3) = mkApp (mkCId "root_det_case") [gf x1, gf x2, gf x3]
  gf (Groot_det_compound x1 x2 x3) = mkApp (mkCId "root_det_compound") [gf x1, gf x2, gf x3]
  gf (Groot_det_compound_compound x1 x2 x3 x4) = mkApp (mkCId "root_det_compound_compound") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_det_compound_compound_nmod_appos x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_det_compound_compound_nmod_appos") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_det_conj_acl x1 x2 x3 x4) = mkApp (mkCId "root_det_conj_acl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_det_conj_nmod x1 x2 x3 x4) = mkApp (mkCId "root_det_conj_nmod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_det_conj_obj x1 x2 x3 x4) = mkApp (mkCId "root_det_conj_obj") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_det_nmod x1 x2 x3) = mkApp (mkCId "root_det_nmod") [gf x1, gf x2, gf x3]
  gf (Groot_det_nmodPoss x1 x2 x3) = mkApp (mkCId "root_det_nmodPoss") [gf x1, gf x2, gf x3]
  gf (Groot_det_nmodPoss_compound x1 x2 x3 x4) = mkApp (mkCId "root_det_nmodPoss_compound") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_discourse x1 x2) = mkApp (mkCId "root_discourse") [gf x1, gf x2]
  gf (Groot_fixed x1 x2) = mkApp (mkCId "root_fixed") [gf x1, gf x2]
  gf (Groot_goeswith x1 x2) = mkApp (mkCId "root_goeswith") [gf x1, gf x2]
  gf (Groot_goeswith_det_amod_nmod x1 x2 x3 x4 x5) = mkApp (mkCId "root_goeswith_det_amod_nmod") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_goeswith_goeswith x1 x2 x3) = mkApp (mkCId "root_goeswith_goeswith") [gf x1, gf x2, gf x3]
  gf (Groot_mark x1 x2) = mkApp (mkCId "root_mark") [gf x1, gf x2]
  gf (Groot_mark_case_det_nmod x1 x2 x3 x4 x5) = mkApp (mkCId "root_mark_case_det_nmod") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_mark_cc_mark_obj x1 x2 x3 x4 x5) = mkApp (mkCId "root_mark_cc_mark_obj") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_mark_det_obj x1 x2 x3 x4) = mkApp (mkCId "root_mark_det_obj") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_mark_expl_cop_xcomp x1 x2 x3 x4 x5) = mkApp (mkCId "root_mark_expl_cop_xcomp") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_mark_expl_nsubj x1 x2 x3 x4) = mkApp (mkCId "root_mark_expl_nsubj") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_mark_nsubj x1 x2 x3) = mkApp (mkCId "root_mark_nsubj") [gf x1, gf x2, gf x3]
  gf (Groot_mark_nsubjPass_auxPass_obl x1 x2 x3 x4 x5) = mkApp (mkCId "root_mark_nsubjPass_auxPass_obl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_mark_nsubj_aux_advmod_obj x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_mark_nsubj_aux_advmod_obj") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_mark_nsubj_aux_aux x1 x2 x3 x4 x5) = mkApp (mkCId "root_mark_nsubj_aux_aux") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_mark_nsubj_cop x1 x2 x3 x4) = mkApp (mkCId "root_mark_nsubj_cop") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_mark_nsubj_cop_case_det x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_mark_nsubj_cop_case_det") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_mark_nsubj_cop_det_amod_compound_conj x1 x2 x3 x4 x5 x6 x7 x8) = mkApp (mkCId "root_mark_nsubj_cop_det_amod_compound_conj") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6, gf x7, gf x8]
  gf (Groot_mark_nsubj_cop_det_case x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_mark_nsubj_cop_det_case") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_mark_nsubj_cop_det_compound_compound x1 x2 x3 x4 x5 x6 x7) = mkApp (mkCId "root_mark_nsubj_cop_det_compound_compound") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6, gf x7]
  gf (Groot_mark_nsubj_cop_obl x1 x2 x3 x4 x5) = mkApp (mkCId "root_mark_nsubj_cop_obl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_mark_nsubj_obj x1 x2 x3 x4) = mkApp (mkCId "root_mark_nsubj_obj") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_mark_nsubj_obl x1 x2 x3 x4) = mkApp (mkCId "root_mark_nsubj_obl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_mark_nummod x1 x2 x3) = mkApp (mkCId "root_mark_nummod") [gf x1, gf x2, gf x3]
  gf (Groot_nmod x1 x2) = mkApp (mkCId "root_nmod") [gf x1, gf x2]
  gf (Groot_nmodPoss_advmod x1 x2 x3) = mkApp (mkCId "root_nmodPoss_advmod") [gf x1, gf x2, gf x3]
  gf (Groot_nmodPoss_nmodPoss x1 x2 x3) = mkApp (mkCId "root_nmodPoss_nmodPoss") [gf x1, gf x2, gf x3]
  gf (Groot_nmod_acl x1 x2 x3) = mkApp (mkCId "root_nmod_acl") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj x1 x2) = mkApp (mkCId "root_nsubj") [gf x1, gf x2]
  gf (Groot_nsubjPass_auxPass x1 x2 x3) = mkApp (mkCId "root_nsubjPass_auxPass") [gf x1, gf x2, gf x3]
  gf (Groot_nsubjPass_auxPass_advmod_advcl x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubjPass_auxPass_advmod_advcl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubjPass_auxPass_advmod_xcomp x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubjPass_auxPass_advmod_xcomp") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubjPass_auxPass_xcomp x1 x2 x3 x4) = mkApp (mkCId "root_nsubjPass_auxPass_xcomp") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubjPass_aux_auxPass x1 x2 x3 x4) = mkApp (mkCId "root_nsubjPass_aux_auxPass") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubjPass_aux_auxPass_obl_advmod x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nsubjPass_aux_auxPass_obl_advmod") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nsubjPass_aux_auxPass_obl_conj x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nsubjPass_aux_auxPass_obl_conj") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nsubjPass_aux_auxPass_obl_obl_advcl x1 x2 x3 x4 x5 x6 x7) = mkApp (mkCId "root_nsubjPass_aux_auxPass_obl_obl_advcl") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6, gf x7]
  gf (Groot_nsubjPass_aux_auxPass_obl_obl_advmod x1 x2 x3 x4 x5 x6 x7) = mkApp (mkCId "root_nsubjPass_aux_auxPass_obl_obl_advmod") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6, gf x7]
  gf (Groot_nsubj_advmod x1 x2 x3) = mkApp (mkCId "root_nsubj_advmod") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj_advmod_case_det x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubj_advmod_case_det") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_advmod_obj x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_advmod_obj") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_aux x1 x2 x3) = mkApp (mkCId "root_nsubj_aux") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj_aux_aclRelcl x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_aux_aclRelcl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_aux_aclRelcl_obl x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubj_aux_aclRelcl_obl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_aux_advmod x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_aux_advmod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_aux_advmod_obj_advcl x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nsubj_aux_advmod_obj_advcl") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nsubj_aux_aux x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_aux_aux") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_aux_conj x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_aux_conj") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_aux_conj_obl x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubj_aux_conj_obl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_aux_obj x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_aux_obj") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_aux_obj_conj_conj x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nsubj_aux_obj_conj_conj") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nsubj_aux_obj_obl x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubj_aux_obj_obl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_aux_obj_obl_advmod_advcl x1 x2 x3 x4 x5 x6 x7) = mkApp (mkCId "root_nsubj_aux_obj_obl_advmod_advcl") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6, gf x7]
  gf (Groot_nsubj_aux_obj_obl_obl x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nsubj_aux_obj_obl_obl") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nsubj_aux_obl x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_aux_obl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_ccomp x1 x2 x3) = mkApp (mkCId "root_nsubj_ccomp") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj_conj x1 x2 x3) = mkApp (mkCId "root_nsubj_conj") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj_conj_obl x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_conj_obl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_cop x1 x2 x3) = mkApp (mkCId "root_nsubj_cop") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj_cop_aclRelcl x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_cop_aclRelcl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_cop_aclRelcl_obl x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubj_cop_aclRelcl_obl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_cop_advcl x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_cop_advcl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_cop_advmod x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_cop_advmod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_cop_case_nmod_acl x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nsubj_cop_case_nmod_acl") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nsubj_cop_cc_conj x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubj_cop_cc_conj") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_cop_det_amod_advcl x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nsubj_cop_det_amod_advcl") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nsubj_cop_det_amod_compound x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nsubj_cop_det_amod_compound") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nsubj_cop_det_compound x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubj_cop_det_compound") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_cop_det_compound_conj x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nsubj_cop_det_compound_conj") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nsubj_cop_det_conj x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubj_cop_det_conj") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_cop_det_nmod x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubj_cop_det_nmod") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_cop_nmod x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_cop_nmod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_cop_nmodPoss x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_cop_nmodPoss") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_cop_obl x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_cop_obl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_det x1 x2 x3) = mkApp (mkCId "root_nsubj_det") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj_det_nmod_nmod x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubj_det_nmod_nmod") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_obj x1 x2 x3) = mkApp (mkCId "root_nsubj_obj") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj_obj_xcomp x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_obj_xcomp") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_obl x1 x2 x3) = mkApp (mkCId "root_nsubj_obl") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj_xcomp x1 x2 x3) = mkApp (mkCId "root_nsubj_xcomp") [gf x1, gf x2, gf x3]
  gf (Groot_nummod x1 x2) = mkApp (mkCId "root_nummod") [gf x1, gf x2]
  gf (Groot_nummod_appos x1 x2 x3) = mkApp (mkCId "root_nummod_appos") [gf x1, gf x2, gf x3]
  gf (Groot_nummod_auxPass_cc_aux_auxPass_obl_obl x1 x2 x3 x4 x5 x6 x7 x8) = mkApp (mkCId "root_nummod_auxPass_cc_aux_auxPass_obl_obl") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6, gf x7, gf x8]
  gf (Groot_nummod_conj x1 x2 x3) = mkApp (mkCId "root_nummod_conj") [gf x1, gf x2, gf x3]
  gf (Groot_nummod_cop_cc_aux_cop_det_nmod x1 x2 x3 x4 x5 x6 x7 x8) = mkApp (mkCId "root_nummod_cop_cc_aux_cop_det_nmod") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6, gf x7, gf x8]
  gf (Groot_nummod_det_acl x1 x2 x3 x4) = mkApp (mkCId "root_nummod_det_acl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nummod_det_aclRelcl x1 x2 x3 x4) = mkApp (mkCId "root_nummod_det_aclRelcl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nummod_det_amod x1 x2 x3 x4) = mkApp (mkCId "root_nummod_det_amod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nummod_det_amod_conj_conj x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nummod_det_amod_conj_conj") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nummod_det_conj_nmod x1 x2 x3 x4 x5) = mkApp (mkCId "root_nummod_det_conj_nmod") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nummod_det_conj_nmod_cc x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nummod_det_conj_nmod_cc") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nummod_det_nmod x1 x2 x3 x4) = mkApp (mkCId "root_nummod_det_nmod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nummod_mark_obj x1 x2 x3 x4) = mkApp (mkCId "root_nummod_mark_obj") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nummod_mark_obj_cc x1 x2 x3 x4 x5) = mkApp (mkCId "root_nummod_mark_obj_cc") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nummod_nmod x1 x2 x3) = mkApp (mkCId "root_nummod_nmod") [gf x1, gf x2, gf x3]
  gf (Groot_nummod_nsubjPass_nsubjPass_auxPass_cc x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nummod_nsubjPass_nsubjPass_auxPass_cc") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nummod_obl x1 x2 x3) = mkApp (mkCId "root_nummod_obl") [gf x1, gf x2, gf x3]
  gf (Groot_nummod_obl_cc x1 x2 x3 x4) = mkApp (mkCId "root_nummod_obl_cc") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_obj x1 x2) = mkApp (mkCId "root_obj") [gf x1, gf x2]
  gf (Groot_obj_ccomp x1 x2 x3) = mkApp (mkCId "root_obj_ccomp") [gf x1, gf x2, gf x3]
  gf (Groot_obj_nmod x1 x2 x3) = mkApp (mkCId "root_obj_nmod") [gf x1, gf x2, gf x3]
  gf (Groot_obl x1 x2) = mkApp (mkCId "root_obl") [gf x1, gf x2]
  gf (Groot_obl_appos x1 x2 x3) = mkApp (mkCId "root_obl_appos") [gf x1, gf x2, gf x3]
  gf (Groot_obl_aux x1 x2 x3) = mkApp (mkCId "root_obl_aux") [gf x1, gf x2, gf x3]
  gf (Groot_obl_case x1 x2 x3) = mkApp (mkCId "root_obl_case") [gf x1, gf x2, gf x3]
  gf (Groot_obl_obj x1 x2 x3) = mkApp (mkCId "root_obl_obj") [gf x1, gf x2, gf x3]
  gf (Groot_obl_obl x1 x2 x3) = mkApp (mkCId "root_obl_obl") [gf x1, gf x2, gf x3]
  gf (Groot_obl_obl_obl_cc x1 x2 x3 x4) = mkApp (mkCId "root_obl_obl_obl_cc") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_obl_xcomp x1 x2 x3) = mkApp (mkCId "root_obl_xcomp") [gf x1, gf x2, gf x3]
  gf (Groot_only x1) = mkApp (mkCId "root_only") [gf x1]
  gf (Groot_parataxis x1 x2) = mkApp (mkCId "root_parataxis") [gf x1, gf x2]
  gf (Groot_xcomp_ccomp x1 x2 x3) = mkApp (mkCId "root_xcomp_ccomp") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "root_acl" -> Groot_acl (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_acl_nmod" -> Groot_acl_nmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_advcl" -> Groot_advcl (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "root_advmod" -> Groot_advmod (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_advmod_advmod_obl" -> Groot_advmod_advmod_obl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_advmod_amod" -> Groot_advmod_amod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_advmod_nsubj_cop_obl" -> Groot_advmod_nsubj_cop_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2]) | i == mkCId "root_amod" -> Groot_amod (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_amod_nmod" -> Groot_amod_nmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_appos" -> Groot_appos (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_appos_advmod" -> Groot_appos_advmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_auxPass" -> Groot_auxPass (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "root_case" -> Groot_case (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_case_amod" -> Groot_case_amod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_case_amod_amod" -> Groot_case_amod_amod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_case_amod_conj_conj" -> Groot_case_amod_conj_conj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3]) | i == mkCId "root_case_compound" -> Groot_case_compound (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_case_det" -> Groot_case_det (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_case_det_amod" -> Groot_case_det_amod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_case_det_compound_conj" -> Groot_case_det_compound_conj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_case_det_nmod" -> Groot_case_det_nmod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_case_nummod" -> Groot_case_nummod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_case_nummod_acl" -> Groot_case_nummod_acl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_case_nummod_nummod" -> Groot_case_nummod_nummod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "root_cc" -> Groot_cc (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_cc_aux_cop_det_nmod" -> Groot_cc_aux_cop_det_nmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3]) | i == mkCId "root_cc_conj" -> Groot_cc_conj (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_cc_cop_xcomp" -> Groot_cc_cop_xcomp (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_cc_det_nmod" -> Groot_cc_det_nmod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_cc_nmod" -> Groot_cc_nmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_cc_obj" -> Groot_cc_obj (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_ccomp" -> Groot_ccomp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "root_compound" -> Groot_compound (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_compoundPrt_compoundPrt" -> Groot_compoundPrt_compoundPrt (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_compound_acl" -> Groot_compound_acl (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_compound_amod" -> Groot_compound_amod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_compound_appos" -> Groot_compound_appos (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_compound_compound" -> Groot_compound_compound (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_compound_compound_appos" -> Groot_compound_compound_appos (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_compound_compound_conj" -> Groot_compound_compound_conj (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_compound_conj_acl" -> Groot_compound_conj_acl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_compound_flat" -> Groot_compound_flat (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_conj" -> Groot_conj (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_conj_acl" -> Groot_conj_acl (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_conj_appos" -> Groot_conj_appos (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_conj_case" -> Groot_conj_case (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_conj_nmod" -> Groot_conj_nmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_conj_parataxis" -> Groot_conj_parataxis (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_cop" -> Groot_cop (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_cop_advmod" -> Groot_cop_advmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_cop_conj_conj" -> Groot_cop_conj_conj (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_cop_det_compound_amod" -> Groot_cop_det_compound_amod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_cop_det_nmod" -> Groot_cop_det_nmod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "root_csubj" -> Groot_csubj (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_csubj_aux_aux" -> Groot_csubj_aux_aux (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "root_det" -> Groot_det (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_det_acl" -> Groot_det_acl (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_det_aclRelcl" -> Groot_det_aclRelcl (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_det_aclRelcl_nmod" -> Groot_det_aclRelcl_nmod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_det_advmod" -> Groot_det_advmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_det_amod" -> Groot_det_amod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_det_amod_aclRelcl" -> Groot_det_amod_aclRelcl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_det_amod_aclRelcl_nmod" -> Groot_det_amod_aclRelcl_nmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_det_amod_amod_acl_nmod" -> Groot_det_amod_amod_acl_nmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_det_amod_nmod" -> Groot_det_amod_nmod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_det_amod_obl" -> Groot_det_amod_obl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_det_case" -> Groot_det_case (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_det_compound" -> Groot_det_compound (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_det_compound_compound" -> Groot_det_compound_compound (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_det_compound_compound_nmod_appos" -> Groot_det_compound_compound_nmod_appos (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_det_conj_acl" -> Groot_det_conj_acl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_det_conj_nmod" -> Groot_det_conj_nmod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_det_conj_obj" -> Groot_det_conj_obj (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_det_nmod" -> Groot_det_nmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_det_nmodPoss" -> Groot_det_nmodPoss (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_det_nmodPoss_compound" -> Groot_det_nmodPoss_compound (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "root_discourse" -> Groot_discourse (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "root_fixed" -> Groot_fixed (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "root_goeswith" -> Groot_goeswith (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_goeswith_det_amod_nmod" -> Groot_goeswith_det_amod_nmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3]) | i == mkCId "root_goeswith_goeswith" -> Groot_goeswith_goeswith (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_mark" -> Groot_mark (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_mark_case_det_nmod" -> Groot_mark_case_det_nmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_mark_cc_mark_obj" -> Groot_mark_cc_mark_obj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_mark_det_obj" -> Groot_mark_det_obj (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_mark_expl_cop_xcomp" -> Groot_mark_expl_cop_xcomp (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_mark_expl_nsubj" -> Groot_mark_expl_nsubj (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_mark_nsubj" -> Groot_mark_nsubj (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_mark_nsubjPass_auxPass_obl" -> Groot_mark_nsubjPass_auxPass_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_mark_nsubj_aux_advmod_obj" -> Groot_mark_nsubj_aux_advmod_obj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_mark_nsubj_aux_aux" -> Groot_mark_nsubj_aux_aux (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_mark_nsubj_cop" -> Groot_mark_nsubj_cop (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_mark_nsubj_cop_case_det" -> Groot_mark_nsubj_cop_case_det (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5,x6,x7,x8]) | i == mkCId "root_mark_nsubj_cop_det_amod_compound_conj" -> Groot_mark_nsubj_cop_det_amod_compound_conj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6) (fg x7) (fg x8)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_mark_nsubj_cop_det_case" -> Groot_mark_nsubj_cop_det_case (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5,x6,x7]) | i == mkCId "root_mark_nsubj_cop_det_compound_compound" -> Groot_mark_nsubj_cop_det_compound_compound (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6) (fg x7)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_mark_nsubj_cop_obl" -> Groot_mark_nsubj_cop_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_mark_nsubj_obj" -> Groot_mark_nsubj_obj (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_mark_nsubj_obl" -> Groot_mark_nsubj_obl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_mark_nummod" -> Groot_mark_nummod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_nmod" -> Groot_nmod (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nmodPoss_advmod" -> Groot_nmodPoss_advmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nmodPoss_nmodPoss" -> Groot_nmodPoss_nmodPoss (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nmod_acl" -> Groot_nmod_acl (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_nsubj" -> Groot_nsubj (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubjPass_auxPass" -> Groot_nsubjPass_auxPass (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubjPass_auxPass_advmod_advcl" -> Groot_nsubjPass_auxPass_advmod_advcl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubjPass_auxPass_advmod_xcomp" -> Groot_nsubjPass_auxPass_advmod_xcomp (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubjPass_auxPass_xcomp" -> Groot_nsubjPass_auxPass_xcomp (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubjPass_aux_auxPass" -> Groot_nsubjPass_aux_auxPass (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nsubjPass_aux_auxPass_obl_advmod" -> Groot_nsubjPass_aux_auxPass_obl_advmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nsubjPass_aux_auxPass_obl_conj" -> Groot_nsubjPass_aux_auxPass_obl_conj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5,x6,x7]) | i == mkCId "root_nsubjPass_aux_auxPass_obl_obl_advcl" -> Groot_nsubjPass_aux_auxPass_obl_obl_advcl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6) (fg x7)
      Just (i,[x1,x2,x3,x4,x5,x6,x7]) | i == mkCId "root_nsubjPass_aux_auxPass_obl_obl_advmod" -> Groot_nsubjPass_aux_auxPass_obl_obl_advmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6) (fg x7)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_advmod" -> Groot_nsubj_advmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubj_advmod_case_det" -> Groot_nsubj_advmod_case_det (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_advmod_obj" -> Groot_nsubj_advmod_obj (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_aux" -> Groot_nsubj_aux (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_aux_aclRelcl" -> Groot_nsubj_aux_aclRelcl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubj_aux_aclRelcl_obl" -> Groot_nsubj_aux_aclRelcl_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_aux_advmod" -> Groot_nsubj_aux_advmod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nsubj_aux_advmod_obj_advcl" -> Groot_nsubj_aux_advmod_obj_advcl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_aux_aux" -> Groot_nsubj_aux_aux (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_aux_conj" -> Groot_nsubj_aux_conj (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubj_aux_conj_obl" -> Groot_nsubj_aux_conj_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_aux_obj" -> Groot_nsubj_aux_obj (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nsubj_aux_obj_conj_conj" -> Groot_nsubj_aux_obj_conj_conj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubj_aux_obj_obl" -> Groot_nsubj_aux_obj_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5,x6,x7]) | i == mkCId "root_nsubj_aux_obj_obl_advmod_advcl" -> Groot_nsubj_aux_obj_obl_advmod_advcl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6) (fg x7)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nsubj_aux_obj_obl_obl" -> Groot_nsubj_aux_obj_obl_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_aux_obl" -> Groot_nsubj_aux_obl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_ccomp" -> Groot_nsubj_ccomp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_conj" -> Groot_nsubj_conj (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_conj_obl" -> Groot_nsubj_conj_obl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_cop" -> Groot_nsubj_cop (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_cop_aclRelcl" -> Groot_nsubj_cop_aclRelcl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubj_cop_aclRelcl_obl" -> Groot_nsubj_cop_aclRelcl_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_cop_advcl" -> Groot_nsubj_cop_advcl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_cop_advmod" -> Groot_nsubj_cop_advmod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nsubj_cop_case_nmod_acl" -> Groot_nsubj_cop_case_nmod_acl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubj_cop_cc_conj" -> Groot_nsubj_cop_cc_conj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nsubj_cop_det_amod_advcl" -> Groot_nsubj_cop_det_amod_advcl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nsubj_cop_det_amod_compound" -> Groot_nsubj_cop_det_amod_compound (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubj_cop_det_compound" -> Groot_nsubj_cop_det_compound (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nsubj_cop_det_compound_conj" -> Groot_nsubj_cop_det_compound_conj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubj_cop_det_conj" -> Groot_nsubj_cop_det_conj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubj_cop_det_nmod" -> Groot_nsubj_cop_det_nmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_cop_nmod" -> Groot_nsubj_cop_nmod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_cop_nmodPoss" -> Groot_nsubj_cop_nmodPoss (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_cop_obl" -> Groot_nsubj_cop_obl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_det" -> Groot_nsubj_det (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubj_det_nmod_nmod" -> Groot_nsubj_det_nmod_nmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_obj" -> Groot_nsubj_obj (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_obj_xcomp" -> Groot_nsubj_obj_xcomp (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_obl" -> Groot_nsubj_obl (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_xcomp" -> Groot_nsubj_xcomp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_nummod" -> Groot_nummod (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nummod_appos" -> Groot_nummod_appos (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4,x5,x6,x7,x8]) | i == mkCId "root_nummod_auxPass_cc_aux_auxPass_obl_obl" -> Groot_nummod_auxPass_cc_aux_auxPass_obl_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6) (fg x7) (fg x8)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nummod_conj" -> Groot_nummod_conj (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4,x5,x6,x7,x8]) | i == mkCId "root_nummod_cop_cc_aux_cop_det_nmod" -> Groot_nummod_cop_cc_aux_cop_det_nmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6) (fg x7) (fg x8)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nummod_det_acl" -> Groot_nummod_det_acl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nummod_det_aclRelcl" -> Groot_nummod_det_aclRelcl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nummod_det_amod" -> Groot_nummod_det_amod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nummod_det_amod_conj_conj" -> Groot_nummod_det_amod_conj_conj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nummod_det_conj_nmod" -> Groot_nummod_det_conj_nmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nummod_det_conj_nmod_cc" -> Groot_nummod_det_conj_nmod_cc (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nummod_det_nmod" -> Groot_nummod_det_nmod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nummod_mark_obj" -> Groot_nummod_mark_obj (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nummod_mark_obj_cc" -> Groot_nummod_mark_obj_cc (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nummod_nmod" -> Groot_nummod_nmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nummod_nsubjPass_nsubjPass_auxPass_cc" -> Groot_nummod_nsubjPass_nsubjPass_auxPass_cc (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nummod_obl" -> Groot_nummod_obl (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nummod_obl_cc" -> Groot_nummod_obl_cc (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "root_obj" -> Groot_obj (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_obj_ccomp" -> Groot_obj_ccomp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_obj_nmod" -> Groot_obj_nmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_obl" -> Groot_obl (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_obl_appos" -> Groot_obl_appos (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_obl_aux" -> Groot_obl_aux (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_obl_case" -> Groot_obl_case (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_obl_obj" -> Groot_obl_obj (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_obl_obl" -> Groot_obl_obl (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_obl_obl_obl_cc" -> Groot_obl_obl_obl_cc (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_obl_xcomp" -> Groot_obl_xcomp (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "root_only" -> Groot_only (fg x1)
      Just (i,[x1,x2]) | i == mkCId "root_parataxis" -> Groot_parataxis (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_xcomp_ccomp" -> Groot_xcomp_ccomp (fg x1) (fg x2) (fg x3)


      _ -> error ("no UDS " ++ show t)

instance Gf GV where
  gf (LexV x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexV (showCId i)
      _ -> error ("no V " ++ show t)

instance Gf GVP where
  gf (GAdVVP x1 x2) = mkApp (mkCId "AdVVP") [gf x1, gf x2]
  gf (GAdvVP x1 x2) = mkApp (mkCId "AdvVP") [gf x1, gf x2]
  gf (GComplV x1 x2) = mkApp (mkCId "ComplV") [gf x1, gf x2]
  gf (GPassV x1) = mkApp (mkCId "PassV") [gf x1]
  gf (GPassVAgent x1 x2) = mkApp (mkCId "PassVAgent") [gf x1, gf x2]
  gf (GProgrVP x1) = mkApp (mkCId "ProgrVP") [gf x1]
  gf (GUseV x1) = mkApp (mkCId "UseV") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdVVP" -> GAdVVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AdvVP" -> GAdvVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplV" -> GComplV (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "PassV" -> GPassV (fg x1)
      Just (i,[x1,x2]) | i == mkCId "PassVAgent" -> GPassVAgent (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ProgrVP" -> GProgrVP (fg x1)
      Just (i,[x1]) | i == mkCId "UseV" -> GUseV (fg x1)


      _ -> error ("no VP " ++ show t)

instance Gf Gacl where
  gf (GaclUDS_ x1) = mkApp (mkCId "aclUDS_") [gf x1]
  gf (Gacl_ x1) = mkApp (mkCId "acl_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "aclUDS_" -> GaclUDS_ (fg x1)
      Just (i,[x1]) | i == mkCId "acl_" -> Gacl_ (fg x1)


      _ -> error ("no acl " ++ show t)

instance Gf GaclRelcl where
  gf (GaclRelclRS_ x1) = mkApp (mkCId "aclRelclRS_") [gf x1]
  gf (GaclRelclUDS_ x1) = mkApp (mkCId "aclRelclUDS_") [gf x1]
  gf (GpassRelcl_ x1 x2 x3) = mkApp (mkCId "passRelcl_") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "aclRelclRS_" -> GaclRelclRS_ (fg x1)
      Just (i,[x1]) | i == mkCId "aclRelclUDS_" -> GaclRelclUDS_ (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "passRelcl_" -> GpassRelcl_ (fg x1) (fg x2) (fg x3)


      _ -> error ("no aclRelcl " ++ show t)

instance Gf Gadvcl where
  gf (GadvclUDS_ x1) = mkApp (mkCId "advclUDS_") [gf x1]
  gf (Gadvcl_ x1) = mkApp (mkCId "advcl_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "advclUDS_" -> GadvclUDS_ (fg x1)
      Just (i,[x1]) | i == mkCId "advcl_" -> Gadvcl_ (fg x1)


      _ -> error ("no advcl " ++ show t)

instance Gf Gadvmod where
  gf (Gadvmod_ x1) = mkApp (mkCId "advmod_") [gf x1]
  gf Gnot_advmod = mkApp (mkCId "not_advmod") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "advmod_" -> Gadvmod_ (fg x1)
      Just (i,[]) | i == mkCId "not_advmod" -> Gnot_advmod


      _ -> error ("no advmod " ++ show t)

instance Gf GadvmodEmph where
  gf (GadvmodEmph_ x1) = mkApp (mkCId "advmodEmph_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "advmodEmph_" -> GadvmodEmph_ (fg x1)


      _ -> error ("no advmodEmph " ++ show t)

instance Gf GadvmodLmod where
  gf (GadvmodLmod_ x1) = mkApp (mkCId "advmodLmod_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "advmodLmod_" -> GadvmodLmod_ (fg x1)


      _ -> error ("no advmodLmod " ++ show t)

instance Gf Gamod where
  gf (Gamod_ x1) = mkApp (mkCId "amod_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "amod_" -> Gamod_ (fg x1)


      _ -> error ("no amod " ++ show t)

instance Gf Gappos where
  gf (Gappos_ x1) = mkApp (mkCId "appos_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "appos_" -> Gappos_ (fg x1)


      _ -> error ("no appos " ++ show t)

instance Gf Gaux where
  gf (Gaux_ x1) = mkApp (mkCId "aux_") [gf x1]
  gf Gbe_aux = mkApp (mkCId "be_aux") []
  gf Gcan_aux = mkApp (mkCId "can_aux") []
  gf Ghave_aux = mkApp (mkCId "have_aux") []
  gf Gmay_aux = mkApp (mkCId "may_aux") []
  gf Gmust_aux = mkApp (mkCId "must_aux") []
  gf Gshould_aux = mkApp (mkCId "should_aux") []
  gf Gwill_aux = mkApp (mkCId "will_aux") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "aux_" -> Gaux_ (fg x1)
      Just (i,[]) | i == mkCId "be_aux" -> Gbe_aux
      Just (i,[]) | i == mkCId "can_aux" -> Gcan_aux
      Just (i,[]) | i == mkCId "have_aux" -> Ghave_aux
      Just (i,[]) | i == mkCId "may_aux" -> Gmay_aux
      Just (i,[]) | i == mkCId "must_aux" -> Gmust_aux
      Just (i,[]) | i == mkCId "should_aux" -> Gshould_aux
      Just (i,[]) | i == mkCId "will_aux" -> Gwill_aux


      _ -> error ("no aux " ++ show t)

instance Gf GauxPass where
  gf Gbe_auxPass = mkApp (mkCId "be_auxPass") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "be_auxPass" -> Gbe_auxPass


      _ -> error ("no auxPass " ++ show t)

instance Gf Gcase_ where
  gf (Gcase__ x1) = mkApp (mkCId "case__") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "case__" -> Gcase__ (fg x1)


      _ -> error ("no case_ " ++ show t)

instance Gf Gcc where
  gf (Gcc_ x1) = mkApp (mkCId "cc_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "cc_" -> Gcc_ (fg x1)


      _ -> error ("no cc " ++ show t)

instance Gf GccPreconj where
  gf (GccPreconj_ x1) = mkApp (mkCId "ccPreconj_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "ccPreconj_" -> GccPreconj_ (fg x1)


      _ -> error ("no ccPreconj " ++ show t)

instance Gf Gccomp where
  gf (Gccomp_ x1) = mkApp (mkCId "ccomp_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "ccomp_" -> Gccomp_ (fg x1)


      _ -> error ("no ccomp " ++ show t)

instance Gf Gclf where
  gf (Gclf_ x1) = mkApp (mkCId "clf_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "clf_" -> Gclf_ (fg x1)


      _ -> error ("no clf " ++ show t)

instance Gf Gcompound where
  gf (Gcompound_ x1) = mkApp (mkCId "compound_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "compound_" -> Gcompound_ (fg x1)


      _ -> error ("no compound " ++ show t)

instance Gf GcompoundLvc where
  gf (GcompoundLvc_ x1) = mkApp (mkCId "compoundLvc_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "compoundLvc_" -> GcompoundLvc_ (fg x1)


      _ -> error ("no compoundLvc " ++ show t)

instance Gf GcompoundPrt where
  gf (GcompoundPrt_ x1) = mkApp (mkCId "compoundPrt_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "compoundPrt_" -> GcompoundPrt_ (fg x1)


      _ -> error ("no compoundPrt " ++ show t)

instance Gf GcompoundRedup where
  gf (GcompoundRedup_ x1) = mkApp (mkCId "compoundRedup_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "compoundRedup_" -> GcompoundRedup_ (fg x1)


      _ -> error ("no compoundRedup " ++ show t)

instance Gf GcompoundSvc where
  gf (GcompoundSvc_ x1) = mkApp (mkCId "compoundSvc_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "compoundSvc_" -> GcompoundSvc_ (fg x1)


      _ -> error ("no compoundSvc " ++ show t)

instance Gf Gconj where
  gf (GconjA_ x1) = mkApp (mkCId "conjA_") [gf x1]
  gf (GconjAdv_ x1) = mkApp (mkCId "conjAdv_") [gf x1]
  gf (GconjN_ x1) = mkApp (mkCId "conjN_") [gf x1]
  gf (Gconj_ x1) = mkApp (mkCId "conj_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "conjA_" -> GconjA_ (fg x1)
      Just (i,[x1]) | i == mkCId "conjAdv_" -> GconjAdv_ (fg x1)
      Just (i,[x1]) | i == mkCId "conjN_" -> GconjN_ (fg x1)
      Just (i,[x1]) | i == mkCId "conj_" -> Gconj_ (fg x1)


      _ -> error ("no conj " ++ show t)

instance Gf Gcop where
  gf Gbe_cop = mkApp (mkCId "be_cop") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "be_cop" -> Gbe_cop


      _ -> error ("no cop " ++ show t)

instance Gf Gcsubj where
  gf (Gcsubj_ x1) = mkApp (mkCId "csubj_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "csubj_" -> Gcsubj_ (fg x1)


      _ -> error ("no csubj " ++ show t)

instance Gf GcsubjPass where
  gf (GcsubjPass_ x1) = mkApp (mkCId "csubjPass_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "csubjPass_" -> GcsubjPass_ (fg x1)


      _ -> error ("no csubjPass " ++ show t)

instance Gf Gdep where
  gf (Gdep_ x1) = mkApp (mkCId "dep_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "dep_" -> Gdep_ (fg x1)


      _ -> error ("no dep " ++ show t)

instance Gf Gdet where
  gf (Gdet_ x1) = mkApp (mkCId "det_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "det_" -> Gdet_ (fg x1)


      _ -> error ("no det " ++ show t)

instance Gf GdetNumgov where
  gf (GdetNumgov_ x1) = mkApp (mkCId "detNumgov_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "detNumgov_" -> GdetNumgov_ (fg x1)


      _ -> error ("no detNumgov " ++ show t)

instance Gf GdetNummod where
  gf (GdetNummod_ x1) = mkApp (mkCId "detNummod_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "detNummod_" -> GdetNummod_ (fg x1)


      _ -> error ("no detNummod " ++ show t)

instance Gf GdetPoss where
  gf (GdetPoss_ x1) = mkApp (mkCId "detPoss_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "detPoss_" -> GdetPoss_ (fg x1)


      _ -> error ("no detPoss " ++ show t)

instance Gf Gdiscourse where
  gf (Gdiscourse_ x1) = mkApp (mkCId "discourse_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "discourse_" -> Gdiscourse_ (fg x1)


      _ -> error ("no discourse " ++ show t)

instance Gf Gdislocated where
  gf (Gdislocated_ x1) = mkApp (mkCId "dislocated_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "dislocated_" -> Gdislocated_ (fg x1)


      _ -> error ("no dislocated " ++ show t)

instance Gf Gexpl where
  gf (Gexpl_ x1) = mkApp (mkCId "expl_") [gf x1]
  gf Git_expl = mkApp (mkCId "it_expl") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "expl_" -> Gexpl_ (fg x1)
      Just (i,[]) | i == mkCId "it_expl" -> Git_expl


      _ -> error ("no expl " ++ show t)

instance Gf GexplImpers where
  gf (GexplImpers_ x1) = mkApp (mkCId "explImpers_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "explImpers_" -> GexplImpers_ (fg x1)


      _ -> error ("no explImpers " ++ show t)

instance Gf GexplPass where
  gf (GexplPass_ x1) = mkApp (mkCId "explPass_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "explPass_" -> GexplPass_ (fg x1)


      _ -> error ("no explPass " ++ show t)

instance Gf GexplPv where
  gf (GexplPv_ x1) = mkApp (mkCId "explPv_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "explPv_" -> GexplPv_ (fg x1)


      _ -> error ("no explPv " ++ show t)

instance Gf Gfixed where
  gf (Gfixed_ x1) = mkApp (mkCId "fixed_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "fixed_" -> Gfixed_ (fg x1)


      _ -> error ("no fixed " ++ show t)

instance Gf Gflat where
  gf (Gflat_ x1) = mkApp (mkCId "flat_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "flat_" -> Gflat_ (fg x1)


      _ -> error ("no flat " ++ show t)

instance Gf GflatForeign where
  gf (GflatForeign_ x1) = mkApp (mkCId "flatForeign_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "flatForeign_" -> GflatForeign_ (fg x1)


      _ -> error ("no flatForeign " ++ show t)

instance Gf GflatName where
  gf (GflatName_ x1) = mkApp (mkCId "flatName_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "flatName_" -> GflatName_ (fg x1)


      _ -> error ("no flatName " ++ show t)

instance Gf Ggoeswith where
  gf (Ggoeswith_ x1) = mkApp (mkCId "goeswith_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "goeswith_" -> Ggoeswith_ (fg x1)


      _ -> error ("no goeswith " ++ show t)

instance Gf Giobj where
  gf (Giobj_ x1) = mkApp (mkCId "iobj_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "iobj_" -> Giobj_ (fg x1)


      _ -> error ("no iobj " ++ show t)

instance Gf Glist where
  gf (Glist_ x1) = mkApp (mkCId "list_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "list_" -> Glist_ (fg x1)


      _ -> error ("no list " ++ show t)

instance Gf Gmark where
  gf (Gmark_ x1) = mkApp (mkCId "mark_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "mark_" -> Gmark_ (fg x1)


      _ -> error ("no mark " ++ show t)

instance Gf Gnmod where
  gf (Gnmod_ x1 x2) = mkApp (mkCId "nmod_") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "nmod_" -> Gnmod_ (fg x1) (fg x2)


      _ -> error ("no nmod " ++ show t)

instance Gf GnmodPoss where
  gf (GnmodPoss_ x1) = mkApp (mkCId "nmodPoss_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "nmodPoss_" -> GnmodPoss_ (fg x1)


      _ -> error ("no nmodPoss " ++ show t)

instance Gf GnmodTmod where
  gf (GnmodTmod_ x1) = mkApp (mkCId "nmodTmod_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "nmodTmod_" -> GnmodTmod_ (fg x1)


      _ -> error ("no nmodTmod " ++ show t)

instance Gf Gnsubj where
  gf (Gnsubj_ x1) = mkApp (mkCId "nsubj_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "nsubj_" -> Gnsubj_ (fg x1)


      _ -> error ("no nsubj " ++ show t)

instance Gf GnsubjPass where
  gf (GnsubjPass_ x1) = mkApp (mkCId "nsubjPass_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "nsubjPass_" -> GnsubjPass_ (fg x1)


      _ -> error ("no nsubjPass " ++ show t)

instance Gf Gnummod where
  gf (Gnummod_ x1) = mkApp (mkCId "nummod_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "nummod_" -> Gnummod_ (fg x1)


      _ -> error ("no nummod " ++ show t)

instance Gf GnummodGov where
  gf (GnummodGov_ x1) = mkApp (mkCId "nummodGov_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "nummodGov_" -> GnummodGov_ (fg x1)


      _ -> error ("no nummodGov " ++ show t)

instance Gf Gobj where
  gf (Gobj_ x1) = mkApp (mkCId "obj_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "obj_" -> Gobj_ (fg x1)


      _ -> error ("no obj " ++ show t)

instance Gf Gobl where
  gf (GoblPrep_ x1) = mkApp (mkCId "oblPrep_") [gf x1]
  gf (Gobl_ x1) = mkApp (mkCId "obl_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "oblPrep_" -> GoblPrep_ (fg x1)
      Just (i,[x1]) | i == mkCId "obl_" -> Gobl_ (fg x1)


      _ -> error ("no obl " ++ show t)

instance Gf GoblAgent where
  gf (GoblAgent_ x1) = mkApp (mkCId "oblAgent_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "oblAgent_" -> GoblAgent_ (fg x1)


      _ -> error ("no oblAgent " ++ show t)

instance Gf GoblArg where
  gf (GoblArg_ x1) = mkApp (mkCId "oblArg_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "oblArg_" -> GoblArg_ (fg x1)


      _ -> error ("no oblArg " ++ show t)

instance Gf GoblLmod where
  gf (GoblLmod_ x1) = mkApp (mkCId "oblLmod_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "oblLmod_" -> GoblLmod_ (fg x1)


      _ -> error ("no oblLmod " ++ show t)

instance Gf GoblTmod where
  gf (GoblTmod_ x1) = mkApp (mkCId "oblTmod_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "oblTmod_" -> GoblTmod_ (fg x1)


      _ -> error ("no oblTmod " ++ show t)

instance Gf Gorphan where
  gf (Gorphan_ x1) = mkApp (mkCId "orphan_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "orphan_" -> Gorphan_ (fg x1)


      _ -> error ("no orphan " ++ show t)

instance Gf Gparataxis where
  gf (Gparataxis_ x1) = mkApp (mkCId "parataxis_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "parataxis_" -> Gparataxis_ (fg x1)


      _ -> error ("no parataxis " ++ show t)

instance Gf Gpunct where
  gf (Gpunct_ x1) = mkApp (mkCId "punct_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "punct_" -> Gpunct_ (fg x1)


      _ -> error ("no punct " ++ show t)

instance Gf Greparandum where
  gf (Greparandum_ x1) = mkApp (mkCId "reparandum_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "reparandum_" -> Greparandum_ (fg x1)


      _ -> error ("no reparandum " ++ show t)

instance Gf Groot where
  gf (GrootA_ x1) = mkApp (mkCId "rootA_") [gf x1]
  gf (GrootAdv_ x1) = mkApp (mkCId "rootAdv_") [gf x1]
  gf (GrootN_ x1) = mkApp (mkCId "rootN_") [gf x1]
  gf (GrootV_ x1) = mkApp (mkCId "rootV_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "rootA_" -> GrootA_ (fg x1)
      Just (i,[x1]) | i == mkCId "rootAdv_" -> GrootAdv_ (fg x1)
      Just (i,[x1]) | i == mkCId "rootN_" -> GrootN_ (fg x1)
      Just (i,[x1]) | i == mkCId "rootV_" -> GrootV_ (fg x1)


      _ -> error ("no root " ++ show t)

instance Gf Gvocative where
  gf (Gvocative_ x1) = mkApp (mkCId "vocative_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "vocative_" -> Gvocative_ (fg x1)


      _ -> error ("no vocative " ++ show t)

instance Gf Gxcomp where
  gf (GxcompA_ x1) = mkApp (mkCId "xcompA_") [gf x1]
  gf (GxcompAdv_ x1) = mkApp (mkCId "xcompAdv_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "xcompA_" -> GxcompA_ (fg x1)
      Just (i,[x1]) | i == mkCId "xcompAdv_" -> GxcompAdv_ (fg x1)


      _ -> error ("no xcomp " ++ show t)

instance Show GA2

instance Gf GA2 where
  gf _ = undefined
  fg _ = undefined



instance Show GCl

instance Gf GCl where
  gf _ = undefined
  fg _ = undefined



instance Show GClSlash

instance Gf GClSlash where
  gf _ = undefined
  fg _ = undefined



instance Show GComp

instance Gf GComp where
  gf _ = undefined
  fg _ = undefined



instance Show GN3

instance Gf GN3 where
  gf _ = undefined
  fg _ = undefined



instance Show GPhr

instance Gf GPhr where
  gf _ = undefined
  fg _ = undefined



instance Show GQS

instance Gf GQS where
  gf _ = undefined
  fg _ = undefined



instance Show GSC

instance Gf GSC where
  gf _ = undefined
  fg _ = undefined



instance Show GSSlash

instance Gf GSSlash where
  gf _ = undefined
  fg _ = undefined



instance Show GText

instance Gf GText where
  gf _ = undefined
  fg _ = undefined



instance Show GUtt

instance Gf GUtt where
  gf _ = undefined
  fg _ = undefined



instance Show GV2

instance Gf GV2 where
  gf _ = undefined
  fg _ = undefined



instance Show GV2A

instance Gf GV2A where
  gf _ = undefined
  fg _ = undefined



instance Show GV2Q

instance Gf GV2Q where
  gf _ = undefined
  fg _ = undefined



instance Show GV2S

instance Gf GV2S where
  gf _ = undefined
  fg _ = undefined



instance Show GV2V

instance Gf GV2V where
  gf _ = undefined
  fg _ = undefined



instance Show GV3

instance Gf GV3 where
  gf _ = undefined
  fg _ = undefined



instance Show GVA

instance Gf GVA where
  gf _ = undefined
  fg _ = undefined



instance Show GVPSlash

instance Gf GVPSlash where
  gf _ = undefined
  fg _ = undefined



instance Show GVQ

instance Gf GVQ where
  gf _ = undefined
  fg _ = undefined



instance Show GVS

instance Gf GVS where
  gf _ = undefined
  fg _ = undefined



instance Show GVV

instance Gf GVV where
  gf _ = undefined
  fg _ = undefined



instance Show GVoc

instance Gf GVoc where
  gf _ = undefined
  fg _ = undefined



instance Show GX

instance Gf GX where
  gf _ = undefined
  fg _ = undefined




