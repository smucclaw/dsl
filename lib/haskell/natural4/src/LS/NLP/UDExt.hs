{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-all #-}
module LS.NLP.UDExt where

import Control.Monad.Identity
import Data.Monoid
import PGF hiding (Tree)

----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

type GA = Tree GA_
data GA_
type GA2 = Tree GA2_
data GA2_
type GACard = Tree GACard_
data GACard_
type GAP = Tree GAP_
data GAP_
type GAdA = Tree GAdA_
data GAdA_
type GAdN = Tree GAdN_
data GAdN_
type GAdV = Tree GAdV_
data GAdV_
type GAdv = Tree GAdv_
data GAdv_
type GAnt = Tree GAnt_
data GAnt_
type GCAdv = Tree GCAdv_
data GCAdv_
type GCN = Tree GCN_
data GCN_
type GCard = Tree GCard_
data GCard_
type GCl = Tree GCl_
data GCl_
type GClSlash = Tree GClSlash_
data GClSlash_
type GComp = Tree GComp_
data GComp_
type GConj = Tree GConj_
data GConj_
type GDAP = Tree GDAP_
data GDAP_
type GDet = Tree GDet_
data GDet_
type GDig = Tree GDig_
data GDig_
type GDigit = Tree GDigit_
data GDigit_
type GDigits = Tree GDigits_
data GDigits_
type GGen = Tree GGen_
data GGen_
type GIAdv = Tree GIAdv_
data GIAdv_
type GIComp = Tree GIComp_
data GIComp_
type GIDet = Tree GIDet_
data GIDet_
type GIP = Tree GIP_
data GIP_
type GIQuant = Tree GIQuant_
data GIQuant_
type GImp = Tree GImp_
data GImp_
type GInterj = Tree GInterj_
data GInterj_
type GListAP = Tree GListAP_
data GListAP_
type GListAdV = Tree GListAdV_
data GListAdV_
type GListAdv = Tree GListAdv_
data GListAdv_
type GListCN = Tree GListCN_
data GListCN_
type GListDAP = Tree GListDAP_
data GListDAP_
type GListIAdv = Tree GListIAdv_
data GListIAdv_
type GListNP = Tree GListNP_
data GListNP_
type GListPrep = Tree GListPrep_
data GListPrep_
type GListRS = Tree GListRS_
data GListRS_
type GListS = Tree GListS_
data GListS_
type GListUDFragment = Tree GListUDFragment_
data GListUDFragment_
type GListUDS = Tree GListUDS_
data GListUDS_
type GListVP = Tree GListVP_
data GListVP_
type GN = Tree GN_
data GN_
type GN2 = Tree GN2_
data GN2_
type GN3 = Tree GN3_
data GN3_
type GNP = Tree GNP_
data GNP_
type GNum = Tree GNum_
data GNum_
type GNumeral = Tree GNumeral_
data GNumeral_
type GOrd = Tree GOrd_
data GOrd_
type GPConj = Tree GPConj_
data GPConj_
type GPN = Tree GPN_
data GPN_
type GPol = Tree GPol_
data GPol_
type GPredet = Tree GPredet_
data GPredet_
type GPrep = Tree GPrep_
data GPrep_
type GPron = Tree GPron_
data GPron_
type GQCl = Tree GQCl_
data GQCl_
type GQS = Tree GQS_
data GQS_
type GQVP = Tree GQVP_
data GQVP_
type GQuant = Tree GQuant_
data GQuant_
type GRCl = Tree GRCl_
data GRCl_
type GRP = Tree GRP_
data GRP_
type GRS = Tree GRS_
data GRS_
type GS = Tree GS_
data GS_
type GSC = Tree GSC_
data GSC_
type GSSlash = Tree GSSlash_
data GSSlash_
type GSub10 = Tree GSub10_
data GSub10_
type GSub100 = Tree GSub100_
data GSub100_
type GSub1000 = Tree GSub1000_
data GSub1000_
type GSub1000000 = Tree GSub1000000_
data GSub1000000_
type GSubj = Tree GSubj_
data GSubj_
type GSymb = Tree GSymb_
data GSymb_
type GTemp = Tree GTemp_
data GTemp_
type GTense = Tree GTense_
data GTense_
type GUDFragment = Tree GUDFragment_
data GUDFragment_
type GUDS = Tree GUDS_
data GUDS_
type GV = Tree GV_
data GV_
type GVP = Tree GVP_
data GVP_
type Gacl = Tree Gacl_
data Gacl_
type GaclRelcl = Tree GaclRelcl_
data GaclRelcl_
type Gadvcl = Tree Gadvcl_
data Gadvcl_
type Gadvmod = Tree Gadvmod_
data Gadvmod_
type GadvmodEmph = Tree GadvmodEmph_
data GadvmodEmph_
type GadvmodLmod = Tree GadvmodLmod_
data GadvmodLmod_
type Gamod = Tree Gamod_
data Gamod_
type Gappos = Tree Gappos_
data Gappos_
type Gaux = Tree Gaux_
data Gaux_
type GauxPass = Tree GauxPass_
data GauxPass_
type Gcc = Tree Gcc_
data Gcc_
type GccPreconj = Tree GccPreconj_
data GccPreconj_
type Gccomp = Tree Gccomp_
data Gccomp_
type Gclf = Tree Gclf_
data Gclf_
type Gcompound = Tree Gcompound_
data Gcompound_
type GcompoundLvc = Tree GcompoundLvc_
data GcompoundLvc_
type GcompoundPrt = Tree GcompoundPrt_
data GcompoundPrt_
type GcompoundRedup = Tree GcompoundRedup_
data GcompoundRedup_
type GcompoundSvc = Tree GcompoundSvc_
data GcompoundSvc_
type Gcop = Tree Gcop_
data Gcop_
type Gcsubj = Tree Gcsubj_
data Gcsubj_
type GcsubjPass = Tree GcsubjPass_
data GcsubjPass_
type Gdep = Tree Gdep_
data Gdep_
type Gdet = Tree Gdet_
data Gdet_
type GdetNumgov = Tree GdetNumgov_
data GdetNumgov_
type GdetNummod = Tree GdetNummod_
data GdetNummod_
type GdetPoss = Tree GdetPoss_
data GdetPoss_
type Gdiscourse = Tree Gdiscourse_
data Gdiscourse_
type Gdislocated = Tree Gdislocated_
data Gdislocated_
type Gexpl = Tree Gexpl_
data Gexpl_
type GexplImpers = Tree GexplImpers_
data GexplImpers_
type GexplPass = Tree GexplPass_
data GexplPass_
type GexplPv = Tree GexplPv_
data GexplPv_
type Gfixed = Tree Gfixed_
data Gfixed_
type Gflat = Tree Gflat_
data Gflat_
type GflatForeign = Tree GflatForeign_
data GflatForeign_
type GflatName = Tree GflatName_
data GflatName_
type Ggoeswith = Tree Ggoeswith_
data Ggoeswith_
type Giobj = Tree Giobj_
data Giobj_
type Glist = Tree Glist_
data Glist_
type Gmark = Tree Gmark_
data Gmark_
type Gnmod = Tree Gnmod_
data Gnmod_
type GnmodPoss = Tree GnmodPoss_
data GnmodPoss_
type GnmodTmod = Tree GnmodTmod_
data GnmodTmod_
type Gnsubj = Tree Gnsubj_
data Gnsubj_
type GnsubjPass = Tree GnsubjPass_
data GnsubjPass_
type Gnummod = Tree Gnummod_
data Gnummod_
type GnummodGov = Tree GnummodGov_
data GnummodGov_
type Gobj = Tree Gobj_
data Gobj_
type Gobl = Tree Gobl_
data Gobl_
type GoblAgent = Tree GoblAgent_
data GoblAgent_
type GoblArg = Tree GoblArg_
data GoblArg_
type GoblLmod = Tree GoblLmod_
data GoblLmod_
type GoblTmod = Tree GoblTmod_
data GoblTmod_
type Gorphan = Tree Gorphan_
data Gorphan_
type Gparataxis = Tree Gparataxis_
data Gparataxis_
type Gpunct = Tree Gpunct_
data Gpunct_
type Greparandum = Tree Greparandum_
data Greparandum_
type Groot = Tree Groot_
data Groot_
type Gvocative = Tree Gvocative_
data Gvocative_
type Gxcomp = Tree Gxcomp_
data Gxcomp_
type GPhr = Tree GPhr_
data GPhr_
type GText = Tree GText_
data GText_
type GUtt = Tree GUtt_
data GUtt_
type GV2 = Tree GV2_
data GV2_
type GV2A = Tree GV2A_
data GV2A_
type GV2Q = Tree GV2Q_
data GV2Q_
type GV2S = Tree GV2S_
data GV2S_
type GV2V = Tree GV2V_
data GV2V_
type GV3 = Tree GV3_
data GV3_
type GVA = Tree GVA_
data GVA_
type GVPSlash = Tree GVPSlash_
data GVPSlash_
type GVQ = Tree GVQ_
data GVQ_
type GVS = Tree GVS_
data GVS_
type GVV = Tree GVV_
data GVV_
type GVoc = Tree GVoc_
data GVoc_
type GX = Tree GX_
data GX_
type Gcase_ = Tree Gcase__
data Gcase__
type GString = Tree GString_
data GString_
type GInt = Tree GInt_
data GInt_
type GFloat = Tree GFloat_
data GFloat_

data Tree :: * -> * where
  GStrA :: GString -> Tree GA_
  LexA :: String -> Tree GA_
  GMkA2 :: GA -> GPrep -> Tree GA2_
  LexA2 :: String -> Tree GA2_
  LexACard :: String -> Tree GACard_
  GAdAP :: GAdA -> GAP -> Tree GAP_
  GAdjOrd :: GOrd -> Tree GAP_
  GAdvAP :: GAP -> GAdv -> Tree GAP_
  GComplA2 :: GA2 -> GNP -> Tree GAP_
  GConjAP :: GConj -> GListAP -> Tree GAP_
  GParentheticalAP :: GAP -> Tree GAP_
  GPastPartAP :: GVP -> Tree GAP_
  GPastPartAgentAP :: GVP -> GNP -> Tree GAP_
  GPositA :: GA -> Tree GAP_
  GPresPartAP :: GVP -> Tree GAP_
  GStrAP :: GString -> Tree GAP_
  GUseComparA :: GA -> Tree GAP_
  LexAdA :: String -> Tree GAdA_
  GAdnCAdv :: GCAdv -> Tree GAdN_
  LexAdN :: String -> Tree GAdN_
  GConjAdV :: GConj -> GListAdV -> Tree GAdV_
  LexAdV :: String -> Tree GAdV_
  GAdvAdv :: GAdv -> GAdv -> Tree GAdv_
  GComparAdvAdj :: GCAdv -> GA -> GNP -> Tree GAdv_
  GComparAdvAdjS :: GCAdv -> GA -> GS -> Tree GAdv_
  GConjAdv :: GConj -> GListAdv -> Tree GAdv_
  GGerundAdv :: GVP -> Tree GAdv_
  GPositAdvAdj :: GA -> Tree GAdv_
  GPrepNP :: GPrep -> GNP -> Tree GAdv_
  GSubjS :: GSubj -> GS -> Tree GAdv_
  Gacl2Adv :: Gacl -> Tree GAdv_
  Gadvcl2Adv :: Gadvcl -> Tree GAdv_
  Gcsubj2Adv :: Gcsubj -> Tree GAdv_
  Gxcomp2Adv :: Gxcomp -> Tree GAdv_
  LexAdv :: String -> Tree GAdv_
  GAAnter :: Tree GAnt_
  GASimul :: Tree GAnt_
  LexCAdv :: String -> Tree GCAdv_
  GAdjCN :: GAP -> GCN -> Tree GCN_
  GAdvCN :: GCN -> GAdv -> Tree GCN_
  GApposCN :: GCN -> GNP -> Tree GCN_
  GCN_AP_Conj_CNs_of_NP :: GAP -> GConj -> GListCN -> GNP -> Tree GCN_
  GComplN2 :: GN2 -> GNP -> Tree GCN_
  GConjCN :: GConj -> GListCN -> Tree GCN_
  GGerundCN :: GVP -> Tree GCN_
  GPossNP :: GCN -> GNP -> Tree GCN_
  GRelCN :: GCN -> GRS -> Tree GCN_
  GSentCN :: GCN -> GSC -> Tree GCN_
  GUseN :: GN -> Tree GCN_
  Gday_CN :: Tree GCN_
  Ghigher_CN :: Tree GCN_
  Gleave_CN :: Tree GCN_
  Gtricyclic_CN :: Tree GCN_
  GAdNum :: GAdN -> GCard -> Tree GCard_
  GNumDigits :: GDigits -> Tree GCard_
  GNumNumeral :: GNumeral -> Tree GCard_
  GStrCard :: GString -> Tree GCard_
  LexCard :: String -> Tree GCard_
  GExistCN :: GCN -> Tree GCl_
  GExistsNP :: GNP -> Tree GCl_
  GGenericCl :: GVP -> Tree GCl_
  GImpersCl :: GVP -> Tree GCl_
  GPredSCVP :: GSC -> GVP -> Tree GCl_
  GPredVP :: GNP -> GVP -> Tree GCl_
  GAdvSlash :: GClSlash -> GAdv -> Tree GClSlash_
  GSlashCl :: GCl -> Tree GClSlash_
  GSlashPrep :: GCl -> GPrep -> Tree GClSlash_
  GSlashVP :: GNP -> GVPSlash -> Tree GClSlash_
  GSlashVS :: GNP -> GVS -> GSSlash -> Tree GClSlash_
  GCompAP :: GAP -> Tree GComp_
  GCompAdv :: GAdv -> Tree GComp_
  GCompNP :: GNP -> Tree GComp_
  LexConj :: String -> Tree GConj_
  GAdjDAP :: GDAP -> GAP -> Tree GDAP_
  GDetDAP :: GDet -> Tree GDAP_
  GACard2Det :: GACard -> Tree GDet_
  GConjDet :: GConj -> GListDAP -> Tree GDet_
  GDetQuant :: GQuant -> GNum -> Tree GDet_
  GDetQuantOrd :: GQuant -> GNum -> GOrd -> Tree GDet_
  GXorMore :: GCard -> Tree GDet_
  LexDet :: String -> Tree GDet_
  GD_0 :: Tree GDig_
  GD_1 :: Tree GDig_
  GD_2 :: Tree GDig_
  GD_3 :: Tree GDig_
  GD_4 :: Tree GDig_
  GD_5 :: Tree GDig_
  GD_6 :: Tree GDig_
  GD_7 :: Tree GDig_
  GD_8 :: Tree GDig_
  GD_9 :: Tree GDig_
  Gn2 :: Tree GDigit_
  Gn3 :: Tree GDigit_
  Gn4 :: Tree GDigit_
  Gn5 :: Tree GDigit_
  Gn6 :: Tree GDigit_
  Gn7 :: Tree GDigit_
  Gn8 :: Tree GDigit_
  Gn9 :: Tree GDigit_
  GIDig :: GDig -> Tree GDigits_
  GIIDig :: GDig -> GDigits -> Tree GDigits_
  Gs_Gen :: Tree GGen_
  GAdvIAdv :: GIAdv -> GAdv -> Tree GIAdv_
  GConjIAdv :: GConj -> GListIAdv -> Tree GIAdv_
  GIAdvAdv :: GAdv -> Tree GIAdv_
  GPrepIP :: GPrep -> GIP -> Tree GIAdv_
  Ghow_IAdv :: Tree GIAdv_
  Gwhen_IAdv :: Tree GIAdv_
  Gwhere_IAdv :: Tree GIAdv_
  Gwherein_IAdv :: Tree GIAdv_
  Gwhy_IAdv :: Tree GIAdv_
  GCompIAdv :: GIAdv -> Tree GIComp_
  GCompIP :: GIP -> Tree GIComp_
  GICompAP :: GAP -> Tree GIComp_
  GIdetQuant :: GIQuant -> GNum -> Tree GIDet_
  GAdvIP :: GIP -> GAdv -> Tree GIP_
  GIdetCN :: GIDet -> GCN -> Tree GIP_
  GIdetIP :: GIDet -> Tree GIP_
  Gwhat_IP :: Tree GIP_
  Gwho_IP :: Tree GIP_
  Gwhich_IQuant :: Tree GIQuant_
  GAdvImp :: GAdv -> GImp -> Tree GImp_
  GImpVP :: GVP -> Tree GImp_
  LexInterj :: String -> Tree GInterj_
  GListAP :: [GAP] -> Tree GListAP_
  GListAdV :: [GAdV] -> Tree GListAdV_
  GListAdv :: [GAdv] -> Tree GListAdv_
  GListCN :: [GCN] -> Tree GListCN_
  GListDAP :: [GDAP] -> Tree GListDAP_
  GListIAdv :: [GIAdv] -> Tree GListIAdv_
  GListNP :: [GNP] -> Tree GListNP_
  GListPrep :: [GPrep] -> Tree GListPrep_
  GListRS :: [GRS] -> Tree GListRS_
  GListS :: [GS] -> Tree GListS_
  GListUDFragment :: [GUDFragment] -> Tree GListUDFragment_
  GListUDS :: [GUDS] -> Tree GListUDS_
  GListVP :: [GVP] -> Tree GListVP_
  GCompoundCN :: GCN -> GN -> Tree GN_
  GCompoundN :: GN -> GN -> Tree GN_
  GStrN :: GString -> Tree GN_
  LexN :: String -> Tree GN_
  GComplN3 :: GN3 -> GNP -> Tree GN2_
  GUse3N3 :: GN3 -> Tree GN2_
  LexN2 :: String -> Tree GN2_
  GMkN3 :: GN -> GPrep -> GPrep -> Tree GN3_
  LexN3 :: String -> Tree GN3_
  GAdjAsNP :: GAP -> Tree GNP_
  GAdvNP :: GNP -> GAdv -> Tree GNP_
  GApposNP :: GNP -> GNP -> Tree GNP_
  GConjNP :: GConj -> GListNP -> Tree GNP_
  GDefPN :: GPN -> Tree GNP_
  GDetCN :: GDet -> GCN -> Tree GNP_
  GDetNP :: GDet -> Tree GNP_
  GEvery :: GNP -> Tree GNP_
  GExtAdvNP :: GNP -> GAdv -> Tree GNP_
  GGenModNP :: GNum -> GNP -> GCN -> Tree GNP_
  GGerundNP :: GVP -> Tree GNP_
  GIndefPN :: GPN -> Tree GNP_
  GMassNP :: GCN -> Tree GNP_
  GParty :: GNP -> Tree GNP_
  GPredetNP :: GPredet -> GNP -> Tree GNP_
  GRelNP :: GNP -> GRS -> Tree GNP_
  GSentNP :: GNP -> GSC -> Tree GNP_
  GSomeone :: Tree GNP_
  GTokAll :: GNP -> Tree GNP_
  GUsePN :: GPN -> Tree GNP_
  GUsePron :: GPron -> Tree GNP_
  GWho :: GUDS -> GNP -> Tree GNP_
  GYou :: Tree GNP_
  Geuropean_NP :: Tree GNP_
  Gone_NP :: Tree GNP_
  Gwhoever_NP :: Tree GNP_
  GNumCard :: GCard -> Tree GNum_
  GNumPl :: Tree GNum_
  GNumSg :: Tree GNum_
  GStrNum :: GString -> Tree GNum_
  Gnum :: GSub1000000 -> Tree GNumeral_
  GOrdDigits :: GDigits -> Tree GOrd_
  GOrdNumeral :: GNumeral -> Tree GOrd_
  GOrdNumeralSuperl :: GNumeral -> GA -> Tree GOrd_
  GOrdSuperl :: GA -> Tree GOrd_
  Gbut_PConj :: Tree GPConj_
  Gfor_PConj :: Tree GPConj_
  Gso_PConj :: Tree GPConj_
  GStrPN :: GString -> Tree GPN_
  GSymbPN :: GSymb -> Tree GPN_
  LexPN :: String -> Tree GPN_
  GPNeg :: Tree GPol_
  GPPos :: Tree GPol_
  LexPredet :: String -> Tree GPredet_
  GConjPrep :: GConj -> GListPrep -> Tree GPrep_
  LexPrep :: String -> Tree GPrep_
  LexPron :: String -> Tree GPron_
  GPredIAdvVP :: GIAdv -> GVP -> Tree GQCl_
  GQuestCl :: GCl -> Tree GQCl_
  GQuestIAdv :: GIAdv -> GCl -> Tree GQCl_
  GQuestIComp :: GIComp -> GNP -> Tree GQCl_
  GQuestQVP :: GIP -> GQVP -> Tree GQCl_
  GQuestSlash :: GIP -> GClSlash -> Tree GQCl_
  GQuestVP :: GIP -> GVP -> Tree GQCl_
  GExistIPQS :: GTemp -> GPol -> GIP -> Tree GQS_
  GExistNPQS :: GTemp -> GPol -> GNP -> Tree GQS_
  GUseQCl :: GTemp -> GPol -> GQCl -> Tree GQS_
  GAddAdvQVP :: GQVP -> GIAdv -> Tree GQVP_
  GAdvQVP :: GVP -> GIAdv -> Tree GQVP_
  GComplSlashIP :: GVPSlash -> GIP -> Tree GQVP_
  GGenNP :: GNP -> Tree GQuant_
  GPossPron :: GPron -> Tree GQuant_
  LexQuant :: String -> Tree GQuant_
  GRelCl :: GCl -> Tree GRCl_
  GRelSlash :: GRP -> GClSlash -> Tree GRCl_
  GRelVP :: GRP -> GVP -> Tree GRCl_
  GFunRP :: GPrep -> GNP -> GRP -> Tree GRP_
  GGenRP :: GNum -> GCN -> Tree GRP_
  GIdRP :: Tree GRP_
  GPrepRP :: GPrep -> GRP -> Tree GRP_
  Gthat_RP :: Tree GRP_
  Gwho_RP :: Tree GRP_
  GConjRS :: GConj -> GListRS -> Tree GRS_
  GRS_that_NP_VP :: GNP -> GVP -> Tree GRS_
  GUseRCl :: GTemp -> GPol -> GRCl -> Tree GRS_
  GAdvS :: GAdv -> GS -> Tree GS_
  GConjS :: GConj -> GListS -> Tree GS_
  GExistS :: GTemp -> GPol -> GNP -> Tree GS_
  GExtAdvS :: GAdv -> GS -> Tree GS_
  GPostAdvS :: GS -> GAdv -> Tree GS_
  GPredVPS :: GNP -> GVP -> Tree GS_
  GRelS :: GS -> GRS -> Tree GS_
  GSSubjS :: GS -> GSubj -> GS -> Tree GS_
  GUseCl :: GTemp -> GPol -> GCl -> Tree GS_
  GEmbedQS :: GQS -> Tree GSC_
  GEmbedS :: GS -> Tree GSC_
  GEmbedVP :: GVP -> Tree GSC_
  GUseSlash :: GTemp -> GPol -> GClSlash -> Tree GSSlash_
  Gpot0 :: GDigit -> Tree GSub10_
  Gpot01 :: Tree GSub10_
  Gpot0as1 :: GSub10 -> Tree GSub100_
  Gpot1 :: GDigit -> Tree GSub100_
  Gpot110 :: Tree GSub100_
  Gpot111 :: Tree GSub100_
  Gpot1plus :: GDigit -> GSub10 -> Tree GSub100_
  Gpot1to19 :: GDigit -> Tree GSub100_
  Gpot1as2 :: GSub100 -> Tree GSub1000_
  Gpot2 :: GSub10 -> Tree GSub1000_
  Gpot2plus :: GSub10 -> GSub100 -> Tree GSub1000_
  Gpot2as3 :: GSub1000 -> Tree GSub1000000_
  Gpot3 :: GSub1000 -> Tree GSub1000000_
  Gpot3plus :: GSub1000 -> GSub1000 -> Tree GSub1000000_
  LexSubj :: String -> Tree GSubj_
  GStrSymb :: GString -> Tree GSymb_
  GTTAnt :: GTense -> GAnt -> Tree GTemp_
  GTCond :: Tree GTense_
  GTFut :: Tree GTense_
  GTPast :: Tree GTense_
  GTPres :: Tree GTense_
  GAdv_no_later_than_Num_calendar_days_after_the_day_UDS :: GNumeral -> GUDS -> Tree GUDFragment_
  GCond :: GUDS -> GUDFragment -> Tree GUDFragment_
  GCondGiven :: GUDS -> GUDS -> GUDFragment -> Tree GUDFragment_
  GCondStandalone :: GUDS -> Tree GUDFragment_
  GCondTemporal :: GUDS -> GAdv -> GUDFragment -> Tree GUDFragment_
  GCondUpon :: GUDS -> GUDS -> GUDFragment -> Tree GUDFragment_
  GGiven :: GUDS -> GUDFragment -> Tree GUDFragment_
  GGivenStandalone :: GUDS -> Tree GUDFragment_
  GHornClause2 :: GUDFragment -> GUDS -> Tree GUDFragment_
  GMeans :: GNP -> GUDS -> Tree GUDFragment_
  GRPelem :: GNP -> GUDS -> Tree GUDFragment_
  GRPeq :: GNP -> GUDS -> Tree GUDFragment_
  GRPgt :: GNP -> GUDS -> Tree GUDFragment_
  GRPgte :: GNP -> GUDS -> Tree GUDFragment_
  GRPis :: GNP -> GUDS -> Tree GUDFragment_
  GRPlt :: GNP -> GUDS -> Tree GUDFragment_
  GRPlte :: GNP -> GUDS -> Tree GUDFragment_
  GRPnotElem :: GNP -> GUDS -> Tree GUDFragment_
  GTemporal :: GAdv -> GUDFragment -> Tree GUDFragment_
  GTemporalStandalone :: GUDS -> Tree GUDFragment_
  GUDS2Fragment :: GUDS -> Tree GUDFragment_
  GUpon :: GUDS -> GUDFragment -> Tree GUDFragment_
  GUponStandalone :: GUDS -> Tree GUDFragment_
  GsubjAction :: GNP -> GUDS -> Tree GUDFragment_
  GDMay :: GUDS -> Tree GUDS_
  GDMust :: GUDS -> Tree GUDS_
  GDShant :: GUDS -> Tree GUDS_
  Groot_acl :: Groot -> Gacl -> Tree GUDS_
  Groot_aclRelcl :: Groot -> GaclRelcl -> Tree GUDS_
  Groot_aclRelcl_nmod :: Groot -> GaclRelcl -> Gnmod -> Tree GUDS_
  Groot_acl_nmod :: Groot -> Gacl -> Gnmod -> Tree GUDS_
  Groot_advcl :: Groot -> Gadvcl -> Tree GUDS_
  Groot_advcl_nsubjPass_auxPass :: Groot -> Gadvcl -> GnsubjPass -> GauxPass -> Tree GUDS_
  Groot_advcl_nsubjPass_aux_auxPass_xcomp :: Groot -> Gadvcl -> GnsubjPass -> Gaux -> GauxPass -> Gxcomp -> Tree GUDS_
  Groot_advcl_nsubj_aux_advcl :: Groot -> Gadvcl -> Gnsubj -> Gaux -> Gadvcl -> Tree GUDS_
  Groot_advcl_nsubj_aux_advmod_obj :: Groot -> Gadvcl -> Gnsubj -> Gaux -> Gadvmod -> Gobj -> Tree GUDS_
  Groot_advcl_nsubj_aux_ccomp :: Groot -> Gadvcl -> Gnsubj -> Gaux -> Gccomp -> Tree GUDS_
  Groot_advcl_nsubj_aux_obl_obj :: Groot -> Gadvcl -> Gnsubj -> Gaux -> Gobl -> Gobj -> Tree GUDS_
  Groot_advcl_nsubj_cop :: Groot -> Gadvcl -> Gnsubj -> Gcop -> Tree GUDS_
  Groot_advcl_nsubj_cop_case_amod_nmod :: Groot -> Gadvcl -> Gnsubj -> Gcop -> Gcase_ -> Gamod -> Gnmod -> Tree GUDS_
  Groot_advcl_nsubj_cop_nsubj :: Groot -> Gadvcl -> Gnsubj -> Gcop -> Gnsubj -> Tree GUDS_
  Groot_advcl_nsubj_xcomp :: Groot -> Gadvcl -> Gnsubj -> Gxcomp -> Tree GUDS_
  Groot_advmod :: Groot -> Gadvmod -> Tree GUDS_
  Groot_advmod_advcl :: Groot -> Gadvmod -> Gadvcl -> Tree GUDS_
  Groot_advmod_advmod_obl :: Groot -> Gadvmod -> Gadvmod -> Gobl -> Tree GUDS_
  Groot_advmod_amod :: Groot -> Gadvmod -> Gamod -> Tree GUDS_
  Groot_advmod_nsubj_aux_advmod_obj :: Groot -> Gadvmod -> Gnsubj -> Gaux -> Gadvmod -> Gobj -> Tree GUDS_
  Groot_advmod_nsubj_cop :: Groot -> Gadvmod -> Gnsubj -> Gcop -> Tree GUDS_
  Groot_advmod_nsubj_cop_obl :: Groot -> Gadvmod -> Gnsubj -> Gcop -> Gobl -> Tree GUDS_
  Groot_advmod_xcomp :: Groot -> Gadvmod -> Gxcomp -> Tree GUDS_
  Groot_amod_nmod :: Groot -> Gamod -> Gnmod -> Tree GUDS_
  Groot_appos :: Groot -> Gappos -> Tree GUDS_
  Groot_appos_advmod :: Groot -> Gappos -> Gadvmod -> Tree GUDS_
  Groot_auxPass :: Groot -> GauxPass -> Tree GUDS_
  Groot_case :: Groot -> Gcase_ -> Tree GUDS_
  Groot_case_amod :: Groot -> Gcase_ -> Gamod -> Tree GUDS_
  Groot_case_amod_amod :: Groot -> Gcase_ -> Gamod -> Gamod -> Tree GUDS_
  Groot_case_compound :: Groot -> Gcase_ -> Gcompound -> Tree GUDS_
  Groot_case_nummod :: Groot -> Gcase_ -> Gnummod -> Tree GUDS_
  Groot_case_nummod_acl :: Groot -> Gcase_ -> Gnummod -> Gacl -> Tree GUDS_
  Groot_case_nummod_nummod :: Groot -> Gcase_ -> Gnummod -> Gnummod -> Tree GUDS_
  Groot_cc :: Groot -> Gcc -> Tree GUDS_
  Groot_cc_cop_xcomp :: Groot -> Gcc -> Gcop -> Gxcomp -> Tree GUDS_
  Groot_cc_nmod :: Groot -> Gcc -> Gnmod -> Tree GUDS_
  Groot_cc_obj :: Groot -> Gcc -> Gobj -> Tree GUDS_
  Groot_ccomp :: Groot -> Gccomp -> Tree GUDS_
  Groot_compound :: Groot -> Gcompound -> Tree GUDS_
  Groot_compoundPrt_compoundPrt :: Groot -> GcompoundPrt -> GcompoundPrt -> Tree GUDS_
  Groot_compound_acl :: Groot -> Gcompound -> Gacl -> Tree GUDS_
  Groot_compound_amod :: Groot -> Gcompound -> Gamod -> Tree GUDS_
  Groot_compound_appos :: Groot -> Gcompound -> Gappos -> Tree GUDS_
  Groot_compound_compound :: Groot -> Gcompound -> Gcompound -> Tree GUDS_
  Groot_compound_compound_appos :: Groot -> Gcompound -> Gcompound -> Gappos -> Tree GUDS_
  Groot_compound_flat :: Groot -> Gcompound -> Gflat -> Tree GUDS_
  Groot_cop :: Groot -> Gcop -> Tree GUDS_
  Groot_cop_advmod :: Groot -> Gcop -> Gadvmod -> Tree GUDS_
  Groot_csubj :: Groot -> Gcsubj -> Tree GUDS_
  Groot_csubj_aux_aux :: Groot -> Gcsubj -> Gaux -> Gaux -> Tree GUDS_
  Groot_discourse :: Groot -> Gdiscourse -> Tree GUDS_
  Groot_expl_cop_csubj :: Groot -> Gexpl -> Gcop -> Gcsubj -> Tree GUDS_
  Groot_fixed :: Groot -> Gfixed -> Tree GUDS_
  Groot_goeswith :: Groot -> Ggoeswith -> Tree GUDS_
  Groot_goeswith_goeswith :: Groot -> Ggoeswith -> Ggoeswith -> Tree GUDS_
  Groot_mark :: Groot -> Gmark -> Tree GUDS_
  Groot_mark_cc_mark_obj :: Groot -> Gmark -> Gcc -> Gmark -> Gobj -> Tree GUDS_
  Groot_mark_expl_cop_xcomp :: Groot -> Gmark -> Gexpl -> Gcop -> Gxcomp -> Tree GUDS_
  Groot_mark_expl_nsubj :: Groot -> Gmark -> Gexpl -> Gnsubj -> Tree GUDS_
  Groot_mark_nsubj :: Groot -> Gmark -> Gnsubj -> Tree GUDS_
  Groot_mark_nsubjPass_auxPass :: Groot -> Gmark -> GnsubjPass -> GauxPass -> Tree GUDS_
  Groot_mark_nsubjPass_auxPass_obl :: Groot -> Gmark -> GnsubjPass -> GauxPass -> Gobl -> Tree GUDS_
  Groot_mark_nsubj_aux :: Groot -> Gmark -> Gnsubj -> Gaux -> Tree GUDS_
  Groot_mark_nsubj_aux_advmod_obj :: Groot -> Gmark -> Gnsubj -> Gaux -> Gadvmod -> Gobj -> Tree GUDS_
  Groot_mark_nsubj_aux_aux :: Groot -> Gmark -> Gnsubj -> Gaux -> Gaux -> Tree GUDS_
  Groot_mark_nsubj_cop :: Groot -> Gmark -> Gnsubj -> Gcop -> Tree GUDS_
  Groot_mark_nsubj_cop_obl :: Groot -> Gmark -> Gnsubj -> Gcop -> Gobl -> Tree GUDS_
  Groot_mark_nsubj_nsubj_xcomp :: Groot -> Gmark -> Gnsubj -> Gnsubj -> Gxcomp -> Tree GUDS_
  Groot_mark_nsubj_obj :: Groot -> Gmark -> Gnsubj -> Gobj -> Tree GUDS_
  Groot_mark_nsubj_obl :: Groot -> Gmark -> Gnsubj -> Gobl -> Tree GUDS_
  Groot_mark_nsubj_xcomp :: Groot -> Gmark -> Gnsubj -> Gxcomp -> Tree GUDS_
  Groot_mark_nummod :: Groot -> Gmark -> Gnummod -> Tree GUDS_
  Groot_nmod :: Groot -> Gnmod -> Tree GUDS_
  Groot_nmod_acl :: Groot -> Gnmod -> Gacl -> Tree GUDS_
  Groot_nmod_aclRelcl :: Groot -> Gnmod -> GaclRelcl -> Tree GUDS_
  Groot_nmod_nmod :: Groot -> Gnmod -> Gnmod -> Tree GUDS_
  Groot_nsubj :: Groot -> Gnsubj -> Tree GUDS_
  Groot_nsubjPass_auxPass :: Groot -> GnsubjPass -> GauxPass -> Tree GUDS_
  Groot_nsubjPass_auxPass_advmod :: Groot -> GnsubjPass -> GauxPass -> Gadvmod -> Tree GUDS_
  Groot_nsubjPass_auxPass_advmod_advcl :: Groot -> GnsubjPass -> GauxPass -> Gadvmod -> Gadvcl -> Tree GUDS_
  Groot_nsubjPass_auxPass_advmod_xcomp :: Groot -> GnsubjPass -> GauxPass -> Gadvmod -> Gxcomp -> Tree GUDS_
  Groot_nsubjPass_auxPass_xcomp :: Groot -> GnsubjPass -> GauxPass -> Gxcomp -> Tree GUDS_
  Groot_nsubjPass_aux_auxPass :: Groot -> GnsubjPass -> Gaux -> GauxPass -> Tree GUDS_
  Groot_nsubjPass_aux_auxPass_advcl_advcl :: Groot -> GnsubjPass -> Gaux -> GauxPass -> Gadvcl -> Gadvcl -> Tree GUDS_
  Groot_nsubjPass_aux_auxPass_advmod :: Groot -> GnsubjPass -> Gaux -> GauxPass -> Gadvmod -> Tree GUDS_
  Groot_nsubjPass_aux_auxPass_obl :: Groot -> GnsubjPass -> Gaux -> GauxPass -> Gobl -> Tree GUDS_
  Groot_nsubjPass_aux_auxPass_obl_advmod :: Groot -> GnsubjPass -> Gaux -> GauxPass -> Gobl -> Gadvmod -> Tree GUDS_
  Groot_nsubjPass_aux_auxPass_obl_obl_advcl :: Groot -> GnsubjPass -> Gaux -> GauxPass -> Gobl -> Gobl -> Gadvcl -> Tree GUDS_
  Groot_nsubjPass_aux_auxPass_obl_obl_advmod :: Groot -> GnsubjPass -> Gaux -> GauxPass -> Gobl -> Gobl -> Gadvmod -> Tree GUDS_
  Groot_nsubjPass_aux_auxPass_xcomp :: Groot -> GnsubjPass -> Gaux -> GauxPass -> Gxcomp -> Tree GUDS_
  Groot_nsubj_advmod :: Groot -> Gnsubj -> Gadvmod -> Tree GUDS_
  Groot_nsubj_advmod_obj :: Groot -> Gnsubj -> Gadvmod -> Gobj -> Tree GUDS_
  Groot_nsubj_aux :: Groot -> Gnsubj -> Gaux -> Tree GUDS_
  Groot_nsubj_aux_aclRelcl :: Groot -> Gnsubj -> Gaux -> GaclRelcl -> Tree GUDS_
  Groot_nsubj_aux_aclRelcl_obl :: Groot -> Gnsubj -> Gaux -> GaclRelcl -> Gobl -> Tree GUDS_
  Groot_nsubj_aux_advmod :: Groot -> Gnsubj -> Gaux -> Gadvmod -> Tree GUDS_
  Groot_nsubj_aux_advmod_obj_advcl :: Groot -> Gnsubj -> Gaux -> Gadvmod -> Gobj -> Gadvcl -> Tree GUDS_
  Groot_nsubj_aux_aux :: Groot -> Gnsubj -> Gaux -> Gaux -> Tree GUDS_
  Groot_nsubj_aux_cop_nmod :: Groot -> Gnsubj -> Gaux -> Gcop -> Gnmod -> Tree GUDS_
  Groot_nsubj_aux_obj :: Groot -> Gnsubj -> Gaux -> Gobj -> Tree GUDS_
  Groot_nsubj_aux_obj_obl :: Groot -> Gnsubj -> Gaux -> Gobj -> Gobl -> Tree GUDS_
  Groot_nsubj_aux_obj_obl_advmod_advcl :: Groot -> Gnsubj -> Gaux -> Gobj -> Gobl -> Gadvmod -> Gadvcl -> Tree GUDS_
  Groot_nsubj_aux_obj_obl_obl :: Groot -> Gnsubj -> Gaux -> Gobj -> Gobl -> Gobl -> Tree GUDS_
  Groot_nsubj_aux_obl :: Groot -> Gnsubj -> Gaux -> Gobl -> Tree GUDS_
  Groot_nsubj_ccomp :: Groot -> Gnsubj -> Gccomp -> Tree GUDS_
  Groot_nsubj_cop :: Groot -> Gnsubj -> Gcop -> Tree GUDS_
  Groot_nsubj_cop_aclRelcl :: Groot -> Gnsubj -> Gcop -> GaclRelcl -> Tree GUDS_
  Groot_nsubj_cop_aclRelcl_obl :: Groot -> Gnsubj -> Gcop -> GaclRelcl -> Gobl -> Tree GUDS_
  Groot_nsubj_cop_advcl :: Groot -> Gnsubj -> Gcop -> Gadvcl -> Tree GUDS_
  Groot_nsubj_cop_advmod :: Groot -> Gnsubj -> Gcop -> Gadvmod -> Tree GUDS_
  Groot_nsubj_cop_case_nmod_acl :: Groot -> Gnsubj -> Gcop -> Gcase_ -> Gnmod -> Gacl -> Tree GUDS_
  Groot_nsubj_cop_nmod :: Groot -> Gnsubj -> Gcop -> Gnmod -> Tree GUDS_
  Groot_nsubj_cop_nmodPoss :: Groot -> Gnsubj -> Gcop -> GnmodPoss -> Tree GUDS_
  Groot_nsubj_cop_obl :: Groot -> Gnsubj -> Gcop -> Gobl -> Tree GUDS_
  Groot_nsubj_cop_obl_parataxis :: Groot -> Gnsubj -> Gcop -> Gobl -> Gparataxis -> Tree GUDS_
  Groot_nsubj_nsubj_aux_advmod_obj :: Groot -> Gnsubj -> Gnsubj -> Gaux -> Gadvmod -> Gobj -> Tree GUDS_
  Groot_nsubj_obj :: Groot -> Gnsubj -> Gobj -> Tree GUDS_
  Groot_nsubj_obj_advcl :: Groot -> Gnsubj -> Gobj -> Gadvcl -> Tree GUDS_
  Groot_nsubj_obj_xcomp :: Groot -> Gnsubj -> Gobj -> Gxcomp -> Tree GUDS_
  Groot_nsubj_obl :: Groot -> Gnsubj -> Gobl -> Tree GUDS_
  Groot_nsubj_obl_obl :: Groot -> Gnsubj -> Gobl -> Gobl -> Tree GUDS_
  Groot_nsubj_xcomp :: Groot -> Gnsubj -> Gxcomp -> Tree GUDS_
  Groot_nummod :: Groot -> Gnummod -> Tree GUDS_
  Groot_nummod_appos :: Groot -> Gnummod -> Gappos -> Tree GUDS_
  Groot_nummod_auxPass_cc_aux_auxPass_obl_obl :: Groot -> Gnummod -> GauxPass -> Gcc -> Gaux -> GauxPass -> Gobl -> Gobl -> Tree GUDS_
  Groot_nummod_mark_obj :: Groot -> Gnummod -> Gmark -> Gobj -> Tree GUDS_
  Groot_nummod_mark_obj_cc :: Groot -> Gnummod -> Gmark -> Gobj -> Gcc -> Tree GUDS_
  Groot_nummod_nmod :: Groot -> Gnummod -> Gnmod -> Tree GUDS_
  Groot_nummod_nsubjPass_nsubjPass_auxPass_cc :: Groot -> Gnummod -> GnsubjPass -> GnsubjPass -> GauxPass -> Gcc -> Tree GUDS_
  Groot_nummod_obl :: Groot -> Gnummod -> Gobl -> Tree GUDS_
  Groot_nummod_obl_cc :: Groot -> Gnummod -> Gobl -> Gcc -> Tree GUDS_
  Groot_obj :: Groot -> Gobj -> Tree GUDS_
  Groot_obj_ccomp :: Groot -> Gobj -> Gccomp -> Tree GUDS_
  Groot_obj_nmod :: Groot -> Gobj -> Gnmod -> Tree GUDS_
  Groot_obl :: Groot -> Gobl -> Tree GUDS_
  Groot_obl_appos :: Groot -> Gobl -> Gappos -> Tree GUDS_
  Groot_obl_aux :: Groot -> Gobl -> Gaux -> Tree GUDS_
  Groot_obl_case :: Groot -> Gobl -> Gcase_ -> Tree GUDS_
  Groot_obl_obj :: Groot -> Gobl -> Gobj -> Tree GUDS_
  Groot_obl_obl :: Groot -> Gobl -> Gobl -> Tree GUDS_
  Groot_obl_obl_obl_cc :: Groot -> Gobl -> Gobl -> Gobl -> Tree GUDS_
  Groot_obl_xcomp :: Groot -> Gobl -> Gxcomp -> Tree GUDS_
  Groot_only :: Groot -> Tree GUDS_
  Groot_parataxis :: Groot -> Gparataxis -> Tree GUDS_
  Groot_xcomp :: Groot -> Gxcomp -> Tree GUDS_
  Groot_xcomp_ccomp :: Groot -> Gxcomp -> Gccomp -> Tree GUDS_
  LexV :: String -> Tree GV_
  GAdVVP :: GAdV -> GVP -> Tree GVP_
  GAdvVP :: GVP -> GAdv -> Tree GVP_
  GComplV :: GV -> GNP -> Tree GVP_
  GComplVP :: GVP -> GNP -> Tree GVP_
  GConjVP :: GConj -> GListVP -> Tree GVP_
  GPassV :: GV -> Tree GVP_
  GPassVAgent :: GV -> GNP -> Tree GVP_
  GPrepVP :: GVP -> GPrep -> Tree GVP_
  GProgrVP :: GVP -> Tree GVP_
  GUseComp :: GComp -> Tree GVP_
  GUseV :: GV -> Tree GVP_
  GaclUDS_ :: GUDS -> Tree Gacl_
  GaclUDSgerund_ :: GUDS -> Tree Gacl_
  GaclUDSpastpartParens_ :: GUDS -> Tree Gacl_
  GaclUDSpastpart_ :: GUDS -> Tree Gacl_
  Gacl_ :: GX -> Tree Gacl_
  GaclRelclRS_ :: GRS -> Tree GaclRelcl_
  GaclRelclUDS_ :: GUDS -> Tree GaclRelcl_
  GpassRelcl_ :: Groot -> GRP -> GauxPass -> Tree GaclRelcl_
  GadvclMarkUDS_ :: Gmark -> GUDS -> Tree Gadvcl_
  GadvclUDS_ :: GUDS -> Tree Gadvcl_
  Gadvcl_ :: GAdv -> Tree Gadvcl_
  Gadvmod_ :: GAdv -> Tree Gadvmod_
  Gnot_advmod :: Tree Gadvmod_
  GadvmodEmph_ :: GX -> Tree GadvmodEmph_
  GadvmodLmod_ :: GX -> Tree GadvmodLmod_
  Gamod_ :: GAP -> Tree Gamod_
  Gappos_ :: GX -> Tree Gappos_
  Gaux_ :: GX -> Tree Gaux_
  Gbe_aux :: Tree Gaux_
  Gcan_aux :: Tree Gaux_
  Ghave_aux :: Tree Gaux_
  Gmay_aux :: Tree Gaux_
  Gmust_aux :: Tree Gaux_
  Gshall_aux :: Tree Gaux_
  Gshould_aux :: Tree Gaux_
  Gwill_aux :: Tree Gaux_
  Gbe_auxPass :: Tree GauxPass_
  Gcc_ :: GConj -> Tree Gcc_
  GccPreconj_ :: GX -> Tree GccPreconj_
  Gccomp_ :: GUDS -> Tree Gccomp_
  Gclf_ :: GX -> Tree Gclf_
  Gcompound_ :: GX -> Tree Gcompound_
  GcompoundLvc_ :: GX -> Tree GcompoundLvc_
  GcompoundPrt_ :: GX -> Tree GcompoundPrt_
  GcompoundRedup_ :: GX -> Tree GcompoundRedup_
  GcompoundSvc_ :: GX -> Tree GcompoundSvc_
  Gbe_cop :: Tree Gcop_
  Gis_cop :: Tree Gcop_
  GcsubjMarkFinite_ :: Gmark -> GUDS -> Tree Gcsubj_
  GcsubjMarkInfinite_ :: Gmark -> GUDS -> Tree Gcsubj_
  Gcsubj_ :: GUDS -> Tree Gcsubj_
  GcsubjPass_ :: GX -> Tree GcsubjPass_
  Gdep_ :: GX -> Tree Gdep_
  Gdet_ :: GDet -> Tree Gdet_
  GdetNumgov_ :: GX -> Tree GdetNumgov_
  GdetNummod_ :: GX -> Tree GdetNummod_
  GdetPoss_ :: GX -> Tree GdetPoss_
  Gdiscourse_ :: GX -> Tree Gdiscourse_
  Gdislocated_ :: GX -> Tree Gdislocated_
  Gexpl_ :: GPron -> Tree Gexpl_
  Git_expl :: Tree Gexpl_
  GexplImpers_ :: GX -> Tree GexplImpers_
  GexplPass_ :: GX -> Tree GexplPass_
  GexplPv_ :: GX -> Tree GexplPv_
  Gfixed_ :: GX -> Tree Gfixed_
  Gflat_ :: GX -> Tree Gflat_
  GflatForeign_ :: GX -> Tree GflatForeign_
  GflatName_ :: GX -> Tree GflatName_
  Ggoeswith_ :: GX -> Tree Ggoeswith_
  Giobj_ :: GNP -> Tree Giobj_
  Glist_ :: GX -> Tree Glist_
  Gmark_ :: GSubj -> Tree Gmark_
  Gto_mark :: Tree Gmark_
  Gnmod_ :: GPrep -> GNP -> Tree Gnmod_
  GnmodPoss_ :: GX -> Tree GnmodPoss_
  GnmodTmod_ :: GX -> Tree GnmodTmod_
  Gnsubj_ :: GNP -> Tree Gnsubj_
  GnsubjPass_ :: GNP -> Tree GnsubjPass_
  Gnummod_ :: GX -> Tree Gnummod_
  GnummodGov_ :: GX -> Tree GnummodGov_
  Gobj_ :: GNP -> Tree Gobj_
  GoblPrep_ :: GPrep -> Tree Gobl_
  GoblRP_ :: GPrep -> GRP -> Tree Gobl_
  Gobl_ :: GAdv -> Tree Gobl_
  GoblAgent_ :: GAdv -> Tree GoblAgent_
  GoblArg_ :: GAdv -> Tree GoblArg_
  GoblLmod_ :: GAdv -> Tree GoblLmod_
  GoblTmod_ :: GAdv -> Tree GoblTmod_
  Gorphan_ :: GX -> Tree Gorphan_
  Gparataxis_ :: GX -> Tree Gparataxis_
  Gpunct_ :: GX -> Tree Gpunct_
  Greparandum_ :: GX -> Tree Greparandum_
  GrootA_ :: GAP -> Tree Groot_
  GrootAdA_ :: GAdA -> Tree Groot_
  GrootAdv_ :: GAdv -> Tree Groot_
  GrootDAP_ :: GDAP -> Tree Groot_
  GrootDet_ :: GDet -> Tree Groot_
  GrootN_ :: GNP -> Tree Groot_
  GrootPrep_ :: GPrep -> Tree Groot_
  GrootQuant_ :: GQuant -> Tree Groot_
  GrootRP_ :: GRP -> Tree Groot_
  GrootV_ :: GVP -> Tree Groot_
  Gvocative_ :: GNP -> Tree Gvocative_
  GxcompA_ :: GAP -> Tree Gxcomp_
  GxcompA_ccomp_ :: GAP -> Gccomp -> Tree Gxcomp_
  GxcompAdv_ :: GAdv -> Tree Gxcomp_
  GxcompN_ :: GNP -> Tree Gxcomp_
  GxcompToBeN_ :: Gmark -> Gcop -> GNP -> Tree Gxcomp_
  LexText :: String -> Tree GText_
  LexV2 :: String -> Tree GV2_
  LexV2A :: String -> Tree GV2A_
  LexV2Q :: String -> Tree GV2Q_
  LexV2S :: String -> Tree GV2S_
  LexV2V :: String -> Tree GV2V_
  LexV3 :: String -> Tree GV3_
  LexVA :: String -> Tree GVA_
  LexVS :: String -> Tree GVS_
  LexVV :: String -> Tree GVV_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (GStrA x1,GStrA y1) -> and [ x1 == y1 ]
    (LexA x,LexA y) -> x == y
    (GMkA2 x1 x2,GMkA2 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexA2 x,LexA2 y) -> x == y
    (LexACard x,LexACard y) -> x == y
    (GAdAP x1 x2,GAdAP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAdjOrd x1,GAdjOrd y1) -> and [ x1 == y1 ]
    (GAdvAP x1 x2,GAdvAP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GComplA2 x1 x2,GComplA2 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GConjAP x1 x2,GConjAP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GParentheticalAP x1,GParentheticalAP y1) -> and [ x1 == y1 ]
    (GPastPartAP x1,GPastPartAP y1) -> and [ x1 == y1 ]
    (GPastPartAgentAP x1 x2,GPastPartAgentAP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPositA x1,GPositA y1) -> and [ x1 == y1 ]
    (GPresPartAP x1,GPresPartAP y1) -> and [ x1 == y1 ]
    (GStrAP x1,GStrAP y1) -> and [ x1 == y1 ]
    (GUseComparA x1,GUseComparA y1) -> and [ x1 == y1 ]
    (LexAdA x,LexAdA y) -> x == y
    (GAdnCAdv x1,GAdnCAdv y1) -> and [ x1 == y1 ]
    (LexAdN x,LexAdN y) -> x == y
    (GConjAdV x1 x2,GConjAdV y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexAdV x,LexAdV y) -> x == y
    (GAdvAdv x1 x2,GAdvAdv y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GComparAdvAdj x1 x2 x3,GComparAdvAdj y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GComparAdvAdjS x1 x2 x3,GComparAdvAdjS y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GConjAdv x1 x2,GConjAdv y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GGerundAdv x1,GGerundAdv y1) -> and [ x1 == y1 ]
    (GPositAdvAdj x1,GPositAdvAdj y1) -> and [ x1 == y1 ]
    (GPrepNP x1 x2,GPrepNP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSubjS x1 x2,GSubjS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Gacl2Adv x1,Gacl2Adv y1) -> and [ x1 == y1 ]
    (Gadvcl2Adv x1,Gadvcl2Adv y1) -> and [ x1 == y1 ]
    (Gcsubj2Adv x1,Gcsubj2Adv y1) -> and [ x1 == y1 ]
    (Gxcomp2Adv x1,Gxcomp2Adv y1) -> and [ x1 == y1 ]
    (LexAdv x,LexAdv y) -> x == y
    (GAAnter,GAAnter) -> and [ ]
    (GASimul,GASimul) -> and [ ]
    (LexCAdv x,LexCAdv y) -> x == y
    (GAdjCN x1 x2,GAdjCN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAdvCN x1 x2,GAdvCN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GApposCN x1 x2,GApposCN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GCN_AP_Conj_CNs_of_NP x1 x2 x3 x4,GCN_AP_Conj_CNs_of_NP y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GComplN2 x1 x2,GComplN2 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GConjCN x1 x2,GConjCN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GGerundCN x1,GGerundCN y1) -> and [ x1 == y1 ]
    (GPossNP x1 x2,GPossNP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRelCN x1 x2,GRelCN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSentCN x1 x2,GSentCN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GUseN x1,GUseN y1) -> and [ x1 == y1 ]
    (Gday_CN,Gday_CN) -> and [ ]
    (Ghigher_CN,Ghigher_CN) -> and [ ]
    (Gleave_CN,Gleave_CN) -> and [ ]
    (Gtricyclic_CN,Gtricyclic_CN) -> and [ ]
    (GAdNum x1 x2,GAdNum y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNumDigits x1,GNumDigits y1) -> and [ x1 == y1 ]
    (GNumNumeral x1,GNumNumeral y1) -> and [ x1 == y1 ]
    (GStrCard x1,GStrCard y1) -> and [ x1 == y1 ]
    (LexCard x,LexCard y) -> x == y
    (GExistCN x1,GExistCN y1) -> and [ x1 == y1 ]
    (GExistsNP x1,GExistsNP y1) -> and [ x1 == y1 ]
    (GGenericCl x1,GGenericCl y1) -> and [ x1 == y1 ]
    (GImpersCl x1,GImpersCl y1) -> and [ x1 == y1 ]
    (GPredSCVP x1 x2,GPredSCVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPredVP x1 x2,GPredVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAdvSlash x1 x2,GAdvSlash y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSlashCl x1,GSlashCl y1) -> and [ x1 == y1 ]
    (GSlashPrep x1 x2,GSlashPrep y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSlashVP x1 x2,GSlashVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSlashVS x1 x2 x3,GSlashVS y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GCompAP x1,GCompAP y1) -> and [ x1 == y1 ]
    (GCompAdv x1,GCompAdv y1) -> and [ x1 == y1 ]
    (GCompNP x1,GCompNP y1) -> and [ x1 == y1 ]
    (LexConj x,LexConj y) -> x == y
    (GAdjDAP x1 x2,GAdjDAP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GDetDAP x1,GDetDAP y1) -> and [ x1 == y1 ]
    (GACard2Det x1,GACard2Det y1) -> and [ x1 == y1 ]
    (GConjDet x1 x2,GConjDet y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GDetQuant x1 x2,GDetQuant y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GDetQuantOrd x1 x2 x3,GDetQuantOrd y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GXorMore x1,GXorMore y1) -> and [ x1 == y1 ]
    (LexDet x,LexDet y) -> x == y
    (GD_0,GD_0) -> and [ ]
    (GD_1,GD_1) -> and [ ]
    (GD_2,GD_2) -> and [ ]
    (GD_3,GD_3) -> and [ ]
    (GD_4,GD_4) -> and [ ]
    (GD_5,GD_5) -> and [ ]
    (GD_6,GD_6) -> and [ ]
    (GD_7,GD_7) -> and [ ]
    (GD_8,GD_8) -> and [ ]
    (GD_9,GD_9) -> and [ ]
    (Gn2,Gn2) -> and [ ]
    (Gn3,Gn3) -> and [ ]
    (Gn4,Gn4) -> and [ ]
    (Gn5,Gn5) -> and [ ]
    (Gn6,Gn6) -> and [ ]
    (Gn7,Gn7) -> and [ ]
    (Gn8,Gn8) -> and [ ]
    (Gn9,Gn9) -> and [ ]
    (GIDig x1,GIDig y1) -> and [ x1 == y1 ]
    (GIIDig x1 x2,GIIDig y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Gs_Gen,Gs_Gen) -> and [ ]
    (GAdvIAdv x1 x2,GAdvIAdv y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GConjIAdv x1 x2,GConjIAdv y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIAdvAdv x1,GIAdvAdv y1) -> and [ x1 == y1 ]
    (GPrepIP x1 x2,GPrepIP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Ghow_IAdv,Ghow_IAdv) -> and [ ]
    (Gwhen_IAdv,Gwhen_IAdv) -> and [ ]
    (Gwhere_IAdv,Gwhere_IAdv) -> and [ ]
    (Gwherein_IAdv,Gwherein_IAdv) -> and [ ]
    (Gwhy_IAdv,Gwhy_IAdv) -> and [ ]
    (GCompIAdv x1,GCompIAdv y1) -> and [ x1 == y1 ]
    (GCompIP x1,GCompIP y1) -> and [ x1 == y1 ]
    (GICompAP x1,GICompAP y1) -> and [ x1 == y1 ]
    (GIdetQuant x1 x2,GIdetQuant y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAdvIP x1 x2,GAdvIP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIdetCN x1 x2,GIdetCN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIdetIP x1,GIdetIP y1) -> and [ x1 == y1 ]
    (Gwhat_IP,Gwhat_IP) -> and [ ]
    (Gwho_IP,Gwho_IP) -> and [ ]
    (Gwhich_IQuant,Gwhich_IQuant) -> and [ ]
    (GAdvImp x1 x2,GAdvImp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GImpVP x1,GImpVP y1) -> and [ x1 == y1 ]
    (LexInterj x,LexInterj y) -> x == y
    (GListAP x1,GListAP y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListAdV x1,GListAdV y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListAdv x1,GListAdv y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListCN x1,GListCN y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListDAP x1,GListDAP y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListIAdv x1,GListIAdv y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListNP x1,GListNP y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListPrep x1,GListPrep y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListRS x1,GListRS y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListS x1,GListS y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListUDFragment x1,GListUDFragment y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListUDS x1,GListUDS y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListVP x1,GListVP y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GCompoundCN x1 x2,GCompoundCN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GCompoundN x1 x2,GCompoundN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GStrN x1,GStrN y1) -> and [ x1 == y1 ]
    (LexN x,LexN y) -> x == y
    (GComplN3 x1 x2,GComplN3 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GUse3N3 x1,GUse3N3 y1) -> and [ x1 == y1 ]
    (LexN2 x,LexN2 y) -> x == y
    (GMkN3 x1 x2 x3,GMkN3 y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (LexN3 x,LexN3 y) -> x == y
    (GAdjAsNP x1,GAdjAsNP y1) -> and [ x1 == y1 ]
    (GAdvNP x1 x2,GAdvNP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GApposNP x1 x2,GApposNP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GConjNP x1 x2,GConjNP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GDefPN x1,GDefPN y1) -> and [ x1 == y1 ]
    (GDetCN x1 x2,GDetCN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GDetNP x1,GDetNP y1) -> and [ x1 == y1 ]
    (GEvery x1,GEvery y1) -> and [ x1 == y1 ]
    (GExtAdvNP x1 x2,GExtAdvNP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GGenModNP x1 x2 x3,GGenModNP y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GGerundNP x1,GGerundNP y1) -> and [ x1 == y1 ]
    (GIndefPN x1,GIndefPN y1) -> and [ x1 == y1 ]
    (GMassNP x1,GMassNP y1) -> and [ x1 == y1 ]
    (GParty x1,GParty y1) -> and [ x1 == y1 ]
    (GPredetNP x1 x2,GPredetNP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRelNP x1 x2,GRelNP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSentNP x1 x2,GSentNP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSomeone,GSomeone) -> and [ ]
    (GTokAll x1,GTokAll y1) -> and [ x1 == y1 ]
    (GUsePN x1,GUsePN y1) -> and [ x1 == y1 ]
    (GUsePron x1,GUsePron y1) -> and [ x1 == y1 ]
    (GWho x1 x2,GWho y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GYou,GYou) -> and [ ]
    (Geuropean_NP,Geuropean_NP) -> and [ ]
    (Gone_NP,Gone_NP) -> and [ ]
    (Gwhoever_NP,Gwhoever_NP) -> and [ ]
    (GNumCard x1,GNumCard y1) -> and [ x1 == y1 ]
    (GNumPl,GNumPl) -> and [ ]
    (GNumSg,GNumSg) -> and [ ]
    (GStrNum x1,GStrNum y1) -> and [ x1 == y1 ]
    (Gnum x1,Gnum y1) -> and [ x1 == y1 ]
    (GOrdDigits x1,GOrdDigits y1) -> and [ x1 == y1 ]
    (GOrdNumeral x1,GOrdNumeral y1) -> and [ x1 == y1 ]
    (GOrdNumeralSuperl x1 x2,GOrdNumeralSuperl y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GOrdSuperl x1,GOrdSuperl y1) -> and [ x1 == y1 ]
    (Gbut_PConj,Gbut_PConj) -> and [ ]
    (Gfor_PConj,Gfor_PConj) -> and [ ]
    (Gso_PConj,Gso_PConj) -> and [ ]
    (GStrPN x1,GStrPN y1) -> and [ x1 == y1 ]
    (GSymbPN x1,GSymbPN y1) -> and [ x1 == y1 ]
    (LexPN x,LexPN y) -> x == y
    (GPNeg,GPNeg) -> and [ ]
    (GPPos,GPPos) -> and [ ]
    (LexPredet x,LexPredet y) -> x == y
    (GConjPrep x1 x2,GConjPrep y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexPrep x,LexPrep y) -> x == y
    (LexPron x,LexPron y) -> x == y
    (GPredIAdvVP x1 x2,GPredIAdvVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GQuestCl x1,GQuestCl y1) -> and [ x1 == y1 ]
    (GQuestIAdv x1 x2,GQuestIAdv y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GQuestIComp x1 x2,GQuestIComp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GQuestQVP x1 x2,GQuestQVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GQuestSlash x1 x2,GQuestSlash y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GQuestVP x1 x2,GQuestVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GExistIPQS x1 x2 x3,GExistIPQS y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GExistNPQS x1 x2 x3,GExistNPQS y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GUseQCl x1 x2 x3,GUseQCl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GAddAdvQVP x1 x2,GAddAdvQVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAdvQVP x1 x2,GAdvQVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GComplSlashIP x1 x2,GComplSlashIP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GGenNP x1,GGenNP y1) -> and [ x1 == y1 ]
    (GPossPron x1,GPossPron y1) -> and [ x1 == y1 ]
    (LexQuant x,LexQuant y) -> x == y
    (GRelCl x1,GRelCl y1) -> and [ x1 == y1 ]
    (GRelSlash x1 x2,GRelSlash y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRelVP x1 x2,GRelVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GFunRP x1 x2 x3,GFunRP y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GGenRP x1 x2,GGenRP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIdRP,GIdRP) -> and [ ]
    (GPrepRP x1 x2,GPrepRP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Gthat_RP,Gthat_RP) -> and [ ]
    (Gwho_RP,Gwho_RP) -> and [ ]
    (GConjRS x1 x2,GConjRS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRS_that_NP_VP x1 x2,GRS_that_NP_VP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GUseRCl x1 x2 x3,GUseRCl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GAdvS x1 x2,GAdvS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GConjS x1 x2,GConjS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GExistS x1 x2 x3,GExistS y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GExtAdvS x1 x2,GExtAdvS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPostAdvS x1 x2,GPostAdvS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPredVPS x1 x2,GPredVPS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRelS x1 x2,GRelS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSSubjS x1 x2 x3,GSSubjS y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GUseCl x1 x2 x3,GUseCl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GEmbedQS x1,GEmbedQS y1) -> and [ x1 == y1 ]
    (GEmbedS x1,GEmbedS y1) -> and [ x1 == y1 ]
    (GEmbedVP x1,GEmbedVP y1) -> and [ x1 == y1 ]
    (GUseSlash x1 x2 x3,GUseSlash y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Gpot0 x1,Gpot0 y1) -> and [ x1 == y1 ]
    (Gpot01,Gpot01) -> and [ ]
    (Gpot0as1 x1,Gpot0as1 y1) -> and [ x1 == y1 ]
    (Gpot1 x1,Gpot1 y1) -> and [ x1 == y1 ]
    (Gpot110,Gpot110) -> and [ ]
    (Gpot111,Gpot111) -> and [ ]
    (Gpot1plus x1 x2,Gpot1plus y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Gpot1to19 x1,Gpot1to19 y1) -> and [ x1 == y1 ]
    (Gpot1as2 x1,Gpot1as2 y1) -> and [ x1 == y1 ]
    (Gpot2 x1,Gpot2 y1) -> and [ x1 == y1 ]
    (Gpot2plus x1 x2,Gpot2plus y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Gpot2as3 x1,Gpot2as3 y1) -> and [ x1 == y1 ]
    (Gpot3 x1,Gpot3 y1) -> and [ x1 == y1 ]
    (Gpot3plus x1 x2,Gpot3plus y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexSubj x,LexSubj y) -> x == y
    (GStrSymb x1,GStrSymb y1) -> and [ x1 == y1 ]
    (GTTAnt x1 x2,GTTAnt y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GTCond,GTCond) -> and [ ]
    (GTFut,GTFut) -> and [ ]
    (GTPast,GTPast) -> and [ ]
    (GTPres,GTPres) -> and [ ]
    (GAdv_no_later_than_Num_calendar_days_after_the_day_UDS x1 x2,GAdv_no_later_than_Num_calendar_days_after_the_day_UDS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GCond x1 x2,GCond y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GCondGiven x1 x2 x3,GCondGiven y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GCondStandalone x1,GCondStandalone y1) -> and [ x1 == y1 ]
    (GCondTemporal x1 x2 x3,GCondTemporal y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GCondUpon x1 x2 x3,GCondUpon y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GGiven x1 x2,GGiven y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GGivenStandalone x1,GGivenStandalone y1) -> and [ x1 == y1 ]
    (GHornClause2 x1 x2,GHornClause2 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GMeans x1 x2,GMeans y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRPelem x1 x2,GRPelem y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRPeq x1 x2,GRPeq y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRPgt x1 x2,GRPgt y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRPgte x1 x2,GRPgte y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRPis x1 x2,GRPis y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRPlt x1 x2,GRPlt y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRPlte x1 x2,GRPlte y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRPnotElem x1 x2,GRPnotElem y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GTemporal x1 x2,GTemporal y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GTemporalStandalone x1,GTemporalStandalone y1) -> and [ x1 == y1 ]
    (GUDS2Fragment x1,GUDS2Fragment y1) -> and [ x1 == y1 ]
    (GUpon x1 x2,GUpon y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GUponStandalone x1,GUponStandalone y1) -> and [ x1 == y1 ]
    (GsubjAction x1 x2,GsubjAction y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GDMay x1,GDMay y1) -> and [ x1 == y1 ]
    (GDMust x1,GDMust y1) -> and [ x1 == y1 ]
    (GDShant x1,GDShant y1) -> and [ x1 == y1 ]
    (Groot_acl x1 x2,Groot_acl y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_aclRelcl x1 x2,Groot_aclRelcl y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_aclRelcl_nmod x1 x2 x3,Groot_aclRelcl_nmod y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_acl_nmod x1 x2 x3,Groot_acl_nmod y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_advcl x1 x2,Groot_advcl y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_advcl_nsubjPass_auxPass x1 x2 x3 x4,Groot_advcl_nsubjPass_auxPass y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_advcl_nsubjPass_aux_auxPass_xcomp x1 x2 x3 x4 x5 x6,Groot_advcl_nsubjPass_aux_auxPass_xcomp y1 y2 y3 y4 y5 y6) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 ]
    (Groot_advcl_nsubj_aux_advcl x1 x2 x3 x4 x5,Groot_advcl_nsubj_aux_advcl y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_advcl_nsubj_aux_advmod_obj x1 x2 x3 x4 x5 x6,Groot_advcl_nsubj_aux_advmod_obj y1 y2 y3 y4 y5 y6) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 ]
    (Groot_advcl_nsubj_aux_ccomp x1 x2 x3 x4 x5,Groot_advcl_nsubj_aux_ccomp y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_advcl_nsubj_aux_obl_obj x1 x2 x3 x4 x5 x6,Groot_advcl_nsubj_aux_obl_obj y1 y2 y3 y4 y5 y6) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 ]
    (Groot_advcl_nsubj_cop x1 x2 x3 x4,Groot_advcl_nsubj_cop y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_advcl_nsubj_cop_case_amod_nmod x1 x2 x3 x4 x5 x6 x7,Groot_advcl_nsubj_cop_case_amod_nmod y1 y2 y3 y4 y5 y6 y7) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 , x7 == y7 ]
    (Groot_advcl_nsubj_cop_nsubj x1 x2 x3 x4 x5,Groot_advcl_nsubj_cop_nsubj y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_advcl_nsubj_xcomp x1 x2 x3 x4,Groot_advcl_nsubj_xcomp y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_advmod x1 x2,Groot_advmod y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_advmod_advcl x1 x2 x3,Groot_advmod_advcl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_advmod_advmod_obl x1 x2 x3 x4,Groot_advmod_advmod_obl y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_advmod_amod x1 x2 x3,Groot_advmod_amod y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_advmod_nsubj_aux_advmod_obj x1 x2 x3 x4 x5 x6,Groot_advmod_nsubj_aux_advmod_obj y1 y2 y3 y4 y5 y6) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 ]
    (Groot_advmod_nsubj_cop x1 x2 x3 x4,Groot_advmod_nsubj_cop y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_advmod_nsubj_cop_obl x1 x2 x3 x4 x5,Groot_advmod_nsubj_cop_obl y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_advmod_xcomp x1 x2 x3,Groot_advmod_xcomp y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_amod_nmod x1 x2 x3,Groot_amod_nmod y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_appos x1 x2,Groot_appos y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_appos_advmod x1 x2 x3,Groot_appos_advmod y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_auxPass x1 x2,Groot_auxPass y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_case x1 x2,Groot_case y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_case_amod x1 x2 x3,Groot_case_amod y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_case_amod_amod x1 x2 x3 x4,Groot_case_amod_amod y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_case_compound x1 x2 x3,Groot_case_compound y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_case_nummod x1 x2 x3,Groot_case_nummod y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_case_nummod_acl x1 x2 x3 x4,Groot_case_nummod_acl y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_case_nummod_nummod x1 x2 x3 x4,Groot_case_nummod_nummod y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_cc x1 x2,Groot_cc y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_cc_cop_xcomp x1 x2 x3 x4,Groot_cc_cop_xcomp y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_cc_nmod x1 x2 x3,Groot_cc_nmod y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_cc_obj x1 x2 x3,Groot_cc_obj y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_ccomp x1 x2,Groot_ccomp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_compound x1 x2,Groot_compound y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_compoundPrt_compoundPrt x1 x2 x3,Groot_compoundPrt_compoundPrt y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_compound_acl x1 x2 x3,Groot_compound_acl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_compound_amod x1 x2 x3,Groot_compound_amod y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_compound_appos x1 x2 x3,Groot_compound_appos y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_compound_compound x1 x2 x3,Groot_compound_compound y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_compound_compound_appos x1 x2 x3 x4,Groot_compound_compound_appos y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_compound_flat x1 x2 x3,Groot_compound_flat y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_cop x1 x2,Groot_cop y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_cop_advmod x1 x2 x3,Groot_cop_advmod y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_csubj x1 x2,Groot_csubj y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_csubj_aux_aux x1 x2 x3 x4,Groot_csubj_aux_aux y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_discourse x1 x2,Groot_discourse y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_expl_cop_csubj x1 x2 x3 x4,Groot_expl_cop_csubj y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_fixed x1 x2,Groot_fixed y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_goeswith x1 x2,Groot_goeswith y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_goeswith_goeswith x1 x2 x3,Groot_goeswith_goeswith y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_mark x1 x2,Groot_mark y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_mark_cc_mark_obj x1 x2 x3 x4 x5,Groot_mark_cc_mark_obj y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_mark_expl_cop_xcomp x1 x2 x3 x4 x5,Groot_mark_expl_cop_xcomp y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_mark_expl_nsubj x1 x2 x3 x4,Groot_mark_expl_nsubj y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_mark_nsubj x1 x2 x3,Groot_mark_nsubj y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_mark_nsubjPass_auxPass x1 x2 x3 x4,Groot_mark_nsubjPass_auxPass y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_mark_nsubjPass_auxPass_obl x1 x2 x3 x4 x5,Groot_mark_nsubjPass_auxPass_obl y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_mark_nsubj_aux x1 x2 x3 x4,Groot_mark_nsubj_aux y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_mark_nsubj_aux_advmod_obj x1 x2 x3 x4 x5 x6,Groot_mark_nsubj_aux_advmod_obj y1 y2 y3 y4 y5 y6) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 ]
    (Groot_mark_nsubj_aux_aux x1 x2 x3 x4 x5,Groot_mark_nsubj_aux_aux y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_mark_nsubj_cop x1 x2 x3 x4,Groot_mark_nsubj_cop y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_mark_nsubj_cop_obl x1 x2 x3 x4 x5,Groot_mark_nsubj_cop_obl y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_mark_nsubj_nsubj_xcomp x1 x2 x3 x4 x5,Groot_mark_nsubj_nsubj_xcomp y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_mark_nsubj_obj x1 x2 x3 x4,Groot_mark_nsubj_obj y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_mark_nsubj_obl x1 x2 x3 x4,Groot_mark_nsubj_obl y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_mark_nsubj_xcomp x1 x2 x3 x4,Groot_mark_nsubj_xcomp y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_mark_nummod x1 x2 x3,Groot_mark_nummod y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_nmod x1 x2,Groot_nmod y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_nmod_acl x1 x2 x3,Groot_nmod_acl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_nmod_aclRelcl x1 x2 x3,Groot_nmod_aclRelcl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_nmod_nmod x1 x2 x3,Groot_nmod_nmod y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_nsubj x1 x2,Groot_nsubj y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_nsubjPass_auxPass x1 x2 x3,Groot_nsubjPass_auxPass y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_nsubjPass_auxPass_advmod x1 x2 x3 x4,Groot_nsubjPass_auxPass_advmod y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubjPass_auxPass_advmod_advcl x1 x2 x3 x4 x5,Groot_nsubjPass_auxPass_advmod_advcl y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_nsubjPass_auxPass_advmod_xcomp x1 x2 x3 x4 x5,Groot_nsubjPass_auxPass_advmod_xcomp y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_nsubjPass_auxPass_xcomp x1 x2 x3 x4,Groot_nsubjPass_auxPass_xcomp y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubjPass_aux_auxPass x1 x2 x3 x4,Groot_nsubjPass_aux_auxPass y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubjPass_aux_auxPass_advcl_advcl x1 x2 x3 x4 x5 x6,Groot_nsubjPass_aux_auxPass_advcl_advcl y1 y2 y3 y4 y5 y6) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 ]
    (Groot_nsubjPass_aux_auxPass_advmod x1 x2 x3 x4 x5,Groot_nsubjPass_aux_auxPass_advmod y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_nsubjPass_aux_auxPass_obl x1 x2 x3 x4 x5,Groot_nsubjPass_aux_auxPass_obl y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_nsubjPass_aux_auxPass_obl_advmod x1 x2 x3 x4 x5 x6,Groot_nsubjPass_aux_auxPass_obl_advmod y1 y2 y3 y4 y5 y6) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 ]
    (Groot_nsubjPass_aux_auxPass_obl_obl_advcl x1 x2 x3 x4 x5 x6 x7,Groot_nsubjPass_aux_auxPass_obl_obl_advcl y1 y2 y3 y4 y5 y6 y7) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 , x7 == y7 ]
    (Groot_nsubjPass_aux_auxPass_obl_obl_advmod x1 x2 x3 x4 x5 x6 x7,Groot_nsubjPass_aux_auxPass_obl_obl_advmod y1 y2 y3 y4 y5 y6 y7) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 , x7 == y7 ]
    (Groot_nsubjPass_aux_auxPass_xcomp x1 x2 x3 x4 x5,Groot_nsubjPass_aux_auxPass_xcomp y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_nsubj_advmod x1 x2 x3,Groot_nsubj_advmod y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_nsubj_advmod_obj x1 x2 x3 x4,Groot_nsubj_advmod_obj y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubj_aux x1 x2 x3,Groot_nsubj_aux y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_nsubj_aux_aclRelcl x1 x2 x3 x4,Groot_nsubj_aux_aclRelcl y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubj_aux_aclRelcl_obl x1 x2 x3 x4 x5,Groot_nsubj_aux_aclRelcl_obl y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_nsubj_aux_advmod x1 x2 x3 x4,Groot_nsubj_aux_advmod y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubj_aux_advmod_obj_advcl x1 x2 x3 x4 x5 x6,Groot_nsubj_aux_advmod_obj_advcl y1 y2 y3 y4 y5 y6) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 ]
    (Groot_nsubj_aux_aux x1 x2 x3 x4,Groot_nsubj_aux_aux y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubj_aux_cop_nmod x1 x2 x3 x4 x5,Groot_nsubj_aux_cop_nmod y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_nsubj_aux_obj x1 x2 x3 x4,Groot_nsubj_aux_obj y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubj_aux_obj_obl x1 x2 x3 x4 x5,Groot_nsubj_aux_obj_obl y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_nsubj_aux_obj_obl_advmod_advcl x1 x2 x3 x4 x5 x6 x7,Groot_nsubj_aux_obj_obl_advmod_advcl y1 y2 y3 y4 y5 y6 y7) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 , x7 == y7 ]
    (Groot_nsubj_aux_obj_obl_obl x1 x2 x3 x4 x5 x6,Groot_nsubj_aux_obj_obl_obl y1 y2 y3 y4 y5 y6) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 ]
    (Groot_nsubj_aux_obl x1 x2 x3 x4,Groot_nsubj_aux_obl y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubj_ccomp x1 x2 x3,Groot_nsubj_ccomp y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_nsubj_cop x1 x2 x3,Groot_nsubj_cop y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_nsubj_cop_aclRelcl x1 x2 x3 x4,Groot_nsubj_cop_aclRelcl y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubj_cop_aclRelcl_obl x1 x2 x3 x4 x5,Groot_nsubj_cop_aclRelcl_obl y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_nsubj_cop_advcl x1 x2 x3 x4,Groot_nsubj_cop_advcl y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubj_cop_advmod x1 x2 x3 x4,Groot_nsubj_cop_advmod y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubj_cop_case_nmod_acl x1 x2 x3 x4 x5 x6,Groot_nsubj_cop_case_nmod_acl y1 y2 y3 y4 y5 y6) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 ]
    (Groot_nsubj_cop_nmod x1 x2 x3 x4,Groot_nsubj_cop_nmod y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubj_cop_nmodPoss x1 x2 x3 x4,Groot_nsubj_cop_nmodPoss y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubj_cop_obl x1 x2 x3 x4,Groot_nsubj_cop_obl y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubj_cop_obl_parataxis x1 x2 x3 x4 x5,Groot_nsubj_cop_obl_parataxis y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_nsubj_nsubj_aux_advmod_obj x1 x2 x3 x4 x5 x6,Groot_nsubj_nsubj_aux_advmod_obj y1 y2 y3 y4 y5 y6) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 ]
    (Groot_nsubj_obj x1 x2 x3,Groot_nsubj_obj y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_nsubj_obj_advcl x1 x2 x3 x4,Groot_nsubj_obj_advcl y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubj_obj_xcomp x1 x2 x3 x4,Groot_nsubj_obj_xcomp y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubj_obl x1 x2 x3,Groot_nsubj_obl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_nsubj_obl_obl x1 x2 x3 x4,Groot_nsubj_obl_obl y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nsubj_xcomp x1 x2 x3,Groot_nsubj_xcomp y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_nummod x1 x2,Groot_nummod y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_nummod_appos x1 x2 x3,Groot_nummod_appos y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_nummod_auxPass_cc_aux_auxPass_obl_obl x1 x2 x3 x4 x5 x6 x7 x8,Groot_nummod_auxPass_cc_aux_auxPass_obl_obl y1 y2 y3 y4 y5 y6 y7 y8) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 , x7 == y7 , x8 == y8 ]
    (Groot_nummod_mark_obj x1 x2 x3 x4,Groot_nummod_mark_obj y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_nummod_mark_obj_cc x1 x2 x3 x4 x5,Groot_nummod_mark_obj_cc y1 y2 y3 y4 y5) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 ]
    (Groot_nummod_nmod x1 x2 x3,Groot_nummod_nmod y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_nummod_nsubjPass_nsubjPass_auxPass_cc x1 x2 x3 x4 x5 x6,Groot_nummod_nsubjPass_nsubjPass_auxPass_cc y1 y2 y3 y4 y5 y6) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 , x5 == y5 , x6 == y6 ]
    (Groot_nummod_obl x1 x2 x3,Groot_nummod_obl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_nummod_obl_cc x1 x2 x3 x4,Groot_nummod_obl_cc y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_obj x1 x2,Groot_obj y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_obj_ccomp x1 x2 x3,Groot_obj_ccomp y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_obj_nmod x1 x2 x3,Groot_obj_nmod y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_obl x1 x2,Groot_obl y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_obl_appos x1 x2 x3,Groot_obl_appos y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_obl_aux x1 x2 x3,Groot_obl_aux y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_obl_case x1 x2 x3,Groot_obl_case y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_obl_obj x1 x2 x3,Groot_obl_obj y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_obl_obl x1 x2 x3,Groot_obl_obl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_obl_obl_obl_cc x1 x2 x3 x4,Groot_obl_obl_obl_cc y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (Groot_obl_xcomp x1 x2 x3,Groot_obl_xcomp y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (Groot_only x1,Groot_only y1) -> and [ x1 == y1 ]
    (Groot_parataxis x1 x2,Groot_parataxis y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_xcomp x1 x2,Groot_xcomp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Groot_xcomp_ccomp x1 x2 x3,Groot_xcomp_ccomp y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (LexV x,LexV y) -> x == y
    (GAdVVP x1 x2,GAdVVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAdvVP x1 x2,GAdvVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GComplV x1 x2,GComplV y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GComplVP x1 x2,GComplVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GConjVP x1 x2,GConjVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPassV x1,GPassV y1) -> and [ x1 == y1 ]
    (GPassVAgent x1 x2,GPassVAgent y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPrepVP x1 x2,GPrepVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GProgrVP x1,GProgrVP y1) -> and [ x1 == y1 ]
    (GUseComp x1,GUseComp y1) -> and [ x1 == y1 ]
    (GUseV x1,GUseV y1) -> and [ x1 == y1 ]
    (GaclUDS_ x1,GaclUDS_ y1) -> and [ x1 == y1 ]
    (GaclUDSgerund_ x1,GaclUDSgerund_ y1) -> and [ x1 == y1 ]
    (GaclUDSpastpartParens_ x1,GaclUDSpastpartParens_ y1) -> and [ x1 == y1 ]
    (GaclUDSpastpart_ x1,GaclUDSpastpart_ y1) -> and [ x1 == y1 ]
    (Gacl_ x1,Gacl_ y1) -> and [ x1 == y1 ]
    (GaclRelclRS_ x1,GaclRelclRS_ y1) -> and [ x1 == y1 ]
    (GaclRelclUDS_ x1,GaclRelclUDS_ y1) -> and [ x1 == y1 ]
    (GpassRelcl_ x1 x2 x3,GpassRelcl_ y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GadvclMarkUDS_ x1 x2,GadvclMarkUDS_ y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GadvclUDS_ x1,GadvclUDS_ y1) -> and [ x1 == y1 ]
    (Gadvcl_ x1,Gadvcl_ y1) -> and [ x1 == y1 ]
    (Gadvmod_ x1,Gadvmod_ y1) -> and [ x1 == y1 ]
    (Gnot_advmod,Gnot_advmod) -> and [ ]
    (GadvmodEmph_ x1,GadvmodEmph_ y1) -> and [ x1 == y1 ]
    (GadvmodLmod_ x1,GadvmodLmod_ y1) -> and [ x1 == y1 ]
    (Gamod_ x1,Gamod_ y1) -> and [ x1 == y1 ]
    (Gappos_ x1,Gappos_ y1) -> and [ x1 == y1 ]
    (Gaux_ x1,Gaux_ y1) -> and [ x1 == y1 ]
    (Gbe_aux,Gbe_aux) -> and [ ]
    (Gcan_aux,Gcan_aux) -> and [ ]
    (Ghave_aux,Ghave_aux) -> and [ ]
    (Gmay_aux,Gmay_aux) -> and [ ]
    (Gmust_aux,Gmust_aux) -> and [ ]
    (Gshall_aux,Gshall_aux) -> and [ ]
    (Gshould_aux,Gshould_aux) -> and [ ]
    (Gwill_aux,Gwill_aux) -> and [ ]
    (Gbe_auxPass,Gbe_auxPass) -> and [ ]
    (Gcc_ x1,Gcc_ y1) -> and [ x1 == y1 ]
    (GccPreconj_ x1,GccPreconj_ y1) -> and [ x1 == y1 ]
    (Gccomp_ x1,Gccomp_ y1) -> and [ x1 == y1 ]
    (Gclf_ x1,Gclf_ y1) -> and [ x1 == y1 ]
    (Gcompound_ x1,Gcompound_ y1) -> and [ x1 == y1 ]
    (GcompoundLvc_ x1,GcompoundLvc_ y1) -> and [ x1 == y1 ]
    (GcompoundPrt_ x1,GcompoundPrt_ y1) -> and [ x1 == y1 ]
    (GcompoundRedup_ x1,GcompoundRedup_ y1) -> and [ x1 == y1 ]
    (GcompoundSvc_ x1,GcompoundSvc_ y1) -> and [ x1 == y1 ]
    (Gbe_cop,Gbe_cop) -> and [ ]
    (Gis_cop,Gis_cop) -> and [ ]
    (GcsubjMarkFinite_ x1 x2,GcsubjMarkFinite_ y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GcsubjMarkInfinite_ x1 x2,GcsubjMarkInfinite_ y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Gcsubj_ x1,Gcsubj_ y1) -> and [ x1 == y1 ]
    (GcsubjPass_ x1,GcsubjPass_ y1) -> and [ x1 == y1 ]
    (Gdep_ x1,Gdep_ y1) -> and [ x1 == y1 ]
    (Gdet_ x1,Gdet_ y1) -> and [ x1 == y1 ]
    (GdetNumgov_ x1,GdetNumgov_ y1) -> and [ x1 == y1 ]
    (GdetNummod_ x1,GdetNummod_ y1) -> and [ x1 == y1 ]
    (GdetPoss_ x1,GdetPoss_ y1) -> and [ x1 == y1 ]
    (Gdiscourse_ x1,Gdiscourse_ y1) -> and [ x1 == y1 ]
    (Gdislocated_ x1,Gdislocated_ y1) -> and [ x1 == y1 ]
    (Gexpl_ x1,Gexpl_ y1) -> and [ x1 == y1 ]
    (Git_expl,Git_expl) -> and [ ]
    (GexplImpers_ x1,GexplImpers_ y1) -> and [ x1 == y1 ]
    (GexplPass_ x1,GexplPass_ y1) -> and [ x1 == y1 ]
    (GexplPv_ x1,GexplPv_ y1) -> and [ x1 == y1 ]
    (Gfixed_ x1,Gfixed_ y1) -> and [ x1 == y1 ]
    (Gflat_ x1,Gflat_ y1) -> and [ x1 == y1 ]
    (GflatForeign_ x1,GflatForeign_ y1) -> and [ x1 == y1 ]
    (GflatName_ x1,GflatName_ y1) -> and [ x1 == y1 ]
    (Ggoeswith_ x1,Ggoeswith_ y1) -> and [ x1 == y1 ]
    (Giobj_ x1,Giobj_ y1) -> and [ x1 == y1 ]
    (Glist_ x1,Glist_ y1) -> and [ x1 == y1 ]
    (Gmark_ x1,Gmark_ y1) -> and [ x1 == y1 ]
    (Gto_mark,Gto_mark) -> and [ ]
    (Gnmod_ x1 x2,Gnmod_ y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GnmodPoss_ x1,GnmodPoss_ y1) -> and [ x1 == y1 ]
    (GnmodTmod_ x1,GnmodTmod_ y1) -> and [ x1 == y1 ]
    (Gnsubj_ x1,Gnsubj_ y1) -> and [ x1 == y1 ]
    (GnsubjPass_ x1,GnsubjPass_ y1) -> and [ x1 == y1 ]
    (Gnummod_ x1,Gnummod_ y1) -> and [ x1 == y1 ]
    (GnummodGov_ x1,GnummodGov_ y1) -> and [ x1 == y1 ]
    (Gobj_ x1,Gobj_ y1) -> and [ x1 == y1 ]
    (GoblPrep_ x1,GoblPrep_ y1) -> and [ x1 == y1 ]
    (GoblRP_ x1 x2,GoblRP_ y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Gobl_ x1,Gobl_ y1) -> and [ x1 == y1 ]
    (GoblAgent_ x1,GoblAgent_ y1) -> and [ x1 == y1 ]
    (GoblArg_ x1,GoblArg_ y1) -> and [ x1 == y1 ]
    (GoblLmod_ x1,GoblLmod_ y1) -> and [ x1 == y1 ]
    (GoblTmod_ x1,GoblTmod_ y1) -> and [ x1 == y1 ]
    (Gorphan_ x1,Gorphan_ y1) -> and [ x1 == y1 ]
    (Gparataxis_ x1,Gparataxis_ y1) -> and [ x1 == y1 ]
    (Gpunct_ x1,Gpunct_ y1) -> and [ x1 == y1 ]
    (Greparandum_ x1,Greparandum_ y1) -> and [ x1 == y1 ]
    (GrootA_ x1,GrootA_ y1) -> and [ x1 == y1 ]
    (GrootAdA_ x1,GrootAdA_ y1) -> and [ x1 == y1 ]
    (GrootAdv_ x1,GrootAdv_ y1) -> and [ x1 == y1 ]
    (GrootDAP_ x1,GrootDAP_ y1) -> and [ x1 == y1 ]
    (GrootDet_ x1,GrootDet_ y1) -> and [ x1 == y1 ]
    (GrootN_ x1,GrootN_ y1) -> and [ x1 == y1 ]
    (GrootPrep_ x1,GrootPrep_ y1) -> and [ x1 == y1 ]
    (GrootQuant_ x1,GrootQuant_ y1) -> and [ x1 == y1 ]
    (GrootRP_ x1,GrootRP_ y1) -> and [ x1 == y1 ]
    (GrootV_ x1,GrootV_ y1) -> and [ x1 == y1 ]
    (Gvocative_ x1,Gvocative_ y1) -> and [ x1 == y1 ]
    (GxcompA_ x1,GxcompA_ y1) -> and [ x1 == y1 ]
    (GxcompA_ccomp_ x1 x2,GxcompA_ccomp_ y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GxcompAdv_ x1,GxcompAdv_ y1) -> and [ x1 == y1 ]
    (GxcompN_ x1,GxcompN_ y1) -> and [ x1 == y1 ]
    (GxcompToBeN_ x1 x2 x3,GxcompToBeN_ y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (LexText x,LexText y) -> x == y
    (LexV2 x,LexV2 y) -> x == y
    (LexV2A x,LexV2A y) -> x == y
    (LexV2Q x,LexV2Q y) -> x == y
    (LexV2S x,LexV2S y) -> x == y
    (LexV2V x,LexV2V y) -> x == y
    (LexV3 x,LexV3 y) -> x == y
    (LexVA x,LexVA y) -> x == y
    (LexVS x,LexVS y) -> x == y
    (LexVV x,LexVV y) -> x == y
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GA where
  gf (GStrA x1) = mkApp (mkCId "StrA") [gf x1]
  gf (LexA x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "StrA" -> GStrA (fg x1)

      Just (i,[]) -> LexA (showCId i)
      _ -> error ("no A " ++ show t)

instance Gf GA2 where
  gf (GMkA2 x1 x2) = mkApp (mkCId "MkA2") [gf x1, gf x2]
  gf (LexA2 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "MkA2" -> GMkA2 (fg x1) (fg x2)

      Just (i,[]) -> LexA2 (showCId i)
      _ -> error ("no A2 " ++ show t)

instance Gf GACard where
  gf (LexACard x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexACard (showCId i)
      _ -> error ("no ACard " ++ show t)

instance Gf GAP where
  gf (GAdAP x1 x2) = mkApp (mkCId "AdAP") [gf x1, gf x2]
  gf (GAdjOrd x1) = mkApp (mkCId "AdjOrd") [gf x1]
  gf (GAdvAP x1 x2) = mkApp (mkCId "AdvAP") [gf x1, gf x2]
  gf (GComplA2 x1 x2) = mkApp (mkCId "ComplA2") [gf x1, gf x2]
  gf (GConjAP x1 x2) = mkApp (mkCId "ConjAP") [gf x1, gf x2]
  gf (GParentheticalAP x1) = mkApp (mkCId "ParentheticalAP") [gf x1]
  gf (GPastPartAP x1) = mkApp (mkCId "PastPartAP") [gf x1]
  gf (GPastPartAgentAP x1 x2) = mkApp (mkCId "PastPartAgentAP") [gf x1, gf x2]
  gf (GPositA x1) = mkApp (mkCId "PositA") [gf x1]
  gf (GPresPartAP x1) = mkApp (mkCId "PresPartAP") [gf x1]
  gf (GStrAP x1) = mkApp (mkCId "StrAP") [gf x1]
  gf (GUseComparA x1) = mkApp (mkCId "UseComparA") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdAP" -> GAdAP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "AdjOrd" -> GAdjOrd (fg x1)
      Just (i,[x1,x2]) | i == mkCId "AdvAP" -> GAdvAP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplA2" -> GComplA2 (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjAP" -> GConjAP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ParentheticalAP" -> GParentheticalAP (fg x1)
      Just (i,[x1]) | i == mkCId "PastPartAP" -> GPastPartAP (fg x1)
      Just (i,[x1,x2]) | i == mkCId "PastPartAgentAP" -> GPastPartAgentAP (fg x1) (fg x2)
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
  gf (GAdvAdv x1 x2) = mkApp (mkCId "AdvAdv") [gf x1, gf x2]
  gf (GComparAdvAdj x1 x2 x3) = mkApp (mkCId "ComparAdvAdj") [gf x1, gf x2, gf x3]
  gf (GComparAdvAdjS x1 x2 x3) = mkApp (mkCId "ComparAdvAdjS") [gf x1, gf x2, gf x3]
  gf (GConjAdv x1 x2) = mkApp (mkCId "ConjAdv") [gf x1, gf x2]
  gf (GGerundAdv x1) = mkApp (mkCId "GerundAdv") [gf x1]
  gf (GPositAdvAdj x1) = mkApp (mkCId "PositAdvAdj") [gf x1]
  gf (GPrepNP x1 x2) = mkApp (mkCId "PrepNP") [gf x1, gf x2]
  gf (GSubjS x1 x2) = mkApp (mkCId "SubjS") [gf x1, gf x2]
  gf (Gacl2Adv x1) = mkApp (mkCId "acl2Adv") [gf x1]
  gf (Gadvcl2Adv x1) = mkApp (mkCId "advcl2Adv") [gf x1]
  gf (Gcsubj2Adv x1) = mkApp (mkCId "csubj2Adv") [gf x1]
  gf (Gxcomp2Adv x1) = mkApp (mkCId "xcomp2Adv") [gf x1]
  gf (LexAdv x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvAdv" -> GAdvAdv (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "ComparAdvAdj" -> GComparAdvAdj (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "ComparAdvAdjS" -> GComparAdvAdjS (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "ConjAdv" -> GConjAdv (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "GerundAdv" -> GGerundAdv (fg x1)
      Just (i,[x1]) | i == mkCId "PositAdvAdj" -> GPositAdvAdj (fg x1)
      Just (i,[x1,x2]) | i == mkCId "PrepNP" -> GPrepNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SubjS" -> GSubjS (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "acl2Adv" -> Gacl2Adv (fg x1)
      Just (i,[x1]) | i == mkCId "advcl2Adv" -> Gadvcl2Adv (fg x1)
      Just (i,[x1]) | i == mkCId "csubj2Adv" -> Gcsubj2Adv (fg x1)
      Just (i,[x1]) | i == mkCId "xcomp2Adv" -> Gxcomp2Adv (fg x1)

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
  gf (GApposCN x1 x2) = mkApp (mkCId "ApposCN") [gf x1, gf x2]
  gf (GCN_AP_Conj_CNs_of_NP x1 x2 x3 x4) = mkApp (mkCId "CN_AP_Conj_CNs_of_NP") [gf x1, gf x2, gf x3, gf x4]
  gf (GComplN2 x1 x2) = mkApp (mkCId "ComplN2") [gf x1, gf x2]
  gf (GConjCN x1 x2) = mkApp (mkCId "ConjCN") [gf x1, gf x2]
  gf (GGerundCN x1) = mkApp (mkCId "GerundCN") [gf x1]
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
      Just (i,[x1,x2]) | i == mkCId "ApposCN" -> GApposCN (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "CN_AP_Conj_CNs_of_NP" -> GCN_AP_Conj_CNs_of_NP (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "ComplN2" -> GComplN2 (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjCN" -> GConjCN (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "GerundCN" -> GGerundCN (fg x1)
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

instance Gf GCl where
  gf (GExistCN x1) = mkApp (mkCId "ExistCN") [gf x1]
  gf (GExistsNP x1) = mkApp (mkCId "ExistsNP") [gf x1]
  gf (GGenericCl x1) = mkApp (mkCId "GenericCl") [gf x1]
  gf (GImpersCl x1) = mkApp (mkCId "ImpersCl") [gf x1]
  gf (GPredSCVP x1 x2) = mkApp (mkCId "PredSCVP") [gf x1, gf x2]
  gf (GPredVP x1 x2) = mkApp (mkCId "PredVP") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "ExistCN" -> GExistCN (fg x1)
      Just (i,[x1]) | i == mkCId "ExistsNP" -> GExistsNP (fg x1)
      Just (i,[x1]) | i == mkCId "GenericCl" -> GGenericCl (fg x1)
      Just (i,[x1]) | i == mkCId "ImpersCl" -> GImpersCl (fg x1)
      Just (i,[x1,x2]) | i == mkCId "PredSCVP" -> GPredSCVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PredVP" -> GPredVP (fg x1) (fg x2)


      _ -> error ("no Cl " ++ show t)

instance Gf GClSlash where
  gf (GAdvSlash x1 x2) = mkApp (mkCId "AdvSlash") [gf x1, gf x2]
  gf (GSlashCl x1) = mkApp (mkCId "SlashCl") [gf x1]
  gf (GSlashPrep x1 x2) = mkApp (mkCId "SlashPrep") [gf x1, gf x2]
  gf (GSlashVP x1 x2) = mkApp (mkCId "SlashVP") [gf x1, gf x2]
  gf (GSlashVS x1 x2 x3) = mkApp (mkCId "SlashVS") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvSlash" -> GAdvSlash (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "SlashCl" -> GSlashCl (fg x1)
      Just (i,[x1,x2]) | i == mkCId "SlashPrep" -> GSlashPrep (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SlashVP" -> GSlashVP (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "SlashVS" -> GSlashVS (fg x1) (fg x2) (fg x3)


      _ -> error ("no ClSlash " ++ show t)

instance Gf GComp where
  gf (GCompAP x1) = mkApp (mkCId "CompAP") [gf x1]
  gf (GCompAdv x1) = mkApp (mkCId "CompAdv") [gf x1]
  gf (GCompNP x1) = mkApp (mkCId "CompNP") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "CompAP" -> GCompAP (fg x1)
      Just (i,[x1]) | i == mkCId "CompAdv" -> GCompAdv (fg x1)
      Just (i,[x1]) | i == mkCId "CompNP" -> GCompNP (fg x1)


      _ -> error ("no Comp " ++ show t)

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
  gf (GACard2Det x1) = mkApp (mkCId "ACard2Det") [gf x1]
  gf (GConjDet x1 x2) = mkApp (mkCId "ConjDet") [gf x1, gf x2]
  gf (GDetQuant x1 x2) = mkApp (mkCId "DetQuant") [gf x1, gf x2]
  gf (GDetQuantOrd x1 x2 x3) = mkApp (mkCId "DetQuantOrd") [gf x1, gf x2, gf x3]
  gf (GXorMore x1) = mkApp (mkCId "XorMore") [gf x1]
  gf (LexDet x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "ACard2Det" -> GACard2Det (fg x1)
      Just (i,[x1,x2]) | i == mkCId "ConjDet" -> GConjDet (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "DetQuant" -> GDetQuant (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "DetQuantOrd" -> GDetQuantOrd (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "XorMore" -> GXorMore (fg x1)

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

instance Gf GGen where
  gf Gs_Gen = mkApp (mkCId "'\'s_Gen'") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "'\'s_Gen'" -> Gs_Gen 


      _ -> error ("no Gen " ++ show t)

instance Gf GIAdv where
  gf (GAdvIAdv x1 x2) = mkApp (mkCId "AdvIAdv") [gf x1, gf x2]
  gf (GConjIAdv x1 x2) = mkApp (mkCId "ConjIAdv") [gf x1, gf x2]
  gf (GIAdvAdv x1) = mkApp (mkCId "IAdvAdv") [gf x1]
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
      Just (i,[x1]) | i == mkCId "IAdvAdv" -> GIAdvAdv (fg x1)
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
  gf (GICompAP x1) = mkApp (mkCId "ICompAP") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "CompIAdv" -> GCompIAdv (fg x1)
      Just (i,[x1]) | i == mkCId "CompIP" -> GCompIP (fg x1)
      Just (i,[x1]) | i == mkCId "ICompAP" -> GICompAP (fg x1)


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
  gf (GAdvImp x1 x2) = mkApp (mkCId "AdvImp") [gf x1, gf x2]
  gf (GImpVP x1) = mkApp (mkCId "ImpVP") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvImp" -> GAdvImp (fg x1) (fg x2)
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

instance Gf GListPrep where
  gf (GListPrep [x1,x2]) = mkApp (mkCId "BasePrep") [gf x1, gf x2]
  gf (GListPrep (x:xs)) = mkApp (mkCId "ConsPrep") [gf x, gf (GListPrep xs)]
  fg t =
    GListPrep (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BasePrep" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsPrep" -> fg x1 : fgs x2


      _ -> error ("no ListPrep " ++ show t)

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

instance Gf GListUDFragment where
  gf (GListUDFragment [x1,x2]) = mkApp (mkCId "BaseUDFragment") [gf x1, gf x2]
  gf (GListUDFragment (x:xs)) = mkApp (mkCId "ConsUDFragment") [gf x, gf (GListUDFragment xs)]
  fg t =
    GListUDFragment (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseUDFragment" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsUDFragment" -> fg x1 : fgs x2


      _ -> error ("no ListUDFragment " ++ show t)

instance Gf GListUDS where
  gf (GListUDS [x1,x2]) = mkApp (mkCId "BaseUDS") [gf x1, gf x2]
  gf (GListUDS (x:xs)) = mkApp (mkCId "ConsUDS") [gf x, gf (GListUDS xs)]
  fg t =
    GListUDS (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseUDS" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsUDS" -> fg x1 : fgs x2


      _ -> error ("no ListUDS " ++ show t)

instance Gf GListVP where
  gf (GListVP [x1,x2]) = mkApp (mkCId "BaseVP") [gf x1, gf x2]
  gf (GListVP (x:xs)) = mkApp (mkCId "ConsVP") [gf x, gf (GListVP xs)]
  fg t =
    GListVP (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseVP" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsVP" -> fg x1 : fgs x2


      _ -> error ("no ListVP " ++ show t)

instance Gf GN where
  gf (GCompoundCN x1 x2) = mkApp (mkCId "CompoundCN") [gf x1, gf x2]
  gf (GCompoundN x1 x2) = mkApp (mkCId "CompoundN") [gf x1, gf x2]
  gf (GStrN x1) = mkApp (mkCId "StrN") [gf x1]
  gf (LexN x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "CompoundCN" -> GCompoundCN (fg x1) (fg x2)
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

instance Gf GN3 where
  gf (GMkN3 x1 x2 x3) = mkApp (mkCId "MkN3") [gf x1, gf x2, gf x3]
  gf (LexN3 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "MkN3" -> GMkN3 (fg x1) (fg x2) (fg x3)

      Just (i,[]) -> LexN3 (showCId i)
      _ -> error ("no N3 " ++ show t)

instance Gf GNP where
  gf (GAdjAsNP x1) = mkApp (mkCId "AdjAsNP") [gf x1]
  gf (GAdvNP x1 x2) = mkApp (mkCId "AdvNP") [gf x1, gf x2]
  gf (GApposNP x1 x2) = mkApp (mkCId "ApposNP") [gf x1, gf x2]
  gf (GConjNP x1 x2) = mkApp (mkCId "ConjNP") [gf x1, gf x2]
  gf (GDefPN x1) = mkApp (mkCId "DefPN") [gf x1]
  gf (GDetCN x1 x2) = mkApp (mkCId "DetCN") [gf x1, gf x2]
  gf (GDetNP x1) = mkApp (mkCId "DetNP") [gf x1]
  gf (GEvery x1) = mkApp (mkCId "Every") [gf x1]
  gf (GExtAdvNP x1 x2) = mkApp (mkCId "ExtAdvNP") [gf x1, gf x2]
  gf (GGenModNP x1 x2 x3) = mkApp (mkCId "GenModNP") [gf x1, gf x2, gf x3]
  gf (GGerundNP x1) = mkApp (mkCId "GerundNP") [gf x1]
  gf (GIndefPN x1) = mkApp (mkCId "IndefPN") [gf x1]
  gf (GMassNP x1) = mkApp (mkCId "MassNP") [gf x1]
  gf (GParty x1) = mkApp (mkCId "Party") [gf x1]
  gf (GPredetNP x1 x2) = mkApp (mkCId "PredetNP") [gf x1, gf x2]
  gf (GRelNP x1 x2) = mkApp (mkCId "RelNP") [gf x1, gf x2]
  gf (GSentNP x1 x2) = mkApp (mkCId "SentNP") [gf x1, gf x2]
  gf GSomeone = mkApp (mkCId "Someone") []
  gf (GTokAll x1) = mkApp (mkCId "TokAll") [gf x1]
  gf (GUsePN x1) = mkApp (mkCId "UsePN") [gf x1]
  gf (GUsePron x1) = mkApp (mkCId "UsePron") [gf x1]
  gf (GWho x1 x2) = mkApp (mkCId "Who") [gf x1, gf x2]
  gf GYou = mkApp (mkCId "You") []
  gf Geuropean_NP = mkApp (mkCId "european_NP") []
  gf Gone_NP = mkApp (mkCId "one_NP") []
  gf Gwhoever_NP = mkApp (mkCId "whoever_NP") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "AdjAsNP" -> GAdjAsNP (fg x1)
      Just (i,[x1,x2]) | i == mkCId "AdvNP" -> GAdvNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ApposNP" -> GApposNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjNP" -> GConjNP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "DefPN" -> GDefPN (fg x1)
      Just (i,[x1,x2]) | i == mkCId "DetCN" -> GDetCN (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "DetNP" -> GDetNP (fg x1)
      Just (i,[x1]) | i == mkCId "Every" -> GEvery (fg x1)
      Just (i,[x1,x2]) | i == mkCId "ExtAdvNP" -> GExtAdvNP (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "GenModNP" -> GGenModNP (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "GerundNP" -> GGerundNP (fg x1)
      Just (i,[x1]) | i == mkCId "IndefPN" -> GIndefPN (fg x1)
      Just (i,[x1]) | i == mkCId "MassNP" -> GMassNP (fg x1)
      Just (i,[x1]) | i == mkCId "Party" -> GParty (fg x1)
      Just (i,[x1,x2]) | i == mkCId "PredetNP" -> GPredetNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RelNP" -> GRelNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SentNP" -> GSentNP (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Someone" -> GSomeone 
      Just (i,[x1]) | i == mkCId "TokAll" -> GTokAll (fg x1)
      Just (i,[x1]) | i == mkCId "UsePN" -> GUsePN (fg x1)
      Just (i,[x1]) | i == mkCId "UsePron" -> GUsePron (fg x1)
      Just (i,[x1,x2]) | i == mkCId "Who" -> GWho (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "You" -> GYou 
      Just (i,[]) | i == mkCId "european_NP" -> Geuropean_NP 
      Just (i,[]) | i == mkCId "one_NP" -> Gone_NP 
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
  gf (GSymbPN x1) = mkApp (mkCId "SymbPN") [gf x1]
  gf (LexPN x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "StrPN" -> GStrPN (fg x1)
      Just (i,[x1]) | i == mkCId "SymbPN" -> GSymbPN (fg x1)

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
  gf (GConjPrep x1 x2) = mkApp (mkCId "ConjPrep") [gf x1, gf x2]
  gf (LexPrep x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjPrep" -> GConjPrep (fg x1) (fg x2)

      Just (i,[]) -> LexPrep (showCId i)
      _ -> error ("no Prep " ++ show t)

instance Gf GPron where
  gf (LexPron x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexPron (showCId i)
      _ -> error ("no Pron " ++ show t)

instance Gf GQCl where
  gf (GPredIAdvVP x1 x2) = mkApp (mkCId "PredIAdvVP") [gf x1, gf x2]
  gf (GQuestCl x1) = mkApp (mkCId "QuestCl") [gf x1]
  gf (GQuestIAdv x1 x2) = mkApp (mkCId "QuestIAdv") [gf x1, gf x2]
  gf (GQuestIComp x1 x2) = mkApp (mkCId "QuestIComp") [gf x1, gf x2]
  gf (GQuestQVP x1 x2) = mkApp (mkCId "QuestQVP") [gf x1, gf x2]
  gf (GQuestSlash x1 x2) = mkApp (mkCId "QuestSlash") [gf x1, gf x2]
  gf (GQuestVP x1 x2) = mkApp (mkCId "QuestVP") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "PredIAdvVP" -> GPredIAdvVP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "QuestCl" -> GQuestCl (fg x1)
      Just (i,[x1,x2]) | i == mkCId "QuestIAdv" -> GQuestIAdv (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QuestIComp" -> GQuestIComp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QuestQVP" -> GQuestQVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QuestSlash" -> GQuestSlash (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QuestVP" -> GQuestVP (fg x1) (fg x2)


      _ -> error ("no QCl " ++ show t)

instance Gf GQS where
  gf (GExistIPQS x1 x2 x3) = mkApp (mkCId "ExistIPQS") [gf x1, gf x2, gf x3]
  gf (GExistNPQS x1 x2 x3) = mkApp (mkCId "ExistNPQS") [gf x1, gf x2, gf x3]
  gf (GUseQCl x1 x2 x3) = mkApp (mkCId "UseQCl") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "ExistIPQS" -> GExistIPQS (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "ExistNPQS" -> GExistNPQS (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "UseQCl" -> GUseQCl (fg x1) (fg x2) (fg x3)


      _ -> error ("no QS " ++ show t)

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
  gf (GGenRP x1 x2) = mkApp (mkCId "GenRP") [gf x1, gf x2]
  gf GIdRP = mkApp (mkCId "IdRP") []
  gf (GPrepRP x1 x2) = mkApp (mkCId "PrepRP") [gf x1, gf x2]
  gf Gthat_RP = mkApp (mkCId "that_RP") []
  gf Gwho_RP = mkApp (mkCId "who_RP") []

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "FunRP" -> GFunRP (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "GenRP" -> GGenRP (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "IdRP" -> GIdRP 
      Just (i,[x1,x2]) | i == mkCId "PrepRP" -> GPrepRP (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "that_RP" -> Gthat_RP 
      Just (i,[]) | i == mkCId "who_RP" -> Gwho_RP 


      _ -> error ("no RP " ++ show t)

instance Gf GRS where
  gf (GConjRS x1 x2) = mkApp (mkCId "ConjRS") [gf x1, gf x2]
  gf (GRS_that_NP_VP x1 x2) = mkApp (mkCId "RS_that_NP_VP") [gf x1, gf x2]
  gf (GUseRCl x1 x2 x3) = mkApp (mkCId "UseRCl") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjRS" -> GConjRS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RS_that_NP_VP" -> GRS_that_NP_VP (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "UseRCl" -> GUseRCl (fg x1) (fg x2) (fg x3)


      _ -> error ("no RS " ++ show t)

instance Gf GS where
  gf (GAdvS x1 x2) = mkApp (mkCId "AdvS") [gf x1, gf x2]
  gf (GConjS x1 x2) = mkApp (mkCId "ConjS") [gf x1, gf x2]
  gf (GExistS x1 x2 x3) = mkApp (mkCId "ExistS") [gf x1, gf x2, gf x3]
  gf (GExtAdvS x1 x2) = mkApp (mkCId "ExtAdvS") [gf x1, gf x2]
  gf (GPostAdvS x1 x2) = mkApp (mkCId "PostAdvS") [gf x1, gf x2]
  gf (GPredVPS x1 x2) = mkApp (mkCId "PredVPS") [gf x1, gf x2]
  gf (GRelS x1 x2) = mkApp (mkCId "RelS") [gf x1, gf x2]
  gf (GSSubjS x1 x2 x3) = mkApp (mkCId "SSubjS") [gf x1, gf x2, gf x3]
  gf (GUseCl x1 x2 x3) = mkApp (mkCId "UseCl") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvS" -> GAdvS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjS" -> GConjS (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "ExistS" -> GExistS (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "ExtAdvS" -> GExtAdvS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PostAdvS" -> GPostAdvS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PredVPS" -> GPredVPS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RelS" -> GRelS (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "SSubjS" -> GSSubjS (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "UseCl" -> GUseCl (fg x1) (fg x2) (fg x3)


      _ -> error ("no S " ++ show t)

instance Gf GSC where
  gf (GEmbedQS x1) = mkApp (mkCId "EmbedQS") [gf x1]
  gf (GEmbedS x1) = mkApp (mkCId "EmbedS") [gf x1]
  gf (GEmbedVP x1) = mkApp (mkCId "EmbedVP") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "EmbedQS" -> GEmbedQS (fg x1)
      Just (i,[x1]) | i == mkCId "EmbedS" -> GEmbedS (fg x1)
      Just (i,[x1]) | i == mkCId "EmbedVP" -> GEmbedVP (fg x1)


      _ -> error ("no SC " ++ show t)

instance Gf GSSlash where
  gf (GUseSlash x1 x2 x3) = mkApp (mkCId "UseSlash") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "UseSlash" -> GUseSlash (fg x1) (fg x2) (fg x3)


      _ -> error ("no SSlash " ++ show t)

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

instance Gf GSymb where
  gf (GStrSymb x1) = mkApp (mkCId "StrSymb") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "StrSymb" -> GStrSymb (fg x1)


      _ -> error ("no Symb " ++ show t)

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
  gf (GAdv_no_later_than_Num_calendar_days_after_the_day_UDS x1 x2) = mkApp (mkCId "Adv_no_later_than_Num_calendar_days_after_the_day_UDS") [gf x1, gf x2]
  gf (GCond x1 x2) = mkApp (mkCId "Cond") [gf x1, gf x2]
  gf (GCondGiven x1 x2 x3) = mkApp (mkCId "CondGiven") [gf x1, gf x2, gf x3]
  gf (GCondStandalone x1) = mkApp (mkCId "CondStandalone") [gf x1]
  gf (GCondTemporal x1 x2 x3) = mkApp (mkCId "CondTemporal") [gf x1, gf x2, gf x3]
  gf (GCondUpon x1 x2 x3) = mkApp (mkCId "CondUpon") [gf x1, gf x2, gf x3]
  gf (GGiven x1 x2) = mkApp (mkCId "Given") [gf x1, gf x2]
  gf (GGivenStandalone x1) = mkApp (mkCId "GivenStandalone") [gf x1]
  gf (GHornClause2 x1 x2) = mkApp (mkCId "HornClause2") [gf x1, gf x2]
  gf (GMeans x1 x2) = mkApp (mkCId "Means") [gf x1, gf x2]
  gf (GRPelem x1 x2) = mkApp (mkCId "RPelem") [gf x1, gf x2]
  gf (GRPeq x1 x2) = mkApp (mkCId "RPeq") [gf x1, gf x2]
  gf (GRPgt x1 x2) = mkApp (mkCId "RPgt") [gf x1, gf x2]
  gf (GRPgte x1 x2) = mkApp (mkCId "RPgte") [gf x1, gf x2]
  gf (GRPis x1 x2) = mkApp (mkCId "RPis") [gf x1, gf x2]
  gf (GRPlt x1 x2) = mkApp (mkCId "RPlt") [gf x1, gf x2]
  gf (GRPlte x1 x2) = mkApp (mkCId "RPlte") [gf x1, gf x2]
  gf (GRPnotElem x1 x2) = mkApp (mkCId "RPnotElem") [gf x1, gf x2]
  gf (GTemporal x1 x2) = mkApp (mkCId "Temporal") [gf x1, gf x2]
  gf (GTemporalStandalone x1) = mkApp (mkCId "TemporalStandalone") [gf x1]
  gf (GUDS2Fragment x1) = mkApp (mkCId "UDS2Fragment") [gf x1]
  gf (GUpon x1 x2) = mkApp (mkCId "Upon") [gf x1, gf x2]
  gf (GUponStandalone x1) = mkApp (mkCId "UponStandalone") [gf x1]
  gf (GsubjAction x1 x2) = mkApp (mkCId "subjAction") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "Adv_no_later_than_Num_calendar_days_after_the_day_UDS" -> GAdv_no_later_than_Num_calendar_days_after_the_day_UDS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "Cond" -> GCond (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "CondGiven" -> GCondGiven (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "CondStandalone" -> GCondStandalone (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "CondTemporal" -> GCondTemporal (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "CondUpon" -> GCondUpon (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "Given" -> GGiven (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "GivenStandalone" -> GGivenStandalone (fg x1)
      Just (i,[x1,x2]) | i == mkCId "HornClause2" -> GHornClause2 (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "Means" -> GMeans (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RPelem" -> GRPelem (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RPeq" -> GRPeq (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RPgt" -> GRPgt (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RPgte" -> GRPgte (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RPis" -> GRPis (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RPlt" -> GRPlt (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RPlte" -> GRPlte (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RPnotElem" -> GRPnotElem (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "Temporal" -> GTemporal (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "TemporalStandalone" -> GTemporalStandalone (fg x1)
      Just (i,[x1]) | i == mkCId "UDS2Fragment" -> GUDS2Fragment (fg x1)
      Just (i,[x1,x2]) | i == mkCId "Upon" -> GUpon (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "UponStandalone" -> GUponStandalone (fg x1)
      Just (i,[x1,x2]) | i == mkCId "subjAction" -> GsubjAction (fg x1) (fg x2)


      _ -> error ("no UDFragment " ++ show t)

instance Gf GUDS where
  gf (GDMay x1) = mkApp (mkCId "DMay") [gf x1]
  gf (GDMust x1) = mkApp (mkCId "DMust") [gf x1]
  gf (GDShant x1) = mkApp (mkCId "DShant") [gf x1]
  gf (Groot_acl x1 x2) = mkApp (mkCId "root_acl") [gf x1, gf x2]
  gf (Groot_aclRelcl x1 x2) = mkApp (mkCId "root_aclRelcl") [gf x1, gf x2]
  gf (Groot_aclRelcl_nmod x1 x2 x3) = mkApp (mkCId "root_aclRelcl_nmod") [gf x1, gf x2, gf x3]
  gf (Groot_acl_nmod x1 x2 x3) = mkApp (mkCId "root_acl_nmod") [gf x1, gf x2, gf x3]
  gf (Groot_advcl x1 x2) = mkApp (mkCId "root_advcl") [gf x1, gf x2]
  gf (Groot_advcl_nsubjPass_auxPass x1 x2 x3 x4) = mkApp (mkCId "root_advcl_nsubjPass_auxPass") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_advcl_nsubjPass_aux_auxPass_xcomp x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_advcl_nsubjPass_aux_auxPass_xcomp") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_advcl_nsubj_aux_advcl x1 x2 x3 x4 x5) = mkApp (mkCId "root_advcl_nsubj_aux_advcl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_advcl_nsubj_aux_advmod_obj x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_advcl_nsubj_aux_advmod_obj") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_advcl_nsubj_aux_ccomp x1 x2 x3 x4 x5) = mkApp (mkCId "root_advcl_nsubj_aux_ccomp") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_advcl_nsubj_aux_obl_obj x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_advcl_nsubj_aux_obl_obj") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_advcl_nsubj_cop x1 x2 x3 x4) = mkApp (mkCId "root_advcl_nsubj_cop") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_advcl_nsubj_cop_case_amod_nmod x1 x2 x3 x4 x5 x6 x7) = mkApp (mkCId "root_advcl_nsubj_cop_case_amod_nmod") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6, gf x7]
  gf (Groot_advcl_nsubj_cop_nsubj x1 x2 x3 x4 x5) = mkApp (mkCId "root_advcl_nsubj_cop_nsubj") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_advcl_nsubj_xcomp x1 x2 x3 x4) = mkApp (mkCId "root_advcl_nsubj_xcomp") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_advmod x1 x2) = mkApp (mkCId "root_advmod") [gf x1, gf x2]
  gf (Groot_advmod_advcl x1 x2 x3) = mkApp (mkCId "root_advmod_advcl") [gf x1, gf x2, gf x3]
  gf (Groot_advmod_advmod_obl x1 x2 x3 x4) = mkApp (mkCId "root_advmod_advmod_obl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_advmod_amod x1 x2 x3) = mkApp (mkCId "root_advmod_amod") [gf x1, gf x2, gf x3]
  gf (Groot_advmod_nsubj_aux_advmod_obj x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_advmod_nsubj_aux_advmod_obj") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_advmod_nsubj_cop x1 x2 x3 x4) = mkApp (mkCId "root_advmod_nsubj_cop") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_advmod_nsubj_cop_obl x1 x2 x3 x4 x5) = mkApp (mkCId "root_advmod_nsubj_cop_obl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_advmod_xcomp x1 x2 x3) = mkApp (mkCId "root_advmod_xcomp") [gf x1, gf x2, gf x3]
  gf (Groot_amod_nmod x1 x2 x3) = mkApp (mkCId "root_amod_nmod") [gf x1, gf x2, gf x3]
  gf (Groot_appos x1 x2) = mkApp (mkCId "root_appos") [gf x1, gf x2]
  gf (Groot_appos_advmod x1 x2 x3) = mkApp (mkCId "root_appos_advmod") [gf x1, gf x2, gf x3]
  gf (Groot_auxPass x1 x2) = mkApp (mkCId "root_auxPass") [gf x1, gf x2]
  gf (Groot_case x1 x2) = mkApp (mkCId "root_case") [gf x1, gf x2]
  gf (Groot_case_amod x1 x2 x3) = mkApp (mkCId "root_case_amod") [gf x1, gf x2, gf x3]
  gf (Groot_case_amod_amod x1 x2 x3 x4) = mkApp (mkCId "root_case_amod_amod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_case_compound x1 x2 x3) = mkApp (mkCId "root_case_compound") [gf x1, gf x2, gf x3]
  gf (Groot_case_nummod x1 x2 x3) = mkApp (mkCId "root_case_nummod") [gf x1, gf x2, gf x3]
  gf (Groot_case_nummod_acl x1 x2 x3 x4) = mkApp (mkCId "root_case_nummod_acl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_case_nummod_nummod x1 x2 x3 x4) = mkApp (mkCId "root_case_nummod_nummod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_cc x1 x2) = mkApp (mkCId "root_cc") [gf x1, gf x2]
  gf (Groot_cc_cop_xcomp x1 x2 x3 x4) = mkApp (mkCId "root_cc_cop_xcomp") [gf x1, gf x2, gf x3, gf x4]
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
  gf (Groot_compound_flat x1 x2 x3) = mkApp (mkCId "root_compound_flat") [gf x1, gf x2, gf x3]
  gf (Groot_cop x1 x2) = mkApp (mkCId "root_cop") [gf x1, gf x2]
  gf (Groot_cop_advmod x1 x2 x3) = mkApp (mkCId "root_cop_advmod") [gf x1, gf x2, gf x3]
  gf (Groot_csubj x1 x2) = mkApp (mkCId "root_csubj") [gf x1, gf x2]
  gf (Groot_csubj_aux_aux x1 x2 x3 x4) = mkApp (mkCId "root_csubj_aux_aux") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_discourse x1 x2) = mkApp (mkCId "root_discourse") [gf x1, gf x2]
  gf (Groot_expl_cop_csubj x1 x2 x3 x4) = mkApp (mkCId "root_expl_cop_csubj") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_fixed x1 x2) = mkApp (mkCId "root_fixed") [gf x1, gf x2]
  gf (Groot_goeswith x1 x2) = mkApp (mkCId "root_goeswith") [gf x1, gf x2]
  gf (Groot_goeswith_goeswith x1 x2 x3) = mkApp (mkCId "root_goeswith_goeswith") [gf x1, gf x2, gf x3]
  gf (Groot_mark x1 x2) = mkApp (mkCId "root_mark") [gf x1, gf x2]
  gf (Groot_mark_cc_mark_obj x1 x2 x3 x4 x5) = mkApp (mkCId "root_mark_cc_mark_obj") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_mark_expl_cop_xcomp x1 x2 x3 x4 x5) = mkApp (mkCId "root_mark_expl_cop_xcomp") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_mark_expl_nsubj x1 x2 x3 x4) = mkApp (mkCId "root_mark_expl_nsubj") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_mark_nsubj x1 x2 x3) = mkApp (mkCId "root_mark_nsubj") [gf x1, gf x2, gf x3]
  gf (Groot_mark_nsubjPass_auxPass x1 x2 x3 x4) = mkApp (mkCId "root_mark_nsubjPass_auxPass") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_mark_nsubjPass_auxPass_obl x1 x2 x3 x4 x5) = mkApp (mkCId "root_mark_nsubjPass_auxPass_obl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_mark_nsubj_aux x1 x2 x3 x4) = mkApp (mkCId "root_mark_nsubj_aux") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_mark_nsubj_aux_advmod_obj x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_mark_nsubj_aux_advmod_obj") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_mark_nsubj_aux_aux x1 x2 x3 x4 x5) = mkApp (mkCId "root_mark_nsubj_aux_aux") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_mark_nsubj_cop x1 x2 x3 x4) = mkApp (mkCId "root_mark_nsubj_cop") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_mark_nsubj_cop_obl x1 x2 x3 x4 x5) = mkApp (mkCId "root_mark_nsubj_cop_obl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_mark_nsubj_nsubj_xcomp x1 x2 x3 x4 x5) = mkApp (mkCId "root_mark_nsubj_nsubj_xcomp") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_mark_nsubj_obj x1 x2 x3 x4) = mkApp (mkCId "root_mark_nsubj_obj") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_mark_nsubj_obl x1 x2 x3 x4) = mkApp (mkCId "root_mark_nsubj_obl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_mark_nsubj_xcomp x1 x2 x3 x4) = mkApp (mkCId "root_mark_nsubj_xcomp") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_mark_nummod x1 x2 x3) = mkApp (mkCId "root_mark_nummod") [gf x1, gf x2, gf x3]
  gf (Groot_nmod x1 x2) = mkApp (mkCId "root_nmod") [gf x1, gf x2]
  gf (Groot_nmod_acl x1 x2 x3) = mkApp (mkCId "root_nmod_acl") [gf x1, gf x2, gf x3]
  gf (Groot_nmod_aclRelcl x1 x2 x3) = mkApp (mkCId "root_nmod_aclRelcl") [gf x1, gf x2, gf x3]
  gf (Groot_nmod_nmod x1 x2 x3) = mkApp (mkCId "root_nmod_nmod") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj x1 x2) = mkApp (mkCId "root_nsubj") [gf x1, gf x2]
  gf (Groot_nsubjPass_auxPass x1 x2 x3) = mkApp (mkCId "root_nsubjPass_auxPass") [gf x1, gf x2, gf x3]
  gf (Groot_nsubjPass_auxPass_advmod x1 x2 x3 x4) = mkApp (mkCId "root_nsubjPass_auxPass_advmod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubjPass_auxPass_advmod_advcl x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubjPass_auxPass_advmod_advcl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubjPass_auxPass_advmod_xcomp x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubjPass_auxPass_advmod_xcomp") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubjPass_auxPass_xcomp x1 x2 x3 x4) = mkApp (mkCId "root_nsubjPass_auxPass_xcomp") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubjPass_aux_auxPass x1 x2 x3 x4) = mkApp (mkCId "root_nsubjPass_aux_auxPass") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubjPass_aux_auxPass_advcl_advcl x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nsubjPass_aux_auxPass_advcl_advcl") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nsubjPass_aux_auxPass_advmod x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubjPass_aux_auxPass_advmod") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubjPass_aux_auxPass_obl x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubjPass_aux_auxPass_obl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubjPass_aux_auxPass_obl_advmod x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nsubjPass_aux_auxPass_obl_advmod") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nsubjPass_aux_auxPass_obl_obl_advcl x1 x2 x3 x4 x5 x6 x7) = mkApp (mkCId "root_nsubjPass_aux_auxPass_obl_obl_advcl") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6, gf x7]
  gf (Groot_nsubjPass_aux_auxPass_obl_obl_advmod x1 x2 x3 x4 x5 x6 x7) = mkApp (mkCId "root_nsubjPass_aux_auxPass_obl_obl_advmod") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6, gf x7]
  gf (Groot_nsubjPass_aux_auxPass_xcomp x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubjPass_aux_auxPass_xcomp") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_advmod x1 x2 x3) = mkApp (mkCId "root_nsubj_advmod") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj_advmod_obj x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_advmod_obj") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_aux x1 x2 x3) = mkApp (mkCId "root_nsubj_aux") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj_aux_aclRelcl x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_aux_aclRelcl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_aux_aclRelcl_obl x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubj_aux_aclRelcl_obl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_aux_advmod x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_aux_advmod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_aux_advmod_obj_advcl x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nsubj_aux_advmod_obj_advcl") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nsubj_aux_aux x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_aux_aux") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_aux_cop_nmod x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubj_aux_cop_nmod") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_aux_obj x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_aux_obj") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_aux_obj_obl x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubj_aux_obj_obl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_aux_obj_obl_advmod_advcl x1 x2 x3 x4 x5 x6 x7) = mkApp (mkCId "root_nsubj_aux_obj_obl_advmod_advcl") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6, gf x7]
  gf (Groot_nsubj_aux_obj_obl_obl x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nsubj_aux_obj_obl_obl") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nsubj_aux_obl x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_aux_obl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_ccomp x1 x2 x3) = mkApp (mkCId "root_nsubj_ccomp") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj_cop x1 x2 x3) = mkApp (mkCId "root_nsubj_cop") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj_cop_aclRelcl x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_cop_aclRelcl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_cop_aclRelcl_obl x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubj_cop_aclRelcl_obl") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_cop_advcl x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_cop_advcl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_cop_advmod x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_cop_advmod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_cop_case_nmod_acl x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nsubj_cop_case_nmod_acl") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nsubj_cop_nmod x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_cop_nmod") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_cop_nmodPoss x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_cop_nmodPoss") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_cop_obl x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_cop_obl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_cop_obl_parataxis x1 x2 x3 x4 x5) = mkApp (mkCId "root_nsubj_cop_obl_parataxis") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Groot_nsubj_nsubj_aux_advmod_obj x1 x2 x3 x4 x5 x6) = mkApp (mkCId "root_nsubj_nsubj_aux_advmod_obj") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Groot_nsubj_obj x1 x2 x3) = mkApp (mkCId "root_nsubj_obj") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj_obj_advcl x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_obj_advcl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_obj_xcomp x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_obj_xcomp") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_obl x1 x2 x3) = mkApp (mkCId "root_nsubj_obl") [gf x1, gf x2, gf x3]
  gf (Groot_nsubj_obl_obl x1 x2 x3 x4) = mkApp (mkCId "root_nsubj_obl_obl") [gf x1, gf x2, gf x3, gf x4]
  gf (Groot_nsubj_xcomp x1 x2 x3) = mkApp (mkCId "root_nsubj_xcomp") [gf x1, gf x2, gf x3]
  gf (Groot_nummod x1 x2) = mkApp (mkCId "root_nummod") [gf x1, gf x2]
  gf (Groot_nummod_appos x1 x2 x3) = mkApp (mkCId "root_nummod_appos") [gf x1, gf x2, gf x3]
  gf (Groot_nummod_auxPass_cc_aux_auxPass_obl_obl x1 x2 x3 x4 x5 x6 x7 x8) = mkApp (mkCId "root_nummod_auxPass_cc_aux_auxPass_obl_obl") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6, gf x7, gf x8]
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
  gf (Groot_xcomp x1 x2) = mkApp (mkCId "root_xcomp") [gf x1, gf x2]
  gf (Groot_xcomp_ccomp x1 x2 x3) = mkApp (mkCId "root_xcomp_ccomp") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "DMay" -> GDMay (fg x1)
      Just (i,[x1]) | i == mkCId "DMust" -> GDMust (fg x1)
      Just (i,[x1]) | i == mkCId "DShant" -> GDShant (fg x1)
      Just (i,[x1,x2]) | i == mkCId "root_acl" -> Groot_acl (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "root_aclRelcl" -> Groot_aclRelcl (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_aclRelcl_nmod" -> Groot_aclRelcl_nmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_acl_nmod" -> Groot_acl_nmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_advcl" -> Groot_advcl (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_advcl_nsubjPass_auxPass" -> Groot_advcl_nsubjPass_auxPass (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_advcl_nsubjPass_aux_auxPass_xcomp" -> Groot_advcl_nsubjPass_aux_auxPass_xcomp (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_advcl_nsubj_aux_advcl" -> Groot_advcl_nsubj_aux_advcl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_advcl_nsubj_aux_advmod_obj" -> Groot_advcl_nsubj_aux_advmod_obj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_advcl_nsubj_aux_ccomp" -> Groot_advcl_nsubj_aux_ccomp (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_advcl_nsubj_aux_obl_obj" -> Groot_advcl_nsubj_aux_obl_obj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_advcl_nsubj_cop" -> Groot_advcl_nsubj_cop (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5,x6,x7]) | i == mkCId "root_advcl_nsubj_cop_case_amod_nmod" -> Groot_advcl_nsubj_cop_case_amod_nmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6) (fg x7)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_advcl_nsubj_cop_nsubj" -> Groot_advcl_nsubj_cop_nsubj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_advcl_nsubj_xcomp" -> Groot_advcl_nsubj_xcomp (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "root_advmod" -> Groot_advmod (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_advmod_advcl" -> Groot_advmod_advcl (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_advmod_advmod_obl" -> Groot_advmod_advmod_obl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_advmod_amod" -> Groot_advmod_amod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_advmod_nsubj_aux_advmod_obj" -> Groot_advmod_nsubj_aux_advmod_obj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_advmod_nsubj_cop" -> Groot_advmod_nsubj_cop (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_advmod_nsubj_cop_obl" -> Groot_advmod_nsubj_cop_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3]) | i == mkCId "root_advmod_xcomp" -> Groot_advmod_xcomp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_amod_nmod" -> Groot_amod_nmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_appos" -> Groot_appos (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_appos_advmod" -> Groot_appos_advmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_auxPass" -> Groot_auxPass (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "root_case" -> Groot_case (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_case_amod" -> Groot_case_amod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_case_amod_amod" -> Groot_case_amod_amod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_case_compound" -> Groot_case_compound (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_case_nummod" -> Groot_case_nummod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_case_nummod_acl" -> Groot_case_nummod_acl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_case_nummod_nummod" -> Groot_case_nummod_nummod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "root_cc" -> Groot_cc (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_cc_cop_xcomp" -> Groot_cc_cop_xcomp (fg x1) (fg x2) (fg x3) (fg x4)
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
      Just (i,[x1,x2,x3]) | i == mkCId "root_compound_flat" -> Groot_compound_flat (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_cop" -> Groot_cop (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_cop_advmod" -> Groot_cop_advmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_csubj" -> Groot_csubj (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_csubj_aux_aux" -> Groot_csubj_aux_aux (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "root_discourse" -> Groot_discourse (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_expl_cop_csubj" -> Groot_expl_cop_csubj (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "root_fixed" -> Groot_fixed (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "root_goeswith" -> Groot_goeswith (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_goeswith_goeswith" -> Groot_goeswith_goeswith (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_mark" -> Groot_mark (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_mark_cc_mark_obj" -> Groot_mark_cc_mark_obj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_mark_expl_cop_xcomp" -> Groot_mark_expl_cop_xcomp (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_mark_expl_nsubj" -> Groot_mark_expl_nsubj (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_mark_nsubj" -> Groot_mark_nsubj (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_mark_nsubjPass_auxPass" -> Groot_mark_nsubjPass_auxPass (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_mark_nsubjPass_auxPass_obl" -> Groot_mark_nsubjPass_auxPass_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_mark_nsubj_aux" -> Groot_mark_nsubj_aux (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_mark_nsubj_aux_advmod_obj" -> Groot_mark_nsubj_aux_advmod_obj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_mark_nsubj_aux_aux" -> Groot_mark_nsubj_aux_aux (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_mark_nsubj_cop" -> Groot_mark_nsubj_cop (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_mark_nsubj_cop_obl" -> Groot_mark_nsubj_cop_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_mark_nsubj_nsubj_xcomp" -> Groot_mark_nsubj_nsubj_xcomp (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_mark_nsubj_obj" -> Groot_mark_nsubj_obj (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_mark_nsubj_obl" -> Groot_mark_nsubj_obl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_mark_nsubj_xcomp" -> Groot_mark_nsubj_xcomp (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_mark_nummod" -> Groot_mark_nummod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_nmod" -> Groot_nmod (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nmod_acl" -> Groot_nmod_acl (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nmod_aclRelcl" -> Groot_nmod_aclRelcl (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nmod_nmod" -> Groot_nmod_nmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_nsubj" -> Groot_nsubj (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubjPass_auxPass" -> Groot_nsubjPass_auxPass (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubjPass_auxPass_advmod" -> Groot_nsubjPass_auxPass_advmod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubjPass_auxPass_advmod_advcl" -> Groot_nsubjPass_auxPass_advmod_advcl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubjPass_auxPass_advmod_xcomp" -> Groot_nsubjPass_auxPass_advmod_xcomp (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubjPass_auxPass_xcomp" -> Groot_nsubjPass_auxPass_xcomp (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubjPass_aux_auxPass" -> Groot_nsubjPass_aux_auxPass (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nsubjPass_aux_auxPass_advcl_advcl" -> Groot_nsubjPass_aux_auxPass_advcl_advcl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubjPass_aux_auxPass_advmod" -> Groot_nsubjPass_aux_auxPass_advmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubjPass_aux_auxPass_obl" -> Groot_nsubjPass_aux_auxPass_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nsubjPass_aux_auxPass_obl_advmod" -> Groot_nsubjPass_aux_auxPass_obl_advmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4,x5,x6,x7]) | i == mkCId "root_nsubjPass_aux_auxPass_obl_obl_advcl" -> Groot_nsubjPass_aux_auxPass_obl_obl_advcl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6) (fg x7)
      Just (i,[x1,x2,x3,x4,x5,x6,x7]) | i == mkCId "root_nsubjPass_aux_auxPass_obl_obl_advmod" -> Groot_nsubjPass_aux_auxPass_obl_obl_advmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6) (fg x7)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubjPass_aux_auxPass_xcomp" -> Groot_nsubjPass_aux_auxPass_xcomp (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_advmod" -> Groot_nsubj_advmod (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_advmod_obj" -> Groot_nsubj_advmod_obj (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_aux" -> Groot_nsubj_aux (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_aux_aclRelcl" -> Groot_nsubj_aux_aclRelcl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubj_aux_aclRelcl_obl" -> Groot_nsubj_aux_aclRelcl_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_aux_advmod" -> Groot_nsubj_aux_advmod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nsubj_aux_advmod_obj_advcl" -> Groot_nsubj_aux_advmod_obj_advcl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_aux_aux" -> Groot_nsubj_aux_aux (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubj_aux_cop_nmod" -> Groot_nsubj_aux_cop_nmod (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_aux_obj" -> Groot_nsubj_aux_obj (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubj_aux_obj_obl" -> Groot_nsubj_aux_obj_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5,x6,x7]) | i == mkCId "root_nsubj_aux_obj_obl_advmod_advcl" -> Groot_nsubj_aux_obj_obl_advmod_advcl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6) (fg x7)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nsubj_aux_obj_obl_obl" -> Groot_nsubj_aux_obj_obl_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_aux_obl" -> Groot_nsubj_aux_obl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_ccomp" -> Groot_nsubj_ccomp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_cop" -> Groot_nsubj_cop (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_cop_aclRelcl" -> Groot_nsubj_cop_aclRelcl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubj_cop_aclRelcl_obl" -> Groot_nsubj_cop_aclRelcl_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_cop_advcl" -> Groot_nsubj_cop_advcl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_cop_advmod" -> Groot_nsubj_cop_advmod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nsubj_cop_case_nmod_acl" -> Groot_nsubj_cop_case_nmod_acl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_cop_nmod" -> Groot_nsubj_cop_nmod (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_cop_nmodPoss" -> Groot_nsubj_cop_nmodPoss (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_cop_obl" -> Groot_nsubj_cop_obl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "root_nsubj_cop_obl_parataxis" -> Groot_nsubj_cop_obl_parataxis (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "root_nsubj_nsubj_aux_advmod_obj" -> Groot_nsubj_nsubj_aux_advmod_obj (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_obj" -> Groot_nsubj_obj (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_obj_advcl" -> Groot_nsubj_obj_advcl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_obj_xcomp" -> Groot_nsubj_obj_xcomp (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_obl" -> Groot_nsubj_obl (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "root_nsubj_obl_obl" -> Groot_nsubj_obl_obl (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nsubj_xcomp" -> Groot_nsubj_xcomp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "root_nummod" -> Groot_nummod (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "root_nummod_appos" -> Groot_nummod_appos (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4,x5,x6,x7,x8]) | i == mkCId "root_nummod_auxPass_cc_aux_auxPass_obl_obl" -> Groot_nummod_auxPass_cc_aux_auxPass_obl_obl (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6) (fg x7) (fg x8)
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
      Just (i,[x1,x2]) | i == mkCId "root_xcomp" -> Groot_xcomp (fg x1) (fg x2)
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
  gf (GComplVP x1 x2) = mkApp (mkCId "ComplVP") [gf x1, gf x2]
  gf (GConjVP x1 x2) = mkApp (mkCId "ConjVP") [gf x1, gf x2]
  gf (GPassV x1) = mkApp (mkCId "PassV") [gf x1]
  gf (GPassVAgent x1 x2) = mkApp (mkCId "PassVAgent") [gf x1, gf x2]
  gf (GPrepVP x1 x2) = mkApp (mkCId "PrepVP") [gf x1, gf x2]
  gf (GProgrVP x1) = mkApp (mkCId "ProgrVP") [gf x1]
  gf (GUseComp x1) = mkApp (mkCId "UseComp") [gf x1]
  gf (GUseV x1) = mkApp (mkCId "UseV") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdVVP" -> GAdVVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AdvVP" -> GAdvVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplV" -> GComplV (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplVP" -> GComplVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjVP" -> GConjVP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "PassV" -> GPassV (fg x1)
      Just (i,[x1,x2]) | i == mkCId "PassVAgent" -> GPassVAgent (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PrepVP" -> GPrepVP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ProgrVP" -> GProgrVP (fg x1)
      Just (i,[x1]) | i == mkCId "UseComp" -> GUseComp (fg x1)
      Just (i,[x1]) | i == mkCId "UseV" -> GUseV (fg x1)


      _ -> error ("no VP " ++ show t)

instance Gf Gacl where
  gf (GaclUDS_ x1) = mkApp (mkCId "aclUDS_") [gf x1]
  gf (GaclUDSgerund_ x1) = mkApp (mkCId "aclUDSgerund_") [gf x1]
  gf (GaclUDSpastpartParens_ x1) = mkApp (mkCId "aclUDSpastpartParens_") [gf x1]
  gf (GaclUDSpastpart_ x1) = mkApp (mkCId "aclUDSpastpart_") [gf x1]
  gf (Gacl_ x1) = mkApp (mkCId "acl_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "aclUDS_" -> GaclUDS_ (fg x1)
      Just (i,[x1]) | i == mkCId "aclUDSgerund_" -> GaclUDSgerund_ (fg x1)
      Just (i,[x1]) | i == mkCId "aclUDSpastpartParens_" -> GaclUDSpastpartParens_ (fg x1)
      Just (i,[x1]) | i == mkCId "aclUDSpastpart_" -> GaclUDSpastpart_ (fg x1)
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
  gf (GadvclMarkUDS_ x1 x2) = mkApp (mkCId "advclMarkUDS_") [gf x1, gf x2]
  gf (GadvclUDS_ x1) = mkApp (mkCId "advclUDS_") [gf x1]
  gf (Gadvcl_ x1) = mkApp (mkCId "advcl_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "advclMarkUDS_" -> GadvclMarkUDS_ (fg x1) (fg x2)
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
  gf Gshall_aux = mkApp (mkCId "shall_aux") []
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
      Just (i,[]) | i == mkCId "shall_aux" -> Gshall_aux 
      Just (i,[]) | i == mkCId "should_aux" -> Gshould_aux 
      Just (i,[]) | i == mkCId "will_aux" -> Gwill_aux 


      _ -> error ("no aux " ++ show t)

instance Gf GauxPass where
  gf Gbe_auxPass = mkApp (mkCId "be_auxPass") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "be_auxPass" -> Gbe_auxPass 


      _ -> error ("no auxPass " ++ show t)

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

instance Gf Gcop where
  gf Gbe_cop = mkApp (mkCId "be_cop") []
  gf Gis_cop = mkApp (mkCId "is_cop") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "be_cop" -> Gbe_cop 
      Just (i,[]) | i == mkCId "is_cop" -> Gis_cop 


      _ -> error ("no cop " ++ show t)

instance Gf Gcsubj where
  gf (GcsubjMarkFinite_ x1 x2) = mkApp (mkCId "csubjMarkFinite_") [gf x1, gf x2]
  gf (GcsubjMarkInfinite_ x1 x2) = mkApp (mkCId "csubjMarkInfinite_") [gf x1, gf x2]
  gf (Gcsubj_ x1) = mkApp (mkCId "csubj_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "csubjMarkFinite_" -> GcsubjMarkFinite_ (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "csubjMarkInfinite_" -> GcsubjMarkInfinite_ (fg x1) (fg x2)
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
  gf Gto_mark = mkApp (mkCId "to_mark") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "mark_" -> Gmark_ (fg x1)
      Just (i,[]) | i == mkCId "to_mark" -> Gto_mark 


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
  gf (GoblRP_ x1 x2) = mkApp (mkCId "oblRP_") [gf x1, gf x2]
  gf (Gobl_ x1) = mkApp (mkCId "obl_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "oblPrep_" -> GoblPrep_ (fg x1)
      Just (i,[x1,x2]) | i == mkCId "oblRP_" -> GoblRP_ (fg x1) (fg x2)
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
  gf (GrootAdA_ x1) = mkApp (mkCId "rootAdA_") [gf x1]
  gf (GrootAdv_ x1) = mkApp (mkCId "rootAdv_") [gf x1]
  gf (GrootDAP_ x1) = mkApp (mkCId "rootDAP_") [gf x1]
  gf (GrootDet_ x1) = mkApp (mkCId "rootDet_") [gf x1]
  gf (GrootN_ x1) = mkApp (mkCId "rootN_") [gf x1]
  gf (GrootPrep_ x1) = mkApp (mkCId "rootPrep_") [gf x1]
  gf (GrootQuant_ x1) = mkApp (mkCId "rootQuant_") [gf x1]
  gf (GrootRP_ x1) = mkApp (mkCId "rootRP_") [gf x1]
  gf (GrootV_ x1) = mkApp (mkCId "rootV_") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "rootA_" -> GrootA_ (fg x1)
      Just (i,[x1]) | i == mkCId "rootAdA_" -> GrootAdA_ (fg x1)
      Just (i,[x1]) | i == mkCId "rootAdv_" -> GrootAdv_ (fg x1)
      Just (i,[x1]) | i == mkCId "rootDAP_" -> GrootDAP_ (fg x1)
      Just (i,[x1]) | i == mkCId "rootDet_" -> GrootDet_ (fg x1)
      Just (i,[x1]) | i == mkCId "rootN_" -> GrootN_ (fg x1)
      Just (i,[x1]) | i == mkCId "rootPrep_" -> GrootPrep_ (fg x1)
      Just (i,[x1]) | i == mkCId "rootQuant_" -> GrootQuant_ (fg x1)
      Just (i,[x1]) | i == mkCId "rootRP_" -> GrootRP_ (fg x1)
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
  gf (GxcompA_ccomp_ x1 x2) = mkApp (mkCId "xcompA_ccomp_") [gf x1, gf x2]
  gf (GxcompAdv_ x1) = mkApp (mkCId "xcompAdv_") [gf x1]
  gf (GxcompN_ x1) = mkApp (mkCId "xcompN_") [gf x1]
  gf (GxcompToBeN_ x1 x2 x3) = mkApp (mkCId "xcompToBeN_") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "xcompA_" -> GxcompA_ (fg x1)
      Just (i,[x1,x2]) | i == mkCId "xcompA_ccomp_" -> GxcompA_ccomp_ (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "xcompAdv_" -> GxcompAdv_ (fg x1)
      Just (i,[x1]) | i == mkCId "xcompN_" -> GxcompN_ (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "xcompToBeN_" -> GxcompToBeN_ (fg x1) (fg x2) (fg x3)


      _ -> error ("no xcomp " ++ show t)



instance Gf GPhr where
  gf _ = undefined
  fg _ = undefined





instance Gf GText where
  gf _ = undefined
  fg _ = undefined





instance Gf GUtt where
  gf _ = undefined
  fg _ = undefined





instance Gf GV2 where
  gf _ = undefined
  fg _ = undefined





instance Gf GV2A where
  gf _ = undefined
  fg _ = undefined





instance Gf GV2Q where
  gf _ = undefined
  fg _ = undefined





instance Gf GV2S where
  gf _ = undefined
  fg _ = undefined





instance Gf GV2V where
  gf _ = undefined
  fg _ = undefined





instance Gf GV3 where
  gf _ = undefined
  fg _ = undefined





instance Gf GVA where
  gf _ = undefined
  fg _ = undefined





instance Gf GVPSlash where
  gf _ = undefined
  fg _ = undefined





instance Gf GVQ where
  gf _ = undefined
  fg _ = undefined





instance Gf GVS where
  gf _ = undefined
  fg _ = undefined





instance Gf GVV where
  gf _ = undefined
  fg _ = undefined





instance Gf GVoc where
  gf _ = undefined
  fg _ = undefined





instance Gf GX where
  gf _ = undefined
  fg _ = undefined





instance Gf Gcase_ where
  gf _ = undefined
  fg _ = undefined




instance Compos Tree where
  compos r a f t = case t of
    GStrA x1 -> r GStrA `a` f x1
    GMkA2 x1 x2 -> r GMkA2 `a` f x1 `a` f x2
    GAdAP x1 x2 -> r GAdAP `a` f x1 `a` f x2
    GAdjOrd x1 -> r GAdjOrd `a` f x1
    GAdvAP x1 x2 -> r GAdvAP `a` f x1 `a` f x2
    GComplA2 x1 x2 -> r GComplA2 `a` f x1 `a` f x2
    GConjAP x1 x2 -> r GConjAP `a` f x1 `a` f x2
    GParentheticalAP x1 -> r GParentheticalAP `a` f x1
    GPastPartAP x1 -> r GPastPartAP `a` f x1
    GPastPartAgentAP x1 x2 -> r GPastPartAgentAP `a` f x1 `a` f x2
    GPositA x1 -> r GPositA `a` f x1
    GPresPartAP x1 -> r GPresPartAP `a` f x1
    GStrAP x1 -> r GStrAP `a` f x1
    GUseComparA x1 -> r GUseComparA `a` f x1
    GAdnCAdv x1 -> r GAdnCAdv `a` f x1
    GConjAdV x1 x2 -> r GConjAdV `a` f x1 `a` f x2
    GAdvAdv x1 x2 -> r GAdvAdv `a` f x1 `a` f x2
    GComparAdvAdj x1 x2 x3 -> r GComparAdvAdj `a` f x1 `a` f x2 `a` f x3
    GComparAdvAdjS x1 x2 x3 -> r GComparAdvAdjS `a` f x1 `a` f x2 `a` f x3
    GConjAdv x1 x2 -> r GConjAdv `a` f x1 `a` f x2
    GGerundAdv x1 -> r GGerundAdv `a` f x1
    GPositAdvAdj x1 -> r GPositAdvAdj `a` f x1
    GPrepNP x1 x2 -> r GPrepNP `a` f x1 `a` f x2
    GSubjS x1 x2 -> r GSubjS `a` f x1 `a` f x2
    Gacl2Adv x1 -> r Gacl2Adv `a` f x1
    Gadvcl2Adv x1 -> r Gadvcl2Adv `a` f x1
    Gcsubj2Adv x1 -> r Gcsubj2Adv `a` f x1
    Gxcomp2Adv x1 -> r Gxcomp2Adv `a` f x1
    GAdjCN x1 x2 -> r GAdjCN `a` f x1 `a` f x2
    GAdvCN x1 x2 -> r GAdvCN `a` f x1 `a` f x2
    GApposCN x1 x2 -> r GApposCN `a` f x1 `a` f x2
    GCN_AP_Conj_CNs_of_NP x1 x2 x3 x4 -> r GCN_AP_Conj_CNs_of_NP `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GComplN2 x1 x2 -> r GComplN2 `a` f x1 `a` f x2
    GConjCN x1 x2 -> r GConjCN `a` f x1 `a` f x2
    GGerundCN x1 -> r GGerundCN `a` f x1
    GPossNP x1 x2 -> r GPossNP `a` f x1 `a` f x2
    GRelCN x1 x2 -> r GRelCN `a` f x1 `a` f x2
    GSentCN x1 x2 -> r GSentCN `a` f x1 `a` f x2
    GUseN x1 -> r GUseN `a` f x1
    GAdNum x1 x2 -> r GAdNum `a` f x1 `a` f x2
    GNumDigits x1 -> r GNumDigits `a` f x1
    GNumNumeral x1 -> r GNumNumeral `a` f x1
    GStrCard x1 -> r GStrCard `a` f x1
    GExistCN x1 -> r GExistCN `a` f x1
    GExistsNP x1 -> r GExistsNP `a` f x1
    GGenericCl x1 -> r GGenericCl `a` f x1
    GImpersCl x1 -> r GImpersCl `a` f x1
    GPredSCVP x1 x2 -> r GPredSCVP `a` f x1 `a` f x2
    GPredVP x1 x2 -> r GPredVP `a` f x1 `a` f x2
    GAdvSlash x1 x2 -> r GAdvSlash `a` f x1 `a` f x2
    GSlashCl x1 -> r GSlashCl `a` f x1
    GSlashPrep x1 x2 -> r GSlashPrep `a` f x1 `a` f x2
    GSlashVP x1 x2 -> r GSlashVP `a` f x1 `a` f x2
    GSlashVS x1 x2 x3 -> r GSlashVS `a` f x1 `a` f x2 `a` f x3
    GCompAP x1 -> r GCompAP `a` f x1
    GCompAdv x1 -> r GCompAdv `a` f x1
    GCompNP x1 -> r GCompNP `a` f x1
    GAdjDAP x1 x2 -> r GAdjDAP `a` f x1 `a` f x2
    GDetDAP x1 -> r GDetDAP `a` f x1
    GACard2Det x1 -> r GACard2Det `a` f x1
    GConjDet x1 x2 -> r GConjDet `a` f x1 `a` f x2
    GDetQuant x1 x2 -> r GDetQuant `a` f x1 `a` f x2
    GDetQuantOrd x1 x2 x3 -> r GDetQuantOrd `a` f x1 `a` f x2 `a` f x3
    GXorMore x1 -> r GXorMore `a` f x1
    GIDig x1 -> r GIDig `a` f x1
    GIIDig x1 x2 -> r GIIDig `a` f x1 `a` f x2
    GAdvIAdv x1 x2 -> r GAdvIAdv `a` f x1 `a` f x2
    GConjIAdv x1 x2 -> r GConjIAdv `a` f x1 `a` f x2
    GIAdvAdv x1 -> r GIAdvAdv `a` f x1
    GPrepIP x1 x2 -> r GPrepIP `a` f x1 `a` f x2
    GCompIAdv x1 -> r GCompIAdv `a` f x1
    GCompIP x1 -> r GCompIP `a` f x1
    GICompAP x1 -> r GICompAP `a` f x1
    GIdetQuant x1 x2 -> r GIdetQuant `a` f x1 `a` f x2
    GAdvIP x1 x2 -> r GAdvIP `a` f x1 `a` f x2
    GIdetCN x1 x2 -> r GIdetCN `a` f x1 `a` f x2
    GIdetIP x1 -> r GIdetIP `a` f x1
    GAdvImp x1 x2 -> r GAdvImp `a` f x1 `a` f x2
    GImpVP x1 -> r GImpVP `a` f x1
    GCompoundCN x1 x2 -> r GCompoundCN `a` f x1 `a` f x2
    GCompoundN x1 x2 -> r GCompoundN `a` f x1 `a` f x2
    GStrN x1 -> r GStrN `a` f x1
    GComplN3 x1 x2 -> r GComplN3 `a` f x1 `a` f x2
    GUse3N3 x1 -> r GUse3N3 `a` f x1
    GMkN3 x1 x2 x3 -> r GMkN3 `a` f x1 `a` f x2 `a` f x3
    GAdjAsNP x1 -> r GAdjAsNP `a` f x1
    GAdvNP x1 x2 -> r GAdvNP `a` f x1 `a` f x2
    GApposNP x1 x2 -> r GApposNP `a` f x1 `a` f x2
    GConjNP x1 x2 -> r GConjNP `a` f x1 `a` f x2
    GDefPN x1 -> r GDefPN `a` f x1
    GDetCN x1 x2 -> r GDetCN `a` f x1 `a` f x2
    GDetNP x1 -> r GDetNP `a` f x1
    GEvery x1 -> r GEvery `a` f x1
    GExtAdvNP x1 x2 -> r GExtAdvNP `a` f x1 `a` f x2
    GGenModNP x1 x2 x3 -> r GGenModNP `a` f x1 `a` f x2 `a` f x3
    GGerundNP x1 -> r GGerundNP `a` f x1
    GIndefPN x1 -> r GIndefPN `a` f x1
    GMassNP x1 -> r GMassNP `a` f x1
    GParty x1 -> r GParty `a` f x1
    GPredetNP x1 x2 -> r GPredetNP `a` f x1 `a` f x2
    GRelNP x1 x2 -> r GRelNP `a` f x1 `a` f x2
    GSentNP x1 x2 -> r GSentNP `a` f x1 `a` f x2
    GTokAll x1 -> r GTokAll `a` f x1
    GUsePN x1 -> r GUsePN `a` f x1
    GUsePron x1 -> r GUsePron `a` f x1
    GWho x1 x2 -> r GWho `a` f x1 `a` f x2
    GNumCard x1 -> r GNumCard `a` f x1
    GStrNum x1 -> r GStrNum `a` f x1
    Gnum x1 -> r Gnum `a` f x1
    GOrdDigits x1 -> r GOrdDigits `a` f x1
    GOrdNumeral x1 -> r GOrdNumeral `a` f x1
    GOrdNumeralSuperl x1 x2 -> r GOrdNumeralSuperl `a` f x1 `a` f x2
    GOrdSuperl x1 -> r GOrdSuperl `a` f x1
    GStrPN x1 -> r GStrPN `a` f x1
    GSymbPN x1 -> r GSymbPN `a` f x1
    GConjPrep x1 x2 -> r GConjPrep `a` f x1 `a` f x2
    GPredIAdvVP x1 x2 -> r GPredIAdvVP `a` f x1 `a` f x2
    GQuestCl x1 -> r GQuestCl `a` f x1
    GQuestIAdv x1 x2 -> r GQuestIAdv `a` f x1 `a` f x2
    GQuestIComp x1 x2 -> r GQuestIComp `a` f x1 `a` f x2
    GQuestQVP x1 x2 -> r GQuestQVP `a` f x1 `a` f x2
    GQuestSlash x1 x2 -> r GQuestSlash `a` f x1 `a` f x2
    GQuestVP x1 x2 -> r GQuestVP `a` f x1 `a` f x2
    GExistIPQS x1 x2 x3 -> r GExistIPQS `a` f x1 `a` f x2 `a` f x3
    GExistNPQS x1 x2 x3 -> r GExistNPQS `a` f x1 `a` f x2 `a` f x3
    GUseQCl x1 x2 x3 -> r GUseQCl `a` f x1 `a` f x2 `a` f x3
    GAddAdvQVP x1 x2 -> r GAddAdvQVP `a` f x1 `a` f x2
    GAdvQVP x1 x2 -> r GAdvQVP `a` f x1 `a` f x2
    GComplSlashIP x1 x2 -> r GComplSlashIP `a` f x1 `a` f x2
    GGenNP x1 -> r GGenNP `a` f x1
    GPossPron x1 -> r GPossPron `a` f x1
    GRelCl x1 -> r GRelCl `a` f x1
    GRelSlash x1 x2 -> r GRelSlash `a` f x1 `a` f x2
    GRelVP x1 x2 -> r GRelVP `a` f x1 `a` f x2
    GFunRP x1 x2 x3 -> r GFunRP `a` f x1 `a` f x2 `a` f x3
    GGenRP x1 x2 -> r GGenRP `a` f x1 `a` f x2
    GPrepRP x1 x2 -> r GPrepRP `a` f x1 `a` f x2
    GConjRS x1 x2 -> r GConjRS `a` f x1 `a` f x2
    GRS_that_NP_VP x1 x2 -> r GRS_that_NP_VP `a` f x1 `a` f x2
    GUseRCl x1 x2 x3 -> r GUseRCl `a` f x1 `a` f x2 `a` f x3
    GAdvS x1 x2 -> r GAdvS `a` f x1 `a` f x2
    GConjS x1 x2 -> r GConjS `a` f x1 `a` f x2
    GExistS x1 x2 x3 -> r GExistS `a` f x1 `a` f x2 `a` f x3
    GExtAdvS x1 x2 -> r GExtAdvS `a` f x1 `a` f x2
    GPostAdvS x1 x2 -> r GPostAdvS `a` f x1 `a` f x2
    GPredVPS x1 x2 -> r GPredVPS `a` f x1 `a` f x2
    GRelS x1 x2 -> r GRelS `a` f x1 `a` f x2
    GSSubjS x1 x2 x3 -> r GSSubjS `a` f x1 `a` f x2 `a` f x3
    GUseCl x1 x2 x3 -> r GUseCl `a` f x1 `a` f x2 `a` f x3
    GEmbedQS x1 -> r GEmbedQS `a` f x1
    GEmbedS x1 -> r GEmbedS `a` f x1
    GEmbedVP x1 -> r GEmbedVP `a` f x1
    GUseSlash x1 x2 x3 -> r GUseSlash `a` f x1 `a` f x2 `a` f x3
    Gpot0 x1 -> r Gpot0 `a` f x1
    Gpot0as1 x1 -> r Gpot0as1 `a` f x1
    Gpot1 x1 -> r Gpot1 `a` f x1
    Gpot1plus x1 x2 -> r Gpot1plus `a` f x1 `a` f x2
    Gpot1to19 x1 -> r Gpot1to19 `a` f x1
    Gpot1as2 x1 -> r Gpot1as2 `a` f x1
    Gpot2 x1 -> r Gpot2 `a` f x1
    Gpot2plus x1 x2 -> r Gpot2plus `a` f x1 `a` f x2
    Gpot2as3 x1 -> r Gpot2as3 `a` f x1
    Gpot3 x1 -> r Gpot3 `a` f x1
    Gpot3plus x1 x2 -> r Gpot3plus `a` f x1 `a` f x2
    GStrSymb x1 -> r GStrSymb `a` f x1
    GTTAnt x1 x2 -> r GTTAnt `a` f x1 `a` f x2
    GAdv_no_later_than_Num_calendar_days_after_the_day_UDS x1 x2 -> r GAdv_no_later_than_Num_calendar_days_after_the_day_UDS `a` f x1 `a` f x2
    GCond x1 x2 -> r GCond `a` f x1 `a` f x2
    GCondGiven x1 x2 x3 -> r GCondGiven `a` f x1 `a` f x2 `a` f x3
    GCondStandalone x1 -> r GCondStandalone `a` f x1
    GCondTemporal x1 x2 x3 -> r GCondTemporal `a` f x1 `a` f x2 `a` f x3
    GCondUpon x1 x2 x3 -> r GCondUpon `a` f x1 `a` f x2 `a` f x3
    GGiven x1 x2 -> r GGiven `a` f x1 `a` f x2
    GGivenStandalone x1 -> r GGivenStandalone `a` f x1
    GHornClause2 x1 x2 -> r GHornClause2 `a` f x1 `a` f x2
    GMeans x1 x2 -> r GMeans `a` f x1 `a` f x2
    GRPelem x1 x2 -> r GRPelem `a` f x1 `a` f x2
    GRPeq x1 x2 -> r GRPeq `a` f x1 `a` f x2
    GRPgt x1 x2 -> r GRPgt `a` f x1 `a` f x2
    GRPgte x1 x2 -> r GRPgte `a` f x1 `a` f x2
    GRPis x1 x2 -> r GRPis `a` f x1 `a` f x2
    GRPlt x1 x2 -> r GRPlt `a` f x1 `a` f x2
    GRPlte x1 x2 -> r GRPlte `a` f x1 `a` f x2
    GRPnotElem x1 x2 -> r GRPnotElem `a` f x1 `a` f x2
    GTemporal x1 x2 -> r GTemporal `a` f x1 `a` f x2
    GTemporalStandalone x1 -> r GTemporalStandalone `a` f x1
    GUDS2Fragment x1 -> r GUDS2Fragment `a` f x1
    GUpon x1 x2 -> r GUpon `a` f x1 `a` f x2
    GUponStandalone x1 -> r GUponStandalone `a` f x1
    GsubjAction x1 x2 -> r GsubjAction `a` f x1 `a` f x2
    GDMay x1 -> r GDMay `a` f x1
    GDMust x1 -> r GDMust `a` f x1
    GDShant x1 -> r GDShant `a` f x1
    Groot_acl x1 x2 -> r Groot_acl `a` f x1 `a` f x2
    Groot_aclRelcl x1 x2 -> r Groot_aclRelcl `a` f x1 `a` f x2
    Groot_aclRelcl_nmod x1 x2 x3 -> r Groot_aclRelcl_nmod `a` f x1 `a` f x2 `a` f x3
    Groot_acl_nmod x1 x2 x3 -> r Groot_acl_nmod `a` f x1 `a` f x2 `a` f x3
    Groot_advcl x1 x2 -> r Groot_advcl `a` f x1 `a` f x2
    Groot_advcl_nsubjPass_auxPass x1 x2 x3 x4 -> r Groot_advcl_nsubjPass_auxPass `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_advcl_nsubjPass_aux_auxPass_xcomp x1 x2 x3 x4 x5 x6 -> r Groot_advcl_nsubjPass_aux_auxPass_xcomp `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6
    Groot_advcl_nsubj_aux_advcl x1 x2 x3 x4 x5 -> r Groot_advcl_nsubj_aux_advcl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_advcl_nsubj_aux_advmod_obj x1 x2 x3 x4 x5 x6 -> r Groot_advcl_nsubj_aux_advmod_obj `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6
    Groot_advcl_nsubj_aux_ccomp x1 x2 x3 x4 x5 -> r Groot_advcl_nsubj_aux_ccomp `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_advcl_nsubj_aux_obl_obj x1 x2 x3 x4 x5 x6 -> r Groot_advcl_nsubj_aux_obl_obj `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6
    Groot_advcl_nsubj_cop x1 x2 x3 x4 -> r Groot_advcl_nsubj_cop `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_advcl_nsubj_cop_case_amod_nmod x1 x2 x3 x4 x5 x6 x7 -> r Groot_advcl_nsubj_cop_case_amod_nmod `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6 `a` f x7
    Groot_advcl_nsubj_cop_nsubj x1 x2 x3 x4 x5 -> r Groot_advcl_nsubj_cop_nsubj `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_advcl_nsubj_xcomp x1 x2 x3 x4 -> r Groot_advcl_nsubj_xcomp `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_advmod x1 x2 -> r Groot_advmod `a` f x1 `a` f x2
    Groot_advmod_advcl x1 x2 x3 -> r Groot_advmod_advcl `a` f x1 `a` f x2 `a` f x3
    Groot_advmod_advmod_obl x1 x2 x3 x4 -> r Groot_advmod_advmod_obl `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_advmod_amod x1 x2 x3 -> r Groot_advmod_amod `a` f x1 `a` f x2 `a` f x3
    Groot_advmod_nsubj_aux_advmod_obj x1 x2 x3 x4 x5 x6 -> r Groot_advmod_nsubj_aux_advmod_obj `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6
    Groot_advmod_nsubj_cop x1 x2 x3 x4 -> r Groot_advmod_nsubj_cop `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_advmod_nsubj_cop_obl x1 x2 x3 x4 x5 -> r Groot_advmod_nsubj_cop_obl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_advmod_xcomp x1 x2 x3 -> r Groot_advmod_xcomp `a` f x1 `a` f x2 `a` f x3
    Groot_amod_nmod x1 x2 x3 -> r Groot_amod_nmod `a` f x1 `a` f x2 `a` f x3
    Groot_appos x1 x2 -> r Groot_appos `a` f x1 `a` f x2
    Groot_appos_advmod x1 x2 x3 -> r Groot_appos_advmod `a` f x1 `a` f x2 `a` f x3
    Groot_auxPass x1 x2 -> r Groot_auxPass `a` f x1 `a` f x2
    Groot_case x1 x2 -> r Groot_case `a` f x1 `a` f x2
    Groot_case_amod x1 x2 x3 -> r Groot_case_amod `a` f x1 `a` f x2 `a` f x3
    Groot_case_amod_amod x1 x2 x3 x4 -> r Groot_case_amod_amod `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_case_compound x1 x2 x3 -> r Groot_case_compound `a` f x1 `a` f x2 `a` f x3
    Groot_case_nummod x1 x2 x3 -> r Groot_case_nummod `a` f x1 `a` f x2 `a` f x3
    Groot_case_nummod_acl x1 x2 x3 x4 -> r Groot_case_nummod_acl `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_case_nummod_nummod x1 x2 x3 x4 -> r Groot_case_nummod_nummod `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_cc x1 x2 -> r Groot_cc `a` f x1 `a` f x2
    Groot_cc_cop_xcomp x1 x2 x3 x4 -> r Groot_cc_cop_xcomp `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_cc_nmod x1 x2 x3 -> r Groot_cc_nmod `a` f x1 `a` f x2 `a` f x3
    Groot_cc_obj x1 x2 x3 -> r Groot_cc_obj `a` f x1 `a` f x2 `a` f x3
    Groot_ccomp x1 x2 -> r Groot_ccomp `a` f x1 `a` f x2
    Groot_compound x1 x2 -> r Groot_compound `a` f x1 `a` f x2
    Groot_compoundPrt_compoundPrt x1 x2 x3 -> r Groot_compoundPrt_compoundPrt `a` f x1 `a` f x2 `a` f x3
    Groot_compound_acl x1 x2 x3 -> r Groot_compound_acl `a` f x1 `a` f x2 `a` f x3
    Groot_compound_amod x1 x2 x3 -> r Groot_compound_amod `a` f x1 `a` f x2 `a` f x3
    Groot_compound_appos x1 x2 x3 -> r Groot_compound_appos `a` f x1 `a` f x2 `a` f x3
    Groot_compound_compound x1 x2 x3 -> r Groot_compound_compound `a` f x1 `a` f x2 `a` f x3
    Groot_compound_compound_appos x1 x2 x3 x4 -> r Groot_compound_compound_appos `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_compound_flat x1 x2 x3 -> r Groot_compound_flat `a` f x1 `a` f x2 `a` f x3
    Groot_cop x1 x2 -> r Groot_cop `a` f x1 `a` f x2
    Groot_cop_advmod x1 x2 x3 -> r Groot_cop_advmod `a` f x1 `a` f x2 `a` f x3
    Groot_csubj x1 x2 -> r Groot_csubj `a` f x1 `a` f x2
    Groot_csubj_aux_aux x1 x2 x3 x4 -> r Groot_csubj_aux_aux `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_discourse x1 x2 -> r Groot_discourse `a` f x1 `a` f x2
    Groot_expl_cop_csubj x1 x2 x3 x4 -> r Groot_expl_cop_csubj `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_fixed x1 x2 -> r Groot_fixed `a` f x1 `a` f x2
    Groot_goeswith x1 x2 -> r Groot_goeswith `a` f x1 `a` f x2
    Groot_goeswith_goeswith x1 x2 x3 -> r Groot_goeswith_goeswith `a` f x1 `a` f x2 `a` f x3
    Groot_mark x1 x2 -> r Groot_mark `a` f x1 `a` f x2
    Groot_mark_cc_mark_obj x1 x2 x3 x4 x5 -> r Groot_mark_cc_mark_obj `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_mark_expl_cop_xcomp x1 x2 x3 x4 x5 -> r Groot_mark_expl_cop_xcomp `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_mark_expl_nsubj x1 x2 x3 x4 -> r Groot_mark_expl_nsubj `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_mark_nsubj x1 x2 x3 -> r Groot_mark_nsubj `a` f x1 `a` f x2 `a` f x3
    Groot_mark_nsubjPass_auxPass x1 x2 x3 x4 -> r Groot_mark_nsubjPass_auxPass `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_mark_nsubjPass_auxPass_obl x1 x2 x3 x4 x5 -> r Groot_mark_nsubjPass_auxPass_obl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_mark_nsubj_aux x1 x2 x3 x4 -> r Groot_mark_nsubj_aux `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_mark_nsubj_aux_advmod_obj x1 x2 x3 x4 x5 x6 -> r Groot_mark_nsubj_aux_advmod_obj `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6
    Groot_mark_nsubj_aux_aux x1 x2 x3 x4 x5 -> r Groot_mark_nsubj_aux_aux `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_mark_nsubj_cop x1 x2 x3 x4 -> r Groot_mark_nsubj_cop `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_mark_nsubj_cop_obl x1 x2 x3 x4 x5 -> r Groot_mark_nsubj_cop_obl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_mark_nsubj_nsubj_xcomp x1 x2 x3 x4 x5 -> r Groot_mark_nsubj_nsubj_xcomp `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_mark_nsubj_obj x1 x2 x3 x4 -> r Groot_mark_nsubj_obj `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_mark_nsubj_obl x1 x2 x3 x4 -> r Groot_mark_nsubj_obl `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_mark_nsubj_xcomp x1 x2 x3 x4 -> r Groot_mark_nsubj_xcomp `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_mark_nummod x1 x2 x3 -> r Groot_mark_nummod `a` f x1 `a` f x2 `a` f x3
    Groot_nmod x1 x2 -> r Groot_nmod `a` f x1 `a` f x2
    Groot_nmod_acl x1 x2 x3 -> r Groot_nmod_acl `a` f x1 `a` f x2 `a` f x3
    Groot_nmod_aclRelcl x1 x2 x3 -> r Groot_nmod_aclRelcl `a` f x1 `a` f x2 `a` f x3
    Groot_nmod_nmod x1 x2 x3 -> r Groot_nmod_nmod `a` f x1 `a` f x2 `a` f x3
    Groot_nsubj x1 x2 -> r Groot_nsubj `a` f x1 `a` f x2
    Groot_nsubjPass_auxPass x1 x2 x3 -> r Groot_nsubjPass_auxPass `a` f x1 `a` f x2 `a` f x3
    Groot_nsubjPass_auxPass_advmod x1 x2 x3 x4 -> r Groot_nsubjPass_auxPass_advmod `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubjPass_auxPass_advmod_advcl x1 x2 x3 x4 x5 -> r Groot_nsubjPass_auxPass_advmod_advcl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_nsubjPass_auxPass_advmod_xcomp x1 x2 x3 x4 x5 -> r Groot_nsubjPass_auxPass_advmod_xcomp `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_nsubjPass_auxPass_xcomp x1 x2 x3 x4 -> r Groot_nsubjPass_auxPass_xcomp `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubjPass_aux_auxPass x1 x2 x3 x4 -> r Groot_nsubjPass_aux_auxPass `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubjPass_aux_auxPass_advcl_advcl x1 x2 x3 x4 x5 x6 -> r Groot_nsubjPass_aux_auxPass_advcl_advcl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6
    Groot_nsubjPass_aux_auxPass_advmod x1 x2 x3 x4 x5 -> r Groot_nsubjPass_aux_auxPass_advmod `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_nsubjPass_aux_auxPass_obl x1 x2 x3 x4 x5 -> r Groot_nsubjPass_aux_auxPass_obl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_nsubjPass_aux_auxPass_obl_advmod x1 x2 x3 x4 x5 x6 -> r Groot_nsubjPass_aux_auxPass_obl_advmod `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6
    Groot_nsubjPass_aux_auxPass_obl_obl_advcl x1 x2 x3 x4 x5 x6 x7 -> r Groot_nsubjPass_aux_auxPass_obl_obl_advcl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6 `a` f x7
    Groot_nsubjPass_aux_auxPass_obl_obl_advmod x1 x2 x3 x4 x5 x6 x7 -> r Groot_nsubjPass_aux_auxPass_obl_obl_advmod `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6 `a` f x7
    Groot_nsubjPass_aux_auxPass_xcomp x1 x2 x3 x4 x5 -> r Groot_nsubjPass_aux_auxPass_xcomp `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_nsubj_advmod x1 x2 x3 -> r Groot_nsubj_advmod `a` f x1 `a` f x2 `a` f x3
    Groot_nsubj_advmod_obj x1 x2 x3 x4 -> r Groot_nsubj_advmod_obj `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubj_aux x1 x2 x3 -> r Groot_nsubj_aux `a` f x1 `a` f x2 `a` f x3
    Groot_nsubj_aux_aclRelcl x1 x2 x3 x4 -> r Groot_nsubj_aux_aclRelcl `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubj_aux_aclRelcl_obl x1 x2 x3 x4 x5 -> r Groot_nsubj_aux_aclRelcl_obl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_nsubj_aux_advmod x1 x2 x3 x4 -> r Groot_nsubj_aux_advmod `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubj_aux_advmod_obj_advcl x1 x2 x3 x4 x5 x6 -> r Groot_nsubj_aux_advmod_obj_advcl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6
    Groot_nsubj_aux_aux x1 x2 x3 x4 -> r Groot_nsubj_aux_aux `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubj_aux_cop_nmod x1 x2 x3 x4 x5 -> r Groot_nsubj_aux_cop_nmod `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_nsubj_aux_obj x1 x2 x3 x4 -> r Groot_nsubj_aux_obj `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubj_aux_obj_obl x1 x2 x3 x4 x5 -> r Groot_nsubj_aux_obj_obl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_nsubj_aux_obj_obl_advmod_advcl x1 x2 x3 x4 x5 x6 x7 -> r Groot_nsubj_aux_obj_obl_advmod_advcl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6 `a` f x7
    Groot_nsubj_aux_obj_obl_obl x1 x2 x3 x4 x5 x6 -> r Groot_nsubj_aux_obj_obl_obl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6
    Groot_nsubj_aux_obl x1 x2 x3 x4 -> r Groot_nsubj_aux_obl `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubj_ccomp x1 x2 x3 -> r Groot_nsubj_ccomp `a` f x1 `a` f x2 `a` f x3
    Groot_nsubj_cop x1 x2 x3 -> r Groot_nsubj_cop `a` f x1 `a` f x2 `a` f x3
    Groot_nsubj_cop_aclRelcl x1 x2 x3 x4 -> r Groot_nsubj_cop_aclRelcl `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubj_cop_aclRelcl_obl x1 x2 x3 x4 x5 -> r Groot_nsubj_cop_aclRelcl_obl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_nsubj_cop_advcl x1 x2 x3 x4 -> r Groot_nsubj_cop_advcl `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubj_cop_advmod x1 x2 x3 x4 -> r Groot_nsubj_cop_advmod `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubj_cop_case_nmod_acl x1 x2 x3 x4 x5 x6 -> r Groot_nsubj_cop_case_nmod_acl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6
    Groot_nsubj_cop_nmod x1 x2 x3 x4 -> r Groot_nsubj_cop_nmod `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubj_cop_nmodPoss x1 x2 x3 x4 -> r Groot_nsubj_cop_nmodPoss `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubj_cop_obl x1 x2 x3 x4 -> r Groot_nsubj_cop_obl `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubj_cop_obl_parataxis x1 x2 x3 x4 x5 -> r Groot_nsubj_cop_obl_parataxis `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_nsubj_nsubj_aux_advmod_obj x1 x2 x3 x4 x5 x6 -> r Groot_nsubj_nsubj_aux_advmod_obj `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6
    Groot_nsubj_obj x1 x2 x3 -> r Groot_nsubj_obj `a` f x1 `a` f x2 `a` f x3
    Groot_nsubj_obj_advcl x1 x2 x3 x4 -> r Groot_nsubj_obj_advcl `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubj_obj_xcomp x1 x2 x3 x4 -> r Groot_nsubj_obj_xcomp `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubj_obl x1 x2 x3 -> r Groot_nsubj_obl `a` f x1 `a` f x2 `a` f x3
    Groot_nsubj_obl_obl x1 x2 x3 x4 -> r Groot_nsubj_obl_obl `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nsubj_xcomp x1 x2 x3 -> r Groot_nsubj_xcomp `a` f x1 `a` f x2 `a` f x3
    Groot_nummod x1 x2 -> r Groot_nummod `a` f x1 `a` f x2
    Groot_nummod_appos x1 x2 x3 -> r Groot_nummod_appos `a` f x1 `a` f x2 `a` f x3
    Groot_nummod_auxPass_cc_aux_auxPass_obl_obl x1 x2 x3 x4 x5 x6 x7 x8 -> r Groot_nummod_auxPass_cc_aux_auxPass_obl_obl `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6 `a` f x7 `a` f x8
    Groot_nummod_mark_obj x1 x2 x3 x4 -> r Groot_nummod_mark_obj `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_nummod_mark_obj_cc x1 x2 x3 x4 x5 -> r Groot_nummod_mark_obj_cc `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5
    Groot_nummod_nmod x1 x2 x3 -> r Groot_nummod_nmod `a` f x1 `a` f x2 `a` f x3
    Groot_nummod_nsubjPass_nsubjPass_auxPass_cc x1 x2 x3 x4 x5 x6 -> r Groot_nummod_nsubjPass_nsubjPass_auxPass_cc `a` f x1 `a` f x2 `a` f x3 `a` f x4 `a` f x5 `a` f x6
    Groot_nummod_obl x1 x2 x3 -> r Groot_nummod_obl `a` f x1 `a` f x2 `a` f x3
    Groot_nummod_obl_cc x1 x2 x3 x4 -> r Groot_nummod_obl_cc `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_obj x1 x2 -> r Groot_obj `a` f x1 `a` f x2
    Groot_obj_ccomp x1 x2 x3 -> r Groot_obj_ccomp `a` f x1 `a` f x2 `a` f x3
    Groot_obj_nmod x1 x2 x3 -> r Groot_obj_nmod `a` f x1 `a` f x2 `a` f x3
    Groot_obl x1 x2 -> r Groot_obl `a` f x1 `a` f x2
    Groot_obl_appos x1 x2 x3 -> r Groot_obl_appos `a` f x1 `a` f x2 `a` f x3
    Groot_obl_aux x1 x2 x3 -> r Groot_obl_aux `a` f x1 `a` f x2 `a` f x3
    Groot_obl_case x1 x2 x3 -> r Groot_obl_case `a` f x1 `a` f x2 `a` f x3
    Groot_obl_obj x1 x2 x3 -> r Groot_obl_obj `a` f x1 `a` f x2 `a` f x3
    Groot_obl_obl x1 x2 x3 -> r Groot_obl_obl `a` f x1 `a` f x2 `a` f x3
    Groot_obl_obl_obl_cc x1 x2 x3 x4 -> r Groot_obl_obl_obl_cc `a` f x1 `a` f x2 `a` f x3 `a` f x4
    Groot_obl_xcomp x1 x2 x3 -> r Groot_obl_xcomp `a` f x1 `a` f x2 `a` f x3
    Groot_only x1 -> r Groot_only `a` f x1
    Groot_parataxis x1 x2 -> r Groot_parataxis `a` f x1 `a` f x2
    Groot_xcomp x1 x2 -> r Groot_xcomp `a` f x1 `a` f x2
    Groot_xcomp_ccomp x1 x2 x3 -> r Groot_xcomp_ccomp `a` f x1 `a` f x2 `a` f x3
    GAdVVP x1 x2 -> r GAdVVP `a` f x1 `a` f x2
    GAdvVP x1 x2 -> r GAdvVP `a` f x1 `a` f x2
    GComplV x1 x2 -> r GComplV `a` f x1 `a` f x2
    GComplVP x1 x2 -> r GComplVP `a` f x1 `a` f x2
    GConjVP x1 x2 -> r GConjVP `a` f x1 `a` f x2
    GPassV x1 -> r GPassV `a` f x1
    GPassVAgent x1 x2 -> r GPassVAgent `a` f x1 `a` f x2
    GPrepVP x1 x2 -> r GPrepVP `a` f x1 `a` f x2
    GProgrVP x1 -> r GProgrVP `a` f x1
    GUseComp x1 -> r GUseComp `a` f x1
    GUseV x1 -> r GUseV `a` f x1
    GaclUDS_ x1 -> r GaclUDS_ `a` f x1
    GaclUDSgerund_ x1 -> r GaclUDSgerund_ `a` f x1
    GaclUDSpastpartParens_ x1 -> r GaclUDSpastpartParens_ `a` f x1
    GaclUDSpastpart_ x1 -> r GaclUDSpastpart_ `a` f x1
    Gacl_ x1 -> r Gacl_ `a` f x1
    GaclRelclRS_ x1 -> r GaclRelclRS_ `a` f x1
    GaclRelclUDS_ x1 -> r GaclRelclUDS_ `a` f x1
    GpassRelcl_ x1 x2 x3 -> r GpassRelcl_ `a` f x1 `a` f x2 `a` f x3
    GadvclMarkUDS_ x1 x2 -> r GadvclMarkUDS_ `a` f x1 `a` f x2
    GadvclUDS_ x1 -> r GadvclUDS_ `a` f x1
    Gadvcl_ x1 -> r Gadvcl_ `a` f x1
    Gadvmod_ x1 -> r Gadvmod_ `a` f x1
    GadvmodEmph_ x1 -> r GadvmodEmph_ `a` f x1
    GadvmodLmod_ x1 -> r GadvmodLmod_ `a` f x1
    Gamod_ x1 -> r Gamod_ `a` f x1
    Gappos_ x1 -> r Gappos_ `a` f x1
    Gaux_ x1 -> r Gaux_ `a` f x1
    Gcc_ x1 -> r Gcc_ `a` f x1
    GccPreconj_ x1 -> r GccPreconj_ `a` f x1
    Gccomp_ x1 -> r Gccomp_ `a` f x1
    Gclf_ x1 -> r Gclf_ `a` f x1
    Gcompound_ x1 -> r Gcompound_ `a` f x1
    GcompoundLvc_ x1 -> r GcompoundLvc_ `a` f x1
    GcompoundPrt_ x1 -> r GcompoundPrt_ `a` f x1
    GcompoundRedup_ x1 -> r GcompoundRedup_ `a` f x1
    GcompoundSvc_ x1 -> r GcompoundSvc_ `a` f x1
    GcsubjMarkFinite_ x1 x2 -> r GcsubjMarkFinite_ `a` f x1 `a` f x2
    GcsubjMarkInfinite_ x1 x2 -> r GcsubjMarkInfinite_ `a` f x1 `a` f x2
    Gcsubj_ x1 -> r Gcsubj_ `a` f x1
    GcsubjPass_ x1 -> r GcsubjPass_ `a` f x1
    Gdep_ x1 -> r Gdep_ `a` f x1
    Gdet_ x1 -> r Gdet_ `a` f x1
    GdetNumgov_ x1 -> r GdetNumgov_ `a` f x1
    GdetNummod_ x1 -> r GdetNummod_ `a` f x1
    GdetPoss_ x1 -> r GdetPoss_ `a` f x1
    Gdiscourse_ x1 -> r Gdiscourse_ `a` f x1
    Gdislocated_ x1 -> r Gdislocated_ `a` f x1
    Gexpl_ x1 -> r Gexpl_ `a` f x1
    GexplImpers_ x1 -> r GexplImpers_ `a` f x1
    GexplPass_ x1 -> r GexplPass_ `a` f x1
    GexplPv_ x1 -> r GexplPv_ `a` f x1
    Gfixed_ x1 -> r Gfixed_ `a` f x1
    Gflat_ x1 -> r Gflat_ `a` f x1
    GflatForeign_ x1 -> r GflatForeign_ `a` f x1
    GflatName_ x1 -> r GflatName_ `a` f x1
    Ggoeswith_ x1 -> r Ggoeswith_ `a` f x1
    Giobj_ x1 -> r Giobj_ `a` f x1
    Glist_ x1 -> r Glist_ `a` f x1
    Gmark_ x1 -> r Gmark_ `a` f x1
    Gnmod_ x1 x2 -> r Gnmod_ `a` f x1 `a` f x2
    GnmodPoss_ x1 -> r GnmodPoss_ `a` f x1
    GnmodTmod_ x1 -> r GnmodTmod_ `a` f x1
    Gnsubj_ x1 -> r Gnsubj_ `a` f x1
    GnsubjPass_ x1 -> r GnsubjPass_ `a` f x1
    Gnummod_ x1 -> r Gnummod_ `a` f x1
    GnummodGov_ x1 -> r GnummodGov_ `a` f x1
    Gobj_ x1 -> r Gobj_ `a` f x1
    GoblPrep_ x1 -> r GoblPrep_ `a` f x1
    GoblRP_ x1 x2 -> r GoblRP_ `a` f x1 `a` f x2
    Gobl_ x1 -> r Gobl_ `a` f x1
    GoblAgent_ x1 -> r GoblAgent_ `a` f x1
    GoblArg_ x1 -> r GoblArg_ `a` f x1
    GoblLmod_ x1 -> r GoblLmod_ `a` f x1
    GoblTmod_ x1 -> r GoblTmod_ `a` f x1
    Gorphan_ x1 -> r Gorphan_ `a` f x1
    Gparataxis_ x1 -> r Gparataxis_ `a` f x1
    Gpunct_ x1 -> r Gpunct_ `a` f x1
    Greparandum_ x1 -> r Greparandum_ `a` f x1
    GrootA_ x1 -> r GrootA_ `a` f x1
    GrootAdA_ x1 -> r GrootAdA_ `a` f x1
    GrootAdv_ x1 -> r GrootAdv_ `a` f x1
    GrootDAP_ x1 -> r GrootDAP_ `a` f x1
    GrootDet_ x1 -> r GrootDet_ `a` f x1
    GrootN_ x1 -> r GrootN_ `a` f x1
    GrootPrep_ x1 -> r GrootPrep_ `a` f x1
    GrootQuant_ x1 -> r GrootQuant_ `a` f x1
    GrootRP_ x1 -> r GrootRP_ `a` f x1
    GrootV_ x1 -> r GrootV_ `a` f x1
    Gvocative_ x1 -> r Gvocative_ `a` f x1
    GxcompA_ x1 -> r GxcompA_ `a` f x1
    GxcompA_ccomp_ x1 x2 -> r GxcompA_ccomp_ `a` f x1 `a` f x2
    GxcompAdv_ x1 -> r GxcompAdv_ `a` f x1
    GxcompN_ x1 -> r GxcompN_ `a` f x1
    GxcompToBeN_ x1 x2 x3 -> r GxcompToBeN_ `a` f x1 `a` f x2 `a` f x3
    GListAP x1 -> r GListAP `a` foldr (a . a (r (:)) . f) (r []) x1
    GListAdV x1 -> r GListAdV `a` foldr (a . a (r (:)) . f) (r []) x1
    GListAdv x1 -> r GListAdv `a` foldr (a . a (r (:)) . f) (r []) x1
    GListCN x1 -> r GListCN `a` foldr (a . a (r (:)) . f) (r []) x1
    GListDAP x1 -> r GListDAP `a` foldr (a . a (r (:)) . f) (r []) x1
    GListIAdv x1 -> r GListIAdv `a` foldr (a . a (r (:)) . f) (r []) x1
    GListNP x1 -> r GListNP `a` foldr (a . a (r (:)) . f) (r []) x1
    GListPrep x1 -> r GListPrep `a` foldr (a . a (r (:)) . f) (r []) x1
    GListRS x1 -> r GListRS `a` foldr (a . a (r (:)) . f) (r []) x1
    GListS x1 -> r GListS `a` foldr (a . a (r (:)) . f) (r []) x1
    GListUDFragment x1 -> r GListUDFragment `a` foldr (a . a (r (:)) . f) (r []) x1
    GListUDS x1 -> r GListUDS `a` foldr (a . a (r (:)) . f) (r []) x1
    GListVP x1 -> r GListVP `a` foldr (a . a (r (:)) . f) (r []) x1
    _ -> r t

class Compos t where
  compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)
         -> (forall a. t a -> m (t a)) -> t c -> m (t c)

composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c
composOp f = runIdentity . composOpM (Identity . f)

composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)
composOpM = compos return ap

composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t c -> m ()
composOpM_ = composOpFold (return ()) (>>)

composOpMonoid :: (Compos t, Monoid m) => (forall a. t a -> m) -> t c -> m
composOpMonoid = composOpFold mempty mappend

composOpMPlus :: (Compos t, MonadPlus m) => (forall a. t a -> m b) -> t c -> m b
composOpMPlus = composOpFold mzero mplus

composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b
composOpFold z c f = unC . compos (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)

newtype C b a = C { unC :: b }
