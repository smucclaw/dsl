{-# LANGUAGE GADTs, UndecidableInstances #-}
module LS.NLP.NL4 where

import Control.Monad.Identity (Identity ( Identity, runIdentity), MonadPlus (..), ap )
import Data.Monoid ()
import PGF (Expr, mkApp, mkCId, mkFloat, mkInt, mkStr, showCId, showExpr, unApp, unFloat, unInt, unStr )

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
instance (Gf (Tree a)) => Show (Tree a) where
    show = showExpr [] . gf
----------------------------------------------------

type GA = Tree GA_
data GA_
type GA2 = Tree GA2_
data GA2_
type GAP = Tree GAP_
data GAP_
type GAction = Tree GAction_
data GAction_
type GAdA = Tree GAdA_
data GAdA_
type GAdN = Tree GAdN_
data GAdN_
type GAdv = Tree GAdv_
data GAdv_
type GCAdv = Tree GCAdv_
data GCAdv_
type GCN = Tree GCN_
data GCN_
type GCard = Tree GCard_
data GCard_
type GComp = Tree GComp_
data GComp_
type GCond = Tree GCond_
data GCond_
type GConj = Tree GConj_
data GConj_
type GConstraint = Tree GConstraint_
data GConstraint_
type GDate = Tree GDate_
data GDate_
type GDay = Tree GDay_
data GDay_
type GDeontic = Tree GDeontic_
data GDeontic_
type GDet = Tree GDet_
data GDet_
type GDig = Tree GDig_
data GDig_
type GDigit = Tree GDigit_
data GDigit_
type GDigits = Tree GDigits_
data GDigits_
type GListAP = Tree GListAP_
data GListAP_
type GListAdv = Tree GListAdv_
data GListAdv_
type GListCond = Tree GListCond_
data GListCond_
type GListConstraint = Tree GListConstraint_
data GListConstraint_
type GListNP = Tree GListNP_
data GListNP_
type GListPrep = Tree GListPrep_
data GListPrep_
type GListQS = Tree GListQS_
data GListQS_
type GListS = Tree GListS_
data GListS_
type GListTComparison = Tree GListTComparison_
data GListTComparison_
type GListVPS = Tree GListVPS_
data GListVPS_
type GListWho = Tree GListWho_
data GListWho_
type GMonth = Tree GMonth_
data GMonth_
type GN = Tree GN_
data GN_
type GN2 = Tree GN2_
data GN2_
type GNP = Tree GNP_
data GNP_
type GNumeral = Tree GNumeral_
data GNumeral_
type GPN = Tree GPN_
data GPN_
type GPol = Tree GPol_
data GPol_
type GPrePost = Tree GPrePost_
data GPrePost_
type GPrep = Tree GPrep_
data GPrep_
type GQS = Tree GQS_
data GQS_
type GS = Tree GS_
data GS_
type GSub10 = Tree GSub10_
data GSub10_
type GSub100 = Tree GSub100_
data GSub100_
type GSub1000 = Tree GSub1000_
data GSub1000_
type GSub1000000 = Tree GSub1000000_
data GSub1000000_
type GSub1000000000 = Tree GSub1000000000_
data GSub1000000000_
type GSub1000000000000 = Tree GSub1000000000000_
data GSub1000000000000_
type GSubj = Tree GSubj_
data GSubj_
type GSubject = Tree GSubject_
data GSubject_
type GTComparison = Tree GTComparison_
data GTComparison_
type GTemp = Tree GTemp_
data GTemp_
type GTemporal = Tree GTemporal_
data GTemporal_
type GText = Tree GText_
data GText_
type GTimeUnit = Tree GTimeUnit_
data GTimeUnit_
type GUpon = Tree GUpon_
data GUpon_
type GV = Tree GV_
data GV_
type GV2 = Tree GV2_
data GV2_
type GVP = Tree GVP_
data GVP_
type GVPI = Tree GVPI_
data GVPI_
type GVPS = Tree GVPS_
data GVPS_
type GVS = Tree GVS_
data GVS_
type GVV = Tree GVV_
data GVV_
type GWho = Tree GWho_
data GWho_
type GYear = Tree GYear_
data GYear_
type GYearComponent = Tree GYearComponent_
data GYearComponent_
type GAnt = Tree GAnt_
data GAnt_
type GTense = Tree GTense_
data GTense_
type GString = Tree GString_
data GString_
type GInt = Tree GInt_
data GInt_
type GFloat = Tree GFloat_
data GFloat_

data Tree :: * -> * where
  G_accepted_A :: Tree GA_
  G_accidental_A :: Tree GA_
  G_active_A :: Tree GA_
  G_add_A :: Tree GA_
  G_adjusted_A :: Tree GA_
  G_alive_A :: Tree GA_
  G_basic_A :: Tree GA_
  G_biological_A :: Tree GA_
  G_chemical_A :: Tree GA_
  G_claimable_A :: Tree GA_
  G_commercial_A :: Tree GA_
  G_competitive_A :: Tree GA_
  G_current_A :: Tree GA_
  G_dangerous_A :: Tree GA_
  G_dead_A :: Tree GA_
  G_disabled_A :: Tree GA_
  G_diving_A :: Tree GA_
  G_double_A :: Tree GA_
  G_due_A :: Tree GA_
  G_equal_A :: Tree GA_
  G_first_A :: Tree GA_
  G_fit_A :: Tree GA_
  G_fractured_A :: Tree GA_
  G_geographical_A :: Tree GA_
  G_great_A :: Tree GA_
  G_hfmd_A :: Tree GA_
  G_high_A :: Tree GA_
  G_human_A :: Tree GA_
  G_incurable_A :: Tree GA_
  G_infectious_A :: Tree GA_
  G_initial_A :: Tree GA_
  G_intentional_A :: Tree GA_
  G_japanese_A :: Tree GA_
  G_juvenile_A :: Tree GA_
  G_less_A :: Tree GA_
  G_little_A :: Tree GA_
  G_logical_A :: Tree GA_
  G_low_A :: Tree GA_
  G_martial_A :: Tree GA_
  G_maximum_A :: Tree GA_
  G_medical_A :: Tree GA_
  G_mental_A :: Tree GA_
  G_middle_A :: Tree GA_
  G_military_A :: Tree GA_
  G_more_A :: Tree GA_
  G_multiple_A :: Tree GA_
  G_national_A :: Tree GA_
  G_normal_A :: Tree GA_
  G_nuclear_A :: Tree GA_
  G_other_A :: Tree GA_
  G_particular_A :: Tree GA_
  G_past_A :: Tree GA_
  G_payable_A :: Tree GA_
  G_permanent_A :: Tree GA_
  G_physical_A :: Tree GA_
  G_possible_A :: Tree GA_
  G_previous_A :: Tree GA_
  G_private_A :: Tree GA_
  G_professional_A :: Tree GA_
  G_public_A :: Tree GA_
  G_reckless_A :: Tree GA_
  G_registered_A :: Tree GA_
  G_relevant_A :: Tree GA_
  G_subject_A :: Tree GA_
  G_successful_A :: Tree GA_
  G_surgical_A :: Tree GA_
  G_third_A :: Tree GA_
  G_total_A :: Tree GA_
  G_traditional_A :: Tree GA_
  G_triple_A :: Tree GA_
  G_unlawful_A :: Tree GA_
  G_unnecessary_A :: Tree GA_
  G_unsound_A :: Tree GA_
  G_unsuccessful_A :: Tree GA_
  G_upscaled_A :: Tree GA_
  G_valid_A :: Tree GA_
  G_viral_A :: Tree GA_
  G_waterborne_A :: Tree GA_
  G_located_in_A2 :: Tree GA2_
  GComplA2 :: GA2 -> GNP -> Tree GAP_
  GConjAP :: GConj -> GListAP -> Tree GAP_
  GInt_or_older :: GInt -> Tree GAP_
  GPositA :: GA -> Tree GAP_
  Gcaused_by :: GNP -> Tree GAP_
  Gensuing :: GNP -> Tree GAP_
  LexAP :: String -> Tree GAP_
  GACTION :: GVP -> Tree GAction_
  GrecoverUnparsedAction :: GString -> Tree GAction_
  Gonly_AdA :: Tree GAdA_
  GAdnCAdv :: GCAdv -> Tree GAdN_
  Gat_least_AdN :: Tree GAdN_
  Gat_most_AdN :: Tree GAdN_
  GAdAdv :: GAdA -> GAdv -> Tree GAdv_
  GByVP :: GVP -> Tree GAdv_
  GConjAdv :: GConj -> GListAdv -> Tree GAdv_
  GPrepNP :: GPrep -> GNP -> Tree GAdv_
  GSubjS :: GSubj -> GS -> Tree GAdv_
  GWhileDoing :: GVP -> Tree GAdv_
  G_as_Adv :: Tree GAdv_
  G_at_Adv :: Tree GAdv_
  G_first_Adv :: Tree GAdv_
  G_fully_Adv :: Tree GAdv_
  G_hence_Adv :: Tree GAdv_
  G_least_Adv :: Tree GAdv_
  G_on_its_way_Adv :: Tree GAdv_
  G_only_Adv :: Tree GAdv_
  G_permanently_Adv :: Tree GAdv_
  G_pland_Adv :: Tree GAdv_
  G_soon_Adv :: Tree GAdv_
  G_then_Adv :: Tree GAdv_
  G_totally_Adv :: Tree GAdv_
  G_up_Adv :: Tree GAdv_
  Gin_part :: Tree GAdv_
  Gin_whole :: Tree GAdv_
  GrecoverUnparsedAdv :: GString -> Tree GAdv_
  Gless_CAdv :: Tree GCAdv_
  Gmore_CAdv :: Tree GCAdv_
  GAdjCN :: GAP -> GCN -> Tree GCN_
  GCNwhereS :: GCN -> GNP -> GVPS -> Tree GCN_
  GUseN :: GN -> Tree GCN_
  LexCN :: String -> Tree GCN_
  GAdNum :: GAdN -> GCard -> Tree GCard_
  GNumDigits :: GDigits -> Tree GCard_
  GCompAP :: GAP -> Tree GComp_
  GCompAdv :: GAdv -> Tree GComp_
  GCompNP :: GNP -> Tree GComp_
  GConjCond :: GConj -> GListCond -> Tree GCond_
  GConjPreCond :: GPrePost -> GConj -> GListCond -> Tree GCond_
  GConjPrePostCond :: GPrePost -> GPrePost -> GConj -> GListCond -> Tree GCond_
  GRPConstraint :: GCond -> GTComparison -> GDate -> Tree GCond_
  GWHEN :: GNP -> GTemp -> GPol -> GVP -> Tree GCond_
  GrecoverUnparsedCond :: GString -> Tree GCond_
  GAND :: Tree GConj_
  GOR :: Tree GConj_
  GConjConstraint :: GConj -> GListConstraint -> Tree GConstraint_
  GConjPreConstraint :: GPrePost -> GConj -> GListConstraint -> Tree GConstraint_
  GConjPrePostConstraint :: GPrePost -> GPrePost -> GConj -> GListConstraint -> Tree GConstraint_
  GRPleafAP :: GAP -> Tree GConstraint_
  GRPleafAdv :: GAdv -> Tree GConstraint_
  GRPleafNP :: GNP -> Tree GConstraint_
  GRPleafS :: GNP -> GVPS -> Tree GConstraint_
  GRPleafVP :: GVPS -> Tree GConstraint_
  GrecoverRPis :: GString -> GString -> Tree GConstraint_
  GrecoverRPmath :: GString -> GString -> GString -> Tree GConstraint_
  GrecoverUnparsedConstraint :: GString -> Tree GConstraint_
  GMkDate :: GDay -> GMonth -> GYear -> Tree GDate_
  LexDay :: String -> Tree GDay_
  GMAY :: Tree GDeontic_
  GMUST :: Tree GDeontic_
  GSHANT :: Tree GDeontic_
  GaPl :: Tree GDet_
  GaSg :: Tree GDet_
  GthePl :: Tree GDet_
  GtheSg :: Tree GDet_
  Gyour :: Tree GDet_
  LexDig :: String -> Tree GDig_
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
  GListAP :: [GAP] -> Tree GListAP_
  GListAdv :: [GAdv] -> Tree GListAdv_
  GListCond :: [GCond] -> Tree GListCond_
  GListConstraint :: [GConstraint] -> Tree GListConstraint_
  GListNP :: [GNP] -> Tree GListNP_
  GListPrep :: [GPrep] -> Tree GListPrep_
  GListQS :: [GQS] -> Tree GListQS_
  GListS :: [GS] -> Tree GListS_
  GListTComparison :: [GTComparison] -> Tree GListTComparison_
  GListVPS :: [GVPS] -> Tree GListVPS_
  GListWho :: [GWho] -> Tree GListWho_
  LexMonth :: String -> Tree GMonth_
  GCompoundN :: GN -> GN -> Tree GN_
  LexN :: String -> Tree GN_
  G_premise_where_N2 :: Tree GN2_
  GConjNP :: GConj -> GListNP -> Tree GNP_
  GContents :: Tree GNP_
  GDetCN :: GDet -> GCN -> Tree GNP_
  GGerundNP :: GVP -> Tree GNP_
  GLoss_or_Damage :: Tree GNP_
  GMassNP :: GCN -> Tree GNP_
  GNDB_Qualification :: Tree GNP_
  GUsePN :: GPN -> Tree GNP_
  Ganimal :: Tree GNP_
  Gany_other_exclusion :: Tree GNP_
  Gbirds :: Tree GNP_
  Gcancelled :: Tree GNP_
  Gclaim :: Tree GNP_
  Gcondition :: Tree GNP_
  Ghousehold_appliance :: Tree GNP_
  Ginsects :: Tree GNP_
  Gplumbing_heating_or_AC :: Tree GNP_
  Gpremium :: Tree GNP_
  Gresult_from :: GNP -> Tree GNP_
  Grodents :: Tree GNP_
  Gsigned :: Tree GNP_
  Gstay_during_policy_period :: Tree GNP_
  Gstay_overnight :: Tree GNP_
  Gswimming_pool :: Tree GNP_
  Gvermin :: Tree GNP_
  Gwater :: Tree GNP_
  Gnum :: GSub1000000 -> Tree GNumeral_
  G_1012_PN :: Tree GPN_
  G_1013_PN :: Tree GPN_
  G_1014_PN :: Tree GPN_
  G_333A_PN :: Tree GPN_
  G_ADD_PN :: Tree GPN_
  G_AIDS_PN :: Tree GPN_
  G_Accident_PN :: Tree GPN_
  G_Accidental_PN :: Tree GPN_
  G_Accidents_PN :: Tree GPN_
  G_Address_PN :: Tree GPN_
  G_Adjustment_PN :: Tree GPN_
  G_Assured_PN :: Tree GPN_
  G_BSA_PN :: Tree GPN_
  G_Benefit_PN :: Tree GPN_
  G_Cap_PN :: Tree GPN_
  G_Claim_PN :: Tree GPN_
  G_CoV_PN :: Tree GPN_
  G_Conditions_PN :: Tree GPN_
  G_Date_PN :: Tree GPN_
  G_Death_PN :: Tree GPN_
  G_Details_PN :: Tree GPN_
  G_Disease_PN :: Tree GPN_
  G_Dismemberment_PN :: Tree GPN_
  G_Event_PN :: Tree GPN_
  G_Expense_PN :: Tree GPN_
  G_Flu_PN :: Tree GPN_
  G_H5N1_PN :: Tree GPN_
  G_H7N9_PN :: Tree GPN_
  G_H7N_PN :: Tree GPN_
  G_H9N2_PN :: Tree GPN_
  G_HAS_PN :: Tree GPN_
  G_Head_PN :: Tree GPN_
  G_Health_PN :: Tree GPN_
  G_Influenza_PN :: Tree GPN_
  G_Injury_PN :: Tree GPN_
  G_Insurer_PN :: Tree GPN_
  G_LA_PN :: Tree GPN_
  G_LE_PN :: Tree GPN_
  G_Leg_PN :: Tree GPN_
  G_Legionnaires_PN :: Tree GPN_
  G_Life_PN :: Tree GPN_
  G_Limit_PN :: Tree GPN_
  G_MAP_PN :: Tree GPN_
  G_MIN_PN :: Tree GPN_
  G_MR_PN :: Tree GPN_
  G_M_PN :: Tree GPN_
  G_Medicine_PN :: Tree GPN_
  G_Melioidosis_PN :: Tree GPN_
  G_Ministry_PN :: Tree GPN_
  G_Mumps_PN :: Tree GPN_
  G_N_PN :: Tree GPN_
  G_Nipah_PN :: Tree GPN_
  G_Ontology_PN :: Tree GPN_
  G_PS_PN :: Tree GPN_
  G_Plan14_PN :: Tree GPN_
  G_PlanAF_PN :: Tree GPN_
  G_PolicyHolder_PN :: Tree GPN_
  G_Policy_PN :: Tree GPN_
  G_RETURN_PN :: Tree GPN_
  G_Reductions_PN :: Tree GPN_
  G_Removal_PN :: Tree GPN_
  G_Republic_PN :: Tree GPN_
  G_SA_PN :: Tree GPN_
  G_SG_PN :: Tree GPN_
  G_Schedule_PN :: Tree GPN_
  G_Section_PN :: Tree GPN_
  G_Service_PN :: Tree GPN_
  G_Singapore_PN :: Tree GPN_
  G_Step_PN :: Tree GPN_
  G_Subscribed_PN :: Tree GPN_
  G_TABLE_PN :: Tree GPN_
  G_Teeth_PN :: Tree GPN_
  G_Triple_PN :: Tree GPN_
  G_Type_PN :: Tree GPN_
  G_Types_PN :: Tree GPN_
  G_UPON_PN :: Tree GPN_
  G_Wife_PN :: Tree GPN_
  G_Yellow_PN :: Tree GPN_
  G_addSA_PN :: Tree GPN_
  G_benADD_PN :: Tree GPN_
  G_benADDs_PN :: Tree GPN_
  G_benRA_PN :: Tree GPN_
  G_benTCM_PN :: Tree GPN_
  G_circ_PN :: Tree GPN_
  G_dTime_PN :: Tree GPN_
  G_dType_PN :: Tree GPN_
  G_diving_PN :: Tree GPN_
  G_holder_PN :: Tree GPN_
  G_motocross_PN :: Tree GPN_
  G_p_PN :: Tree GPN_
  G_plan3_PN :: Tree GPN_
  G_plan4_PN :: Tree GPN_
  G_planAF_PN :: Tree GPN_
  G_planB_PN :: Tree GPN_
  G_planC_PN :: Tree GPN_
  G_planE_PN :: Tree GPN_
  G_planF_PN :: Tree GPN_
  G_policyHolder_PN :: Tree GPN_
  G_qualifies_for_add_PN :: Tree GPN_
  G_schema_PN :: Tree GPN_
  G_sum_list_PN :: Tree GPN_
  G_x_PN :: Tree GPN_
  G_y_PN :: Tree GPN_
  GNEG :: Tree GPol_
  GPOS :: Tree GPol_
  GAP_PrePost :: GAP -> Tree GPrePost_
  GAdv_PrePost :: GAdv -> Tree GPrePost_
  GNP_PrePost :: GNP -> Tree GPrePost_
  GNP_caused_NP_to_VP_Prep_PrePost :: GNP -> GNP -> GVP -> GPrep -> Tree GPrePost_
  GNP_caused_by_PrePost :: GNP -> Tree GPrePost_
  GS_PrePost :: GNP -> GVPS -> Tree GPrePost_
  GV2_PrePost :: GV2 -> Tree GPrePost_
  GrecoverUnparsedPrePost :: GString -> Tree GPrePost_
  GConjPrep :: GConj -> GListPrep -> Tree GPrep_
  G_across_Prep :: Tree GPrep_
  G_after_Prep :: Tree GPrep_
  G_as_Prep :: Tree GPrep_
  G_at_Prep :: Tree GPrep_
  G_before_Prep :: Tree GPrep_
  G_between_Prep :: Tree GPrep_
  G_by_Prep :: Tree GPrep_
  G_during_Prep :: Tree GPrep_
  G_for_Prep :: Tree GPrep_
  G_from_Prep :: Tree GPrep_
  G_in_Prep :: Tree GPrep_
  G_into_Prep :: Tree GPrep_
  G_of_Prep :: Tree GPrep_
  G_on_Prep :: Tree GPrep_
  G_out_Prep :: Tree GPrep_
  G_over_Prep :: Tree GPrep_
  G_per_Prep :: Tree GPrep_
  G_than_Prep :: Tree GPrep_
  G_through_Prep :: Tree GPrep_
  G_to_Prep :: Tree GPrep_
  G_under_Prep :: Tree GPrep_
  G_up_Prep :: Tree GPrep_
  G_with_Prep :: Tree GPrep_
  G_within_Prep :: Tree GPrep_
  Gabout_Prep :: Tree GPrep_
  Gafter_Prep :: Tree GPrep_
  Gbefore_Prep :: Tree GPrep_
  Gfor_Prep :: Tree GPrep_
  Gfrom_Prep :: Tree GPrep_
  Gon_Prep :: Tree GPrep_
  Gpossess_Prep :: Tree GPrep_
  Gto_Prep :: Tree GPrep_
  Gwithin_Prep :: Tree GPrep_
  GConjPrePostQS :: GString -> GString -> GConj -> GListQS -> Tree GQS_
  GConjQS :: GConj -> GListQS -> Tree GQS_
  GConjPrePostS :: GString -> GString -> GConj -> GListS -> Tree GS_
  GConjS :: GConj -> GListS -> Tree GS_
  GPredVPS :: GNP -> GVPS -> Tree GS_
  GReferenceNP :: GNP -> Tree GS_
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
  Gpot21 :: Tree GSub1000_
  Gpot2plus :: GSub10 -> GSub100 -> Tree GSub1000_
  Gpot2as3 :: GSub1000 -> Tree GSub1000000_
  Gpot3 :: GSub1000 -> Tree GSub1000000_
  Gpot31 :: Tree GSub1000000_
  Gpot3float :: GFloat -> Tree GSub1000000_
  Gpot3plus :: GSub1000 -> GSub1000 -> Tree GSub1000000_
  Gpot3as4 :: GSub1000000 -> Tree GSub1000000000_
  Gpot4 :: GSub1000 -> Tree GSub1000000000_
  Gpot41 :: Tree GSub1000000000_
  Gpot4float :: GFloat -> Tree GSub1000000000_
  Gpot4plus :: GSub1000 -> GSub1000000 -> Tree GSub1000000000_
  Gpot4as5 :: GSub1000000000 -> Tree GSub1000000000000_
  Gpot5 :: GSub1000 -> Tree GSub1000000000000_
  Gpot51 :: Tree GSub1000000000000_
  Gpot5float :: GFloat -> Tree GSub1000000000000_
  Gpot5plus :: GSub1000 -> GSub1000000000 -> Tree GSub1000000000000_
  G_and_Subj :: Tree GSubj_
  G_as_Subj :: Tree GSubj_
  G_before_Subj :: Tree GSubj_
  G_both_Subj :: Tree GSubj_
  G_but_Subj :: Tree GSubj_
  G_if_Subj :: Tree GSubj_
  G_or_Subj :: Tree GSubj_
  G_that_Subj :: Tree GSubj_
  G_when_Subj :: Tree GSubj_
  G_while_Subj :: Tree GSubj_
  GAN :: GCN -> Tree GSubject_
  GEVERY :: GCN -> Tree GSubject_
  GPARTY :: GCN -> Tree GSubject_
  GSubjWho :: GSubject -> GWho -> Tree GSubject_
  GTHE :: GCN -> Tree GSubject_
  GYou :: Tree GSubject_
  GrecoverUnparsedSubj :: GString -> Tree GSubject_
  GAFTER :: Tree GTComparison_
  GBEFORE :: Tree GTComparison_
  GBY :: Tree GTComparison_
  GConjTComparison :: GConj -> GListTComparison -> Tree GTComparison_
  GON :: Tree GTComparison_
  GVAGUE :: Tree GTComparison_
  GpastSimul :: Tree GTemp_
  GpresAnt :: Tree GTemp_
  GpresSimul :: Tree GTemp_
  GTemporalConstraint :: GTComparison -> GDigits -> GTimeUnit -> Tree GTemporal_
  GTemporalConstraintNoDigits :: GTComparison -> GTimeUnit -> Tree GTemporal_
  GRegulative :: GSubject -> GDeontic -> GAction -> Tree GText_
  GadvUPON :: GUpon -> Tree GText_
  GqCOND :: GCond -> Tree GText_
  GqCONSTR :: GConstraint -> Tree GText_
  GqPREPOST :: GPrePost -> Tree GText_
  GqUPON :: GSubject -> GUpon -> Tree GText_
  GqWHO :: GSubject -> GWho -> Tree GText_
  GsCOND :: GCond -> Tree GText_
  GsUPON :: GSubject -> GUpon -> Tree GText_
  GsWHO :: GSubject -> GWho -> Tree GText_
  GDay_Unit :: Tree GTimeUnit_
  GMonth_Unit :: Tree GTimeUnit_
  GYear_Unit :: Tree GTimeUnit_
  GrecoverUnparsedTimeUnit :: GString -> Tree GTimeUnit_
  GUPON :: GVP -> Tree GUpon_
  GrecoverUnparsedUpon :: GString -> Tree GUpon_
  G_H7N7_V :: Tree GV_
  G_adjust_V :: Tree GV_
  G_apply_V :: Tree GV_
  G_asssure_V :: Tree GV_
  G_assure_V :: Tree GV_
  G_benefit_V :: Tree GV_
  G_canoe_V :: Tree GV_
  G_cave_V :: Tree GV_
  G_claim_V :: Tree GV_
  G_cover_V :: Tree GV_
  G_establish_V :: Tree GV_
  G_exception_V :: Tree GV_
  G_give_V :: Tree GV_
  G_glide_V :: Tree GV_
  G_govern_V :: Tree GV_
  G_hand_V :: Tree GV_
  G_hernia_V :: Tree GV_
  G_hunt_V :: Tree GV_
  G_include_V :: Tree GV_
  G_license_V :: Tree GV_
  G_mean_V :: Tree GV_
  G_met_common_requirement_for_add_V :: Tree GV_
  G_mountaineer_V :: Tree GV_
  G_occur_V :: Tree GV_
  G_organise_V :: Tree GV_
  G_parachute_V :: Tree GV_
  G_pay_V :: Tree GV_
  G_policyholder_V :: Tree GV_
  G_pothole_V :: Tree GV_
  G_race_V :: Tree GV_
  G_recognise_V :: Tree GV_
  G_register_V :: Tree GV_
  G_riot_V :: Tree GV_
  G_sail_V :: Tree GV_
  G_skydive_V :: Tree GV_
  G_start_V :: Tree GV_
  G_stepupsumassure_V :: Tree GV_
  G_subscribe_V :: Tree GV_
  G_suffer_V :: Tree GV_
  G_sumassure_V :: Tree GV_
  G_supervise_V :: Tree GV_
  G_train_V :: Tree GV_
  G_windsurf_V :: Tree GV_
  LexV2 :: String -> Tree GV2_
  GAdvVP :: GVP -> GAdv -> Tree GVP_
  GComplV2 :: GV2 -> GNP -> Tree GVP_
  GComplV2S :: GV2 -> GNP -> GS -> Tree GVP_
  GComplVAS :: GV2 -> GAP -> GS -> Tree GVP_
  GComplVSif :: GVS -> GS -> Tree GVP_
  GComplVSthat :: GVS -> GS -> Tree GVP_
  GUseComp :: GComp -> Tree GVP_
  LexVP :: String -> Tree GVP_
  GMkVPI :: GVP -> Tree GVPI_
  GComparison_Card_Years :: GCard -> Tree GVPS_
  GConjPrePostVPS :: GString -> GString -> GConj -> GListVPS -> Tree GVPS_
  GConjVPS :: GConj -> GListVPS -> Tree GVPS_
  GGreaterThan :: GNP -> Tree GVPS_
  GLessThan :: GNP -> Tree GVPS_
  GMayHave :: GVP -> Tree GVPS_
  GMkVPS :: GTemp -> GPol -> GVP -> Tree GVPS_
  LexVS :: String -> Tree GVS_
  LexVV :: String -> Tree GVV_
  GAPWho :: GAP -> Tree GWho_
  GAdvWho :: GAdv -> Tree GWho_
  GConjPrePostWho :: GPrePost -> GPrePost -> GConj -> GListWho -> Tree GWho_
  GConjPreWho :: GPrePost -> GConj -> GListWho -> Tree GWho_
  GConjWho :: GConj -> GListWho -> Tree GWho_
  GWHO :: GTemp -> GPol -> GVP -> Tree GWho_
  GrecoverUnparsedWho :: GString -> Tree GWho_
  GMkYear :: GYearComponent -> GYearComponent -> GYearComponent -> GYearComponent -> Tree GYear_
  LexYearComponent :: String -> Tree GYearComponent_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (G_accepted_A,G_accepted_A) -> and [ ]
    (G_accidental_A,G_accidental_A) -> and [ ]
    (G_active_A,G_active_A) -> and [ ]
    (G_add_A,G_add_A) -> and [ ]
    (G_adjusted_A,G_adjusted_A) -> and [ ]
    (G_alive_A,G_alive_A) -> and [ ]
    (G_basic_A,G_basic_A) -> and [ ]
    (G_biological_A,G_biological_A) -> and [ ]
    (G_chemical_A,G_chemical_A) -> and [ ]
    (G_claimable_A,G_claimable_A) -> and [ ]
    (G_commercial_A,G_commercial_A) -> and [ ]
    (G_competitive_A,G_competitive_A) -> and [ ]
    (G_current_A,G_current_A) -> and [ ]
    (G_dangerous_A,G_dangerous_A) -> and [ ]
    (G_dead_A,G_dead_A) -> and [ ]
    (G_disabled_A,G_disabled_A) -> and [ ]
    (G_diving_A,G_diving_A) -> and [ ]
    (G_double_A,G_double_A) -> and [ ]
    (G_due_A,G_due_A) -> and [ ]
    (G_equal_A,G_equal_A) -> and [ ]
    (G_first_A,G_first_A) -> and [ ]
    (G_fit_A,G_fit_A) -> and [ ]
    (G_fractured_A,G_fractured_A) -> and [ ]
    (G_geographical_A,G_geographical_A) -> and [ ]
    (G_great_A,G_great_A) -> and [ ]
    (G_hfmd_A,G_hfmd_A) -> and [ ]
    (G_high_A,G_high_A) -> and [ ]
    (G_human_A,G_human_A) -> and [ ]
    (G_incurable_A,G_incurable_A) -> and [ ]
    (G_infectious_A,G_infectious_A) -> and [ ]
    (G_initial_A,G_initial_A) -> and [ ]
    (G_intentional_A,G_intentional_A) -> and [ ]
    (G_japanese_A,G_japanese_A) -> and [ ]
    (G_juvenile_A,G_juvenile_A) -> and [ ]
    (G_less_A,G_less_A) -> and [ ]
    (G_little_A,G_little_A) -> and [ ]
    (G_logical_A,G_logical_A) -> and [ ]
    (G_low_A,G_low_A) -> and [ ]
    (G_martial_A,G_martial_A) -> and [ ]
    (G_maximum_A,G_maximum_A) -> and [ ]
    (G_medical_A,G_medical_A) -> and [ ]
    (G_mental_A,G_mental_A) -> and [ ]
    (G_middle_A,G_middle_A) -> and [ ]
    (G_military_A,G_military_A) -> and [ ]
    (G_more_A,G_more_A) -> and [ ]
    (G_multiple_A,G_multiple_A) -> and [ ]
    (G_national_A,G_national_A) -> and [ ]
    (G_normal_A,G_normal_A) -> and [ ]
    (G_nuclear_A,G_nuclear_A) -> and [ ]
    (G_other_A,G_other_A) -> and [ ]
    (G_particular_A,G_particular_A) -> and [ ]
    (G_past_A,G_past_A) -> and [ ]
    (G_payable_A,G_payable_A) -> and [ ]
    (G_permanent_A,G_permanent_A) -> and [ ]
    (G_physical_A,G_physical_A) -> and [ ]
    (G_possible_A,G_possible_A) -> and [ ]
    (G_previous_A,G_previous_A) -> and [ ]
    (G_private_A,G_private_A) -> and [ ]
    (G_professional_A,G_professional_A) -> and [ ]
    (G_public_A,G_public_A) -> and [ ]
    (G_reckless_A,G_reckless_A) -> and [ ]
    (G_registered_A,G_registered_A) -> and [ ]
    (G_relevant_A,G_relevant_A) -> and [ ]
    (G_subject_A,G_subject_A) -> and [ ]
    (G_successful_A,G_successful_A) -> and [ ]
    (G_surgical_A,G_surgical_A) -> and [ ]
    (G_third_A,G_third_A) -> and [ ]
    (G_total_A,G_total_A) -> and [ ]
    (G_traditional_A,G_traditional_A) -> and [ ]
    (G_triple_A,G_triple_A) -> and [ ]
    (G_unlawful_A,G_unlawful_A) -> and [ ]
    (G_unnecessary_A,G_unnecessary_A) -> and [ ]
    (G_unsound_A,G_unsound_A) -> and [ ]
    (G_unsuccessful_A,G_unsuccessful_A) -> and [ ]
    (G_upscaled_A,G_upscaled_A) -> and [ ]
    (G_valid_A,G_valid_A) -> and [ ]
    (G_viral_A,G_viral_A) -> and [ ]
    (G_waterborne_A,G_waterborne_A) -> and [ ]
    (G_located_in_A2,G_located_in_A2) -> and [ ]
    (GComplA2 x1 x2,GComplA2 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GConjAP x1 x2,GConjAP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GInt_or_older x1,GInt_or_older y1) -> and [ x1 == y1 ]
    (GPositA x1,GPositA y1) -> and [ x1 == y1 ]
    (Gcaused_by x1,Gcaused_by y1) -> and [ x1 == y1 ]
    (Gensuing x1,Gensuing y1) -> and [ x1 == y1 ]
    (LexAP x,LexAP y) -> x == y
    (GACTION x1,GACTION y1) -> and [ x1 == y1 ]
    (GrecoverUnparsedAction x1,GrecoverUnparsedAction y1) -> and [ x1 == y1 ]
    (Gonly_AdA,Gonly_AdA) -> and [ ]
    (GAdnCAdv x1,GAdnCAdv y1) -> and [ x1 == y1 ]
    (Gat_least_AdN,Gat_least_AdN) -> and [ ]
    (Gat_most_AdN,Gat_most_AdN) -> and [ ]
    (GAdAdv x1 x2,GAdAdv y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GByVP x1,GByVP y1) -> and [ x1 == y1 ]
    (GConjAdv x1 x2,GConjAdv y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPrepNP x1 x2,GPrepNP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSubjS x1 x2,GSubjS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GWhileDoing x1,GWhileDoing y1) -> and [ x1 == y1 ]
    (G_as_Adv,G_as_Adv) -> and [ ]
    (G_at_Adv,G_at_Adv) -> and [ ]
    (G_first_Adv,G_first_Adv) -> and [ ]
    (G_fully_Adv,G_fully_Adv) -> and [ ]
    (G_hence_Adv,G_hence_Adv) -> and [ ]
    (G_least_Adv,G_least_Adv) -> and [ ]
    (G_on_its_way_Adv,G_on_its_way_Adv) -> and [ ]
    (G_only_Adv,G_only_Adv) -> and [ ]
    (G_permanently_Adv,G_permanently_Adv) -> and [ ]
    (G_pland_Adv,G_pland_Adv) -> and [ ]
    (G_soon_Adv,G_soon_Adv) -> and [ ]
    (G_then_Adv,G_then_Adv) -> and [ ]
    (G_totally_Adv,G_totally_Adv) -> and [ ]
    (G_up_Adv,G_up_Adv) -> and [ ]
    (Gin_part,Gin_part) -> and [ ]
    (Gin_whole,Gin_whole) -> and [ ]
    (GrecoverUnparsedAdv x1,GrecoverUnparsedAdv y1) -> and [ x1 == y1 ]
    (Gless_CAdv,Gless_CAdv) -> and [ ]
    (Gmore_CAdv,Gmore_CAdv) -> and [ ]
    (GAdjCN x1 x2,GAdjCN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GCNwhereS x1 x2 x3,GCNwhereS y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GUseN x1,GUseN y1) -> and [ x1 == y1 ]
    (LexCN x,LexCN y) -> x == y
    (GAdNum x1 x2,GAdNum y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNumDigits x1,GNumDigits y1) -> and [ x1 == y1 ]
    (GCompAP x1,GCompAP y1) -> and [ x1 == y1 ]
    (GCompAdv x1,GCompAdv y1) -> and [ x1 == y1 ]
    (GCompNP x1,GCompNP y1) -> and [ x1 == y1 ]
    (GConjCond x1 x2,GConjCond y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GConjPreCond x1 x2 x3,GConjPreCond y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GConjPrePostCond x1 x2 x3 x4,GConjPrePostCond y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GRPConstraint x1 x2 x3,GRPConstraint y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GWHEN x1 x2 x3 x4,GWHEN y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GrecoverUnparsedCond x1,GrecoverUnparsedCond y1) -> and [ x1 == y1 ]
    (GAND,GAND) -> and [ ]
    (GOR,GOR) -> and [ ]
    (GConjConstraint x1 x2,GConjConstraint y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GConjPreConstraint x1 x2 x3,GConjPreConstraint y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GConjPrePostConstraint x1 x2 x3 x4,GConjPrePostConstraint y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GRPleafAP x1,GRPleafAP y1) -> and [ x1 == y1 ]
    (GRPleafAdv x1,GRPleafAdv y1) -> and [ x1 == y1 ]
    (GRPleafNP x1,GRPleafNP y1) -> and [ x1 == y1 ]
    (GRPleafS x1 x2,GRPleafS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRPleafVP x1,GRPleafVP y1) -> and [ x1 == y1 ]
    (GrecoverRPis x1 x2,GrecoverRPis y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GrecoverRPmath x1 x2 x3,GrecoverRPmath y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GrecoverUnparsedConstraint x1,GrecoverUnparsedConstraint y1) -> and [ x1 == y1 ]
    (GMkDate x1 x2 x3,GMkDate y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (LexDay x,LexDay y) -> x == y
    (GMAY,GMAY) -> and [ ]
    (GMUST,GMUST) -> and [ ]
    (GSHANT,GSHANT) -> and [ ]
    (GaPl,GaPl) -> and [ ]
    (GaSg,GaSg) -> and [ ]
    (GthePl,GthePl) -> and [ ]
    (GtheSg,GtheSg) -> and [ ]
    (Gyour,Gyour) -> and [ ]
    (LexDig x,LexDig y) -> x == y
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
    (GListAP x1,GListAP y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListAdv x1,GListAdv y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListCond x1,GListCond y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListConstraint x1,GListConstraint y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListNP x1,GListNP y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListPrep x1,GListPrep y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListQS x1,GListQS y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListS x1,GListS y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListTComparison x1,GListTComparison y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListVPS x1,GListVPS y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListWho x1,GListWho y1) -> and [x == y | (x,y) <- zip x1 y1]
    (LexMonth x,LexMonth y) -> x == y
    (GCompoundN x1 x2,GCompoundN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexN x,LexN y) -> x == y
    (G_premise_where_N2,G_premise_where_N2) -> and [ ]
    (GConjNP x1 x2,GConjNP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GContents,GContents) -> and [ ]
    (GDetCN x1 x2,GDetCN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GGerundNP x1,GGerundNP y1) -> and [ x1 == y1 ]
    (GLoss_or_Damage,GLoss_or_Damage) -> and [ ]
    (GMassNP x1,GMassNP y1) -> and [ x1 == y1 ]
    (GNDB_Qualification,GNDB_Qualification) -> and [ ]
    (GUsePN x1,GUsePN y1) -> and [ x1 == y1 ]
    (Ganimal,Ganimal) -> and [ ]
    (Gany_other_exclusion,Gany_other_exclusion) -> and [ ]
    (Gbirds,Gbirds) -> and [ ]
    (Gcancelled,Gcancelled) -> and [ ]
    (Gclaim,Gclaim) -> and [ ]
    (Gcondition,Gcondition) -> and [ ]
    (Ghousehold_appliance,Ghousehold_appliance) -> and [ ]
    (Ginsects,Ginsects) -> and [ ]
    (Gplumbing_heating_or_AC,Gplumbing_heating_or_AC) -> and [ ]
    (Gpremium,Gpremium) -> and [ ]
    (Gresult_from x1,Gresult_from y1) -> and [ x1 == y1 ]
    (Grodents,Grodents) -> and [ ]
    (Gsigned,Gsigned) -> and [ ]
    (Gstay_during_policy_period,Gstay_during_policy_period) -> and [ ]
    (Gstay_overnight,Gstay_overnight) -> and [ ]
    (Gswimming_pool,Gswimming_pool) -> and [ ]
    (Gvermin,Gvermin) -> and [ ]
    (Gwater,Gwater) -> and [ ]
    (Gnum x1,Gnum y1) -> and [ x1 == y1 ]
    (G_1012_PN,G_1012_PN) -> and [ ]
    (G_1013_PN,G_1013_PN) -> and [ ]
    (G_1014_PN,G_1014_PN) -> and [ ]
    (G_333A_PN,G_333A_PN) -> and [ ]
    (G_ADD_PN,G_ADD_PN) -> and [ ]
    (G_AIDS_PN,G_AIDS_PN) -> and [ ]
    (G_Accident_PN,G_Accident_PN) -> and [ ]
    (G_Accidental_PN,G_Accidental_PN) -> and [ ]
    (G_Accidents_PN,G_Accidents_PN) -> and [ ]
    (G_Address_PN,G_Address_PN) -> and [ ]
    (G_Adjustment_PN,G_Adjustment_PN) -> and [ ]
    (G_Assured_PN,G_Assured_PN) -> and [ ]
    (G_BSA_PN,G_BSA_PN) -> and [ ]
    (G_Benefit_PN,G_Benefit_PN) -> and [ ]
    (G_Cap_PN,G_Cap_PN) -> and [ ]
    (G_Claim_PN,G_Claim_PN) -> and [ ]
    (G_CoV_PN,G_CoV_PN) -> and [ ]
    (G_Conditions_PN,G_Conditions_PN) -> and [ ]
    (G_Date_PN,G_Date_PN) -> and [ ]
    (G_Death_PN,G_Death_PN) -> and [ ]
    (G_Details_PN,G_Details_PN) -> and [ ]
    (G_Disease_PN,G_Disease_PN) -> and [ ]
    (G_Dismemberment_PN,G_Dismemberment_PN) -> and [ ]
    (G_Event_PN,G_Event_PN) -> and [ ]
    (G_Expense_PN,G_Expense_PN) -> and [ ]
    (G_Flu_PN,G_Flu_PN) -> and [ ]
    (G_H5N1_PN,G_H5N1_PN) -> and [ ]
    (G_H7N9_PN,G_H7N9_PN) -> and [ ]
    (G_H7N_PN,G_H7N_PN) -> and [ ]
    (G_H9N2_PN,G_H9N2_PN) -> and [ ]
    (G_HAS_PN,G_HAS_PN) -> and [ ]
    (G_Head_PN,G_Head_PN) -> and [ ]
    (G_Health_PN,G_Health_PN) -> and [ ]
    (G_Influenza_PN,G_Influenza_PN) -> and [ ]
    (G_Injury_PN,G_Injury_PN) -> and [ ]
    (G_Insurer_PN,G_Insurer_PN) -> and [ ]
    (G_LA_PN,G_LA_PN) -> and [ ]
    (G_LE_PN,G_LE_PN) -> and [ ]
    (G_Leg_PN,G_Leg_PN) -> and [ ]
    (G_Legionnaires_PN,G_Legionnaires_PN) -> and [ ]
    (G_Life_PN,G_Life_PN) -> and [ ]
    (G_Limit_PN,G_Limit_PN) -> and [ ]
    (G_MAP_PN,G_MAP_PN) -> and [ ]
    (G_MIN_PN,G_MIN_PN) -> and [ ]
    (G_MR_PN,G_MR_PN) -> and [ ]
    (G_M_PN,G_M_PN) -> and [ ]
    (G_Medicine_PN,G_Medicine_PN) -> and [ ]
    (G_Melioidosis_PN,G_Melioidosis_PN) -> and [ ]
    (G_Ministry_PN,G_Ministry_PN) -> and [ ]
    (G_Mumps_PN,G_Mumps_PN) -> and [ ]
    (G_N_PN,G_N_PN) -> and [ ]
    (G_Nipah_PN,G_Nipah_PN) -> and [ ]
    (G_Ontology_PN,G_Ontology_PN) -> and [ ]
    (G_PS_PN,G_PS_PN) -> and [ ]
    (G_Plan14_PN,G_Plan14_PN) -> and [ ]
    (G_PlanAF_PN,G_PlanAF_PN) -> and [ ]
    (G_PolicyHolder_PN,G_PolicyHolder_PN) -> and [ ]
    (G_Policy_PN,G_Policy_PN) -> and [ ]
    (G_RETURN_PN,G_RETURN_PN) -> and [ ]
    (G_Reductions_PN,G_Reductions_PN) -> and [ ]
    (G_Removal_PN,G_Removal_PN) -> and [ ]
    (G_Republic_PN,G_Republic_PN) -> and [ ]
    (G_SA_PN,G_SA_PN) -> and [ ]
    (G_SG_PN,G_SG_PN) -> and [ ]
    (G_Schedule_PN,G_Schedule_PN) -> and [ ]
    (G_Section_PN,G_Section_PN) -> and [ ]
    (G_Service_PN,G_Service_PN) -> and [ ]
    (G_Singapore_PN,G_Singapore_PN) -> and [ ]
    (G_Step_PN,G_Step_PN) -> and [ ]
    (G_Subscribed_PN,G_Subscribed_PN) -> and [ ]
    (G_TABLE_PN,G_TABLE_PN) -> and [ ]
    (G_Teeth_PN,G_Teeth_PN) -> and [ ]
    (G_Triple_PN,G_Triple_PN) -> and [ ]
    (G_Type_PN,G_Type_PN) -> and [ ]
    (G_Types_PN,G_Types_PN) -> and [ ]
    (G_UPON_PN,G_UPON_PN) -> and [ ]
    (G_Wife_PN,G_Wife_PN) -> and [ ]
    (G_Yellow_PN,G_Yellow_PN) -> and [ ]
    (G_addSA_PN,G_addSA_PN) -> and [ ]
    (G_benADD_PN,G_benADD_PN) -> and [ ]
    (G_benADDs_PN,G_benADDs_PN) -> and [ ]
    (G_benRA_PN,G_benRA_PN) -> and [ ]
    (G_benTCM_PN,G_benTCM_PN) -> and [ ]
    (G_circ_PN,G_circ_PN) -> and [ ]
    (G_dTime_PN,G_dTime_PN) -> and [ ]
    (G_dType_PN,G_dType_PN) -> and [ ]
    (G_diving_PN,G_diving_PN) -> and [ ]
    (G_holder_PN,G_holder_PN) -> and [ ]
    (G_motocross_PN,G_motocross_PN) -> and [ ]
    (G_p_PN,G_p_PN) -> and [ ]
    (G_plan3_PN,G_plan3_PN) -> and [ ]
    (G_plan4_PN,G_plan4_PN) -> and [ ]
    (G_planAF_PN,G_planAF_PN) -> and [ ]
    (G_planB_PN,G_planB_PN) -> and [ ]
    (G_planC_PN,G_planC_PN) -> and [ ]
    (G_planE_PN,G_planE_PN) -> and [ ]
    (G_planF_PN,G_planF_PN) -> and [ ]
    (G_policyHolder_PN,G_policyHolder_PN) -> and [ ]
    (G_qualifies_for_add_PN,G_qualifies_for_add_PN) -> and [ ]
    (G_schema_PN,G_schema_PN) -> and [ ]
    (G_sum_list_PN,G_sum_list_PN) -> and [ ]
    (G_x_PN,G_x_PN) -> and [ ]
    (G_y_PN,G_y_PN) -> and [ ]
    (GNEG,GNEG) -> and [ ]
    (GPOS,GPOS) -> and [ ]
    (GAP_PrePost x1,GAP_PrePost y1) -> and [ x1 == y1 ]
    (GAdv_PrePost x1,GAdv_PrePost y1) -> and [ x1 == y1 ]
    (GNP_PrePost x1,GNP_PrePost y1) -> and [ x1 == y1 ]
    (GNP_caused_NP_to_VP_Prep_PrePost x1 x2 x3 x4,GNP_caused_NP_to_VP_Prep_PrePost y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GNP_caused_by_PrePost x1,GNP_caused_by_PrePost y1) -> and [ x1 == y1 ]
    (GS_PrePost x1 x2,GS_PrePost y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GV2_PrePost x1,GV2_PrePost y1) -> and [ x1 == y1 ]
    (GrecoverUnparsedPrePost x1,GrecoverUnparsedPrePost y1) -> and [ x1 == y1 ]
    (GConjPrep x1 x2,GConjPrep y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (G_across_Prep,G_across_Prep) -> and [ ]
    (G_after_Prep,G_after_Prep) -> and [ ]
    (G_as_Prep,G_as_Prep) -> and [ ]
    (G_at_Prep,G_at_Prep) -> and [ ]
    (G_before_Prep,G_before_Prep) -> and [ ]
    (G_between_Prep,G_between_Prep) -> and [ ]
    (G_by_Prep,G_by_Prep) -> and [ ]
    (G_during_Prep,G_during_Prep) -> and [ ]
    (G_for_Prep,G_for_Prep) -> and [ ]
    (G_from_Prep,G_from_Prep) -> and [ ]
    (G_in_Prep,G_in_Prep) -> and [ ]
    (G_into_Prep,G_into_Prep) -> and [ ]
    (G_of_Prep,G_of_Prep) -> and [ ]
    (G_on_Prep,G_on_Prep) -> and [ ]
    (G_out_Prep,G_out_Prep) -> and [ ]
    (G_over_Prep,G_over_Prep) -> and [ ]
    (G_per_Prep,G_per_Prep) -> and [ ]
    (G_than_Prep,G_than_Prep) -> and [ ]
    (G_through_Prep,G_through_Prep) -> and [ ]
    (G_to_Prep,G_to_Prep) -> and [ ]
    (G_under_Prep,G_under_Prep) -> and [ ]
    (G_up_Prep,G_up_Prep) -> and [ ]
    (G_with_Prep,G_with_Prep) -> and [ ]
    (G_within_Prep,G_within_Prep) -> and [ ]
    (Gabout_Prep,Gabout_Prep) -> and [ ]
    (Gafter_Prep,Gafter_Prep) -> and [ ]
    (Gbefore_Prep,Gbefore_Prep) -> and [ ]
    (Gfor_Prep,Gfor_Prep) -> and [ ]
    (Gfrom_Prep,Gfrom_Prep) -> and [ ]
    (Gon_Prep,Gon_Prep) -> and [ ]
    (Gpossess_Prep,Gpossess_Prep) -> and [ ]
    (Gto_Prep,Gto_Prep) -> and [ ]
    (Gwithin_Prep,Gwithin_Prep) -> and [ ]
    (GConjPrePostQS x1 x2 x3 x4,GConjPrePostQS y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GConjQS x1 x2,GConjQS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GConjPrePostS x1 x2 x3 x4,GConjPrePostS y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GConjS x1 x2,GConjS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPredVPS x1 x2,GPredVPS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GReferenceNP x1,GReferenceNP y1) -> and [ x1 == y1 ]
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
    (Gpot21,Gpot21) -> and [ ]
    (Gpot2plus x1 x2,Gpot2plus y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Gpot2as3 x1,Gpot2as3 y1) -> and [ x1 == y1 ]
    (Gpot3 x1,Gpot3 y1) -> and [ x1 == y1 ]
    (Gpot31,Gpot31) -> and [ ]
    (Gpot3float x1,Gpot3float y1) -> and [ x1 == y1 ]
    (Gpot3plus x1 x2,Gpot3plus y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Gpot3as4 x1,Gpot3as4 y1) -> and [ x1 == y1 ]
    (Gpot4 x1,Gpot4 y1) -> and [ x1 == y1 ]
    (Gpot41,Gpot41) -> and [ ]
    (Gpot4float x1,Gpot4float y1) -> and [ x1 == y1 ]
    (Gpot4plus x1 x2,Gpot4plus y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Gpot4as5 x1,Gpot4as5 y1) -> and [ x1 == y1 ]
    (Gpot5 x1,Gpot5 y1) -> and [ x1 == y1 ]
    (Gpot51,Gpot51) -> and [ ]
    (Gpot5float x1,Gpot5float y1) -> and [ x1 == y1 ]
    (Gpot5plus x1 x2,Gpot5plus y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (G_and_Subj,G_and_Subj) -> and [ ]
    (G_as_Subj,G_as_Subj) -> and [ ]
    (G_before_Subj,G_before_Subj) -> and [ ]
    (G_both_Subj,G_both_Subj) -> and [ ]
    (G_but_Subj,G_but_Subj) -> and [ ]
    (G_if_Subj,G_if_Subj) -> and [ ]
    (G_or_Subj,G_or_Subj) -> and [ ]
    (G_that_Subj,G_that_Subj) -> and [ ]
    (G_when_Subj,G_when_Subj) -> and [ ]
    (G_while_Subj,G_while_Subj) -> and [ ]
    (GAN x1,GAN y1) -> and [ x1 == y1 ]
    (GEVERY x1,GEVERY y1) -> and [ x1 == y1 ]
    (GPARTY x1,GPARTY y1) -> and [ x1 == y1 ]
    (GSubjWho x1 x2,GSubjWho y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GTHE x1,GTHE y1) -> and [ x1 == y1 ]
    (GYou,GYou) -> and [ ]
    (GrecoverUnparsedSubj x1,GrecoverUnparsedSubj y1) -> and [ x1 == y1 ]
    (GAFTER,GAFTER) -> and [ ]
    (GBEFORE,GBEFORE) -> and [ ]
    (GBY,GBY) -> and [ ]
    (GConjTComparison x1 x2,GConjTComparison y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GON,GON) -> and [ ]
    (GVAGUE,GVAGUE) -> and [ ]
    (GpastSimul,GpastSimul) -> and [ ]
    (GpresAnt,GpresAnt) -> and [ ]
    (GpresSimul,GpresSimul) -> and [ ]
    (GTemporalConstraint x1 x2 x3,GTemporalConstraint y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GTemporalConstraintNoDigits x1 x2,GTemporalConstraintNoDigits y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRegulative x1 x2 x3,GRegulative y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GadvUPON x1,GadvUPON y1) -> and [ x1 == y1 ]
    (GqCOND x1,GqCOND y1) -> and [ x1 == y1 ]
    (GqCONSTR x1,GqCONSTR y1) -> and [ x1 == y1 ]
    (GqPREPOST x1,GqPREPOST y1) -> and [ x1 == y1 ]
    (GqUPON x1 x2,GqUPON y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GqWHO x1 x2,GqWHO y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GsCOND x1,GsCOND y1) -> and [ x1 == y1 ]
    (GsUPON x1 x2,GsUPON y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GsWHO x1 x2,GsWHO y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GDay_Unit,GDay_Unit) -> and [ ]
    (GMonth_Unit,GMonth_Unit) -> and [ ]
    (GYear_Unit,GYear_Unit) -> and [ ]
    (GrecoverUnparsedTimeUnit x1,GrecoverUnparsedTimeUnit y1) -> and [ x1 == y1 ]
    (GUPON x1,GUPON y1) -> and [ x1 == y1 ]
    (GrecoverUnparsedUpon x1,GrecoverUnparsedUpon y1) -> and [ x1 == y1 ]
    (G_H7N7_V,G_H7N7_V) -> and [ ]
    (G_adjust_V,G_adjust_V) -> and [ ]
    (G_apply_V,G_apply_V) -> and [ ]
    (G_asssure_V,G_asssure_V) -> and [ ]
    (G_assure_V,G_assure_V) -> and [ ]
    (G_benefit_V,G_benefit_V) -> and [ ]
    (G_canoe_V,G_canoe_V) -> and [ ]
    (G_cave_V,G_cave_V) -> and [ ]
    (G_claim_V,G_claim_V) -> and [ ]
    (G_cover_V,G_cover_V) -> and [ ]
    (G_establish_V,G_establish_V) -> and [ ]
    (G_exception_V,G_exception_V) -> and [ ]
    (G_give_V,G_give_V) -> and [ ]
    (G_glide_V,G_glide_V) -> and [ ]
    (G_govern_V,G_govern_V) -> and [ ]
    (G_hand_V,G_hand_V) -> and [ ]
    (G_hernia_V,G_hernia_V) -> and [ ]
    (G_hunt_V,G_hunt_V) -> and [ ]
    (G_include_V,G_include_V) -> and [ ]
    (G_license_V,G_license_V) -> and [ ]
    (G_mean_V,G_mean_V) -> and [ ]
    (G_met_common_requirement_for_add_V,G_met_common_requirement_for_add_V) -> and [ ]
    (G_mountaineer_V,G_mountaineer_V) -> and [ ]
    (G_occur_V,G_occur_V) -> and [ ]
    (G_organise_V,G_organise_V) -> and [ ]
    (G_parachute_V,G_parachute_V) -> and [ ]
    (G_pay_V,G_pay_V) -> and [ ]
    (G_policyholder_V,G_policyholder_V) -> and [ ]
    (G_pothole_V,G_pothole_V) -> and [ ]
    (G_race_V,G_race_V) -> and [ ]
    (G_recognise_V,G_recognise_V) -> and [ ]
    (G_register_V,G_register_V) -> and [ ]
    (G_riot_V,G_riot_V) -> and [ ]
    (G_sail_V,G_sail_V) -> and [ ]
    (G_skydive_V,G_skydive_V) -> and [ ]
    (G_start_V,G_start_V) -> and [ ]
    (G_stepupsumassure_V,G_stepupsumassure_V) -> and [ ]
    (G_subscribe_V,G_subscribe_V) -> and [ ]
    (G_suffer_V,G_suffer_V) -> and [ ]
    (G_sumassure_V,G_sumassure_V) -> and [ ]
    (G_supervise_V,G_supervise_V) -> and [ ]
    (G_train_V,G_train_V) -> and [ ]
    (G_windsurf_V,G_windsurf_V) -> and [ ]
    (LexV2 x,LexV2 y) -> x == y
    (GAdvVP x1 x2,GAdvVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GComplV2 x1 x2,GComplV2 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GComplV2S x1 x2 x3,GComplV2S y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GComplVAS x1 x2 x3,GComplVAS y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GComplVSif x1 x2,GComplVSif y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GComplVSthat x1 x2,GComplVSthat y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GUseComp x1,GUseComp y1) -> and [ x1 == y1 ]
    (LexVP x,LexVP y) -> x == y
    (GMkVPI x1,GMkVPI y1) -> and [ x1 == y1 ]
    (GComparison_Card_Years x1,GComparison_Card_Years y1) -> and [ x1 == y1 ]
    (GConjPrePostVPS x1 x2 x3 x4,GConjPrePostVPS y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GConjVPS x1 x2,GConjVPS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GGreaterThan x1,GGreaterThan y1) -> and [ x1 == y1 ]
    (GLessThan x1,GLessThan y1) -> and [ x1 == y1 ]
    (GMayHave x1,GMayHave y1) -> and [ x1 == y1 ]
    (GMkVPS x1 x2 x3,GMkVPS y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (LexVS x,LexVS y) -> x == y
    (LexVV x,LexVV y) -> x == y
    (GAPWho x1,GAPWho y1) -> and [ x1 == y1 ]
    (GAdvWho x1,GAdvWho y1) -> and [ x1 == y1 ]
    (GConjPrePostWho x1 x2 x3 x4,GConjPrePostWho y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GConjPreWho x1 x2 x3,GConjPreWho y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GConjWho x1 x2,GConjWho y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GWHO x1 x2 x3,GWHO y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GrecoverUnparsedWho x1,GrecoverUnparsedWho y1) -> and [ x1 == y1 ]
    (GMkYear x1 x2 x3 x4,GMkYear y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (LexYearComponent x,LexYearComponent y) -> x == y
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GA where
  gf G_accepted_A = mkApp (mkCId "_accepted_A") []
  gf G_accidental_A = mkApp (mkCId "_accidental_A") []
  gf G_active_A = mkApp (mkCId "_active_A") []
  gf G_add_A = mkApp (mkCId "_add_A") []
  gf G_adjusted_A = mkApp (mkCId "_adjusted_A") []
  gf G_alive_A = mkApp (mkCId "_alive_A") []
  gf G_basic_A = mkApp (mkCId "_basic_A") []
  gf G_biological_A = mkApp (mkCId "_biological_A") []
  gf G_chemical_A = mkApp (mkCId "_chemical_A") []
  gf G_claimable_A = mkApp (mkCId "_claimable_A") []
  gf G_commercial_A = mkApp (mkCId "_commercial_A") []
  gf G_competitive_A = mkApp (mkCId "_competitive_A") []
  gf G_current_A = mkApp (mkCId "_current_A") []
  gf G_dangerous_A = mkApp (mkCId "_dangerous_A") []
  gf G_dead_A = mkApp (mkCId "_dead_A") []
  gf G_disabled_A = mkApp (mkCId "_disabled_A") []
  gf G_diving_A = mkApp (mkCId "_diving_A") []
  gf G_double_A = mkApp (mkCId "_double_A") []
  gf G_due_A = mkApp (mkCId "_due_A") []
  gf G_equal_A = mkApp (mkCId "_equal_A") []
  gf G_first_A = mkApp (mkCId "_first_A") []
  gf G_fit_A = mkApp (mkCId "_fit_A") []
  gf G_fractured_A = mkApp (mkCId "_fractured_A") []
  gf G_geographical_A = mkApp (mkCId "_geographical_A") []
  gf G_great_A = mkApp (mkCId "_great_A") []
  gf G_hfmd_A = mkApp (mkCId "_hfmd_A") []
  gf G_high_A = mkApp (mkCId "_high_A") []
  gf G_human_A = mkApp (mkCId "_human_A") []
  gf G_incurable_A = mkApp (mkCId "_incurable_A") []
  gf G_infectious_A = mkApp (mkCId "_infectious_A") []
  gf G_initial_A = mkApp (mkCId "_initial_A") []
  gf G_intentional_A = mkApp (mkCId "_intentional_A") []
  gf G_japanese_A = mkApp (mkCId "_japanese_A") []
  gf G_juvenile_A = mkApp (mkCId "_juvenile_A") []
  gf G_less_A = mkApp (mkCId "_less_A") []
  gf G_little_A = mkApp (mkCId "_little_A") []
  gf G_logical_A = mkApp (mkCId "_logical_A") []
  gf G_low_A = mkApp (mkCId "_low_A") []
  gf G_martial_A = mkApp (mkCId "_martial_A") []
  gf G_maximum_A = mkApp (mkCId "_maximum_A") []
  gf G_medical_A = mkApp (mkCId "_medical_A") []
  gf G_mental_A = mkApp (mkCId "_mental_A") []
  gf G_middle_A = mkApp (mkCId "_middle_A") []
  gf G_military_A = mkApp (mkCId "_military_A") []
  gf G_more_A = mkApp (mkCId "_more_A") []
  gf G_multiple_A = mkApp (mkCId "_multiple_A") []
  gf G_national_A = mkApp (mkCId "_national_A") []
  gf G_normal_A = mkApp (mkCId "_normal_A") []
  gf G_nuclear_A = mkApp (mkCId "_nuclear_A") []
  gf G_other_A = mkApp (mkCId "_other_A") []
  gf G_particular_A = mkApp (mkCId "_particular_A") []
  gf G_past_A = mkApp (mkCId "_past_A") []
  gf G_payable_A = mkApp (mkCId "_payable_A") []
  gf G_permanent_A = mkApp (mkCId "_permanent_A") []
  gf G_physical_A = mkApp (mkCId "_physical_A") []
  gf G_possible_A = mkApp (mkCId "_possible_A") []
  gf G_previous_A = mkApp (mkCId "_previous_A") []
  gf G_private_A = mkApp (mkCId "_private_A") []
  gf G_professional_A = mkApp (mkCId "_professional_A") []
  gf G_public_A = mkApp (mkCId "_public_A") []
  gf G_reckless_A = mkApp (mkCId "_reckless_A") []
  gf G_registered_A = mkApp (mkCId "_registered_A") []
  gf G_relevant_A = mkApp (mkCId "_relevant_A") []
  gf G_subject_A = mkApp (mkCId "_subject_A") []
  gf G_successful_A = mkApp (mkCId "_successful_A") []
  gf G_surgical_A = mkApp (mkCId "_surgical_A") []
  gf G_third_A = mkApp (mkCId "_third_A") []
  gf G_total_A = mkApp (mkCId "_total_A") []
  gf G_traditional_A = mkApp (mkCId "_traditional_A") []
  gf G_triple_A = mkApp (mkCId "_triple_A") []
  gf G_unlawful_A = mkApp (mkCId "_unlawful_A") []
  gf G_unnecessary_A = mkApp (mkCId "_unnecessary_A") []
  gf G_unsound_A = mkApp (mkCId "_unsound_A") []
  gf G_unsuccessful_A = mkApp (mkCId "_unsuccessful_A") []
  gf G_upscaled_A = mkApp (mkCId "_upscaled_A") []
  gf G_valid_A = mkApp (mkCId "_valid_A") []
  gf G_viral_A = mkApp (mkCId "_viral_A") []
  gf G_waterborne_A = mkApp (mkCId "_waterborne_A") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "_accepted_A" -> G_accepted_A 
      Just (i,[]) | i == mkCId "_accidental_A" -> G_accidental_A 
      Just (i,[]) | i == mkCId "_active_A" -> G_active_A 
      Just (i,[]) | i == mkCId "_add_A" -> G_add_A 
      Just (i,[]) | i == mkCId "_adjusted_A" -> G_adjusted_A 
      Just (i,[]) | i == mkCId "_alive_A" -> G_alive_A 
      Just (i,[]) | i == mkCId "_basic_A" -> G_basic_A 
      Just (i,[]) | i == mkCId "_biological_A" -> G_biological_A 
      Just (i,[]) | i == mkCId "_chemical_A" -> G_chemical_A 
      Just (i,[]) | i == mkCId "_claimable_A" -> G_claimable_A 
      Just (i,[]) | i == mkCId "_commercial_A" -> G_commercial_A 
      Just (i,[]) | i == mkCId "_competitive_A" -> G_competitive_A 
      Just (i,[]) | i == mkCId "_current_A" -> G_current_A 
      Just (i,[]) | i == mkCId "_dangerous_A" -> G_dangerous_A 
      Just (i,[]) | i == mkCId "_dead_A" -> G_dead_A 
      Just (i,[]) | i == mkCId "_disabled_A" -> G_disabled_A 
      Just (i,[]) | i == mkCId "_diving_A" -> G_diving_A 
      Just (i,[]) | i == mkCId "_double_A" -> G_double_A 
      Just (i,[]) | i == mkCId "_due_A" -> G_due_A 
      Just (i,[]) | i == mkCId "_equal_A" -> G_equal_A 
      Just (i,[]) | i == mkCId "_first_A" -> G_first_A 
      Just (i,[]) | i == mkCId "_fit_A" -> G_fit_A 
      Just (i,[]) | i == mkCId "_fractured_A" -> G_fractured_A 
      Just (i,[]) | i == mkCId "_geographical_A" -> G_geographical_A 
      Just (i,[]) | i == mkCId "_great_A" -> G_great_A 
      Just (i,[]) | i == mkCId "_hfmd_A" -> G_hfmd_A 
      Just (i,[]) | i == mkCId "_high_A" -> G_high_A 
      Just (i,[]) | i == mkCId "_human_A" -> G_human_A 
      Just (i,[]) | i == mkCId "_incurable_A" -> G_incurable_A 
      Just (i,[]) | i == mkCId "_infectious_A" -> G_infectious_A 
      Just (i,[]) | i == mkCId "_initial_A" -> G_initial_A 
      Just (i,[]) | i == mkCId "_intentional_A" -> G_intentional_A 
      Just (i,[]) | i == mkCId "_japanese_A" -> G_japanese_A 
      Just (i,[]) | i == mkCId "_juvenile_A" -> G_juvenile_A 
      Just (i,[]) | i == mkCId "_less_A" -> G_less_A 
      Just (i,[]) | i == mkCId "_little_A" -> G_little_A 
      Just (i,[]) | i == mkCId "_logical_A" -> G_logical_A 
      Just (i,[]) | i == mkCId "_low_A" -> G_low_A 
      Just (i,[]) | i == mkCId "_martial_A" -> G_martial_A 
      Just (i,[]) | i == mkCId "_maximum_A" -> G_maximum_A 
      Just (i,[]) | i == mkCId "_medical_A" -> G_medical_A 
      Just (i,[]) | i == mkCId "_mental_A" -> G_mental_A 
      Just (i,[]) | i == mkCId "_middle_A" -> G_middle_A 
      Just (i,[]) | i == mkCId "_military_A" -> G_military_A 
      Just (i,[]) | i == mkCId "_more_A" -> G_more_A 
      Just (i,[]) | i == mkCId "_multiple_A" -> G_multiple_A 
      Just (i,[]) | i == mkCId "_national_A" -> G_national_A 
      Just (i,[]) | i == mkCId "_normal_A" -> G_normal_A 
      Just (i,[]) | i == mkCId "_nuclear_A" -> G_nuclear_A 
      Just (i,[]) | i == mkCId "_other_A" -> G_other_A 
      Just (i,[]) | i == mkCId "_particular_A" -> G_particular_A 
      Just (i,[]) | i == mkCId "_past_A" -> G_past_A 
      Just (i,[]) | i == mkCId "_payable_A" -> G_payable_A 
      Just (i,[]) | i == mkCId "_permanent_A" -> G_permanent_A 
      Just (i,[]) | i == mkCId "_physical_A" -> G_physical_A 
      Just (i,[]) | i == mkCId "_possible_A" -> G_possible_A 
      Just (i,[]) | i == mkCId "_previous_A" -> G_previous_A 
      Just (i,[]) | i == mkCId "_private_A" -> G_private_A 
      Just (i,[]) | i == mkCId "_professional_A" -> G_professional_A 
      Just (i,[]) | i == mkCId "_public_A" -> G_public_A 
      Just (i,[]) | i == mkCId "_reckless_A" -> G_reckless_A 
      Just (i,[]) | i == mkCId "_registered_A" -> G_registered_A 
      Just (i,[]) | i == mkCId "_relevant_A" -> G_relevant_A 
      Just (i,[]) | i == mkCId "_subject_A" -> G_subject_A 
      Just (i,[]) | i == mkCId "_successful_A" -> G_successful_A 
      Just (i,[]) | i == mkCId "_surgical_A" -> G_surgical_A 
      Just (i,[]) | i == mkCId "_third_A" -> G_third_A 
      Just (i,[]) | i == mkCId "_total_A" -> G_total_A 
      Just (i,[]) | i == mkCId "_traditional_A" -> G_traditional_A 
      Just (i,[]) | i == mkCId "_triple_A" -> G_triple_A 
      Just (i,[]) | i == mkCId "_unlawful_A" -> G_unlawful_A 
      Just (i,[]) | i == mkCId "_unnecessary_A" -> G_unnecessary_A 
      Just (i,[]) | i == mkCId "_unsound_A" -> G_unsound_A 
      Just (i,[]) | i == mkCId "_unsuccessful_A" -> G_unsuccessful_A 
      Just (i,[]) | i == mkCId "_upscaled_A" -> G_upscaled_A 
      Just (i,[]) | i == mkCId "_valid_A" -> G_valid_A 
      Just (i,[]) | i == mkCId "_viral_A" -> G_viral_A 
      Just (i,[]) | i == mkCId "_waterborne_A" -> G_waterborne_A 


      _ -> error ("no A " ++ show t)

instance Gf GA2 where
  gf G_located_in_A2 = mkApp (mkCId "_located_in_A2") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "_located_in_A2" -> G_located_in_A2 


      _ -> error ("no A2 " ++ show t)

instance Gf GAP where
  gf (GComplA2 x1 x2) = mkApp (mkCId "ComplA2") [gf x1, gf x2]
  gf (GConjAP x1 x2) = mkApp (mkCId "ConjAP") [gf x1, gf x2]
  gf (GInt_or_older x1) = mkApp (mkCId "Int_or_older") [gf x1]
  gf (GPositA x1) = mkApp (mkCId "PositA") [gf x1]
  gf (Gcaused_by x1) = mkApp (mkCId "caused_by") [gf x1]
  gf (Gensuing x1) = mkApp (mkCId "ensuing") [gf x1]
  gf (LexAP x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ComplA2" -> GComplA2 (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjAP" -> GConjAP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "Int_or_older" -> GInt_or_older (fg x1)
      Just (i,[x1]) | i == mkCId "PositA" -> GPositA (fg x1)
      Just (i,[x1]) | i == mkCId "caused_by" -> Gcaused_by (fg x1)
      Just (i,[x1]) | i == mkCId "ensuing" -> Gensuing (fg x1)

      Just (i,[]) -> LexAP (showCId i)
      _ -> error ("no AP " ++ show t)

instance Gf GAction where
  gf (GACTION x1) = mkApp (mkCId "ACTION") [gf x1]
  gf (GrecoverUnparsedAction x1) = mkApp (mkCId "recoverUnparsedAction") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "ACTION" -> GACTION (fg x1)
      Just (i,[x1]) | i == mkCId "recoverUnparsedAction" -> GrecoverUnparsedAction (fg x1)


      _ -> error ("no Action " ++ show t)

instance Gf GAdA where
  gf Gonly_AdA = mkApp (mkCId "only_AdA") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "only_AdA" -> Gonly_AdA 


      _ -> error ("no AdA " ++ show t)

instance Gf GAdN where
  gf (GAdnCAdv x1) = mkApp (mkCId "AdnCAdv") [gf x1]
  gf Gat_least_AdN = mkApp (mkCId "at_least_AdN") []
  gf Gat_most_AdN = mkApp (mkCId "at_most_AdN") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "AdnCAdv" -> GAdnCAdv (fg x1)
      Just (i,[]) | i == mkCId "at_least_AdN" -> Gat_least_AdN 
      Just (i,[]) | i == mkCId "at_most_AdN" -> Gat_most_AdN 


      _ -> error ("no AdN " ++ show t)

instance Gf GAdv where
  gf (GAdAdv x1 x2) = mkApp (mkCId "AdAdv") [gf x1, gf x2]
  gf (GByVP x1) = mkApp (mkCId "ByVP") [gf x1]
  gf (GConjAdv x1 x2) = mkApp (mkCId "ConjAdv") [gf x1, gf x2]
  gf (GPrepNP x1 x2) = mkApp (mkCId "PrepNP") [gf x1, gf x2]
  gf (GSubjS x1 x2) = mkApp (mkCId "SubjS") [gf x1, gf x2]
  gf (GWhileDoing x1) = mkApp (mkCId "WhileDoing") [gf x1]
  gf G_as_Adv = mkApp (mkCId "_as_Adv") []
  gf G_at_Adv = mkApp (mkCId "_at_Adv") []
  gf G_first_Adv = mkApp (mkCId "_first_Adv") []
  gf G_fully_Adv = mkApp (mkCId "_fully_Adv") []
  gf G_hence_Adv = mkApp (mkCId "_hence_Adv") []
  gf G_least_Adv = mkApp (mkCId "_least_Adv") []
  gf G_on_its_way_Adv = mkApp (mkCId "_on_its_way_Adv") []
  gf G_only_Adv = mkApp (mkCId "_only_Adv") []
  gf G_permanently_Adv = mkApp (mkCId "_permanently_Adv") []
  gf G_pland_Adv = mkApp (mkCId "_pland_Adv") []
  gf G_soon_Adv = mkApp (mkCId "_soon_Adv") []
  gf G_then_Adv = mkApp (mkCId "_then_Adv") []
  gf G_totally_Adv = mkApp (mkCId "_totally_Adv") []
  gf G_up_Adv = mkApp (mkCId "_up_Adv") []
  gf Gin_part = mkApp (mkCId "in_part") []
  gf Gin_whole = mkApp (mkCId "in_whole") []
  gf (GrecoverUnparsedAdv x1) = mkApp (mkCId "recoverUnparsedAdv") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdAdv" -> GAdAdv (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ByVP" -> GByVP (fg x1)
      Just (i,[x1,x2]) | i == mkCId "ConjAdv" -> GConjAdv (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PrepNP" -> GPrepNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SubjS" -> GSubjS (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "WhileDoing" -> GWhileDoing (fg x1)
      Just (i,[]) | i == mkCId "_as_Adv" -> G_as_Adv 
      Just (i,[]) | i == mkCId "_at_Adv" -> G_at_Adv 
      Just (i,[]) | i == mkCId "_first_Adv" -> G_first_Adv 
      Just (i,[]) | i == mkCId "_fully_Adv" -> G_fully_Adv 
      Just (i,[]) | i == mkCId "_hence_Adv" -> G_hence_Adv 
      Just (i,[]) | i == mkCId "_least_Adv" -> G_least_Adv 
      Just (i,[]) | i == mkCId "_on_its_way_Adv" -> G_on_its_way_Adv 
      Just (i,[]) | i == mkCId "_only_Adv" -> G_only_Adv 
      Just (i,[]) | i == mkCId "_permanently_Adv" -> G_permanently_Adv 
      Just (i,[]) | i == mkCId "_pland_Adv" -> G_pland_Adv 
      Just (i,[]) | i == mkCId "_soon_Adv" -> G_soon_Adv 
      Just (i,[]) | i == mkCId "_then_Adv" -> G_then_Adv 
      Just (i,[]) | i == mkCId "_totally_Adv" -> G_totally_Adv 
      Just (i,[]) | i == mkCId "_up_Adv" -> G_up_Adv 
      Just (i,[]) | i == mkCId "in_part" -> Gin_part 
      Just (i,[]) | i == mkCId "in_whole" -> Gin_whole 
      Just (i,[x1]) | i == mkCId "recoverUnparsedAdv" -> GrecoverUnparsedAdv (fg x1)


      _ -> error ("no Adv " ++ show t)

instance Gf GCAdv where
  gf Gless_CAdv = mkApp (mkCId "less_CAdv") []
  gf Gmore_CAdv = mkApp (mkCId "more_CAdv") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "less_CAdv" -> Gless_CAdv 
      Just (i,[]) | i == mkCId "more_CAdv" -> Gmore_CAdv 


      _ -> error ("no CAdv " ++ show t)

instance Gf GCN where
  gf (GAdjCN x1 x2) = mkApp (mkCId "AdjCN") [gf x1, gf x2]
  gf (GCNwhereS x1 x2 x3) = mkApp (mkCId "CNwhereS") [gf x1, gf x2, gf x3]
  gf (GUseN x1) = mkApp (mkCId "UseN") [gf x1]
  gf (LexCN x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdjCN" -> GAdjCN (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "CNwhereS" -> GCNwhereS (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "UseN" -> GUseN (fg x1)

      Just (i,[]) -> LexCN (showCId i)
      _ -> error ("no CN " ++ show t)

instance Gf GCard where
  gf (GAdNum x1 x2) = mkApp (mkCId "AdNum") [gf x1, gf x2]
  gf (GNumDigits x1) = mkApp (mkCId "NumDigits") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdNum" -> GAdNum (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "NumDigits" -> GNumDigits (fg x1)


      _ -> error ("no Card " ++ show t)

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

instance Gf GCond where
  gf (GConjCond x1 x2) = mkApp (mkCId "ConjCond") [gf x1, gf x2]
  gf (GConjPreCond x1 x2 x3) = mkApp (mkCId "ConjPreCond") [gf x1, gf x2, gf x3]
  gf (GConjPrePostCond x1 x2 x3 x4) = mkApp (mkCId "ConjPrePostCond") [gf x1, gf x2, gf x3, gf x4]
  gf (GRPConstraint x1 x2 x3) = mkApp (mkCId "RPConstraint") [gf x1, gf x2, gf x3]
  gf (GWHEN x1 x2 x3 x4) = mkApp (mkCId "WHEN") [gf x1, gf x2, gf x3, gf x4]
  gf (GrecoverUnparsedCond x1) = mkApp (mkCId "recoverUnparsedCond") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjCond" -> GConjCond (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "ConjPreCond" -> GConjPreCond (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "ConjPrePostCond" -> GConjPrePostCond (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "RPConstraint" -> GRPConstraint (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "WHEN" -> GWHEN (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1]) | i == mkCId "recoverUnparsedCond" -> GrecoverUnparsedCond (fg x1)


      _ -> error ("no Cond " ++ show t)

instance Gf GConj where
  gf GAND = mkApp (mkCId "AND") []
  gf GOR = mkApp (mkCId "OR") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "AND" -> GAND 
      Just (i,[]) | i == mkCId "OR" -> GOR 


      _ -> error ("no Conj " ++ show t)

instance Gf GConstraint where
  gf (GConjConstraint x1 x2) = mkApp (mkCId "ConjConstraint") [gf x1, gf x2]
  gf (GConjPreConstraint x1 x2 x3) = mkApp (mkCId "ConjPreConstraint") [gf x1, gf x2, gf x3]
  gf (GConjPrePostConstraint x1 x2 x3 x4) = mkApp (mkCId "ConjPrePostConstraint") [gf x1, gf x2, gf x3, gf x4]
  gf (GRPleafAP x1) = mkApp (mkCId "RPleafAP") [gf x1]
  gf (GRPleafAdv x1) = mkApp (mkCId "RPleafAdv") [gf x1]
  gf (GRPleafNP x1) = mkApp (mkCId "RPleafNP") [gf x1]
  gf (GRPleafS x1 x2) = mkApp (mkCId "RPleafS") [gf x1, gf x2]
  gf (GRPleafVP x1) = mkApp (mkCId "RPleafVP") [gf x1]
  gf (GrecoverRPis x1 x2) = mkApp (mkCId "recoverRPis") [gf x1, gf x2]
  gf (GrecoverRPmath x1 x2 x3) = mkApp (mkCId "recoverRPmath") [gf x1, gf x2, gf x3]
  gf (GrecoverUnparsedConstraint x1) = mkApp (mkCId "recoverUnparsedConstraint") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjConstraint" -> GConjConstraint (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "ConjPreConstraint" -> GConjPreConstraint (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "ConjPrePostConstraint" -> GConjPrePostConstraint (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1]) | i == mkCId "RPleafAP" -> GRPleafAP (fg x1)
      Just (i,[x1]) | i == mkCId "RPleafAdv" -> GRPleafAdv (fg x1)
      Just (i,[x1]) | i == mkCId "RPleafNP" -> GRPleafNP (fg x1)
      Just (i,[x1,x2]) | i == mkCId "RPleafS" -> GRPleafS (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "RPleafVP" -> GRPleafVP (fg x1)
      Just (i,[x1,x2]) | i == mkCId "recoverRPis" -> GrecoverRPis (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "recoverRPmath" -> GrecoverRPmath (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "recoverUnparsedConstraint" -> GrecoverUnparsedConstraint (fg x1)


      _ -> error ("no Constraint " ++ show t)

instance Gf GDate where
  gf (GMkDate x1 x2 x3) = mkApp (mkCId "MkDate") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "MkDate" -> GMkDate (fg x1) (fg x2) (fg x3)


      _ -> error ("no Date " ++ show t)

instance Gf GDay where
  gf (LexDay x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexDay (showCId i)
      _ -> error ("no Day " ++ show t)

instance Gf GDeontic where
  gf GMAY = mkApp (mkCId "MAY") []
  gf GMUST = mkApp (mkCId "MUST") []
  gf GSHANT = mkApp (mkCId "SHANT") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "MAY" -> GMAY 
      Just (i,[]) | i == mkCId "MUST" -> GMUST 
      Just (i,[]) | i == mkCId "SHANT" -> GSHANT 


      _ -> error ("no Deontic " ++ show t)

instance Gf GDet where
  gf GaPl = mkApp (mkCId "aPl") []
  gf GaSg = mkApp (mkCId "aSg") []
  gf GthePl = mkApp (mkCId "thePl") []
  gf GtheSg = mkApp (mkCId "theSg") []
  gf Gyour = mkApp (mkCId "your") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "aPl" -> GaPl 
      Just (i,[]) | i == mkCId "aSg" -> GaSg 
      Just (i,[]) | i == mkCId "thePl" -> GthePl 
      Just (i,[]) | i == mkCId "theSg" -> GtheSg 
      Just (i,[]) | i == mkCId "your" -> Gyour 


      _ -> error ("no Det " ++ show t)

instance Gf GDig where
  gf (LexDig x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexDig (showCId i)
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

instance Gf GListAP where
  gf (GListAP [x1,x2]) = mkApp (mkCId "BaseAP") [gf x1, gf x2]
  gf (GListAP (x:xs)) = mkApp (mkCId "ConsAP") [gf x, gf (GListAP xs)]
  fg t =
    GListAP (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseAP" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsAP" -> fg x1 : fgs x2


      _ -> error ("no ListAP " ++ show t)

instance Gf GListAdv where
  gf (GListAdv [x1,x2]) = mkApp (mkCId "BaseAdv") [gf x1, gf x2]
  gf (GListAdv (x:xs)) = mkApp (mkCId "ConsAdv") [gf x, gf (GListAdv xs)]
  fg t =
    GListAdv (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseAdv" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsAdv" -> fg x1 : fgs x2


      _ -> error ("no ListAdv " ++ show t)

instance Gf GListCond where
  gf (GListCond [x1,x2]) = mkApp (mkCId "BaseCond") [gf x1, gf x2]
  gf (GListCond (x:xs)) = mkApp (mkCId "ConsCond") [gf x, gf (GListCond xs)]
  fg t =
    GListCond (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseCond" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsCond" -> fg x1 : fgs x2


      _ -> error ("no ListCond " ++ show t)

instance Gf GListConstraint where
  gf (GListConstraint [x1,x2]) = mkApp (mkCId "BaseConstraint") [gf x1, gf x2]
  gf (GListConstraint (x:xs)) = mkApp (mkCId "ConsConstraint") [gf x, gf (GListConstraint xs)]
  fg t =
    GListConstraint (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseConstraint" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsConstraint" -> fg x1 : fgs x2


      _ -> error ("no ListConstraint " ++ show t)

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

instance Gf GListQS where
  gf (GListQS [x1,x2]) = mkApp (mkCId "BaseQS") [gf x1, gf x2]
  gf (GListQS (x:xs)) = mkApp (mkCId "ConsQS") [gf x, gf (GListQS xs)]
  fg t =
    GListQS (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseQS" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsQS" -> fg x1 : fgs x2


      _ -> error ("no ListQS " ++ show t)

instance Gf GListS where
  gf (GListS [x1,x2]) = mkApp (mkCId "BaseS") [gf x1, gf x2]
  gf (GListS (x:xs)) = mkApp (mkCId "ConsS") [gf x, gf (GListS xs)]
  fg t =
    GListS (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseS" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsS" -> fg x1 : fgs x2


      _ -> error ("no ListS " ++ show t)

instance Gf GListTComparison where
  gf (GListTComparison [x1,x2]) = mkApp (mkCId "BaseTComparison") [gf x1, gf x2]
  gf (GListTComparison (x:xs)) = mkApp (mkCId "ConsTComparison") [gf x, gf (GListTComparison xs)]
  fg t =
    GListTComparison (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseTComparison" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsTComparison" -> fg x1 : fgs x2


      _ -> error ("no ListTComparison " ++ show t)

instance Gf GListVPS where
  gf (GListVPS [x1,x2]) = mkApp (mkCId "BaseVPS") [gf x1, gf x2]
  gf (GListVPS (x:xs)) = mkApp (mkCId "ConsVPS") [gf x, gf (GListVPS xs)]
  fg t =
    GListVPS (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseVPS" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsVPS" -> fg x1 : fgs x2


      _ -> error ("no ListVPS " ++ show t)

instance Gf GListWho where
  gf (GListWho [x1,x2]) = mkApp (mkCId "BaseWho") [gf x1, gf x2]
  gf (GListWho (x:xs)) = mkApp (mkCId "ConsWho") [gf x, gf (GListWho xs)]
  fg t =
    GListWho (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseWho" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsWho" -> fg x1 : fgs x2


      _ -> error ("no ListWho " ++ show t)

instance Gf GMonth where
  gf (LexMonth x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexMonth (showCId i)
      _ -> error ("no Month " ++ show t)

instance Gf GN where
  gf (GCompoundN x1 x2) = mkApp (mkCId "CompoundN") [gf x1, gf x2]
  gf (LexN x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "CompoundN" -> GCompoundN (fg x1) (fg x2)

      Just (i,[]) -> LexN (showCId i)
      _ -> error ("no N " ++ show t)

instance Gf GN2 where
  gf G_premise_where_N2 = mkApp (mkCId "_premise_where_N2") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "_premise_where_N2" -> G_premise_where_N2 


      _ -> error ("no N2 " ++ show t)

instance Gf GNP where
  gf (GConjNP x1 x2) = mkApp (mkCId "ConjNP") [gf x1, gf x2]
  gf GContents = mkApp (mkCId "Contents") []
  gf (GDetCN x1 x2) = mkApp (mkCId "DetCN") [gf x1, gf x2]
  gf (GGerundNP x1) = mkApp (mkCId "GerundNP") [gf x1]
  gf GLoss_or_Damage = mkApp (mkCId "Loss_or_Damage") []
  gf (GMassNP x1) = mkApp (mkCId "MassNP") [gf x1]
  gf GNDB_Qualification = mkApp (mkCId "NDB_Qualification") []
  gf (GUsePN x1) = mkApp (mkCId "UsePN") [gf x1]
  gf Ganimal = mkApp (mkCId "animal") []
  gf Gany_other_exclusion = mkApp (mkCId "any_other_exclusion") []
  gf Gbirds = mkApp (mkCId "birds") []
  gf Gcancelled = mkApp (mkCId "cancelled") []
  gf Gclaim = mkApp (mkCId "claim") []
  gf Gcondition = mkApp (mkCId "condition") []
  gf Ghousehold_appliance = mkApp (mkCId "household_appliance") []
  gf Ginsects = mkApp (mkCId "insects") []
  gf Gplumbing_heating_or_AC = mkApp (mkCId "plumbing_heating_or_AC") []
  gf Gpremium = mkApp (mkCId "premium") []
  gf (Gresult_from x1) = mkApp (mkCId "result_from") [gf x1]
  gf Grodents = mkApp (mkCId "rodents") []
  gf Gsigned = mkApp (mkCId "signed") []
  gf Gstay_during_policy_period = mkApp (mkCId "stay_during_policy_period") []
  gf Gstay_overnight = mkApp (mkCId "stay_overnight") []
  gf Gswimming_pool = mkApp (mkCId "swimming_pool") []
  gf Gvermin = mkApp (mkCId "vermin") []
  gf Gwater = mkApp (mkCId "water") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjNP" -> GConjNP (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Contents" -> GContents 
      Just (i,[x1,x2]) | i == mkCId "DetCN" -> GDetCN (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "GerundNP" -> GGerundNP (fg x1)
      Just (i,[]) | i == mkCId "Loss_or_Damage" -> GLoss_or_Damage 
      Just (i,[x1]) | i == mkCId "MassNP" -> GMassNP (fg x1)
      Just (i,[]) | i == mkCId "NDB_Qualification" -> GNDB_Qualification 
      Just (i,[x1]) | i == mkCId "UsePN" -> GUsePN (fg x1)
      Just (i,[]) | i == mkCId "animal" -> Ganimal 
      Just (i,[]) | i == mkCId "any_other_exclusion" -> Gany_other_exclusion 
      Just (i,[]) | i == mkCId "birds" -> Gbirds 
      Just (i,[]) | i == mkCId "cancelled" -> Gcancelled 
      Just (i,[]) | i == mkCId "claim" -> Gclaim 
      Just (i,[]) | i == mkCId "condition" -> Gcondition 
      Just (i,[]) | i == mkCId "household_appliance" -> Ghousehold_appliance 
      Just (i,[]) | i == mkCId "insects" -> Ginsects 
      Just (i,[]) | i == mkCId "plumbing_heating_or_AC" -> Gplumbing_heating_or_AC 
      Just (i,[]) | i == mkCId "premium" -> Gpremium 
      Just (i,[x1]) | i == mkCId "result_from" -> Gresult_from (fg x1)
      Just (i,[]) | i == mkCId "rodents" -> Grodents 
      Just (i,[]) | i == mkCId "signed" -> Gsigned 
      Just (i,[]) | i == mkCId "stay_during_policy_period" -> Gstay_during_policy_period 
      Just (i,[]) | i == mkCId "stay_overnight" -> Gstay_overnight 
      Just (i,[]) | i == mkCId "swimming_pool" -> Gswimming_pool 
      Just (i,[]) | i == mkCId "vermin" -> Gvermin 
      Just (i,[]) | i == mkCId "water" -> Gwater 


      _ -> error ("no NP " ++ show t)

instance Gf GNumeral where
  gf (Gnum x1) = mkApp (mkCId "num") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "num" -> Gnum (fg x1)


      _ -> error ("no Numeral " ++ show t)

instance Gf GPN where
  gf G_1012_PN = mkApp (mkCId "_1012_PN") []
  gf G_1013_PN = mkApp (mkCId "_1013_PN") []
  gf G_1014_PN = mkApp (mkCId "_1014_PN") []
  gf G_333A_PN = mkApp (mkCId "_333A_PN") []
  gf G_ADD_PN = mkApp (mkCId "_ADD_PN") []
  gf G_AIDS_PN = mkApp (mkCId "_AIDS_PN") []
  gf G_Accident_PN = mkApp (mkCId "_Accident_PN") []
  gf G_Accidental_PN = mkApp (mkCId "_Accidental_PN") []
  gf G_Accidents_PN = mkApp (mkCId "_Accidents_PN") []
  gf G_Address_PN = mkApp (mkCId "_Address_PN") []
  gf G_Adjustment_PN = mkApp (mkCId "_Adjustment_PN") []
  gf G_Assured_PN = mkApp (mkCId "_Assured_PN") []
  gf G_BSA_PN = mkApp (mkCId "_BSA_PN") []
  gf G_Benefit_PN = mkApp (mkCId "_Benefit_PN") []
  gf G_Cap_PN = mkApp (mkCId "_Cap_PN") []
  gf G_Claim_PN = mkApp (mkCId "_Claim_PN") []
  gf G_CoV_PN = mkApp (mkCId "_CoV_PN") []
  gf G_Conditions_PN = mkApp (mkCId "_Conditions_PN") []
  gf G_Date_PN = mkApp (mkCId "_Date_PN") []
  gf G_Death_PN = mkApp (mkCId "_Death_PN") []
  gf G_Details_PN = mkApp (mkCId "_Details_PN") []
  gf G_Disease_PN = mkApp (mkCId "_Disease_PN") []
  gf G_Dismemberment_PN = mkApp (mkCId "_Dismemberment_PN") []
  gf G_Event_PN = mkApp (mkCId "_Event_PN") []
  gf G_Expense_PN = mkApp (mkCId "_Expense_PN") []
  gf G_Flu_PN = mkApp (mkCId "_Flu_PN") []
  gf G_H5N1_PN = mkApp (mkCId "_H5N1_PN") []
  gf G_H7N9_PN = mkApp (mkCId "_H7N9_PN") []
  gf G_H7N_PN = mkApp (mkCId "_H7N_PN") []
  gf G_H9N2_PN = mkApp (mkCId "_H9N2_PN") []
  gf G_HAS_PN = mkApp (mkCId "_HAS_PN") []
  gf G_Head_PN = mkApp (mkCId "_Head_PN") []
  gf G_Health_PN = mkApp (mkCId "_Health_PN") []
  gf G_Influenza_PN = mkApp (mkCId "_Influenza_PN") []
  gf G_Injury_PN = mkApp (mkCId "_Injury_PN") []
  gf G_Insurer_PN = mkApp (mkCId "_Insurer_PN") []
  gf G_LA_PN = mkApp (mkCId "_LA_PN") []
  gf G_LE_PN = mkApp (mkCId "_LE_PN") []
  gf G_Leg_PN = mkApp (mkCId "_Leg_PN") []
  gf G_Legionnaires_PN = mkApp (mkCId "_Legionnaires_PN") []
  gf G_Life_PN = mkApp (mkCId "_Life_PN") []
  gf G_Limit_PN = mkApp (mkCId "_Limit_PN") []
  gf G_MAP_PN = mkApp (mkCId "_MAP_PN") []
  gf G_MIN_PN = mkApp (mkCId "_MIN_PN") []
  gf G_MR_PN = mkApp (mkCId "_MR_PN") []
  gf G_M_PN = mkApp (mkCId "_M_PN") []
  gf G_Medicine_PN = mkApp (mkCId "_Medicine_PN") []
  gf G_Melioidosis_PN = mkApp (mkCId "_Melioidosis_PN") []
  gf G_Ministry_PN = mkApp (mkCId "_Ministry_PN") []
  gf G_Mumps_PN = mkApp (mkCId "_Mumps_PN") []
  gf G_N_PN = mkApp (mkCId "_N_PN") []
  gf G_Nipah_PN = mkApp (mkCId "_Nipah_PN") []
  gf G_Ontology_PN = mkApp (mkCId "_Ontology_PN") []
  gf G_PS_PN = mkApp (mkCId "_PS_PN") []
  gf G_Plan14_PN = mkApp (mkCId "_Plan14_PN") []
  gf G_PlanAF_PN = mkApp (mkCId "_PlanAF_PN") []
  gf G_PolicyHolder_PN = mkApp (mkCId "_PolicyHolder_PN") []
  gf G_Policy_PN = mkApp (mkCId "_Policy_PN") []
  gf G_RETURN_PN = mkApp (mkCId "_RETURN_PN") []
  gf G_Reductions_PN = mkApp (mkCId "_Reductions_PN") []
  gf G_Removal_PN = mkApp (mkCId "_Removal_PN") []
  gf G_Republic_PN = mkApp (mkCId "_Republic_PN") []
  gf G_SA_PN = mkApp (mkCId "_SA_PN") []
  gf G_SG_PN = mkApp (mkCId "_SG_PN") []
  gf G_Schedule_PN = mkApp (mkCId "_Schedule_PN") []
  gf G_Section_PN = mkApp (mkCId "_Section_PN") []
  gf G_Service_PN = mkApp (mkCId "_Service_PN") []
  gf G_Singapore_PN = mkApp (mkCId "_Singapore_PN") []
  gf G_Step_PN = mkApp (mkCId "_Step_PN") []
  gf G_Subscribed_PN = mkApp (mkCId "_Subscribed_PN") []
  gf G_TABLE_PN = mkApp (mkCId "_TABLE_PN") []
  gf G_Teeth_PN = mkApp (mkCId "_Teeth_PN") []
  gf G_Triple_PN = mkApp (mkCId "_Triple_PN") []
  gf G_Type_PN = mkApp (mkCId "_Type_PN") []
  gf G_Types_PN = mkApp (mkCId "_Types_PN") []
  gf G_UPON_PN = mkApp (mkCId "_UPON_PN") []
  gf G_Wife_PN = mkApp (mkCId "_Wife_PN") []
  gf G_Yellow_PN = mkApp (mkCId "_Yellow_PN") []
  gf G_addSA_PN = mkApp (mkCId "_addSA_PN") []
  gf G_benADD_PN = mkApp (mkCId "_benADD_PN") []
  gf G_benADDs_PN = mkApp (mkCId "_benADDs_PN") []
  gf G_benRA_PN = mkApp (mkCId "_benRA_PN") []
  gf G_benTCM_PN = mkApp (mkCId "_benTCM_PN") []
  gf G_circ_PN = mkApp (mkCId "_circ_PN") []
  gf G_dTime_PN = mkApp (mkCId "_dTime_PN") []
  gf G_dType_PN = mkApp (mkCId "_dType_PN") []
  gf G_diving_PN = mkApp (mkCId "_diving_PN") []
  gf G_holder_PN = mkApp (mkCId "_holder_PN") []
  gf G_motocross_PN = mkApp (mkCId "_motocross_PN") []
  gf G_p_PN = mkApp (mkCId "_p_PN") []
  gf G_plan3_PN = mkApp (mkCId "_plan3_PN") []
  gf G_plan4_PN = mkApp (mkCId "_plan4_PN") []
  gf G_planAF_PN = mkApp (mkCId "_planAF_PN") []
  gf G_planB_PN = mkApp (mkCId "_planB_PN") []
  gf G_planC_PN = mkApp (mkCId "_planC_PN") []
  gf G_planE_PN = mkApp (mkCId "_planE_PN") []
  gf G_planF_PN = mkApp (mkCId "_planF_PN") []
  gf G_policyHolder_PN = mkApp (mkCId "_policyHolder_PN") []
  gf G_qualifies_for_add_PN = mkApp (mkCId "_qualifies_for_add_PN") []
  gf G_schema_PN = mkApp (mkCId "_schema_PN") []
  gf G_sum_list_PN = mkApp (mkCId "_sum_list_PN") []
  gf G_x_PN = mkApp (mkCId "_x_PN") []
  gf G_y_PN = mkApp (mkCId "_y_PN") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "_1012_PN" -> G_1012_PN 
      Just (i,[]) | i == mkCId "_1013_PN" -> G_1013_PN 
      Just (i,[]) | i == mkCId "_1014_PN" -> G_1014_PN 
      Just (i,[]) | i == mkCId "_333A_PN" -> G_333A_PN 
      Just (i,[]) | i == mkCId "_ADD_PN" -> G_ADD_PN 
      Just (i,[]) | i == mkCId "_AIDS_PN" -> G_AIDS_PN 
      Just (i,[]) | i == mkCId "_Accident_PN" -> G_Accident_PN 
      Just (i,[]) | i == mkCId "_Accidental_PN" -> G_Accidental_PN 
      Just (i,[]) | i == mkCId "_Accidents_PN" -> G_Accidents_PN 
      Just (i,[]) | i == mkCId "_Address_PN" -> G_Address_PN 
      Just (i,[]) | i == mkCId "_Adjustment_PN" -> G_Adjustment_PN 
      Just (i,[]) | i == mkCId "_Assured_PN" -> G_Assured_PN 
      Just (i,[]) | i == mkCId "_BSA_PN" -> G_BSA_PN 
      Just (i,[]) | i == mkCId "_Benefit_PN" -> G_Benefit_PN 
      Just (i,[]) | i == mkCId "_Cap_PN" -> G_Cap_PN 
      Just (i,[]) | i == mkCId "_Claim_PN" -> G_Claim_PN 
      Just (i,[]) | i == mkCId "_CoV_PN" -> G_CoV_PN 
      Just (i,[]) | i == mkCId "_Conditions_PN" -> G_Conditions_PN 
      Just (i,[]) | i == mkCId "_Date_PN" -> G_Date_PN 
      Just (i,[]) | i == mkCId "_Death_PN" -> G_Death_PN 
      Just (i,[]) | i == mkCId "_Details_PN" -> G_Details_PN 
      Just (i,[]) | i == mkCId "_Disease_PN" -> G_Disease_PN 
      Just (i,[]) | i == mkCId "_Dismemberment_PN" -> G_Dismemberment_PN 
      Just (i,[]) | i == mkCId "_Event_PN" -> G_Event_PN 
      Just (i,[]) | i == mkCId "_Expense_PN" -> G_Expense_PN 
      Just (i,[]) | i == mkCId "_Flu_PN" -> G_Flu_PN 
      Just (i,[]) | i == mkCId "_H5N1_PN" -> G_H5N1_PN 
      Just (i,[]) | i == mkCId "_H7N9_PN" -> G_H7N9_PN 
      Just (i,[]) | i == mkCId "_H7N_PN" -> G_H7N_PN 
      Just (i,[]) | i == mkCId "_H9N2_PN" -> G_H9N2_PN 
      Just (i,[]) | i == mkCId "_HAS_PN" -> G_HAS_PN 
      Just (i,[]) | i == mkCId "_Head_PN" -> G_Head_PN 
      Just (i,[]) | i == mkCId "_Health_PN" -> G_Health_PN 
      Just (i,[]) | i == mkCId "_Influenza_PN" -> G_Influenza_PN 
      Just (i,[]) | i == mkCId "_Injury_PN" -> G_Injury_PN 
      Just (i,[]) | i == mkCId "_Insurer_PN" -> G_Insurer_PN 
      Just (i,[]) | i == mkCId "_LA_PN" -> G_LA_PN 
      Just (i,[]) | i == mkCId "_LE_PN" -> G_LE_PN 
      Just (i,[]) | i == mkCId "_Leg_PN" -> G_Leg_PN 
      Just (i,[]) | i == mkCId "_Legionnaires_PN" -> G_Legionnaires_PN 
      Just (i,[]) | i == mkCId "_Life_PN" -> G_Life_PN 
      Just (i,[]) | i == mkCId "_Limit_PN" -> G_Limit_PN 
      Just (i,[]) | i == mkCId "_MAP_PN" -> G_MAP_PN 
      Just (i,[]) | i == mkCId "_MIN_PN" -> G_MIN_PN 
      Just (i,[]) | i == mkCId "_MR_PN" -> G_MR_PN 
      Just (i,[]) | i == mkCId "_M_PN" -> G_M_PN 
      Just (i,[]) | i == mkCId "_Medicine_PN" -> G_Medicine_PN 
      Just (i,[]) | i == mkCId "_Melioidosis_PN" -> G_Melioidosis_PN 
      Just (i,[]) | i == mkCId "_Ministry_PN" -> G_Ministry_PN 
      Just (i,[]) | i == mkCId "_Mumps_PN" -> G_Mumps_PN 
      Just (i,[]) | i == mkCId "_N_PN" -> G_N_PN 
      Just (i,[]) | i == mkCId "_Nipah_PN" -> G_Nipah_PN 
      Just (i,[]) | i == mkCId "_Ontology_PN" -> G_Ontology_PN 
      Just (i,[]) | i == mkCId "_PS_PN" -> G_PS_PN 
      Just (i,[]) | i == mkCId "_Plan14_PN" -> G_Plan14_PN 
      Just (i,[]) | i == mkCId "_PlanAF_PN" -> G_PlanAF_PN 
      Just (i,[]) | i == mkCId "_PolicyHolder_PN" -> G_PolicyHolder_PN 
      Just (i,[]) | i == mkCId "_Policy_PN" -> G_Policy_PN 
      Just (i,[]) | i == mkCId "_RETURN_PN" -> G_RETURN_PN 
      Just (i,[]) | i == mkCId "_Reductions_PN" -> G_Reductions_PN 
      Just (i,[]) | i == mkCId "_Removal_PN" -> G_Removal_PN 
      Just (i,[]) | i == mkCId "_Republic_PN" -> G_Republic_PN 
      Just (i,[]) | i == mkCId "_SA_PN" -> G_SA_PN 
      Just (i,[]) | i == mkCId "_SG_PN" -> G_SG_PN 
      Just (i,[]) | i == mkCId "_Schedule_PN" -> G_Schedule_PN 
      Just (i,[]) | i == mkCId "_Section_PN" -> G_Section_PN 
      Just (i,[]) | i == mkCId "_Service_PN" -> G_Service_PN 
      Just (i,[]) | i == mkCId "_Singapore_PN" -> G_Singapore_PN 
      Just (i,[]) | i == mkCId "_Step_PN" -> G_Step_PN 
      Just (i,[]) | i == mkCId "_Subscribed_PN" -> G_Subscribed_PN 
      Just (i,[]) | i == mkCId "_TABLE_PN" -> G_TABLE_PN 
      Just (i,[]) | i == mkCId "_Teeth_PN" -> G_Teeth_PN 
      Just (i,[]) | i == mkCId "_Triple_PN" -> G_Triple_PN 
      Just (i,[]) | i == mkCId "_Type_PN" -> G_Type_PN 
      Just (i,[]) | i == mkCId "_Types_PN" -> G_Types_PN 
      Just (i,[]) | i == mkCId "_UPON_PN" -> G_UPON_PN 
      Just (i,[]) | i == mkCId "_Wife_PN" -> G_Wife_PN 
      Just (i,[]) | i == mkCId "_Yellow_PN" -> G_Yellow_PN 
      Just (i,[]) | i == mkCId "_addSA_PN" -> G_addSA_PN 
      Just (i,[]) | i == mkCId "_benADD_PN" -> G_benADD_PN 
      Just (i,[]) | i == mkCId "_benADDs_PN" -> G_benADDs_PN 
      Just (i,[]) | i == mkCId "_benRA_PN" -> G_benRA_PN 
      Just (i,[]) | i == mkCId "_benTCM_PN" -> G_benTCM_PN 
      Just (i,[]) | i == mkCId "_circ_PN" -> G_circ_PN 
      Just (i,[]) | i == mkCId "_dTime_PN" -> G_dTime_PN 
      Just (i,[]) | i == mkCId "_dType_PN" -> G_dType_PN 
      Just (i,[]) | i == mkCId "_diving_PN" -> G_diving_PN 
      Just (i,[]) | i == mkCId "_holder_PN" -> G_holder_PN 
      Just (i,[]) | i == mkCId "_motocross_PN" -> G_motocross_PN 
      Just (i,[]) | i == mkCId "_p_PN" -> G_p_PN 
      Just (i,[]) | i == mkCId "_plan3_PN" -> G_plan3_PN 
      Just (i,[]) | i == mkCId "_plan4_PN" -> G_plan4_PN 
      Just (i,[]) | i == mkCId "_planAF_PN" -> G_planAF_PN 
      Just (i,[]) | i == mkCId "_planB_PN" -> G_planB_PN 
      Just (i,[]) | i == mkCId "_planC_PN" -> G_planC_PN 
      Just (i,[]) | i == mkCId "_planE_PN" -> G_planE_PN 
      Just (i,[]) | i == mkCId "_planF_PN" -> G_planF_PN 
      Just (i,[]) | i == mkCId "_policyHolder_PN" -> G_policyHolder_PN 
      Just (i,[]) | i == mkCId "_qualifies_for_add_PN" -> G_qualifies_for_add_PN 
      Just (i,[]) | i == mkCId "_schema_PN" -> G_schema_PN 
      Just (i,[]) | i == mkCId "_sum_list_PN" -> G_sum_list_PN 
      Just (i,[]) | i == mkCId "_x_PN" -> G_x_PN 
      Just (i,[]) | i == mkCId "_y_PN" -> G_y_PN 


      _ -> error ("no PN " ++ show t)

instance Gf GPol where
  gf GNEG = mkApp (mkCId "NEG") []
  gf GPOS = mkApp (mkCId "POS") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "NEG" -> GNEG 
      Just (i,[]) | i == mkCId "POS" -> GPOS 


      _ -> error ("no Pol " ++ show t)

instance Gf GPrePost where
  gf (GAP_PrePost x1) = mkApp (mkCId "AP_PrePost") [gf x1]
  gf (GAdv_PrePost x1) = mkApp (mkCId "Adv_PrePost") [gf x1]
  gf (GNP_PrePost x1) = mkApp (mkCId "NP_PrePost") [gf x1]
  gf (GNP_caused_NP_to_VP_Prep_PrePost x1 x2 x3 x4) = mkApp (mkCId "NP_caused_NP_to_VP_Prep_PrePost") [gf x1, gf x2, gf x3, gf x4]
  gf (GNP_caused_by_PrePost x1) = mkApp (mkCId "NP_caused_by_PrePost") [gf x1]
  gf (GS_PrePost x1 x2) = mkApp (mkCId "S_PrePost") [gf x1, gf x2]
  gf (GV2_PrePost x1) = mkApp (mkCId "V2_PrePost") [gf x1]
  gf (GrecoverUnparsedPrePost x1) = mkApp (mkCId "recoverUnparsedPrePost") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "AP_PrePost" -> GAP_PrePost (fg x1)
      Just (i,[x1]) | i == mkCId "Adv_PrePost" -> GAdv_PrePost (fg x1)
      Just (i,[x1]) | i == mkCId "NP_PrePost" -> GNP_PrePost (fg x1)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "NP_caused_NP_to_VP_Prep_PrePost" -> GNP_caused_NP_to_VP_Prep_PrePost (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1]) | i == mkCId "NP_caused_by_PrePost" -> GNP_caused_by_PrePost (fg x1)
      Just (i,[x1,x2]) | i == mkCId "S_PrePost" -> GS_PrePost (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "V2_PrePost" -> GV2_PrePost (fg x1)
      Just (i,[x1]) | i == mkCId "recoverUnparsedPrePost" -> GrecoverUnparsedPrePost (fg x1)


      _ -> error ("no PrePost " ++ show t)

instance Gf GPrep where
  gf (GConjPrep x1 x2) = mkApp (mkCId "ConjPrep") [gf x1, gf x2]
  gf G_across_Prep = mkApp (mkCId "_across_Prep") []
  gf G_after_Prep = mkApp (mkCId "_after_Prep") []
  gf G_as_Prep = mkApp (mkCId "_as_Prep") []
  gf G_at_Prep = mkApp (mkCId "_at_Prep") []
  gf G_before_Prep = mkApp (mkCId "_before_Prep") []
  gf G_between_Prep = mkApp (mkCId "_between_Prep") []
  gf G_by_Prep = mkApp (mkCId "_by_Prep") []
  gf G_during_Prep = mkApp (mkCId "_during_Prep") []
  gf G_for_Prep = mkApp (mkCId "_for_Prep") []
  gf G_from_Prep = mkApp (mkCId "_from_Prep") []
  gf G_in_Prep = mkApp (mkCId "_in_Prep") []
  gf G_into_Prep = mkApp (mkCId "_into_Prep") []
  gf G_of_Prep = mkApp (mkCId "_of_Prep") []
  gf G_on_Prep = mkApp (mkCId "_on_Prep") []
  gf G_out_Prep = mkApp (mkCId "_out_Prep") []
  gf G_over_Prep = mkApp (mkCId "_over_Prep") []
  gf G_per_Prep = mkApp (mkCId "_per_Prep") []
  gf G_than_Prep = mkApp (mkCId "_than_Prep") []
  gf G_through_Prep = mkApp (mkCId "_through_Prep") []
  gf G_to_Prep = mkApp (mkCId "_to_Prep") []
  gf G_under_Prep = mkApp (mkCId "_under_Prep") []
  gf G_up_Prep = mkApp (mkCId "_up_Prep") []
  gf G_with_Prep = mkApp (mkCId "_with_Prep") []
  gf G_within_Prep = mkApp (mkCId "_within_Prep") []
  gf Gabout_Prep = mkApp (mkCId "about_Prep") []
  gf Gafter_Prep = mkApp (mkCId "after_Prep") []
  gf Gbefore_Prep = mkApp (mkCId "before_Prep") []
  gf Gfor_Prep = mkApp (mkCId "for_Prep") []
  gf Gfrom_Prep = mkApp (mkCId "from_Prep") []
  gf Gon_Prep = mkApp (mkCId "on_Prep") []
  gf Gpossess_Prep = mkApp (mkCId "possess_Prep") []
  gf Gto_Prep = mkApp (mkCId "to_Prep") []
  gf Gwithin_Prep = mkApp (mkCId "within_Prep") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjPrep" -> GConjPrep (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "_across_Prep" -> G_across_Prep 
      Just (i,[]) | i == mkCId "_after_Prep" -> G_after_Prep 
      Just (i,[]) | i == mkCId "_as_Prep" -> G_as_Prep 
      Just (i,[]) | i == mkCId "_at_Prep" -> G_at_Prep 
      Just (i,[]) | i == mkCId "_before_Prep" -> G_before_Prep 
      Just (i,[]) | i == mkCId "_between_Prep" -> G_between_Prep 
      Just (i,[]) | i == mkCId "_by_Prep" -> G_by_Prep 
      Just (i,[]) | i == mkCId "_during_Prep" -> G_during_Prep 
      Just (i,[]) | i == mkCId "_for_Prep" -> G_for_Prep 
      Just (i,[]) | i == mkCId "_from_Prep" -> G_from_Prep 
      Just (i,[]) | i == mkCId "_in_Prep" -> G_in_Prep 
      Just (i,[]) | i == mkCId "_into_Prep" -> G_into_Prep 
      Just (i,[]) | i == mkCId "_of_Prep" -> G_of_Prep 
      Just (i,[]) | i == mkCId "_on_Prep" -> G_on_Prep 
      Just (i,[]) | i == mkCId "_out_Prep" -> G_out_Prep 
      Just (i,[]) | i == mkCId "_over_Prep" -> G_over_Prep 
      Just (i,[]) | i == mkCId "_per_Prep" -> G_per_Prep 
      Just (i,[]) | i == mkCId "_than_Prep" -> G_than_Prep 
      Just (i,[]) | i == mkCId "_through_Prep" -> G_through_Prep 
      Just (i,[]) | i == mkCId "_to_Prep" -> G_to_Prep 
      Just (i,[]) | i == mkCId "_under_Prep" -> G_under_Prep 
      Just (i,[]) | i == mkCId "_up_Prep" -> G_up_Prep 
      Just (i,[]) | i == mkCId "_with_Prep" -> G_with_Prep 
      Just (i,[]) | i == mkCId "_within_Prep" -> G_within_Prep 
      Just (i,[]) | i == mkCId "about_Prep" -> Gabout_Prep 
      Just (i,[]) | i == mkCId "after_Prep" -> Gafter_Prep 
      Just (i,[]) | i == mkCId "before_Prep" -> Gbefore_Prep 
      Just (i,[]) | i == mkCId "for_Prep" -> Gfor_Prep 
      Just (i,[]) | i == mkCId "from_Prep" -> Gfrom_Prep 
      Just (i,[]) | i == mkCId "on_Prep" -> Gon_Prep 
      Just (i,[]) | i == mkCId "possess_Prep" -> Gpossess_Prep 
      Just (i,[]) | i == mkCId "to_Prep" -> Gto_Prep 
      Just (i,[]) | i == mkCId "within_Prep" -> Gwithin_Prep 


      _ -> error ("no Prep " ++ show t)

instance Gf GQS where
  gf (GConjPrePostQS x1 x2 x3 x4) = mkApp (mkCId "ConjPrePostQS") [gf x1, gf x2, gf x3, gf x4]
  gf (GConjQS x1 x2) = mkApp (mkCId "ConjQS") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3,x4]) | i == mkCId "ConjPrePostQS" -> GConjPrePostQS (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "ConjQS" -> GConjQS (fg x1) (fg x2)


      _ -> error ("no QS " ++ show t)

instance Gf GS where
  gf (GConjPrePostS x1 x2 x3 x4) = mkApp (mkCId "ConjPrePostS") [gf x1, gf x2, gf x3, gf x4]
  gf (GConjS x1 x2) = mkApp (mkCId "ConjS") [gf x1, gf x2]
  gf (GPredVPS x1 x2) = mkApp (mkCId "PredVPS") [gf x1, gf x2]
  gf (GReferenceNP x1) = mkApp (mkCId "ReferenceNP") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3,x4]) | i == mkCId "ConjPrePostS" -> GConjPrePostS (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "ConjS" -> GConjS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PredVPS" -> GPredVPS (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ReferenceNP" -> GReferenceNP (fg x1)


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
  gf Gpot21 = mkApp (mkCId "pot21") []
  gf (Gpot2plus x1 x2) = mkApp (mkCId "pot2plus") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "pot1as2" -> Gpot1as2 (fg x1)
      Just (i,[x1]) | i == mkCId "pot2" -> Gpot2 (fg x1)
      Just (i,[]) | i == mkCId "pot21" -> Gpot21 
      Just (i,[x1,x2]) | i == mkCId "pot2plus" -> Gpot2plus (fg x1) (fg x2)


      _ -> error ("no Sub1000 " ++ show t)

instance Gf GSub1000000 where
  gf (Gpot2as3 x1) = mkApp (mkCId "pot2as3") [gf x1]
  gf (Gpot3 x1) = mkApp (mkCId "pot3") [gf x1]
  gf Gpot31 = mkApp (mkCId "pot31") []
  gf (Gpot3float x1) = mkApp (mkCId "pot3float") [gf x1]
  gf (Gpot3plus x1 x2) = mkApp (mkCId "pot3plus") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "pot2as3" -> Gpot2as3 (fg x1)
      Just (i,[x1]) | i == mkCId "pot3" -> Gpot3 (fg x1)
      Just (i,[]) | i == mkCId "pot31" -> Gpot31 
      Just (i,[x1]) | i == mkCId "pot3float" -> Gpot3float (fg x1)
      Just (i,[x1,x2]) | i == mkCId "pot3plus" -> Gpot3plus (fg x1) (fg x2)


      _ -> error ("no Sub1000000 " ++ show t)

instance Gf GSub1000000000 where
  gf (Gpot3as4 x1) = mkApp (mkCId "pot3as4") [gf x1]
  gf (Gpot4 x1) = mkApp (mkCId "pot4") [gf x1]
  gf Gpot41 = mkApp (mkCId "pot41") []
  gf (Gpot4float x1) = mkApp (mkCId "pot4float") [gf x1]
  gf (Gpot4plus x1 x2) = mkApp (mkCId "pot4plus") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "pot3as4" -> Gpot3as4 (fg x1)
      Just (i,[x1]) | i == mkCId "pot4" -> Gpot4 (fg x1)
      Just (i,[]) | i == mkCId "pot41" -> Gpot41 
      Just (i,[x1]) | i == mkCId "pot4float" -> Gpot4float (fg x1)
      Just (i,[x1,x2]) | i == mkCId "pot4plus" -> Gpot4plus (fg x1) (fg x2)


      _ -> error ("no Sub1000000000 " ++ show t)

instance Gf GSub1000000000000 where
  gf (Gpot4as5 x1) = mkApp (mkCId "pot4as5") [gf x1]
  gf (Gpot5 x1) = mkApp (mkCId "pot5") [gf x1]
  gf Gpot51 = mkApp (mkCId "pot51") []
  gf (Gpot5float x1) = mkApp (mkCId "pot5float") [gf x1]
  gf (Gpot5plus x1 x2) = mkApp (mkCId "pot5plus") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "pot4as5" -> Gpot4as5 (fg x1)
      Just (i,[x1]) | i == mkCId "pot5" -> Gpot5 (fg x1)
      Just (i,[]) | i == mkCId "pot51" -> Gpot51 
      Just (i,[x1]) | i == mkCId "pot5float" -> Gpot5float (fg x1)
      Just (i,[x1,x2]) | i == mkCId "pot5plus" -> Gpot5plus (fg x1) (fg x2)


      _ -> error ("no Sub1000000000000 " ++ show t)

instance Gf GSubj where
  gf G_and_Subj = mkApp (mkCId "_and_Subj") []
  gf G_as_Subj = mkApp (mkCId "_as_Subj") []
  gf G_before_Subj = mkApp (mkCId "_before_Subj") []
  gf G_both_Subj = mkApp (mkCId "_both_Subj") []
  gf G_but_Subj = mkApp (mkCId "_but_Subj") []
  gf G_if_Subj = mkApp (mkCId "_if_Subj") []
  gf G_or_Subj = mkApp (mkCId "_or_Subj") []
  gf G_that_Subj = mkApp (mkCId "_that_Subj") []
  gf G_when_Subj = mkApp (mkCId "_when_Subj") []
  gf G_while_Subj = mkApp (mkCId "_while_Subj") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "_and_Subj" -> G_and_Subj 
      Just (i,[]) | i == mkCId "_as_Subj" -> G_as_Subj 
      Just (i,[]) | i == mkCId "_before_Subj" -> G_before_Subj 
      Just (i,[]) | i == mkCId "_both_Subj" -> G_both_Subj 
      Just (i,[]) | i == mkCId "_but_Subj" -> G_but_Subj 
      Just (i,[]) | i == mkCId "_if_Subj" -> G_if_Subj 
      Just (i,[]) | i == mkCId "_or_Subj" -> G_or_Subj 
      Just (i,[]) | i == mkCId "_that_Subj" -> G_that_Subj 
      Just (i,[]) | i == mkCId "_when_Subj" -> G_when_Subj 
      Just (i,[]) | i == mkCId "_while_Subj" -> G_while_Subj 


      _ -> error ("no Subj " ++ show t)

instance Gf GSubject where
  gf (GAN x1) = mkApp (mkCId "AN") [gf x1]
  gf (GEVERY x1) = mkApp (mkCId "EVERY") [gf x1]
  gf (GPARTY x1) = mkApp (mkCId "PARTY") [gf x1]
  gf (GSubjWho x1 x2) = mkApp (mkCId "SubjWho") [gf x1, gf x2]
  gf (GTHE x1) = mkApp (mkCId "THE") [gf x1]
  gf GYou = mkApp (mkCId "You") []
  gf (GrecoverUnparsedSubj x1) = mkApp (mkCId "recoverUnparsedSubj") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "AN" -> GAN (fg x1)
      Just (i,[x1]) | i == mkCId "EVERY" -> GEVERY (fg x1)
      Just (i,[x1]) | i == mkCId "PARTY" -> GPARTY (fg x1)
      Just (i,[x1,x2]) | i == mkCId "SubjWho" -> GSubjWho (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "THE" -> GTHE (fg x1)
      Just (i,[]) | i == mkCId "You" -> GYou 
      Just (i,[x1]) | i == mkCId "recoverUnparsedSubj" -> GrecoverUnparsedSubj (fg x1)


      _ -> error ("no Subject " ++ show t)

instance Gf GTComparison where
  gf GAFTER = mkApp (mkCId "AFTER") []
  gf GBEFORE = mkApp (mkCId "BEFORE") []
  gf GBY = mkApp (mkCId "BY") []
  gf (GConjTComparison x1 x2) = mkApp (mkCId "ConjTComparison") [gf x1, gf x2]
  gf GON = mkApp (mkCId "ON") []
  gf GVAGUE = mkApp (mkCId "VAGUE") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "AFTER" -> GAFTER 
      Just (i,[]) | i == mkCId "BEFORE" -> GBEFORE 
      Just (i,[]) | i == mkCId "BY" -> GBY 
      Just (i,[x1,x2]) | i == mkCId "ConjTComparison" -> GConjTComparison (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "ON" -> GON 
      Just (i,[]) | i == mkCId "VAGUE" -> GVAGUE 


      _ -> error ("no TComparison " ++ show t)

instance Gf GTemp where
  gf GpastSimul = mkApp (mkCId "pastSimul") []
  gf GpresAnt = mkApp (mkCId "presAnt") []
  gf GpresSimul = mkApp (mkCId "presSimul") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "pastSimul" -> GpastSimul 
      Just (i,[]) | i == mkCId "presAnt" -> GpresAnt 
      Just (i,[]) | i == mkCId "presSimul" -> GpresSimul 


      _ -> error ("no Temp " ++ show t)

instance Gf GTemporal where
  gf (GTemporalConstraint x1 x2 x3) = mkApp (mkCId "TemporalConstraint") [gf x1, gf x2, gf x3]
  gf (GTemporalConstraintNoDigits x1 x2) = mkApp (mkCId "TemporalConstraintNoDigits") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "TemporalConstraint" -> GTemporalConstraint (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "TemporalConstraintNoDigits" -> GTemporalConstraintNoDigits (fg x1) (fg x2)


      _ -> error ("no Temporal " ++ show t)

instance Gf GText where
  gf (GRegulative x1 x2 x3) = mkApp (mkCId "Regulative") [gf x1, gf x2, gf x3]
  gf (GadvUPON x1) = mkApp (mkCId "advUPON") [gf x1]
  gf (GqCOND x1) = mkApp (mkCId "qCOND") [gf x1]
  gf (GqCONSTR x1) = mkApp (mkCId "qCONSTR") [gf x1]
  gf (GqPREPOST x1) = mkApp (mkCId "qPREPOST") [gf x1]
  gf (GqUPON x1 x2) = mkApp (mkCId "qUPON") [gf x1, gf x2]
  gf (GqWHO x1 x2) = mkApp (mkCId "qWHO") [gf x1, gf x2]
  gf (GsCOND x1) = mkApp (mkCId "sCOND") [gf x1]
  gf (GsUPON x1 x2) = mkApp (mkCId "sUPON") [gf x1, gf x2]
  gf (GsWHO x1 x2) = mkApp (mkCId "sWHO") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "Regulative" -> GRegulative (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "advUPON" -> GadvUPON (fg x1)
      Just (i,[x1]) | i == mkCId "qCOND" -> GqCOND (fg x1)
      Just (i,[x1]) | i == mkCId "qCONSTR" -> GqCONSTR (fg x1)
      Just (i,[x1]) | i == mkCId "qPREPOST" -> GqPREPOST (fg x1)
      Just (i,[x1,x2]) | i == mkCId "qUPON" -> GqUPON (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "qWHO" -> GqWHO (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "sCOND" -> GsCOND (fg x1)
      Just (i,[x1,x2]) | i == mkCId "sUPON" -> GsUPON (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "sWHO" -> GsWHO (fg x1) (fg x2)


      _ -> error ("no Text " ++ show t)

instance Gf GTimeUnit where
  gf GDay_Unit = mkApp (mkCId "Day_Unit") []
  gf GMonth_Unit = mkApp (mkCId "Month_Unit") []
  gf GYear_Unit = mkApp (mkCId "Year_Unit") []
  gf (GrecoverUnparsedTimeUnit x1) = mkApp (mkCId "recoverUnparsedTimeUnit") [gf x1]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Day_Unit" -> GDay_Unit 
      Just (i,[]) | i == mkCId "Month_Unit" -> GMonth_Unit 
      Just (i,[]) | i == mkCId "Year_Unit" -> GYear_Unit 
      Just (i,[x1]) | i == mkCId "recoverUnparsedTimeUnit" -> GrecoverUnparsedTimeUnit (fg x1)


      _ -> error ("no TimeUnit " ++ show t)

instance Gf GUpon where
  gf (GUPON x1) = mkApp (mkCId "UPON") [gf x1]
  gf (GrecoverUnparsedUpon x1) = mkApp (mkCId "recoverUnparsedUpon") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "UPON" -> GUPON (fg x1)
      Just (i,[x1]) | i == mkCId "recoverUnparsedUpon" -> GrecoverUnparsedUpon (fg x1)


      _ -> error ("no Upon " ++ show t)

instance Gf GV where
  gf G_H7N7_V = mkApp (mkCId "_H7N7_V") []
  gf G_adjust_V = mkApp (mkCId "_adjust_V") []
  gf G_apply_V = mkApp (mkCId "_apply_V") []
  gf G_asssure_V = mkApp (mkCId "_asssure_V") []
  gf G_assure_V = mkApp (mkCId "_assure_V") []
  gf G_benefit_V = mkApp (mkCId "_benefit_V") []
  gf G_canoe_V = mkApp (mkCId "_canoe_V") []
  gf G_cave_V = mkApp (mkCId "_cave_V") []
  gf G_claim_V = mkApp (mkCId "_claim_V") []
  gf G_cover_V = mkApp (mkCId "_cover_V") []
  gf G_establish_V = mkApp (mkCId "_establish_V") []
  gf G_exception_V = mkApp (mkCId "_exception_V") []
  gf G_give_V = mkApp (mkCId "_give_V") []
  gf G_glide_V = mkApp (mkCId "_glide_V") []
  gf G_govern_V = mkApp (mkCId "_govern_V") []
  gf G_hand_V = mkApp (mkCId "_hand_V") []
  gf G_hernia_V = mkApp (mkCId "_hernia_V") []
  gf G_hunt_V = mkApp (mkCId "_hunt_V") []
  gf G_include_V = mkApp (mkCId "_include_V") []
  gf G_license_V = mkApp (mkCId "_license_V") []
  gf G_mean_V = mkApp (mkCId "_mean_V") []
  gf G_met_common_requirement_for_add_V = mkApp (mkCId "_met_common_requirement_for_add_V") []
  gf G_mountaineer_V = mkApp (mkCId "_mountaineer_V") []
  gf G_occur_V = mkApp (mkCId "_occur_V") []
  gf G_organise_V = mkApp (mkCId "_organise_V") []
  gf G_parachute_V = mkApp (mkCId "_parachute_V") []
  gf G_pay_V = mkApp (mkCId "_pay_V") []
  gf G_policyholder_V = mkApp (mkCId "_policyholder_V") []
  gf G_pothole_V = mkApp (mkCId "_pothole_V") []
  gf G_race_V = mkApp (mkCId "_race_V") []
  gf G_recognise_V = mkApp (mkCId "_recognise_V") []
  gf G_register_V = mkApp (mkCId "_register_V") []
  gf G_riot_V = mkApp (mkCId "_riot_V") []
  gf G_sail_V = mkApp (mkCId "_sail_V") []
  gf G_skydive_V = mkApp (mkCId "_skydive_V") []
  gf G_start_V = mkApp (mkCId "_start_V") []
  gf G_stepupsumassure_V = mkApp (mkCId "_stepupsumassure_V") []
  gf G_subscribe_V = mkApp (mkCId "_subscribe_V") []
  gf G_suffer_V = mkApp (mkCId "_suffer_V") []
  gf G_sumassure_V = mkApp (mkCId "_sumassure_V") []
  gf G_supervise_V = mkApp (mkCId "_supervise_V") []
  gf G_train_V = mkApp (mkCId "_train_V") []
  gf G_windsurf_V = mkApp (mkCId "_windsurf_V") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "_H7N7_V" -> G_H7N7_V 
      Just (i,[]) | i == mkCId "_adjust_V" -> G_adjust_V 
      Just (i,[]) | i == mkCId "_apply_V" -> G_apply_V 
      Just (i,[]) | i == mkCId "_asssure_V" -> G_asssure_V 
      Just (i,[]) | i == mkCId "_assure_V" -> G_assure_V 
      Just (i,[]) | i == mkCId "_benefit_V" -> G_benefit_V 
      Just (i,[]) | i == mkCId "_canoe_V" -> G_canoe_V 
      Just (i,[]) | i == mkCId "_cave_V" -> G_cave_V 
      Just (i,[]) | i == mkCId "_claim_V" -> G_claim_V 
      Just (i,[]) | i == mkCId "_cover_V" -> G_cover_V 
      Just (i,[]) | i == mkCId "_establish_V" -> G_establish_V 
      Just (i,[]) | i == mkCId "_exception_V" -> G_exception_V 
      Just (i,[]) | i == mkCId "_give_V" -> G_give_V 
      Just (i,[]) | i == mkCId "_glide_V" -> G_glide_V 
      Just (i,[]) | i == mkCId "_govern_V" -> G_govern_V 
      Just (i,[]) | i == mkCId "_hand_V" -> G_hand_V 
      Just (i,[]) | i == mkCId "_hernia_V" -> G_hernia_V 
      Just (i,[]) | i == mkCId "_hunt_V" -> G_hunt_V 
      Just (i,[]) | i == mkCId "_include_V" -> G_include_V 
      Just (i,[]) | i == mkCId "_license_V" -> G_license_V 
      Just (i,[]) | i == mkCId "_mean_V" -> G_mean_V 
      Just (i,[]) | i == mkCId "_met_common_requirement_for_add_V" -> G_met_common_requirement_for_add_V 
      Just (i,[]) | i == mkCId "_mountaineer_V" -> G_mountaineer_V 
      Just (i,[]) | i == mkCId "_occur_V" -> G_occur_V 
      Just (i,[]) | i == mkCId "_organise_V" -> G_organise_V 
      Just (i,[]) | i == mkCId "_parachute_V" -> G_parachute_V 
      Just (i,[]) | i == mkCId "_pay_V" -> G_pay_V 
      Just (i,[]) | i == mkCId "_policyholder_V" -> G_policyholder_V 
      Just (i,[]) | i == mkCId "_pothole_V" -> G_pothole_V 
      Just (i,[]) | i == mkCId "_race_V" -> G_race_V 
      Just (i,[]) | i == mkCId "_recognise_V" -> G_recognise_V 
      Just (i,[]) | i == mkCId "_register_V" -> G_register_V 
      Just (i,[]) | i == mkCId "_riot_V" -> G_riot_V 
      Just (i,[]) | i == mkCId "_sail_V" -> G_sail_V 
      Just (i,[]) | i == mkCId "_skydive_V" -> G_skydive_V 
      Just (i,[]) | i == mkCId "_start_V" -> G_start_V 
      Just (i,[]) | i == mkCId "_stepupsumassure_V" -> G_stepupsumassure_V 
      Just (i,[]) | i == mkCId "_subscribe_V" -> G_subscribe_V 
      Just (i,[]) | i == mkCId "_suffer_V" -> G_suffer_V 
      Just (i,[]) | i == mkCId "_sumassure_V" -> G_sumassure_V 
      Just (i,[]) | i == mkCId "_supervise_V" -> G_supervise_V 
      Just (i,[]) | i == mkCId "_train_V" -> G_train_V 
      Just (i,[]) | i == mkCId "_windsurf_V" -> G_windsurf_V 


      _ -> error ("no V " ++ show t)

instance Gf GV2 where
  gf (LexV2 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexV2 (showCId i)
      _ -> error ("no V2 " ++ show t)

instance Gf GVP where
  gf (GAdvVP x1 x2) = mkApp (mkCId "AdvVP") [gf x1, gf x2]
  gf (GComplV2 x1 x2) = mkApp (mkCId "ComplV2") [gf x1, gf x2]
  gf (GComplV2S x1 x2 x3) = mkApp (mkCId "ComplV2S") [gf x1, gf x2, gf x3]
  gf (GComplVAS x1 x2 x3) = mkApp (mkCId "ComplVAS") [gf x1, gf x2, gf x3]
  gf (GComplVSif x1 x2) = mkApp (mkCId "ComplVSif") [gf x1, gf x2]
  gf (GComplVSthat x1 x2) = mkApp (mkCId "ComplVSthat") [gf x1, gf x2]
  gf (GUseComp x1) = mkApp (mkCId "UseComp") [gf x1]
  gf (LexVP x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvVP" -> GAdvVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplV2" -> GComplV2 (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "ComplV2S" -> GComplV2S (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "ComplVAS" -> GComplVAS (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "ComplVSif" -> GComplVSif (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplVSthat" -> GComplVSthat (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "UseComp" -> GUseComp (fg x1)

      Just (i,[]) -> LexVP (showCId i)
      _ -> error ("no VP " ++ show t)

instance Gf GVPI where
  gf (GMkVPI x1) = mkApp (mkCId "MkVPI") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "MkVPI" -> GMkVPI (fg x1)


      _ -> error ("no VPI " ++ show t)

instance Gf GVPS where
  gf (GComparison_Card_Years x1) = mkApp (mkCId "Comparison_Card_Years") [gf x1]
  gf (GConjPrePostVPS x1 x2 x3 x4) = mkApp (mkCId "ConjPrePostVPS") [gf x1, gf x2, gf x3, gf x4]
  gf (GConjVPS x1 x2) = mkApp (mkCId "ConjVPS") [gf x1, gf x2]
  gf (GGreaterThan x1) = mkApp (mkCId "GreaterThan") [gf x1]
  gf (GLessThan x1) = mkApp (mkCId "LessThan") [gf x1]
  gf (GMayHave x1) = mkApp (mkCId "MayHave") [gf x1]
  gf (GMkVPS x1 x2 x3) = mkApp (mkCId "MkVPS") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "Comparison_Card_Years" -> GComparison_Card_Years (fg x1)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "ConjPrePostVPS" -> GConjPrePostVPS (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "ConjVPS" -> GConjVPS (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "GreaterThan" -> GGreaterThan (fg x1)
      Just (i,[x1]) | i == mkCId "LessThan" -> GLessThan (fg x1)
      Just (i,[x1]) | i == mkCId "MayHave" -> GMayHave (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "MkVPS" -> GMkVPS (fg x1) (fg x2) (fg x3)


      _ -> error ("no VPS " ++ show t)

instance Gf GVS where
  gf (LexVS x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexVS (showCId i)
      _ -> error ("no VS " ++ show t)

instance Gf GVV where
  gf (LexVV x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexVV (showCId i)
      _ -> error ("no VV " ++ show t)

instance Gf GWho where
  gf (GAPWho x1) = mkApp (mkCId "APWho") [gf x1]
  gf (GAdvWho x1) = mkApp (mkCId "AdvWho") [gf x1]
  gf (GConjPrePostWho x1 x2 x3 x4) = mkApp (mkCId "ConjPrePostWho") [gf x1, gf x2, gf x3, gf x4]
  gf (GConjPreWho x1 x2 x3) = mkApp (mkCId "ConjPreWho") [gf x1, gf x2, gf x3]
  gf (GConjWho x1 x2) = mkApp (mkCId "ConjWho") [gf x1, gf x2]
  gf (GWHO x1 x2 x3) = mkApp (mkCId "WHO") [gf x1, gf x2, gf x3]
  gf (GrecoverUnparsedWho x1) = mkApp (mkCId "recoverUnparsedWho") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "APWho" -> GAPWho (fg x1)
      Just (i,[x1]) | i == mkCId "AdvWho" -> GAdvWho (fg x1)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "ConjPrePostWho" -> GConjPrePostWho (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "ConjPreWho" -> GConjPreWho (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "ConjWho" -> GConjWho (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "WHO" -> GWHO (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "recoverUnparsedWho" -> GrecoverUnparsedWho (fg x1)


      _ -> error ("no Who " ++ show t)

instance Gf GYear where
  gf (GMkYear x1 x2 x3 x4) = mkApp (mkCId "MkYear") [gf x1, gf x2, gf x3, gf x4]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3,x4]) | i == mkCId "MkYear" -> GMkYear (fg x1) (fg x2) (fg x3) (fg x4)


      _ -> error ("no Year " ++ show t)

instance Gf GYearComponent where
  gf (LexYearComponent x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexYearComponent (showCId i)
      _ -> error ("no YearComponent " ++ show t)



instance Gf GAnt where
  gf _ = undefined
  fg _ = undefined





instance Gf GTense where
  gf _ = undefined
  fg _ = undefined




instance Compos Tree where
  compos r a f t = case t of
    GComplA2 x1 x2 -> r GComplA2 `a` f x1 `a` f x2
    GConjAP x1 x2 -> r GConjAP `a` f x1 `a` f x2
    GInt_or_older x1 -> r GInt_or_older `a` f x1
    GPositA x1 -> r GPositA `a` f x1
    Gcaused_by x1 -> r Gcaused_by `a` f x1
    Gensuing x1 -> r Gensuing `a` f x1
    GACTION x1 -> r GACTION `a` f x1
    GrecoverUnparsedAction x1 -> r GrecoverUnparsedAction `a` f x1
    GAdnCAdv x1 -> r GAdnCAdv `a` f x1
    GAdAdv x1 x2 -> r GAdAdv `a` f x1 `a` f x2
    GByVP x1 -> r GByVP `a` f x1
    GConjAdv x1 x2 -> r GConjAdv `a` f x1 `a` f x2
    GPrepNP x1 x2 -> r GPrepNP `a` f x1 `a` f x2
    GSubjS x1 x2 -> r GSubjS `a` f x1 `a` f x2
    GWhileDoing x1 -> r GWhileDoing `a` f x1
    GrecoverUnparsedAdv x1 -> r GrecoverUnparsedAdv `a` f x1
    GAdjCN x1 x2 -> r GAdjCN `a` f x1 `a` f x2
    GCNwhereS x1 x2 x3 -> r GCNwhereS `a` f x1 `a` f x2 `a` f x3
    GUseN x1 -> r GUseN `a` f x1
    GAdNum x1 x2 -> r GAdNum `a` f x1 `a` f x2
    GNumDigits x1 -> r GNumDigits `a` f x1
    GCompAP x1 -> r GCompAP `a` f x1
    GCompAdv x1 -> r GCompAdv `a` f x1
    GCompNP x1 -> r GCompNP `a` f x1
    GConjCond x1 x2 -> r GConjCond `a` f x1 `a` f x2
    GConjPreCond x1 x2 x3 -> r GConjPreCond `a` f x1 `a` f x2 `a` f x3
    GConjPrePostCond x1 x2 x3 x4 -> r GConjPrePostCond `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GRPConstraint x1 x2 x3 -> r GRPConstraint `a` f x1 `a` f x2 `a` f x3
    GWHEN x1 x2 x3 x4 -> r GWHEN `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GrecoverUnparsedCond x1 -> r GrecoverUnparsedCond `a` f x1
    GConjConstraint x1 x2 -> r GConjConstraint `a` f x1 `a` f x2
    GConjPreConstraint x1 x2 x3 -> r GConjPreConstraint `a` f x1 `a` f x2 `a` f x3
    GConjPrePostConstraint x1 x2 x3 x4 -> r GConjPrePostConstraint `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GRPleafAP x1 -> r GRPleafAP `a` f x1
    GRPleafAdv x1 -> r GRPleafAdv `a` f x1
    GRPleafNP x1 -> r GRPleafNP `a` f x1
    GRPleafS x1 x2 -> r GRPleafS `a` f x1 `a` f x2
    GRPleafVP x1 -> r GRPleafVP `a` f x1
    GrecoverRPis x1 x2 -> r GrecoverRPis `a` f x1 `a` f x2
    GrecoverRPmath x1 x2 x3 -> r GrecoverRPmath `a` f x1 `a` f x2 `a` f x3
    GrecoverUnparsedConstraint x1 -> r GrecoverUnparsedConstraint `a` f x1
    GMkDate x1 x2 x3 -> r GMkDate `a` f x1 `a` f x2 `a` f x3
    GIDig x1 -> r GIDig `a` f x1
    GIIDig x1 x2 -> r GIIDig `a` f x1 `a` f x2
    GCompoundN x1 x2 -> r GCompoundN `a` f x1 `a` f x2
    GConjNP x1 x2 -> r GConjNP `a` f x1 `a` f x2
    GDetCN x1 x2 -> r GDetCN `a` f x1 `a` f x2
    GGerundNP x1 -> r GGerundNP `a` f x1
    GMassNP x1 -> r GMassNP `a` f x1
    GUsePN x1 -> r GUsePN `a` f x1
    Gresult_from x1 -> r Gresult_from `a` f x1
    Gnum x1 -> r Gnum `a` f x1
    GAP_PrePost x1 -> r GAP_PrePost `a` f x1
    GAdv_PrePost x1 -> r GAdv_PrePost `a` f x1
    GNP_PrePost x1 -> r GNP_PrePost `a` f x1
    GNP_caused_NP_to_VP_Prep_PrePost x1 x2 x3 x4 -> r GNP_caused_NP_to_VP_Prep_PrePost `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GNP_caused_by_PrePost x1 -> r GNP_caused_by_PrePost `a` f x1
    GS_PrePost x1 x2 -> r GS_PrePost `a` f x1 `a` f x2
    GV2_PrePost x1 -> r GV2_PrePost `a` f x1
    GrecoverUnparsedPrePost x1 -> r GrecoverUnparsedPrePost `a` f x1
    GConjPrep x1 x2 -> r GConjPrep `a` f x1 `a` f x2
    GConjPrePostQS x1 x2 x3 x4 -> r GConjPrePostQS `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GConjQS x1 x2 -> r GConjQS `a` f x1 `a` f x2
    GConjPrePostS x1 x2 x3 x4 -> r GConjPrePostS `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GConjS x1 x2 -> r GConjS `a` f x1 `a` f x2
    GPredVPS x1 x2 -> r GPredVPS `a` f x1 `a` f x2
    GReferenceNP x1 -> r GReferenceNP `a` f x1
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
    Gpot3float x1 -> r Gpot3float `a` f x1
    Gpot3plus x1 x2 -> r Gpot3plus `a` f x1 `a` f x2
    Gpot3as4 x1 -> r Gpot3as4 `a` f x1
    Gpot4 x1 -> r Gpot4 `a` f x1
    Gpot4float x1 -> r Gpot4float `a` f x1
    Gpot4plus x1 x2 -> r Gpot4plus `a` f x1 `a` f x2
    Gpot4as5 x1 -> r Gpot4as5 `a` f x1
    Gpot5 x1 -> r Gpot5 `a` f x1
    Gpot5float x1 -> r Gpot5float `a` f x1
    Gpot5plus x1 x2 -> r Gpot5plus `a` f x1 `a` f x2
    GAN x1 -> r GAN `a` f x1
    GEVERY x1 -> r GEVERY `a` f x1
    GPARTY x1 -> r GPARTY `a` f x1
    GSubjWho x1 x2 -> r GSubjWho `a` f x1 `a` f x2
    GTHE x1 -> r GTHE `a` f x1
    GrecoverUnparsedSubj x1 -> r GrecoverUnparsedSubj `a` f x1
    GConjTComparison x1 x2 -> r GConjTComparison `a` f x1 `a` f x2
    GTemporalConstraint x1 x2 x3 -> r GTemporalConstraint `a` f x1 `a` f x2 `a` f x3
    GTemporalConstraintNoDigits x1 x2 -> r GTemporalConstraintNoDigits `a` f x1 `a` f x2
    GRegulative x1 x2 x3 -> r GRegulative `a` f x1 `a` f x2 `a` f x3
    GadvUPON x1 -> r GadvUPON `a` f x1
    GqCOND x1 -> r GqCOND `a` f x1
    GqCONSTR x1 -> r GqCONSTR `a` f x1
    GqPREPOST x1 -> r GqPREPOST `a` f x1
    GqUPON x1 x2 -> r GqUPON `a` f x1 `a` f x2
    GqWHO x1 x2 -> r GqWHO `a` f x1 `a` f x2
    GsCOND x1 -> r GsCOND `a` f x1
    GsUPON x1 x2 -> r GsUPON `a` f x1 `a` f x2
    GsWHO x1 x2 -> r GsWHO `a` f x1 `a` f x2
    GrecoverUnparsedTimeUnit x1 -> r GrecoverUnparsedTimeUnit `a` f x1
    GUPON x1 -> r GUPON `a` f x1
    GrecoverUnparsedUpon x1 -> r GrecoverUnparsedUpon `a` f x1
    GAdvVP x1 x2 -> r GAdvVP `a` f x1 `a` f x2
    GComplV2 x1 x2 -> r GComplV2 `a` f x1 `a` f x2
    GComplV2S x1 x2 x3 -> r GComplV2S `a` f x1 `a` f x2 `a` f x3
    GComplVAS x1 x2 x3 -> r GComplVAS `a` f x1 `a` f x2 `a` f x3
    GComplVSif x1 x2 -> r GComplVSif `a` f x1 `a` f x2
    GComplVSthat x1 x2 -> r GComplVSthat `a` f x1 `a` f x2
    GUseComp x1 -> r GUseComp `a` f x1
    GMkVPI x1 -> r GMkVPI `a` f x1
    GComparison_Card_Years x1 -> r GComparison_Card_Years `a` f x1
    GConjPrePostVPS x1 x2 x3 x4 -> r GConjPrePostVPS `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GConjVPS x1 x2 -> r GConjVPS `a` f x1 `a` f x2
    GGreaterThan x1 -> r GGreaterThan `a` f x1
    GLessThan x1 -> r GLessThan `a` f x1
    GMayHave x1 -> r GMayHave `a` f x1
    GMkVPS x1 x2 x3 -> r GMkVPS `a` f x1 `a` f x2 `a` f x3
    GAPWho x1 -> r GAPWho `a` f x1
    GAdvWho x1 -> r GAdvWho `a` f x1
    GConjPrePostWho x1 x2 x3 x4 -> r GConjPrePostWho `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GConjPreWho x1 x2 x3 -> r GConjPreWho `a` f x1 `a` f x2 `a` f x3
    GConjWho x1 x2 -> r GConjWho `a` f x1 `a` f x2
    GWHO x1 x2 x3 -> r GWHO `a` f x1 `a` f x2 `a` f x3
    GrecoverUnparsedWho x1 -> r GrecoverUnparsedWho `a` f x1
    GMkYear x1 x2 x3 x4 -> r GMkYear `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GListAP x1 -> r GListAP `a` foldr (a . a (r (:)) . f) (r []) x1
    GListAdv x1 -> r GListAdv `a` foldr (a . a (r (:)) . f) (r []) x1
    GListCond x1 -> r GListCond `a` foldr (a . a (r (:)) . f) (r []) x1
    GListConstraint x1 -> r GListConstraint `a` foldr (a . a (r (:)) . f) (r []) x1
    GListNP x1 -> r GListNP `a` foldr (a . a (r (:)) . f) (r []) x1
    GListPrep x1 -> r GListPrep `a` foldr (a . a (r (:)) . f) (r []) x1
    GListQS x1 -> r GListQS `a` foldr (a . a (r (:)) . f) (r []) x1
    GListS x1 -> r GListS `a` foldr (a . a (r (:)) . f) (r []) x1
    GListTComparison x1 -> r GListTComparison `a` foldr (a . a (r (:)) . f) (r []) x1
    GListVPS x1 -> r GListVPS `a` foldr (a . a (r (:)) . f) (r []) x1
    GListWho x1 -> r GListWho `a` foldr (a . a (r (:)) . f) (r []) x1
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
