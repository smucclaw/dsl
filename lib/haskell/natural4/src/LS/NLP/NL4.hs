{-# LANGUAGE GADTs, UndecidableInstances #-}
module LS.NLP.NL4 where

import Control.Monad (MonadPlus (..), ap)
import Control.Monad.Identity (Identity (Identity, runIdentity))
import PGF
  ( Expr,
    mkApp,
    mkCId,
    mkFloat,
    mkInt,
    mkStr,
    showCId,
    showExpr,
    unApp,
    unFloat,
    unInt,
    unStr,
  )

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
type GNum = Tree GNum_
data GNum_
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
type GRP = Tree GRP_
data GRP_
type GRS = Tree GRS_
data GRS_
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

data Tree a where
  LexA :: String -> Tree GA_
  G_located_in_A2 :: Tree GA2_
  Gdue_to_A2 :: Tree GA2_
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
  G_directly_Adv :: Tree GAdv_
  G_first_Adv :: Tree GAdv_
  G_fully_Adv :: Tree GAdv_
  G_hence_Adv :: Tree GAdv_
  G_here_Adv :: Tree GAdv_
  G_indirectly_Adv :: Tree GAdv_
  G_least_Adv :: Tree GAdv_
  G_long_Adv :: Tree GAdv_
  G_more_Adv :: Tree GAdv_
  G_no_Adv :: Tree GAdv_
  G_on_its_way_Adv :: Tree GAdv_
  G_only_Adv :: Tree GAdv_
  G_permanently_Adv :: Tree GAdv_
  G_pland_Adv :: Tree GAdv_
  G_so_Adv :: Tree GAdv_
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
  GComplN2 :: GN2 -> GNP -> Tree GCN_
  GRelCN :: GCN -> GRS -> Tree GCN_
  GUseN :: GN -> Tree GCN_
  G_CN_of_any_kind_CN :: GCN -> Tree GCN_
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
  LexConj :: String -> Tree GConj_
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
  G_travel_by_N2 :: Tree GN2_
  GConjNP :: GConj -> GListNP -> Tree GNP_
  GContents :: Tree GNP_
  GDetCN :: GDet -> GCN -> Tree GNP_
  GEVERY :: GCN -> Tree GNP_
  GGenModNP :: GNum -> GNP -> GCN -> Tree GNP_
  GGerundNP :: GVP -> Tree GNP_
  GLoss_or_Damage :: Tree GNP_
  GMassNP :: GCN -> Tree GNP_
  GNDB_Qualification :: Tree GNP_
  GSubjWho :: GNP -> GWho -> Tree GNP_
  GUsePN :: GPN -> Tree GNP_
  GYou :: Tree GNP_
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
  GrecoverUnparsedNP :: GString -> Tree GNP_
  Gresult_from :: GNP -> Tree GNP_
  Grodents :: Tree GNP_
  Gsigned :: Tree GNP_
  Gstay_during_policy_period :: Tree GNP_
  Gstay_overnight :: Tree GNP_
  Gswimming_pool :: Tree GNP_
  Gvermin :: Tree GNP_
  Gwater :: Tree GNP_
  GNumPl :: Tree GNum_
  GNumSg :: Tree GNum_
  Gnum :: GSub1000000 -> Tree GNumeral_
  LexPN :: String -> Tree GPN_
  GNEG :: Tree GPol_
  GPOS :: Tree GPol_
  GAP_PrePost :: GAP -> Tree GPrePost_
  GAdv_PrePost :: GAdv -> Tree GPrePost_
  GNP_PrePost :: GNP -> Tree GPrePost_
  GNP_caused_NP_to_VP_Prep_PrePost :: GNP -> GNP -> GVP -> GPrep -> Tree GPrePost_
  GNP_caused_by_PrePost :: GNP -> Tree GPrePost_
  GSSlash_PrePost :: GNP -> GTemp -> GPol -> GV2 -> Tree GPrePost_
  GS_PrePost :: GNP -> GVPS -> Tree GPrePost_
  GV2_PrePost :: GTemp -> GPol -> GV2 -> Tree GPrePost_
  GrecoverUnparsedPrePost :: GString -> Tree GPrePost_
  GConjPrep :: GConj -> GListPrep -> Tree GPrep_
  LexPrep :: String -> Tree GPrep_
  GConjPrePostQS :: GString -> GString -> GConj -> GListQS -> Tree GQS_
  GConjQS :: GConj -> GListQS -> Tree GQS_
  GIdRP :: Tree GRP_
  GRelVPS :: GRP -> GVPS -> Tree GRS_
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
  LexSubj :: String -> Tree GSubj_
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
  GRegulative :: GNP -> GDeontic -> GAction -> Tree GText_
  GadvUPON :: GUpon -> Tree GText_
  GqCOND :: GCond -> Tree GText_
  GqCONSTR :: GConstraint -> Tree GText_
  GqPREPOST :: GPrePost -> Tree GText_
  GqUPON :: GNP -> GUpon -> Tree GText_
  GqWHO :: GNP -> GWho -> Tree GText_
  GsCOND :: GCond -> Tree GText_
  GsUPON :: GNP -> GUpon -> Tree GText_
  GsWHO :: GNP -> GWho -> Tree GText_
  GDay_Unit :: Tree GTimeUnit_
  GMonth_Unit :: Tree GTimeUnit_
  GYear_Unit :: Tree GTimeUnit_
  GrecoverUnparsedTimeUnit :: GString -> Tree GTimeUnit_
  GUPON :: GVP -> Tree GUpon_
  GUPONnp :: GNP -> GVP -> Tree GUpon_
  GrecoverUnparsedUpon :: GString -> Tree GUpon_
  LexV :: String -> Tree GV_
  LexV2 :: String -> Tree GV2_
  GAdvVP :: GVP -> GAdv -> Tree GVP_
  GComplV2 :: GV2 -> GNP -> Tree GVP_
  GComplV2S :: GV2 -> GNP -> GS -> Tree GVP_
  GComplVAS :: GV2 -> GAP -> GS -> Tree GVP_
  GComplVSif :: GVS -> GS -> Tree GVP_
  GComplVSthat :: GVS -> GS -> Tree GVP_
  GUseComp :: GComp -> Tree GVP_
  GUseV :: GV -> Tree GVP_
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
    (LexA x,LexA y) -> x == y
    (G_located_in_A2,G_located_in_A2) -> and [ ]
    (Gdue_to_A2,Gdue_to_A2) -> and [ ]
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
    (G_directly_Adv,G_directly_Adv) -> and [ ]
    (G_first_Adv,G_first_Adv) -> and [ ]
    (G_fully_Adv,G_fully_Adv) -> and [ ]
    (G_hence_Adv,G_hence_Adv) -> and [ ]
    (G_here_Adv,G_here_Adv) -> and [ ]
    (G_indirectly_Adv,G_indirectly_Adv) -> and [ ]
    (G_least_Adv,G_least_Adv) -> and [ ]
    (G_long_Adv,G_long_Adv) -> and [ ]
    (G_more_Adv,G_more_Adv) -> and [ ]
    (G_no_Adv,G_no_Adv) -> and [ ]
    (G_on_its_way_Adv,G_on_its_way_Adv) -> and [ ]
    (G_only_Adv,G_only_Adv) -> and [ ]
    (G_permanently_Adv,G_permanently_Adv) -> and [ ]
    (G_pland_Adv,G_pland_Adv) -> and [ ]
    (G_so_Adv,G_so_Adv) -> and [ ]
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
    (GComplN2 x1 x2,GComplN2 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRelCN x1 x2,GRelCN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GUseN x1,GUseN y1) -> and [ x1 == y1 ]
    (G_CN_of_any_kind_CN x1,G_CN_of_any_kind_CN y1) -> and [ x1 == y1 ]
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
    (LexConj x,LexConj y) -> x == y
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
    (G_travel_by_N2,G_travel_by_N2) -> and [ ]
    (GConjNP x1 x2,GConjNP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GContents,GContents) -> and [ ]
    (GDetCN x1 x2,GDetCN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GEVERY x1,GEVERY y1) -> and [ x1 == y1 ]
    (GGenModNP x1 x2 x3,GGenModNP y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GGerundNP x1,GGerundNP y1) -> and [ x1 == y1 ]
    (GLoss_or_Damage,GLoss_or_Damage) -> and [ ]
    (GMassNP x1,GMassNP y1) -> and [ x1 == y1 ]
    (GNDB_Qualification,GNDB_Qualification) -> and [ ]
    (GSubjWho x1 x2,GSubjWho y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GUsePN x1,GUsePN y1) -> and [ x1 == y1 ]
    (GYou,GYou) -> and [ ]
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
    (GrecoverUnparsedNP x1,GrecoverUnparsedNP y1) -> and [ x1 == y1 ]
    (Gresult_from x1,Gresult_from y1) -> and [ x1 == y1 ]
    (Grodents,Grodents) -> and [ ]
    (Gsigned,Gsigned) -> and [ ]
    (Gstay_during_policy_period,Gstay_during_policy_period) -> and [ ]
    (Gstay_overnight,Gstay_overnight) -> and [ ]
    (Gswimming_pool,Gswimming_pool) -> and [ ]
    (Gvermin,Gvermin) -> and [ ]
    (Gwater,Gwater) -> and [ ]
    (GNumPl,GNumPl) -> and [ ]
    (GNumSg,GNumSg) -> and [ ]
    (Gnum x1,Gnum y1) -> and [ x1 == y1 ]
    (LexPN x,LexPN y) -> x == y
    (GNEG,GNEG) -> and [ ]
    (GPOS,GPOS) -> and [ ]
    (GAP_PrePost x1,GAP_PrePost y1) -> and [ x1 == y1 ]
    (GAdv_PrePost x1,GAdv_PrePost y1) -> and [ x1 == y1 ]
    (GNP_PrePost x1,GNP_PrePost y1) -> and [ x1 == y1 ]
    (GNP_caused_NP_to_VP_Prep_PrePost x1 x2 x3 x4,GNP_caused_NP_to_VP_Prep_PrePost y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GNP_caused_by_PrePost x1,GNP_caused_by_PrePost y1) -> and [ x1 == y1 ]
    (GSSlash_PrePost x1 x2 x3 x4,GSSlash_PrePost y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GS_PrePost x1 x2,GS_PrePost y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GV2_PrePost x1 x2 x3,GV2_PrePost y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GrecoverUnparsedPrePost x1,GrecoverUnparsedPrePost y1) -> and [ x1 == y1 ]
    (GConjPrep x1 x2,GConjPrep y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexPrep x,LexPrep y) -> x == y
    (GConjPrePostQS x1 x2 x3 x4,GConjPrePostQS y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GConjQS x1 x2,GConjQS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIdRP,GIdRP) -> and [ ]
    (GRelVPS x1 x2,GRelVPS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
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
    (LexSubj x,LexSubj y) -> x == y
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
    (GUPONnp x1 x2,GUPONnp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GrecoverUnparsedUpon x1,GrecoverUnparsedUpon y1) -> and [ x1 == y1 ]
    (LexV x,LexV y) -> x == y
    (LexV2 x,LexV2 y) -> x == y
    (GAdvVP x1 x2,GAdvVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GComplV2 x1 x2,GComplV2 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GComplV2S x1 x2 x3,GComplV2S y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GComplVAS x1 x2 x3,GComplVAS y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GComplVSif x1 x2,GComplVSif y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GComplVSthat x1 x2,GComplVSthat y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GUseComp x1,GUseComp y1) -> and [ x1 == y1 ]
    (GUseV x1,GUseV y1) -> and [ x1 == y1 ]
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
  gf (LexA x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexA (showCId i)
      _ -> error ("no A " ++ show t)

instance Gf GA2 where
  gf G_located_in_A2 = mkApp (mkCId "_located_in_A2") []
  gf Gdue_to_A2 = mkApp (mkCId "due_to_A2") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "_located_in_A2" -> G_located_in_A2 
      Just (i,[]) | i == mkCId "due_to_A2" -> Gdue_to_A2 


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
  gf G_directly_Adv = mkApp (mkCId "_directly_Adv") []
  gf G_first_Adv = mkApp (mkCId "_first_Adv") []
  gf G_fully_Adv = mkApp (mkCId "_fully_Adv") []
  gf G_hence_Adv = mkApp (mkCId "_hence_Adv") []
  gf G_here_Adv = mkApp (mkCId "_here_Adv") []
  gf G_indirectly_Adv = mkApp (mkCId "_indirectly_Adv") []
  gf G_least_Adv = mkApp (mkCId "_least_Adv") []
  gf G_long_Adv = mkApp (mkCId "_long_Adv") []
  gf G_more_Adv = mkApp (mkCId "_more_Adv") []
  gf G_no_Adv = mkApp (mkCId "_no_Adv") []
  gf G_on_its_way_Adv = mkApp (mkCId "_on_its_way_Adv") []
  gf G_only_Adv = mkApp (mkCId "_only_Adv") []
  gf G_permanently_Adv = mkApp (mkCId "_permanently_Adv") []
  gf G_pland_Adv = mkApp (mkCId "_pland_Adv") []
  gf G_so_Adv = mkApp (mkCId "_so_Adv") []
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
      Just (i,[]) | i == mkCId "_directly_Adv" -> G_directly_Adv 
      Just (i,[]) | i == mkCId "_first_Adv" -> G_first_Adv 
      Just (i,[]) | i == mkCId "_fully_Adv" -> G_fully_Adv 
      Just (i,[]) | i == mkCId "_hence_Adv" -> G_hence_Adv 
      Just (i,[]) | i == mkCId "_here_Adv" -> G_here_Adv 
      Just (i,[]) | i == mkCId "_indirectly_Adv" -> G_indirectly_Adv 
      Just (i,[]) | i == mkCId "_least_Adv" -> G_least_Adv 
      Just (i,[]) | i == mkCId "_long_Adv" -> G_long_Adv 
      Just (i,[]) | i == mkCId "_more_Adv" -> G_more_Adv 
      Just (i,[]) | i == mkCId "_no_Adv" -> G_no_Adv 
      Just (i,[]) | i == mkCId "_on_its_way_Adv" -> G_on_its_way_Adv 
      Just (i,[]) | i == mkCId "_only_Adv" -> G_only_Adv 
      Just (i,[]) | i == mkCId "_permanently_Adv" -> G_permanently_Adv 
      Just (i,[]) | i == mkCId "_pland_Adv" -> G_pland_Adv 
      Just (i,[]) | i == mkCId "_so_Adv" -> G_so_Adv 
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
  gf (GComplN2 x1 x2) = mkApp (mkCId "ComplN2") [gf x1, gf x2]
  gf (GRelCN x1 x2) = mkApp (mkCId "RelCN") [gf x1, gf x2]
  gf (GUseN x1) = mkApp (mkCId "UseN") [gf x1]
  gf (G_CN_of_any_kind_CN x1) = mkApp (mkCId "_CN_of_any_kind_CN") [gf x1]
  gf (LexCN x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdjCN" -> GAdjCN (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "CNwhereS" -> GCNwhereS (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "ComplN2" -> GComplN2 (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RelCN" -> GRelCN (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "UseN" -> GUseN (fg x1)
      Just (i,[x1]) | i == mkCId "_CN_of_any_kind_CN" -> G_CN_of_any_kind_CN (fg x1)

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
  gf (LexConj x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexConj (showCId i)
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
  gf G_travel_by_N2 = mkApp (mkCId "_travel_by_N2") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "_premise_where_N2" -> G_premise_where_N2 
      Just (i,[]) | i == mkCId "_travel_by_N2" -> G_travel_by_N2 


      _ -> error ("no N2 " ++ show t)

instance Gf GNP where
  gf (GConjNP x1 x2) = mkApp (mkCId "ConjNP") [gf x1, gf x2]
  gf GContents = mkApp (mkCId "Contents") []
  gf (GDetCN x1 x2) = mkApp (mkCId "DetCN") [gf x1, gf x2]
  gf (GEVERY x1) = mkApp (mkCId "EVERY") [gf x1]
  gf (GGenModNP x1 x2 x3) = mkApp (mkCId "GenModNP") [gf x1, gf x2, gf x3]
  gf (GGerundNP x1) = mkApp (mkCId "GerundNP") [gf x1]
  gf GLoss_or_Damage = mkApp (mkCId "Loss_or_Damage") []
  gf (GMassNP x1) = mkApp (mkCId "MassNP") [gf x1]
  gf GNDB_Qualification = mkApp (mkCId "NDB_Qualification") []
  gf (GSubjWho x1 x2) = mkApp (mkCId "SubjWho") [gf x1, gf x2]
  gf (GUsePN x1) = mkApp (mkCId "UsePN") [gf x1]
  gf GYou = mkApp (mkCId "You") []
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
  gf (GrecoverUnparsedNP x1) = mkApp (mkCId "recoverUnparsedNP") [gf x1]
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
      Just (i,[x1]) | i == mkCId "EVERY" -> GEVERY (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "GenModNP" -> GGenModNP (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "GerundNP" -> GGerundNP (fg x1)
      Just (i,[]) | i == mkCId "Loss_or_Damage" -> GLoss_or_Damage 
      Just (i,[x1]) | i == mkCId "MassNP" -> GMassNP (fg x1)
      Just (i,[]) | i == mkCId "NDB_Qualification" -> GNDB_Qualification 
      Just (i,[x1,x2]) | i == mkCId "SubjWho" -> GSubjWho (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "UsePN" -> GUsePN (fg x1)
      Just (i,[]) | i == mkCId "You" -> GYou 
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
      Just (i,[x1]) | i == mkCId "recoverUnparsedNP" -> GrecoverUnparsedNP (fg x1)
      Just (i,[x1]) | i == mkCId "result_from" -> Gresult_from (fg x1)
      Just (i,[]) | i == mkCId "rodents" -> Grodents 
      Just (i,[]) | i == mkCId "signed" -> Gsigned 
      Just (i,[]) | i == mkCId "stay_during_policy_period" -> Gstay_during_policy_period 
      Just (i,[]) | i == mkCId "stay_overnight" -> Gstay_overnight 
      Just (i,[]) | i == mkCId "swimming_pool" -> Gswimming_pool 
      Just (i,[]) | i == mkCId "vermin" -> Gvermin 
      Just (i,[]) | i == mkCId "water" -> Gwater 


      _ -> error ("no NP " ++ show t)

instance Gf GNum where
  gf GNumPl = mkApp (mkCId "NumPl") []
  gf GNumSg = mkApp (mkCId "NumSg") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "NumPl" -> GNumPl 
      Just (i,[]) | i == mkCId "NumSg" -> GNumSg 


      _ -> error ("no Num " ++ show t)

instance Gf GNumeral where
  gf (Gnum x1) = mkApp (mkCId "num") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "num" -> Gnum (fg x1)


      _ -> error ("no Numeral " ++ show t)

instance Gf GPN where
  gf (LexPN x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexPN (showCId i)
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
  gf (GSSlash_PrePost x1 x2 x3 x4) = mkApp (mkCId "SSlash_PrePost") [gf x1, gf x2, gf x3, gf x4]
  gf (GS_PrePost x1 x2) = mkApp (mkCId "S_PrePost") [gf x1, gf x2]
  gf (GV2_PrePost x1 x2 x3) = mkApp (mkCId "V2_PrePost") [gf x1, gf x2, gf x3]
  gf (GrecoverUnparsedPrePost x1) = mkApp (mkCId "recoverUnparsedPrePost") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "AP_PrePost" -> GAP_PrePost (fg x1)
      Just (i,[x1]) | i == mkCId "Adv_PrePost" -> GAdv_PrePost (fg x1)
      Just (i,[x1]) | i == mkCId "NP_PrePost" -> GNP_PrePost (fg x1)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "NP_caused_NP_to_VP_Prep_PrePost" -> GNP_caused_NP_to_VP_Prep_PrePost (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1]) | i == mkCId "NP_caused_by_PrePost" -> GNP_caused_by_PrePost (fg x1)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "SSlash_PrePost" -> GSSlash_PrePost (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "S_PrePost" -> GS_PrePost (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "V2_PrePost" -> GV2_PrePost (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "recoverUnparsedPrePost" -> GrecoverUnparsedPrePost (fg x1)


      _ -> error ("no PrePost " ++ show t)

instance Gf GPrep where
  gf (GConjPrep x1 x2) = mkApp (mkCId "ConjPrep") [gf x1, gf x2]
  gf (LexPrep x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjPrep" -> GConjPrep (fg x1) (fg x2)

      Just (i,[]) -> LexPrep (showCId i)
      _ -> error ("no Prep " ++ show t)

instance Gf GQS where
  gf (GConjPrePostQS x1 x2 x3 x4) = mkApp (mkCId "ConjPrePostQS") [gf x1, gf x2, gf x3, gf x4]
  gf (GConjQS x1 x2) = mkApp (mkCId "ConjQS") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3,x4]) | i == mkCId "ConjPrePostQS" -> GConjPrePostQS (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "ConjQS" -> GConjQS (fg x1) (fg x2)


      _ -> error ("no QS " ++ show t)

instance Gf GRP where
  gf GIdRP = mkApp (mkCId "IdRP") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "IdRP" -> GIdRP 


      _ -> error ("no RP " ++ show t)

instance Gf GRS where
  gf (GRelVPS x1 x2) = mkApp (mkCId "RelVPS") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "RelVPS" -> GRelVPS (fg x1) (fg x2)


      _ -> error ("no RS " ++ show t)

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
  gf (LexSubj x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexSubj (showCId i)
      _ -> error ("no Subj " ++ show t)

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
  gf (GUPONnp x1 x2) = mkApp (mkCId "UPONnp") [gf x1, gf x2]
  gf (GrecoverUnparsedUpon x1) = mkApp (mkCId "recoverUnparsedUpon") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "UPON" -> GUPON (fg x1)
      Just (i,[x1,x2]) | i == mkCId "UPONnp" -> GUPONnp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "recoverUnparsedUpon" -> GrecoverUnparsedUpon (fg x1)


      _ -> error ("no Upon " ++ show t)

instance Gf GV where
  gf (LexV x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexV (showCId i)
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
  gf (GUseV x1) = mkApp (mkCId "UseV") [gf x1]
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
      Just (i,[x1]) | i == mkCId "UseV" -> GUseV (fg x1)

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
    GComplN2 x1 x2 -> r GComplN2 `a` f x1 `a` f x2
    GRelCN x1 x2 -> r GRelCN `a` f x1 `a` f x2
    GUseN x1 -> r GUseN `a` f x1
    G_CN_of_any_kind_CN x1 -> r G_CN_of_any_kind_CN `a` f x1
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
    GEVERY x1 -> r GEVERY `a` f x1
    GGenModNP x1 x2 x3 -> r GGenModNP `a` f x1 `a` f x2 `a` f x3
    GGerundNP x1 -> r GGerundNP `a` f x1
    GMassNP x1 -> r GMassNP `a` f x1
    GSubjWho x1 x2 -> r GSubjWho `a` f x1 `a` f x2
    GUsePN x1 -> r GUsePN `a` f x1
    GrecoverUnparsedNP x1 -> r GrecoverUnparsedNP `a` f x1
    Gresult_from x1 -> r Gresult_from `a` f x1
    Gnum x1 -> r Gnum `a` f x1
    GAP_PrePost x1 -> r GAP_PrePost `a` f x1
    GAdv_PrePost x1 -> r GAdv_PrePost `a` f x1
    GNP_PrePost x1 -> r GNP_PrePost `a` f x1
    GNP_caused_NP_to_VP_Prep_PrePost x1 x2 x3 x4 -> r GNP_caused_NP_to_VP_Prep_PrePost `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GNP_caused_by_PrePost x1 -> r GNP_caused_by_PrePost `a` f x1
    GSSlash_PrePost x1 x2 x3 x4 -> r GSSlash_PrePost `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GS_PrePost x1 x2 -> r GS_PrePost `a` f x1 `a` f x2
    GV2_PrePost x1 x2 x3 -> r GV2_PrePost `a` f x1 `a` f x2 `a` f x3
    GrecoverUnparsedPrePost x1 -> r GrecoverUnparsedPrePost `a` f x1
    GConjPrep x1 x2 -> r GConjPrep `a` f x1 `a` f x2
    GConjPrePostQS x1 x2 x3 x4 -> r GConjPrePostQS `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GConjQS x1 x2 -> r GConjQS `a` f x1 `a` f x2
    GRelVPS x1 x2 -> r GRelVPS `a` f x1 `a` f x2
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
    GUPONnp x1 x2 -> r GUPONnp `a` f x1 `a` f x2
    GrecoverUnparsedUpon x1 -> r GrecoverUnparsedUpon `a` f x1
    GAdvVP x1 x2 -> r GAdvVP `a` f x1 `a` f x2
    GComplV2 x1 x2 -> r GComplV2 `a` f x1 `a` f x2
    GComplV2S x1 x2 x3 -> r GComplV2S `a` f x1 `a` f x2 `a` f x3
    GComplVAS x1 x2 x3 -> r GComplVAS `a` f x1 `a` f x2 `a` f x3
    GComplVSif x1 x2 -> r GComplVSif `a` f x1 `a` f x2
    GComplVSthat x1 x2 -> r GComplVSthat `a` f x1 `a` f x2
    GUseComp x1 -> r GUseComp `a` f x1
    GUseV x1 -> r GUseV `a` f x1
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
