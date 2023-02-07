{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-all #-}
module LS.NLP.NL4 where

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

type GAction = Tree GAction_
data GAction_
type GCN = Tree GCN_
data GCN_
type GConj = Tree GConj_
data GConj_
type GDeontic = Tree GDeontic_
data GDeontic_
type GDig = Tree GDig_
data GDig_
type GDigit = Tree GDigit_
data GDigit_
type GDigits = Tree GDigits_
data GDigits_
type GListWho = Tree GListWho_
data GListWho_
type GNumeral = Tree GNumeral_
data GNumeral_
type GPol = Tree GPol_
data GPol_
type GRule = Tree GRule_
data GRule_
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
type GTemp = Tree GTemp_
data GTemp_
type GVP = Tree GVP_
data GVP_
type GVPI = Tree GVPI_
data GVPI_
type GVPS = Tree GVPS_
data GVPS_
type GWho = Tree GWho_
data GWho_
type GA = Tree GA_
data GA_
type GA2 = Tree GA2_
data GA2_
type GAP = Tree GAP_
data GAP_
type GAnt = Tree GAnt_
data GAnt_
type GN = Tree GN_
data GN_
type GN2 = Tree GN2_
data GN2_
type GNP = Tree GNP_
data GNP_
type GTense = Tree GTense_
data GTense_
type GV = Tree GV_
data GV_
type GV2 = Tree GV2_
data GV2_
type GString = Tree GString_
data GString_
type GInt = Tree GInt_
data GInt_
type GFloat = Tree GFloat_
data GFloat_

data Tree :: * -> * where
  GACTION :: GVPI -> Tree GAction_
  LexCN :: String -> Tree GCN_
  GAND :: Tree GConj_
  GOR :: Tree GConj_
  GMAY :: Tree GDeontic_
  GMUST :: Tree GDeontic_
  GSHANT :: Tree GDeontic_
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
  GListWho :: [GWho] -> Tree GListWho_
  Gnum :: GSub1000000 -> Tree GNumeral_
  GNEG :: Tree GPol_
  GPOS :: Tree GPol_
  GRegulative :: GSubj -> GDeontic -> GAction -> Tree GRule_
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
  GEVERY :: GCN -> Tree GSubj_
  GPARTY :: GCN -> Tree GSubj_
  GSubjWho :: GSubj -> GWho -> Tree GSubj_
  GpresentIndicative :: Tree GTemp_
  LexVP :: String -> Tree GVP_
  GMkVPI :: GVP -> Tree GVPI_
  GMkVPS :: GTemp -> GPol -> GVP -> Tree GVPS_
  GConjWho :: GConj -> GListWho -> Tree GWho_
  GWHO :: GVPS -> Tree GWho_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (GACTION x1,GACTION y1) -> and [ x1 == y1 ]
    (LexCN x,LexCN y) -> x == y
    (GAND,GAND) -> and [ ]
    (GOR,GOR) -> and [ ]
    (GMAY,GMAY) -> and [ ]
    (GMUST,GMUST) -> and [ ]
    (GSHANT,GSHANT) -> and [ ]
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
    (GListWho x1,GListWho y1) -> and [x == y | (x,y) <- zip x1 y1]
    (Gnum x1,Gnum y1) -> and [ x1 == y1 ]
    (GNEG,GNEG) -> and [ ]
    (GPOS,GPOS) -> and [ ]
    (GRegulative x1 x2 x3,GRegulative y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
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
    (GEVERY x1,GEVERY y1) -> and [ x1 == y1 ]
    (GPARTY x1,GPARTY y1) -> and [ x1 == y1 ]
    (GSubjWho x1 x2,GSubjWho y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GpresentIndicative,GpresentIndicative) -> and [ ]
    (LexVP x,LexVP y) -> x == y
    (GMkVPI x1,GMkVPI y1) -> and [ x1 == y1 ]
    (GMkVPS x1 x2 x3,GMkVPS y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GConjWho x1 x2,GConjWho y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GWHO x1,GWHO y1) -> and [ x1 == y1 ]
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GAction where
  gf (GACTION x1) = mkApp (mkCId "ACTION") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "ACTION" -> GACTION (fg x1)


      _ -> error ("no Action " ++ show t)

instance Gf GCN where
  gf (LexCN x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexCN (showCId i)
      _ -> error ("no CN " ++ show t)

instance Gf GConj where
  gf GAND = mkApp (mkCId "AND") []
  gf GOR = mkApp (mkCId "OR") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "AND" -> GAND 
      Just (i,[]) | i == mkCId "OR" -> GOR 


      _ -> error ("no Conj " ++ show t)

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

instance Gf GListWho where
  gf (GListWho [x1,x2]) = mkApp (mkCId "BaseWho") [gf x1, gf x2]
  gf (GListWho (x:xs)) = mkApp (mkCId "ConsWho") [gf x, gf (GListWho xs)]
  fg t =
    GListWho (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseWho" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsWho" -> fg x1 : fgs x2


      _ -> error ("no ListWho " ++ show t)

instance Gf GNumeral where
  gf (Gnum x1) = mkApp (mkCId "num") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "num" -> Gnum (fg x1)


      _ -> error ("no Numeral " ++ show t)

instance Gf GPol where
  gf GNEG = mkApp (mkCId "NEG") []
  gf GPOS = mkApp (mkCId "POS") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "NEG" -> GNEG 
      Just (i,[]) | i == mkCId "POS" -> GPOS 


      _ -> error ("no Pol " ++ show t)

instance Gf GRule where
  gf (GRegulative x1 x2 x3) = mkApp (mkCId "Regulative") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "Regulative" -> GRegulative (fg x1) (fg x2) (fg x3)


      _ -> error ("no Rule " ++ show t)

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
  gf (GEVERY x1) = mkApp (mkCId "EVERY") [gf x1]
  gf (GPARTY x1) = mkApp (mkCId "PARTY") [gf x1]
  gf (GSubjWho x1 x2) = mkApp (mkCId "SubjWho") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "EVERY" -> GEVERY (fg x1)
      Just (i,[x1]) | i == mkCId "PARTY" -> GPARTY (fg x1)
      Just (i,[x1,x2]) | i == mkCId "SubjWho" -> GSubjWho (fg x1) (fg x2)


      _ -> error ("no Subj " ++ show t)

instance Gf GTemp where
  gf GpresentIndicative = mkApp (mkCId "presentIndicative") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "presentIndicative" -> GpresentIndicative 


      _ -> error ("no Temp " ++ show t)

instance Gf GVP where
  gf (LexVP x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexVP (showCId i)
      _ -> error ("no VP " ++ show t)

instance Gf GVPI where
  gf (GMkVPI x1) = mkApp (mkCId "MkVPI") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "MkVPI" -> GMkVPI (fg x1)


      _ -> error ("no VPI " ++ show t)

instance Gf GVPS where
  gf (GMkVPS x1 x2 x3) = mkApp (mkCId "MkVPS") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "MkVPS" -> GMkVPS (fg x1) (fg x2) (fg x3)


      _ -> error ("no VPS " ++ show t)

instance Gf GWho where
  gf (GConjWho x1 x2) = mkApp (mkCId "ConjWho") [gf x1, gf x2]
  gf (GWHO x1) = mkApp (mkCId "WHO") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjWho" -> GConjWho (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "WHO" -> GWHO (fg x1)


      _ -> error ("no Who " ++ show t)



instance Gf GA where
  gf _ = undefined
  fg _ = undefined





instance Gf GA2 where
  gf _ = undefined
  fg _ = undefined





instance Gf GAP where
  gf _ = undefined
  fg _ = undefined





instance Gf GAnt where
  gf _ = undefined
  fg _ = undefined





instance Gf GN where
  gf _ = undefined
  fg _ = undefined





instance Gf GN2 where
  gf _ = undefined
  fg _ = undefined





instance Gf GNP where
  gf _ = undefined
  fg _ = undefined





instance Gf GTense where
  gf _ = undefined
  fg _ = undefined





instance Gf GV where
  gf _ = undefined
  fg _ = undefined





instance Gf GV2 where
  gf _ = undefined
  fg _ = undefined




instance Compos Tree where
  compos r a f t = case t of
    GACTION x1 -> r GACTION `a` f x1
    GIDig x1 -> r GIDig `a` f x1
    GIIDig x1 x2 -> r GIIDig `a` f x1 `a` f x2
    Gnum x1 -> r Gnum `a` f x1
    GRegulative x1 x2 x3 -> r GRegulative `a` f x1 `a` f x2 `a` f x3
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
    GEVERY x1 -> r GEVERY `a` f x1
    GPARTY x1 -> r GPARTY `a` f x1
    GSubjWho x1 x2 -> r GSubjWho `a` f x1 `a` f x2
    GMkVPI x1 -> r GMkVPI `a` f x1
    GMkVPS x1 x2 x3 -> r GMkVPS `a` f x1 `a` f x2 `a` f x3
    GConjWho x1 x2 -> r GConjWho `a` f x1 `a` f x2
    GWHO x1 -> r GWHO `a` f x1
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
