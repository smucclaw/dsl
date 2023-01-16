{-# LANGUAGE RankNTypes, NamedFieldPuns, GADTs #-}

module LS.NLP.TreeTransformations where

import qualified AnyAll as AA
import LS.NLP.UDExt
import PGF (PGF, Expr, CId, mkCId, mkApp, linearize, showType, inferExpr, ppTcError)
import qualified PGF
import qualified GF.Text.Pretty as GfPretty
import Data.Char (toUpper)
import Data.Maybe (mapMaybe, fromMaybe)
import Debug.Trace (trace)

-----------------------------------------------------------------------------
-- generic utility functions

-- redefine showExpr without the first argument []

showExpr :: Expr -> String
showExpr = PGF.showExpr []

-- dummy expression, used when constructing real expression fails
dummyExpr :: String -> PGF.Expr
dummyExpr msg = gf $ Groot_only (GrootN_ (GUsePN (GStrPN (GString msg))))

dummyNP :: GNP
dummyNP = Gwhoever_NP

dummyAP :: GAP
dummyAP = GStrAP (GString [])

dummyAdv :: GAdv
dummyAdv = LexAdv "never_Adv"

dummyCl :: GCl
dummyCl = GExistsNP dummyNP

orConj, andConj :: GConj
orConj = LexConj "or_Conj"
andConj = LexConj "and_Conj"

peelNP :: Expr -> GNP
peelNP np = fromMaybe dummyNP (npFromUDS $ fg np)

useRCl :: GRCl -> GRS
useRCl = GUseRCl presSimul GPPos

useCl :: GCl -> GS
useCl = GUseCl presSimul GPPos

--     UseQCl   : Temp -> Pol -> QCl -> QS ;
useQCl :: GQCl -> GQS
useQCl = GUseQCl presSimul GPPos

presSimul :: GTemp
presSimul = GTTAnt GTPres GASimul

-----------------------------------------------------------------------------
-- Most generic transformations, useful beyond the current grammar?

definiteNP :: forall a . Tree a -> Tree a
definiteNP np@(GDetCN (LexDet "theSg_Det") _) = np
definiteNP np@(GDetCN (LexDet "thePl_Det") _) = np
definiteNP t@(GComplV _ _) = t -- don't change objects
definiteNP t@(GUseComp _) = t -- don't change copula complements
definiteNP (GDetCN _ cn) = GDetCN (LexDet "theSg_Det") cn
definiteNP (GMassNP cn) = GDetCN (LexDet "theSg_Det") cn
definiteNP x = composOp definiteNP x

indefiniteNP :: forall a . Tree a -> Tree a
indefiniteNP np@(GDetCN (LexDet "aSg_Det") _) = np
indefiniteNP np@(GDetCN (LexDet "aPl_Det") _) = np
indefiniteNP (GDetCN _ cn) = GDetCN (LexDet "aSg_Det") cn
indefiniteNP (GMassNP cn) = GDetCN (LexDet "aSg_Det") cn
indefiniteNP x = composOp indefiniteNP x

-----------------------------------------------------------------------------
--- all the *FromUDS functions
-- | Constructs a RGL NP from a UDS.
-- If the UDS is like "kills a cat", the NP will be "a killed cat".
--
-- The functions cnFromUDS and nonMassNPfromUDS all rely on the UDS-matching done by npFromUDS.


-- Specialised version of npFromUDS: return Nothing if the NP is MassNP
nonMassNpFromUDS :: GUDS -> Maybe GNP
nonMassNpFromUDS x = case npFromUDS x of
  Just (GMassNP _) -> Nothing
  _ -> npFromUDS x

-- | Takes the RGL NP returned by npFromUDS, and extracts a CN out of it.
--
-- All UD-to-RGL work happens in npFromUDS, this is just peeling off the layers of the RGL functions.
cnFromUDS :: GUDS -> Maybe GCN
cnFromUDS x = np2cn =<< npFromUDS x

np2cn :: GNP -> Maybe GCN
np2cn np = case np of
  GMassNP   cn          -> Just cn
  GDetCN    _det cn     -> Just cn
  GGenModNP _num _np cn -> Just cn
  GExtAdvNP np   adv    -> fmap (`GAdvCN` adv) (np2cn np)
  GAdvNP    np   adv    -> fmap (`GAdvCN` adv) (np2cn np)
  GRelNP    np   rs     -> fmap (`GRelCN` rs) (np2cn np)
  GPredetNP _pre np     -> np2cn np
  _                     -> Nothing


-- | Constructs a RGL NP from a UDS.
-- If the UDS is like "kills a cat", the NP will be "a killed cat".
--
-- The functions cnFromUDS and nonMassNPfromUDS all rely on the UDS-matching done by npFromUDS.
npFromUDS :: GUDS -> Maybe GNP
npFromUDS x = case x of
  Groot_only (GrootN_ someNP) -> Just someNP
  Groot_only (GrootAdv_ (GPrepNP _ someNP)) -> Just someNP -- extract NP out of an Adv
  Groot_nsubj (GrootV_ _t _p vp) (Gnsubj_ someNP) -> Just $ GSentNP someNP (GEmbedVP vp)
  -- assessment (that sucks)
  Groot_aclRelcl (GrootN_ np) (GaclRelclUDSRP_ _rp relcl) -> Just $ GRelNP np (udRelcl2rglRS relcl)
  -- the occurence at the beach
  Groot_nmod (GrootN_ rootNP) (Gnmod_ prep nmodNP) -> Just $ GAdvNP rootNP (GPrepNP prep nmodNP)
  -- each of the notifiable individuals
  -- Groot_nmod (GrootDAP_ det_DAP) (Gnmod_ prep )
  -- service from the provider to the payer
  Groot_nmod_nmod (GrootN_ service_NP) (Gnmod_ from_Prep provider_NP) (Gnmod_ to_Prep payer_NP) -> Just $ GAdvNP (GAdvNP service_NP (GPrepNP from_Prep provider_NP)) (GPrepNP to_Prep payer_NP)
  -- great harm that she suffered
  Groot_acl (GrootN_ great_harm_NP) (GaclUDS_ (Groot_nsubj (GrootV_ _temp _pol suffer_VP) (Gnsubj_ she_NP))) -> Just $ GRelNP great_harm_NP (GRS_that_NP_VP she_NP suffer_VP)
  -- Groot_obj (GrootN_ five_NP) (GRelclNP np aclrelcl) -> Just $ GRelclNP (GApposNP five_NP np) aclrelcl
  ----np  gnp
  _ -> case getRoot x of -- TODO: fill in other cases
              GrootN_ np:_ -> Just np
              _            -> Nothing


-- | Constructs a RGL RS from a UDS.
udRelcl2rglRS :: GUDS -> GRS
udRelcl2rglRS uds = case uds of
  Groot_nsubj (GrootV_ t p vp) _ -> vp2rs (GMkVPS t p vp) -- TODO: check if nsubj contains something important
  _ -> maybe err vp2rs (verbFromUDS uds)
  where
    vp2rs :: GVPS -> GRS
    vp2rs (GMkVPS t p vp) = GUseRCl t p (GRelVP GIdRP vp)
    vp2rs vps = useRCl (GRelVP GIdRP (vps2vp vps))
    err = error ("udRelcl2rglRCl: doesn't handle yet " ++ showExpr (gf uds))

pnFromUDS :: GUDS -> Maybe GPN
pnFromUDS x = np2pn =<< npFromUDS x
  where
    np2pn :: GNP -> Maybe GPN
    np2pn np = case np of
      GUsePN pn -> Just pn
      _         -> Nothing





pronFromUDS :: GUDS -> Maybe GPron
pronFromUDS x = np2pron =<< npFromUDS x
  where
    np2pron :: GNP -> Maybe GPron
    np2pron np = case np of
      GUsePron pron -> Just pron
      _             -> Nothing

apFromUDS :: GUDS -> Maybe GAP
apFromUDS x = case x of
  Groot_only (GrootA_ ap) -> Just ap
  Groot_obl (GrootA_ ap) (Gobl_ adv) -> Just $ GAdvAP ap adv
  Groot_advmod (GrootA_ ap) (Gadvmod_ adv) -> Just $ GAdvAP ap adv
  Groot_advmod (GrootV_ _ _ v) (Gadvmod_ adv) -> Just $ GAdvAP (GPastPartAP v) adv
  Groot_ccomp (GrootA_ ap) (GccompMarkUDS_ (Gmark_ subj) uds) -> do
    sent <- sFromUDS uds
    pure $ GAdvAP ap (GSubjS subj sent)
  _ -> case getRoot x of -- TODO: fill in other cases
              GrootA_ ap:_ -> Just ap
              _            -> Nothing


advFromUDS :: GUDS -> Maybe GAdv
advFromUDS x = case x of
  GaddMark (Gmark_ subj) uds -> do
    s <- sFromUDS uds
    pure $ GSubjS subj s
  Groot_only (GrootAdv_ someAdv) -> Just someAdv
  Groot_obl (GrootAdv_ someAdv) (Gobl_ oblAdv) -> Just $ GAdvAdv someAdv oblAdv
  -- very much overfitted to catch "unless we go where it's warm"
  Groot_nmod  (GrootDAP_ (GDetDAP det)) (Gnmod_ prep np) -> Just $ GPrepNP prep (GApposNP (GDetNP det) np)
  Groot_advcl (GrootAdv_ whereItsWarm) (GadvclMarkUDS_ (Gmark_ subj) uds) -> do
    weGo <- sFromUDS uds
    let weGoWarm = GPostAdvS weGo whereItsWarm
    pure $ GSubjS subj weGoWarm
  _ -> case [ adv | GrootAdv_ adv <- getRoot x] of
         adv:_ -> Just adv
         []    -> Nothing
{-         []    -> trace errorMsg Nothing
  where
    uds = showExpr (gf x)
    errorMsg = unlines $
      [ "advFromUDS: caught " ++ uds ++ ", couldn't turn it into an Adv."
      , "getRoot " ++ uds ++ " returns:"]
      ++ (showExpr . gf <$> getRoot x)
-}

detFromUDS :: GUDS -> Maybe GDet
detFromUDS x = case x of
  Groot_only (GrootDet_ someDet) -> Just someDet
  _ -> case getRoot x of -- TODO: fill in other cases
              GrootDet_ det:_ -> Just det
              _               -> Nothing

prepFromUDS :: GUDS -> Maybe GPrep
prepFromUDS x = case getRoot x of
  GrootPrep_ p:_ -> Just p
  _              -> Nothing




rpFromUDS :: GUDS -> Maybe GRP
rpFromUDS x = case getRoot x of
  GrootRP_ rp:_ -> Just rp
  _             -> Nothing

verbFromUDS :: GUDS -> Maybe GVPS
verbFromUDS = verbFromUDS' False

verbFromUDS' :: Bool -> GUDS -> Maybe GVPS
verbFromUDS' verbose x = case getNsubj x of
  (_:_) ->
    if verbose
      then trace ("\n\n **** vpFromUDS: has a nsubj in " ++ showExpr (gf x)) Nothing
      else Nothing
--  (_:_) -> Nothing  -- if the UDS has a subject, then it should be handled by sFromUDS instead
  [] -> case x of    -- no nsubj, move on to pattern match UDS constructors
    Groot_obl (GrootV_ t p vp) (Gobl_ adv) -> Just $ GMkVPS t p $ GAdvVP vp adv
    Groot_obj (GrootV_ t p vp) (Gobj_ np) -> Just $ GMkVPS t p $ complVP vp np
    Groot_obl_obj (GrootV_ t p vp) (Gobl_ adv) (Gobj_ obj) -> Just $ GMkVPS t p $ GAdvVP (complVP vp obj) adv
    Groot_obj_obl (GrootV_ t p vp) (Gobj_ obj) (Gobl_ adv) -> Just $ GMkVPS t p $ GAdvVP (complVP vp obj) adv
    Groot_obl_obl (GrootV_ t p vp) (Gobl_ obl1) (Gobl_ obl2) -> Just $ GMkVPS t p $ GAdvVP (GAdvVP vp obl1) obl2
    Groot_obl_xcomp (GrootV_ t p vp) (Gobl_ obl) (GxcompAdv_ xc) -> Just $ GMkVPS t p $ GAdvVP (GAdvVP vp obl) xc
    Groot_xcomp (GrootV_ t p vp) xcomp -> Just $ GMkVPS t p $ GAdvVP vp (xcomp2adv xcomp)
    Groot_obj_xcomp (GrootV_ t p vp) (Gobj_ obj) xcomp -> Just $ GMkVPS t p $ GAdvVP (complVP vp obj) (xcomp2adv xcomp)
    Groot_advmod (GrootV_ t p vp) (Gadvmod_ adv) ->
      Just $ GMkVPS t p $ GAdvVP vp adv
    Groot_acl_nmod root         (GaclUDSgerund_ uds) (Gnmod_ prep np) -> do
      GMkVPS t p vp <- verbFromUDS (Groot_only root) -- recursively calling verbFromUDS, now with a UDS that is guaranteed to go to the _ case below, and getRoot will be called, and a VP will be constructed
      GMkVPS _ _ vpToBecomeGerund <- verbFromUDS uds -- :: GVPS
      let gerundAdv = GGerundAdv vpToBecomeGerund -- :: GAdv
      let nmodAdv = GPrepNP prep np
      return $ GMkVPS t p $ GAdvVP (GAdvVP vp gerundAdv) nmodAdv
    _ -> case getRoot x of -- TODO: fill in other cases
                GrootV_ t p vp:_ -> Just $ GMkVPS t p vp
                GrootVaux_ t p aux vp:_ -> Just $ GComplAux aux t p vp ;
                -- Here we want only verby roots, for other root constructors we use root2vps!
                _ -> if verbose
                      then trace ("\n\n **** verbFromUDS: couldn't match " ++ showExpr (gf x)) Nothing
                      else Nothing



scFromUDS :: GUDS -> Maybe GSC
scFromUDS x = case sFromUDS x of
  Just s -> pure $ GEmbedS s
  _ -> case verbFromUDS x of
    Just (GMkVPS _t _p vp) -> pure $ GEmbedVP vp
    _ -> error $ "scFromUDS: can't handle " ++ showExpr (gf x)


-- TODO: use composOp to grab all (finite) UD labels and put them together nicely
sFromUDS :: GUDS -> Maybe GS
sFromUDS x = case getNsubj x of
  --[] -> trace ("\n\n **** sFromUDS: no nsubj in " ++ showExpr (gf x)) Nothing  -- if the UDS doesn't have a subject, then it should be handled by vpFromUDS instead
  [] -> Nothing
  _ -> case x of
    Groot_acl root acl -> do
      uds <- udsFromacl acl
      np <- npFromUDS uds
      predVPS np <$> (root2vps root)
    Groot_aclRelcl root (GaclRelclUDS_ uds) -> do
      np <- npFromUDS uds
      predVPS np <$> root2vps root
    Groot_expl_cop_csubj root _expl _cop csubj -> do
      GMkVPS t p vp <- (root2vps root)
      let pred = GAdvVP vp (Gcsubj2Adv csubj)
      pure $ GUseCl t p $ GImpersCl pred
    Groot_nmod root (Gnmod_ prep np) ->
      predVPS np <$> root2vps root
    Groot_nsubj root (Gnsubj_ np) -> predVPS np <$> root2vps root
    Groot_csubj root (Gcsubj_ cs) -> do
      GMkVPS t p vp <- (root2vps root)
      sc <- scFromUDS cs
      pure $ GUseCl t p $ GPredSCVP sc vp
    Groot_nsubj_advmod root (Gnsubj_ np) (Gadvmod_ adv) -> do
      GMkVPS t p vp <- (root2vps root)
      pure $ GUseCl t p $ GPredVP np (GAdvVP vp adv)
    Groot_nsubj_obj_advcl root (Gnsubj_ subj) (Gobj_ obj) advcl -> do
      GMkVPS t p vp <- (root2vps root)
      let adv = Gadvcl2Adv advcl
          pred = GAdvVP (complVP vp obj) adv
      pure $ GUseCl t p $ GPredVP subj pred
    Groot_nsubj_advmod_obj root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_aux_advmod root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_aux_advmod_obj_advcl root (Gnsubj_ np) _ _ _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_aux_cop_nmod root (Gnsubj_ np)_ _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_aux_obj root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_aux_obj_obl root (Gnsubj_ np) _ _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_aux_obj_obl_advmod_advcl root (Gnsubj_ np) _ _ _ _ _  -> predVPS np <$> root2vps root
    Groot_nsubj_aux_obj_obl_obl root (Gnsubj_ np) _ _ _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_ccomp root (Gnsubj_ np) _ -> predVPS np <$> root2vps root
    Groot_nsubj_cop root (Gnsubj_ np) _ -> predVPS np <$> root2vps root
    Groot_nsubj_cop_aclRelcl root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_cop_advcl root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_cop_case_nmod_acl root (Gnsubj_ np) _ _ _ _  -> predVPS np <$> root2vps root
    Groot_nsubj_cop_nmod root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_cop_nmodPoss root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_cop_obl root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_obj root (Gnsubj_ np) obj -> predVPS np <$> verbFromUDSVerbose (Groot_obj root obj)
    Groot_nsubj_obj_xcomp root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_obl root (Gnsubj_ np) (Gobl_ adv) -> do
      GMkVPS t p vp <- (root2vps root)
      pure $ GUseCl t p $ GPredVP np (GAdvVP vp adv)
    Groot_nsubj_obl_obl root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_xcomp root (Gnsubj_ np) _ -> predVPS np <$> root2vps root
    Groot_nsubj_aux_obl root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_obj root (Gobj_ obj) -> predVPS obj <$> root2vps root
    Groot_obj_ccomp root (Gobj_ obj) _ -> predVPS obj <$> root2vps root
    Groot_xcomp root xcomp -> case xcomp of
      GxcompN_ np -> predVPS np <$> root2vps root
      GxcompToBeN_ _ _ np -> predVPS np <$> root2vps root
      _ -> error ("sFromUDS: doesn't handle yet " <> showExpr (gf xcomp))
    -- todo: add other xcomps
    GaddMark (Gmark_ subj) (Groot_nsubj_cop root (Gnsubj_ nsubj) cop) -> do
      xcomp <- pure $ GxcompToBeN_ (Gmark_ subj) cop nsubj
      sFromUDS $ Groot_xcomp root xcomp
    Groot_ccomp root (Gccomp_ ccomp) -> do
      GMkVPS t p vp <- (root2vps root)
      sc <- GEmbedS <$> sFromUDS ccomp
      pure $ GUseCl t p $ GPredSCVP sc vp
    _ -> case verbFromUDSVerbose x of -- TODO: fill in other cases
                Just (GMkVPS t p vp) -> Just $ GUseCl t p $ GGenericCl vp
                --_       -> Nothing
                _    -> trace ("\n\n **** sFromUDS: couldn't match " ++ showExpr (gf x)) Nothing
    where
      verbFromUDSVerbose = verbFromUDS
      -- verbFromUDSVerbose = verbFromUDS' True -- uncomment when you want really verbose debug output

----------------------------------
-- inverse: turn any Expr into UDS

toUDS :: PGF -> Expr -> GUDS
toUDS pgf e = case findType pgf e of
  "UDS" -> fg e -- it's already a UDS
  "NP" -> Groot_only (GrootN_                 (fg e))
  "CN" -> Groot_only (GrootN_ (GMassNP        (fg e)))
  "N"  -> Groot_only (GrootN_ (GMassNP (GUseN (fg e))))
  "AP" -> Groot_only (GrootA_          (fg e))
  "A"  -> Groot_only (GrootA_ (GPositA (fg e)))
  "VP" -> Groot_only (GrootV_ presSimul GPPos (fg e))
  "VPS" -> vps2uds (fg e)
  "V"  -> Groot_only (GrootV_ presSimul GPPos (GUseV (fg e)))
  "Adv"-> Groot_only (GrootAdv_ (fg e))
  "Det"-> Groot_only (GrootDet_ (fg e))
 -- "Quant"-> Groot_only (GrootQuant_ (fg e))
  "Quant"-> Groot_only (GrootDet_ (GDetQuant (fg e) GNumSg))
  "ACard" -> Groot_only (GrootDet_ (GACard2Det (fg e)))
  "AdA" -> Groot_only (GrootAdA_ (fg e)) -- added from gf
  "Prep" -> Groot_only (GrootPrep_ (fg e))
  "RP" -> Groot_only (GrootRP_ (fg e))
  "RCl" -> case fg e :: GRCl of
             GRelVP _rp vp -> toUDS pgf (gf vp)
             GRelSlash _rp (GSlashCl cl) -> toUDS pgf (gf cl)
             _ -> fg $ dummyExpr ("unable to convert to UDS rcl: " ++ showExpr e )
  "Cl" -> case fg e :: GCl of
            GPredVP np vp -> Groot_nsubj (GrootV_ presSimul GPPos vp) (Gnsubj_ np)
            GGenericCl vp -> toUDS pgf (gf vp)
            _ -> fg  $ dummyExpr ("unable to convert to UDS cl: " ++ showExpr e )
  "S" -> case fg e :: GS of
    GUseCl t p (GPredVP np vp) -> Groot_nsubj (GrootV_ t p vp) (Gnsubj_ np)
    GConjS c (GListS (s:ss)) -> toUDS pgf (gf s)
    _ -> fg  $ dummyExpr ("unable to convert to UDS S: " ++ showExpr e )
    -- vps2vp (GConjVPS c (GListVPS vps)) = GConjVP c (GListVP (map vps2vp vps))
  _ -> fg $ dummyExpr $ "unable to convert to UDS all: " ++ showExpr e
  where
    vps2uds :: GVPS -> GUDS
    vps2uds (GMkVPS t p vp) = Groot_only (GrootV_ t p vp)
    vps2uds vps = Groot_only (GrootV_ presSimul GPPos (vps2vp vps)) -- This causes trouble in other places TODO investigate

findType :: PGF -> Expr -> String
findType pgf e = case inferExpr pgf e of
  Left te -> error $ "Tried to infer type of:\n\t* " ++ showExpr e  ++ "\nGot the error:\n\t* " ++ GfPretty.render (ppTcError te) -- gives string of error
  Right (_, typ) -> showType [] typ -- string of type

------------------------------------------------------------------------------
-- TreeGroups

-- | A data structure for GF trees, which has different bins for different RGL categories.
--
-- A single UDS tree may become several of these; e.g. a root_nsubj sentence pattern could become
-- a S, "a breach occurs", but also a NP, "an occurring breach".
-- The different NLG functions make their decisions on how to combine phrases based on which fields are filled.
data TreeGroups = TG {
    gfAP   :: Maybe GAP  -- 1
  , gfAdv  :: Maybe GAdv -- 2
  , gfNP   :: Maybe GNP  -- 3
  , gfDet  :: Maybe GDet -- 4
  , gfCN   :: Maybe GCN  -- 5
  , gfPrep :: Maybe GPrep -- 6
  , gfRP   :: Maybe GRP  -- 7
  , gfVP   :: Maybe GVPS  -- 8
  , gfS    :: Maybe GS  -- 9
   } deriving (Eq)

emptyTG :: TreeGroups
emptyTG = TG Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

npTG :: GNP -> TreeGroups
npTG np = emptyTG {gfNP = Just np}

apTG :: GAP -> TreeGroups
apTG ap = emptyTG {gfAP = Just ap}

cnTG :: GCN -> TreeGroups
cnTG cn = emptyTG {gfCN = Just cn}

vpTG :: GVPS -> TreeGroups
vpTG vp = emptyTG {gfVP = Just vp}

sTG :: GS -> TreeGroups
sTG s = emptyTG {gfS = Just s}

-- | for documentation: which RGL types are accepted currently
acceptedRGLtypes :: String
acceptedRGLtypes = "AP Adv NP Det CN Prep RP VPS S"

instance Show TreeGroups where
  show tg = case flattenGFTrees tg of
             [] -> "the TreeGroups is empty"
             xs -> unlines $ map showExpr xs

-- | Workaround to flatten TreeGroups into a list of Exprs.
flattenGFTrees :: TreeGroups -> [Expr]
flattenGFTrees TG {gfAP, gfAdv, gfNP, gfDet, gfCN, gfPrep, gfRP, gfVP, gfS} =
  gfAP <: gfAdv <: gfNP <: gfCN <: gfDet <: gfPrep <: gfRP <: gfVP <: gfS <: []
  where
    infixr 5 <:
    (<:) :: (Gf a) => Maybe a -> [Expr] -> [Expr]
    Nothing <: exprs = exprs
    Just ap <: exprs = gf ap : exprs

-- this function is only to keep track of preferred order
-- if it can be parsed as S, use the S
-- if not, then try VP, if not, then try AP, etc.
treeContents :: GConj -> [GUDS] -> Expr
treeContents conj contents = case groupByRGLtype conj contents of
  TG {gfS    = Just x} -> gf x
--  TG {gfVP   = Just x, gfAP = Nothing , gfCN = Nothing , gfNP = Nothing, gfAdv = Nothing} -> gf x
  -- TODO match
  TG {gfVP   = Just x, gfAP = Just y} -> gf $ GConjVPS conj (GListVPS [x, GMkVPS presSimul GPPos (GUseComp (GCompAP y))])
  TG {gfVP   = Just (GMkVPS _ _ (GUseComp (GCompAP x)))} -> gf x
  TG {gfVP   = Just (GMkVPS _ _ (GUseComp (GCompNP x)))} -> gf x
  -- TG {gfVP   = Just (GMkVPS _ _ (GUseComp (GCompCN x)))} -> gf x
  TG {gfVP   = Just (GMkVPS _ _ (GUseComp (GCompAdv x)))} -> gf x
  TG {gfVP   = Just x} -> gf x
  TG {gfAdv  = Just x} -> gf x
  TG {gfAP   = Just x} -> gf x
  TG {gfNP   = Just x} -> gf x
  TG {gfCN   = Just x} -> gf x
  TG {gfDet  = Just x} -> gf x
  TG {gfRP   = Just x} -> gf x
  TG {gfPrep = Just x} -> gf x
  _                    -> error $ "treeContents: no contents"


-- | Takes a list of UDS, and puts them into different bins according to their underlying RGL category.
groupByRGLtype :: GConj -> [GUDS] -> TreeGroups
groupByRGLtype conj contentsUDS = TG treeAP treeAdv treeNP treeDet treeCN treePrep treeRP treeVP treeS
  -- TODO: what if they are different types?
  where
    treeAdv :: Maybe GAdv
    treeAdv = case mapMaybe advFromUDS contentsUDS :: [GAdv] of
                []    -> Nothing
                [adv] -> Just adv
                advs  -> Just $ GConjAdv conj (GListAdv advs)

    treeAP :: Maybe GAP
    treeAP = case mapMaybe apFromUDS contentsUDS :: [GAP] of
                []   -> Nothing
                [ap] -> Just ap
                aps  -> Just $ GConjAP conj (GListAP aps)

{- -- maybe too granular? reconsider this
    treePN :: Maybe Expr
    treePN = case mapMaybe pnFromUDS contentsUDS :: [GPN] of
                []   -> Nothing
                [pn] -> Just $ GUsePN pn
                pns  -> Just $ GConjNP conj (GListNP (map GUsePN pns))

    treePron :: Maybe Expr
    treePron = case mapMaybe pronFromUDS contentsUDS :: [GPron] of
                []   -> Nothing
                [pron] -> Just $ GUsePron pron
                prons  -> Just $ GConjNP conj (GListNP (map GUsePron prons))
-}
    -- Exclude MassNP here! MassNPs will be matched in treeCN
    treeNP :: Maybe GNP
    treeNP = case mapMaybe nonMassNpFromUDS contentsUDS :: [GNP] of
                []   -> Nothing
                [np] -> Just np
                nps  -> Just $ GConjNP conj (GListNP nps)

    -- All CNs will match NP, but we only match here if it's a MassNP
    treeCN :: Maybe GCN
    treeCN = case mapMaybe cnFromUDS contentsUDS :: [GCN] of
                []   -> Nothing
                [cn] -> Just cn
                cns  -> Just $ GConjCN conj (GListCN cns)

    treeDet :: Maybe GDet
    treeDet = case mapMaybe detFromUDS contentsUDS :: [GDet] of
                []    -> Nothing
                [det] -> Just det
                dets  -> Just $ GConjDet conj (GListDAP $ map GDetDAP dets)

    treeVP :: Maybe GVPS
    treeVP = case mapMaybe verbFromUDS contentsUDS :: [GVPS] of
                []    -> Nothing
                [vp]  -> Just vp
                vps   -> Just $ GConjVPS conj (GListVPS vps)

    treePrep :: Maybe GPrep
    treePrep = case mapMaybe prepFromUDS contentsUDS :: [GPrep] of
                []     -> Nothing
                [prep] -> Just prep
                preps  -> Just $ GConjPrep conj (GListPrep preps)


    treeRP :: Maybe GRP
    treeRP = case mapMaybe rpFromUDS contentsUDS :: [GRP] of
                []    -> Nothing
                r:_  -> Just r

    treeS :: Maybe GS
    treeS = case mapMaybe sFromUDS contentsUDS :: [GS] of
                []  -> Nothing
                [s] -> Just s
                ss  -> Just $ GConjS conj (GListS ss)

-- | Takes a UDS, peels off the UD layer, returns a pair ("RGL type", the peeled off Expr)
udsToTreeGroups :: GUDS -> TreeGroups
udsToTreeGroups uds = groupByRGLtype (LexConj "") [uds]

expr2TreeGroups :: PGF -> Expr -> TreeGroups
expr2TreeGroups gr e = case findType gr e of  -- to avoid converting back and forth between UDS and RGL cats
      "VPS" -> vpTG $ fg e
      "S"   -> sTG $ fg e
      "NP"  -> npTG $ fg e

      _     -> udsToTreeGroups (toUDS gr e)



-----------------------------------------------------------------------------
-- Combining expressions

-- | Takes two UDSs, puts them together, returns Expr and a string that tells which type the result is.
combineExpr :: GUDS -> GUDS -> (String, Expr)
combineExpr pred compl = result
  where
    predExpr = gf pred -- used for error msg
    complExpr = gf compl -- used for error msg
    predTyped = udsToTreeGroups pred
    complTyped = udsToTreeGroups compl
    result = case predTyped of
      -- root_only (rootN_ (MassNP (UseN (StrN "propernoun"))))with complement root_nmod (rootDAP_ (DetDAP each_Det)) (nmod_ of_Prep (DetCN thePl_Det (AdjCN (PositA notifiable_A) (UseN individual_N))))

      -- TG {gfNP = Just propernoun} ->
      --   ("RS", case complTyped of
      --     TG {gfAdv = Just (Groot_nmod $ (GrootDAP_ (GDetDAP each)) (Gnmod_ of_Prep individuals))} -> gf $ GApposNP propernoun $ GAdvNP (GDetNP each) (GPrepNP of_Prep individuals)
      --   )
        -- each of the notifiable individuals

        -- root_nmod (rootDAP_ (DetDAP each_Det)) (nmod_ of_Prep (DetCN thePl_Det (AdjCN (PositA notifiable_A) (UseN individual_N))))

        -- root_only (rootN_ (MassNP (UseN (StrN "propernoun")))) with complement root_nmod (rootDAP_ (DetDAP each_Det)) (nmod_ of_Prep (DetCN thePl_Det (AdjCN (PositA notifiable_A) (UseN individual_N))))

      TG {gfRP=Just for_which} ->
        ("RS", case complTyped of
          TG {gfS= Just (GUseCl t p you_work)} -> gf $ GUseRCl t p $ GRelSlash for_which (GSlashCl you_work)
          TG {gfVP= Just (GMkVPS t p works)}   -> gf $ GUseRCl t p $ GRelVP for_which works
          _ -> error ("rp combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
        )
      TG {gfPrep=Just under} ->
        case complTyped of
          TG {gfAP=Just haunted} -> ("Adv", gf $ GPrepNP under (GAdjAsNP haunted))
          -- TG {gfAdv=Just quickly} -> ???
          TG {gfNP=Just johnson} -> ("Adv", gf $ GPrepNP under johnson)
          TG {gfDet=Just my}     -> ("Adv", gf $ GPrepNP under (GDetNP my))
          TG {gfCN=Just car}     -> ("Adv", gf $ GPrepNP under (GMassNP car))
          TG {gfPrep=Just with}  -> ("Prep", gf $ GConjPrep andConj (GListPrep [under, with]))
          TG {gfRP=Just which}   -> ("RP", gf $ GPrepRP under which)
          TG {gfVP=Just (GMkVPS _t _p haunt)}   -> ("Adv", gf $ GPrepNP under (GGerundNP haunt))
          -- TG {gfS=Just you_see} -> ???
          _ -> error ("prep combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)

      TG {gfVP=Just (GMkVPS t p notify)} ->
        ("VPS", case complTyped of
          TG {gfS=Just you_see}  -> gf $ GMkVPS t p $ GComplSVP notify you_see
          TG {gfAP=Just haunted} -> gf $ GMkVPS t p $ complVP notify (GAdjAsNP haunted)
          TG {gfAdv=Just quickly}-> gf $ GMkVPS t p $ GAdvVP   notify quickly
          TG {gfNP=Just johnson} -> gf $ GMkVPS t p $ complVP notify johnson
          TG {gfDet=Just my}     -> gf $ GMkVPS t p $ complVP notify (GDetNP my)
          TG {gfCN=Just car}     -> gf $ GMkVPS t p $ complVP notify (GMassNP car)
          TG {gfPrep=Just with}  -> gf $ GMkVPS t p $ GPrepVP notify with
          -- TG {gfNP = Just (GDet each (GPrepNP of_Prep theindividual))} -> gf $ GMkVPS t p $ GAdvVP notify (GPrepNP of_Prep theindividual)
          -- TG {gfRP=Just which}   -> ("RP", gf $ GPrepRP under which)
          -- TG {gfVP=Just haunt}   -> ("Adv", gf $ GPrepNP under (GGerundNP haunt))
-- natural4-exe: vp combineExpr: can't combine predicate root_only (rootV_ (TTAnt TPres ASimul) PPos (UseV notify_V))with complement root_nmod (rootDAP_ (DetDAP each_Det)) (nmod_ of_Prep (DetCN thePl_Det (AdjCN (PositA notifiable_A) (UseN individual_N))))

          _ -> error ("vp combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr ++ " pred " ++ show predTyped ++ " compl " ++ show complTyped)
        )
      TG {gfCN=Just house} ->
        ("CN", case complTyped of
          TG {gfCN=Just car}      -> gf $ GApposCN house (GMassNP car)
          TG {gfNP=Just johnson}  -> gf $ GApposCN house johnson
          TG {gfDet=Just my}      -> gf $ GApposCN house (GDetNP my)
          TG {gfAdv=Just quickly} -> gf $ GAdvCN  house quickly
          TG {gfAP=Just haunted}  -> gf $ GAdjCN  haunted house

          _ -> error ("cn combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
        )
      TG {gfNP=Just the_customer} ->
        ("NP", case complTyped of
          TG {gfCN=Just house}    -> gf $ GGenModNP GNumSg the_customer house
          TG {gfNP=Just johnson}  -> gf $ GApposNP  the_customer johnson
          TG {gfDet=Just my}      -> gf $ GApposNP  the_customer (GDetNP my)
          TG {gfAdv=Just quickly} -> gf $ GAdvNP    the_customer quickly
          TG {gfAP=Just haunted}  -> gf $ GApposNP  the_customer (GAdjAsNP haunted)
          _ -> error ("np combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
        )
      TG {gfDet=Just any} -> case complTyped of
          TG {gfCN=Just house}   -> ("NP", gf $ GDetCN any house)
          TG {gfAP=Just haunted} -> ("DAP", gf $ GAdjDAP (GDetDAP any) haunted)
          _ -> error ("det combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
      TG {gfAdv=Just happily} -> case complTyped of
        TG {gfCN=Just house}   -> ("CN", gf $ GAdvCN house happily)
        TG {gfNP=Just johnson} -> ("NP", gf $ GAdvNP johnson happily)
        TG {gfAP=Just haunted} -> ("AP", gf $ GAdvAP haunted happily)
        _ -> error ("adv combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
      TG {gfAP=Just happy} -> case complTyped of
        TG {gfCN=Just house}    -> ("CN", gf $ GAdjCN happy house)
        TG {gfNP=Just johnson}  -> ("NP", gf $ GApposNP (GAdjAsNP happy) johnson)
        TG {gfDet=Just my}      -> ("DAP", gf $ GAdjDAP (GDetDAP my) happy)
        TG {gfAdv=Just quickly} -> ("AP", gf $ GAdvAP happy quickly)
        TG {gfAP=Just (GPositA haunted)} -> ("AP", gf $ GAdvAP happy (GPositAdvAdj haunted))
        _ -> error ("ap combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
      -- ("Cl", you_work) -> case complTyped of
      --   ("RP", for_which) -> ("RS", gf $ GRelSlash (fg for_which) (GSlashCl (fg you_work)))
      --   _ -> error ("combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
      tg -> error ("all combineExpr: can't find type " ++ show tg ++ " for the predicate " ++ showExpr predExpr)


-- Special case used in bsr2gf
-- TODO: can treePre and treePrePost be reimplemented in terms of TreeTransformations.combineExpr?
treePre :: GConj -> [GUDS] -> GUDS -> Expr
treePre conj contents pre = case groupByRGLtype conj <$> [contents, [pre]] of
  [TG {gfCN=Just cn}, TG {gfAP=Just ap}] -> gf $ GAdjCN ap cn
  [TG {gfNP = Just np}, TG {gfVP = Just vp}] -> gf $ predVPS np vp
  [TG {gfS = Just (GUseCl t p (GPredVP np vp))}, TG {gfNP = Just np2}] -> gf $ predVPS np2 (GMkVPS t p (GComplVP vp np))
  _ -> trace ("bsr2gf: heyy can't handle the combination pre=" ++ showExpr (gf pre) ++ "+ contents=" ++ showExpr (treeContents conj contents))
           $ treeContents conj contents

treePrePost :: GConj -> [GUDS] -> GUDS -> GUDS -> Expr
treePrePost conj contents pre post =
  case groupByRGLtype conj <$> [contents, [pre], [post]] of
          [TG {gfCN=Just cn}, _, TG {gfAdv=Just (GPrepNP _of personal_data)}] ->
            let listcn = case cn of
                  GConjCN _ cns -> cns
                  _ -> GListCN [cn, cn]
            in constructTreeAPCNsOfNP listcn conj personal_data pre
          [TG {gfDet=Just det}, TG {gfS=Just (GUseCl t p cl)}, TG {gfCN=Just cn}] ->
            let obj = GDetCN det cn
            in case cl of
                  GPredVP np vp -> gf $ GUseCl t p $ GPredVP np (complVP vp obj)
                  GGenericCl vp -> gf $ GUseCl t p $ GGenericCl (complVP vp obj)
                  _ -> error $ "bsr2gf: can't handle the Cl " ++ showExpr (gf cl)
          _ -> dummyExpr $ "bsr2gf: can't handle the combination " ++ showExpr (gf pre) ++ "+" ++ showExpr (gf post)


-- Another special case for bsp2gf
-- | Takes the main action, a list of modifiers, and combines them into one Expr
combineActionMods :: (String,Expr) -> [(String, Expr)] -> Expr
combineActionMods ("CN", cn) [] = mkApp (mkCId "MassNP") [cn] -- elevate into NP, because we assume this is going to be subjA
combineActionMods (_, expr) [] = expr -- other cats, leave as is (no action mods)
combineActionMods ("CN",noun) (("RS",mod):rest) = combineActionMods ("CN", gf resultCN) rest
  where
    -- cnrs
    resultCN :: GCN
    resultCN = GRelCN (fg noun) (fg mod)
combineActionMods ("CN", noun) (("Adv",mod):rest) = combineActionMods ("CN", gf resultCN) rest
  where
    resultCN :: GCN
    resultCN = GAdvCN (fg noun) (fg mod)
combineActionMods ("CN", noun) (("NP", mod):rest) = combineActionMods ("CN", gf resultCN) rest
  where
    resultCN :: GCN
    resultCN = GPossNP (fg noun) (fg mod)
combineActionMods ("VPS",act) (("Adv",mod):rest) = combineActionMods ("VPS", gf resultVP) rest
  where
    resultVP :: GVPS
    resultVP = advVPS (fg act) (fg mod)

    advVPS :: GVPS -> GAdv -> GVPS
    advVPS vps adv = GMkVPS presSimul GPPos $ GAdvVP (vps2vp vps) adv
combineActionMods ("VP",act) (("Adv",mod):rest) = combineActionMods ("VPS", gf resultVP) rest
  where
    resultVP :: GVPS
    resultVP = advVPS (fg act) (fg mod)

    advVPS :: GVPS -> GAdv -> GVPS
    advVPS vps adv = GMkVPS presSimul GPPos $ GAdvVP (vps2vp vps) adv

combineActionMods ("VPS",act) (("NP",mod):rest) = combineActionMods ("VPS", gf resultVP) rest
  where
    resultVP :: GVPS
    resultVP = npVPS (fg act) (fg mod)

    npVPS :: GVPS -> GNP -> GVPS
    npVPS vps np = GMkVPS presSimul GPPos $ GComplVP (vps2vp vps) np

combineActionMods ("VPS",act) (("CN",mod):rest) = combineActionMods ("VPS", gf resultVP) rest
  where
    resultVP :: GVPS
    resultVP = cnVPS (fg act) (fg mod)

    cnVPS :: GVPS -> GCN -> GVPS
    cnVPS vps cn = GMkVPS presSimul GPPos $ GComplVP (vps2vp vps) (GMassNP cn)

combineActionMods ("NP",act) (("Adv",mod):rest) = combineActionMods ("VPS", gf resultVP) rest
  where
    resultVP :: GVPS
    resultVP = advVPS (GUseComp (GCompNP (fg act))) (fg mod)

    advVPS :: GVP -> GAdv -> GVPS
    advVPS vp adv = GMkVPS presSimul GPPos $ GAdvVP vp adv

combineActionMods ("VPS",act) (("RS",mod):rest) = combineActionMods ("VPS", gf resultVP) rest
  where
    -- Assumption: RCl doesn't modify the whole VP, but rather the object of the VP
    resultVP :: GVPS
    resultVP = rsVPS (fg act) (fg mod)

    rsVPS :: GVPS -> GRS -> GVPS
    rsVPS vps rs = case vps of
      GMkVPS t p vp -> case vp of
        GComplV   v  np -> GMkVPS t p $ GComplV v   (GRelNP np rs)
        GComplVP vp' np -> GMkVPS t p $ complVP vp' (GRelNP np rs)
        GUseComp (GCompNP np) -> GMkVPS t p $ GUseComp $ GCompNP (GRelNP np rs)
        _               -> GMkVPS t p $ complVP vp (GRelNP dummyNP rs)
      _ -> error $ "combineActionMods: expected VPS, got something else" -- ++ showExpr act

combineActionMods (tAct,_) ((tMods,_):_) = error $ "combineActionMods: not supported yet " ++ tAct ++ "+" ++ tMods

------------------------------------------------------------------------------
-- Creating questions

--     ExistNPQS  : Temp -> Pol -> NP -> QS ;   -- was there a party
-- SQuestVPS  : NP   -> VPS -> QS ;
-- get GQS from Trees
--     ExistIPQS  : Temp -> Pol -> IP -> QS ;   -- what was there
--     QuestIAdv   : IAdv -> Cl -> QCl ;    -- why does John walk
--     ExistIP   : IP -> QCl ;       -- which houses are there

mkQs :: QFun -> PGF -> CId -> Expr -> TreeGroups -> [AA.OptionallyLabeledBoolStruct String]
mkQs qfun gr lang subj tg = case tg of
  TG {gfS = Just sent} -> case sent of
    GConjS _conj (GListS ss) -> concat $ mapM (mkQs qfun gr lang subj) (sTG <$> ss)
    _ -> mapM lin $ qfun subj $ sTG sent
  TG {gfVP = Just vp} -> case vp of
    GConjVPS _conj (GListVPS vps) -> concat $ mapM (mkQs qfun gr lang subj) (vpTG <$> vps)
    _ -> mapM lin $ qfun subj $ vpTG vp
  TG {gfNP = Just np} -> case np of
    GConjNP _conj (GListNP nps) -> concat $ mapM (mkQs qfun gr lang subj) (npTG <$> nps)
    _ -> mapM lin $ qfun subj $ npTG np
  TG {gfCN = Just cn} -> case cn of
    GConjCN _conj (GListCN cns) -> concat $ mapM (mkQs qfun gr lang subj) (cnTG <$> cns)
    _ -> mapM lin $ qfun subj $ cnTG cn
  TG {gfAP = Just ap} -> case ap of
    GConjAP _conj (GListAP aps) -> concat $ mapM (mkQs qfun gr lang subj) (apTG <$> aps)
    _ -> mapM lin $ qfun subj $ apTG ap

  -- TG {gfDet = Just det} ->
  -- TG {gfAdv = Just adv} ->
  _ -> []

  where
    lin :: GQS -> [String]
    lin x = qnPunct [linearize gr lang (gf x)]

    qnPunct :: [String] -> [String]
    qnPunct [] = []
    qnPunct [l] = [toUpper (head l) :( tail l ++ "?")]
    qnPunct (l:ls) = [toUpper (head l)] : tail l : concat ls : ["?"]
type QFun = Expr -> TreeGroups -> AA.OptionallyLabeledBoolStruct GQS
qsWho :: QFun
qsCond :: QFun
qsHaving :: QFun

qsWho subj whichTG = case whichTG of
  TG {gfS = Just (GUseCl t GPPos cl)} -> AA.Leaf $ GUseQCl t GPPos $ GQuestCl (definiteNP cl) -- is the cat cute?
  TG {gfS = Just (GUseCl t GPNeg cl)} -> AA.Not $ AA.Leaf $ GUseQCl t GPPos $ GQuestCl (definiteNP cl) -- same but original sentence was neg!
  TG {gfNP = Just np} -> AA.Leaf $ useQCl $ GQuestCl $ GPredVP sub (GUseComp (GCompNP (indefiniteNP np))) -- are you the cat? (if it was originally MassNP, becomes "are you a cat")
  TG {gfCN = Just cn} -> AA.Leaf $ useQCl $ GQuestCl $ GPredVP sub (GUseComp (GCompNP (GDetCN (LexDet "aSg_Det") cn))) -- are you a cat?
  TG {gfVP = Just (GMkVPS t GPPos vp)} -> AA.Leaf $ GUseQCl t GPPos $ GQuestCl $ GPredVP sub vp -- do you eat cat food?
  TG {gfVP = Just (GMkVPS t GPNeg vp)} -> AA.Not $ AA.Leaf $ GUseQCl t GPPos $ GQuestCl $ GPredVP sub vp -- same but original sentence was neg!
  TG {gfAP = Just ap} -> AA.Leaf $ useQCl $ GQuestCl $ GPredVP sub (GUseComp (GCompAP ap))
  TG {gfDet = Just det} -> AA.Leaf $ useQCl $ GQuestCl $ GPredVP sub (GUseComp (GCompNP (GDetNP det)))
  TG {gfAdv = Just adv} -> AA.Leaf $ useQCl $ GQuestCl $ GPredVP sub (GUseComp $ GCompAdv adv)
  _ -> AA.Leaf $ useQCl $ GQuestCl dummyCl
  where sub = definiteNP $ fg subj


qsCond _sub whichTG = case whichTG of
  TG {gfS = Just (GUseCl t GPPos cl)} -> AA.Leaf $ GUseQCl t GPPos $ GQuestCl (definiteNP cl) -- is the cat cute?
  TG {gfS = Just (GUseCl t GPNeg cl)} -> AA.Not $ AA.Leaf $ GUseQCl t GPPos $ GQuestCl (definiteNP cl) -- same but original sentence was neg!
  TG {gfNP = Just np} -> AA.Leaf $ GExistNPQS presSimul GPPos (indefiniteNP np) -- is there a cat?
  TG {gfCN = Just cn} -> AA.Leaf $ useQCl $ GQuestCl $ GExistCN cn -- is there a cat?
  TG {gfVP = Just (GMkVPS t GPPos vp)} -> AA.Leaf $ GUseQCl t GPPos $ GQuestCl $ GPredVP GSomeone vp -- does someone eat cat food?
  TG {gfVP = Just (GMkVPS t GPNeg vp)} -> AA.Not $ AA.Leaf $ GUseQCl t GPPos $ GQuestCl $ GPredVP GSomeone vp -- same but original sentence was neg!
  TG {gfAP = Just ap} -> AA.Leaf $ useQCl $ GQuestCl $ GExistsNP (GAdjAsNP ap) -- is there a green one?
  TG {gfDet = Just det} -> AA.Leaf $ useQCl $ GQuestCl $ GExistsNP (GDetNP det) -- is there this?
  TG {gfAdv = Just adv} -> AA.Leaf $ useQCl $ GQuestCl $ GPredVP GSomeone (GUseComp $ GCompAdv adv) -- is someone here?
  _ -> AA.Leaf $ useQCl $ GQuestCl dummyCl

qsHaving = undefined

-- checkIAdv :: GAdv -> GIAdv
-- checkIAdv adv
--   | adv `elem` [Galways_Adv, Gnever_Adv, Gsometimes_Adv] = Gwhen_IAdv
--   | adv `elem` [Geverywhere_Adv, Ghere_Adv, Gsomewhere_Adv, Gthere_Adv] = Gwhere_IAdv
--   | otherwise = Gwhy_IAdv

-- only used in VueJSON.hs — TODO deprecate in favour of ruleQuestions?
getQSFromTrees :: TreeGroups -> GQS
getQSFromTrees whichTG = case whichTG of
  TG {gfS = Just (GUseCl t p cl)} -> GUseQCl t p $ GQuestCl (definiteNP cl)
  TG {gfNP = Just np} -> GExistNPQS presSimul GPPos (indefiniteNP np)
  TG {gfCN = Just cn} -> useQCl $ GQuestCl $ GExistCN cn
  TG {gfVP = Just (GMkVPS t p vp)} -> GUseQCl t p $ GQuestCl $ GPredVP GYou vp -- how to get what or who?
  TG {gfAP = Just ap} -> useQCl $ GQuestIComp (GICompAP ap) (GAdjAsNP ap)
  TG {gfDet = Just det} -> GExistNPQS presSimul GPPos $ GDetNP det
  TG {gfAdv = Just adv} -> useQCl $ GQuestCl (GImpersCl (GUseComp $ GCompAdv adv))
  _ -> useQCl $ GQuestCl dummyCl

------------------------------------------------------------------------------
-- misc tree manipulation

-- constructs "the [unauthorised]:AP [copying, use or modification]:ListCN of NP"
constructTreeAPCNsOfNP :: GListCN -> GConj -> GNP -> GUDS -> Expr
constructTreeAPCNsOfNP cns conj nmod qualUDS = finalTree
  where
    amod = case getRoot qualUDS of
      [] -> dummyAP
      x:_ -> case x of
        (GrootA_ am) -> am
        (GrootDAP_ (GAdjDAP _d am)) -> am
        _ -> dummyAP
    cn = GCN_AP_Conj_CNs_of_NP amod conj cns nmod
    maybedet = case getRoot qualUDS of
      [] -> Nothing
      x:_ -> case x of
        (GrootDAP_ (GAdjDAP (GDetDAP d) _a)) -> Just d
        (GrootDet_ d) -> Just d
        (GrootQuant_ q) -> Just $ GDetQuant q GNumSg
        _ -> Nothing
    finalTree = gf $ case maybedet of
      Nothing -> GMassNP cn
      Just det -> GDetCN det cn

ap2s :: GAP -> GS
ap2s ap = GPredVPS GSomeone (GMkVPS presSimul GPPos (GUseComp (GCompAP ap)))

xcomp2adv :: Gxcomp -> GAdv
xcomp2adv xc = case xc of
  GxcompAdv_ adv -> adv
  _ -> Gxcomp2Adv xc
  -- GxcompN_ : NP -> xcomp ;
  -- GxcompToBeN_ : mark -> cop -> NP -> xcomp ;
  -- GxcompA_ ap -> GPositAdvAdj ap
  -- GxcompA_ccomp_ : AP -> ccomp -> xcomp ;

-- | Two first cases overlap with verbFromUDS: rootV_ and rootVaux_ always become VPS.
-- Rest don't, because this is called for any root ever that we want to turn into VPS.
root2vps :: Groot -> Maybe GVPS
root2vps root = case root of
  GrootV_ t p vp -> Just $ GMkVPS t p vp
  GrootVaux_ t p aux vp -> Just $ GComplAux aux t p vp ;
  GrootN_  np -> Just $ GMkVPS presSimul GPPos $ GUseComp (GCompNP np)
  GrootA_  ap -> Just $ GMkVPS presSimul GPPos $ GUseComp (GCompAP ap)
  GrootAdv_ a -> Just $ GMkVPS presSimul GPPos $ GUseComp (GCompAdv a)
  -- TODO: add cases fora
  -- GrootAdA_, GrootDet_ in the GF grammar, so we can add the cases here
  _            -> Nothing

udsFromacl :: Gacl -> Maybe GUDS
udsFromacl x = case x of
  GaclUDSgerund_ u -> Just u
  GaclUDSpastpart_ u -> Just u
  _ -> error $ "udsFromacl: can't handle " ++ showExpr (gf x)

getRoot :: Tree a -> [Groot]
getRoot rt@(GrootA_ _) = [rt]
getRoot rt@(GrootN_ _) = [rt]
getRoot rt@(GrootV_ _ _ _) = [rt]
getRoot rt@(GrootVaux_ _ _ _ _) = [rt]
getRoot rt@(GrootDet_ _) = [rt]
getRoot rt@(GrootDAP_ _) = [rt]
getRoot rt@(GrootQuant_ _) = [rt]
getRoot rt@(GrootAdA_ _) = [rt]
getRoot rt@(GrootAdv_ _) = [rt]
getRoot rt@(GrootPrep_ _) = [rt]
getRoot rt@(GrootRP_ _) = [rt]
getRoot x = composOpMonoid getRoot x

getNsubj :: Tree a -> [Gnsubj]
getNsubj ns@(Gnsubj_ _) = [ns]
getNsubj (GadvclMarkUDS_ _ _) = []
getNsubj x = composOpMonoid getNsubj x

predVPS :: GNP -> GVPS -> GS
predVPS np (GMkVPS t p vp) = GUseCl t p (GPredVP np vp)
predVPS np vps = useCl $ GPredVP np $ vps2vp vps

vps2vp :: GVPS -> GVP
vps2vp (GMkVPS _t _p vp) = vp
vps2vp (GComplAux _a _t _p vp) = vp
vps2vp (GConjVPS c (GListVPS vps)) = GConjVP c (GListVP (map vps2vp vps))

complVP :: GVP -> GNP -> GVP
complVP (GUseV v) np = GComplV v np
complVP (GAdvVP vp adv) np = GAdvVP (complVP vp np) adv
complVP (GAdVVP adv vp) np = GAdVVP adv (complVP vp np)
complVP (GProgrVP vp) np = GProgrVP (complVP vp np)
complVP (GPassV v) np = GPassVAgent v np
complVP vp@(GUseComp _) np = GComplVP vp np -- last resort, probably something's misparsed somewhere
complVP vp _ = error $ "complVP: doesn't handle argument " ++ showExpr (gf vp)

mkPhrasal :: PGF -> Expr -> Expr
mkPhrasal pgf e = case findType pgf e of
  "N"  -> gf (GMassNP (GUseN (fg e)))
  "A"  -> gf (GPositA (fg e))
  "V"  -> gf (GUseV (fg e))
  _ -> e

toFragment :: GUDS -> GUDFragment
toFragment = GUDS2Fragment