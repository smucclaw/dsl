

abstract UDExt = UDApp ,
  Extend [
    S, ExistS, ExistsNP, ExistCN, ExistNPQS, ExistIPQS
    ,ApposNP, AdjAsNP, GerundCN, GerundAdv
    ,ICompAP, IAdvAdv, PredIAdvVP
    ,PredVPS
  ],
  Idiom [
    GenericCl, ImpersCl
  ],
  Sentence [PredSCVP, EmbedS, EmbedQS]
  ** {

--------------------------------------------------------------------------------------------
-- This set of functions used to live in BareRG, but they weren't actually used for parsing
-- They look more like extensions to the RGL, so add them here.

  fun
    SlashCl : Cl -> ClSlash ; -- make a full Cl into ClSlash

    AdvAdv : Adv -> Adv -> Adv ;

    ACard2Det : ACard -> Det ;

    -- SentNP : NP -> SC -> NP ; -- like SentCN but for NP instead

    ComplVP : VP -> NP -> VP ; -- "eat enthusiastically pizza"--the first argument is already VP. TODO improve NLG.hs so we can remove this
    ComplSVP : VP -> S -> VP ; -- [assess]:VP [if it is a NDB]:S
    PrepVP : VP -> Prep -> VP ; -- like VPSlashPrep but on VPs. Probably this is also better to handle by other means and should be removed later.
    -- ComplA : A -> NP -> AP ; -- applicable to X  (TODO: where to put prep?)
    -- ComplN : N -> NP -> CN ; -- mother of X  (TODO: where to put prep?)

    MkA2 : A -> Prep -> A2 ;
    MkN3 : N -> Prep -> Prep -> N3;

    PostAdvS : S -> Adv -> S ; -- [we go [where it's warm]:Adv ]:S

    You : NP ; -- Many of the rules talk about a You, so just add it here.
    Someone : NP ;

    -- This version of ComplVV allows "may have occurred"
    -- The one in core RGL would only become "has been allowed to occur"
   -- ComplVV     : VV  -> Ant -> Pol -> VP -> VP ;
    ComplAux    : aux -> Temp -> Pol -> VP -> VPS ;

    -- Make the UDS categories that have Adv as a lincat, but aren't constructed from Adv, into Adv
    -- the rest can just peel off the constructor, e.g. obl with `obl_ : Adv -> obl`
    advcl2Adv : advcl -> Adv ;
    acl2Adv : acl -> Adv ;
    xcomp2Adv : xcomp -> Adv ;
    csubj2Adv : csubj -> Adv ; -- TODO also add csubj2SC?

-- Aarne's additions
  RS_that_NP_VP : NP -> VP -> RS ;
{-  apply_concurrently_VP : VP ;
  does_not_apply_to_V : V ;
  on_or_after_Prep : Prep ;
  prior_to_the_occurrence_of_Prep : Prep ;
  that_other_Det : Det ;

  CN_CN_relating_to_NP : CN -> NP -> CN ;
  CN_obligation_of_NP_to_VP : NP -> VP -> CN ;
  NP_all_the_CN_RS : CN -> RS -> NP ;
  NP_the_loss_of_any_CN_RS : CN -> RS -> NP ;
  NP_the_unauthorised_N2_of_NP : CN -> NP -> NP ;
  NP_the_unauthorised_ConjN2_of_NP : [CN] -> NP -> NP ;
  Adv_Adv__but_in_any_case_Adv : Adv -> Adv -> Adv ;
  Adv_at_the_time_NP_notifies_NP : NP -> NP -> Adv ;
  RS_to_whom_NP_VP : NP -> VP -> RS ;
  VP_assesses__Adv__that_S : Adv -> S -> VP ;
  VP_may__SeqAdv__VP : [Adv] -> VP -> VP ;
  VP_must__SeqAdv__VP : [Adv] -> VP -> VP ;
  VP_notify_NP_of_NP : NP -> NP -> VP ; -}

--------------------------------------------------------------------------------------------
-- This set of functions is for the more high-level NLG stuff
-- They mimic more the structure of the Natural L4 abstract syntax
  cat
    UDFragment ;
    [UDFragment]{2};

  fun

    UDS2Fragment : UDS -> UDFragment ;

    -- Compositional,
    -- Add a UDS as an adverbial
    Cond : UDS -> UDFragment -> UDFragment ; -- the king must sing if the prime minister is happy
    Temporal : Adv -> UDFragment -> UDFragment ; -- the king must sing after noon
    Given : UDS -> UDFragment -> UDFragment ; -- the king must sing given that the prime minister is happy
    Upon : (becoming_aware : UDS) ->
           (king_may_sing : UDFragment) ->
           UDFragment ; -- Upon becoming aware, the king may sing

    -- Standalone sentence of each field
    CondStandalone,
      TemporalStandalone,
      GivenStandalone,
      UponStandalone : UDS -> UDFragment ;

    -- Templates
    CondTemporal : (cond : UDS) -> (temp : Adv) -> (king_must_sing : UDFragment) -> UDFragment ; -- if the prime minister is happy, [the king must sing] by noon.

    -- the king must sing by noon, if the following conditions hold:
    -- * PM is happy
    -- * queen had a nice breakfast

    CondUpon : (cond, upon : UDS) -> (king_must_sing: UDFragment) -> UDFragment ;
    -- if the prime minister is happy, upon opening the door, [the king must sing] .

    CondGiven : (cond, given : UDS) -> (king_must_sing: UDFragment) -> UDFragment ;
    -- [the king must sing], if the following conditions hold:
      -- * the prime minister is happy, the door is open.

    -- CondGivenTemporal : (cond, given, temp : UDS) -> (king_must_sing : UDFragment) -> UDFragment ;

    -- CondTemporalUpon : (cond, temp, upon : UDS) -> (king_must_sing : UDFragment)  -> UDFragment ;
    -- -- if the prime minister is happy, [the king must sing] after lunch upon opening the door

    -- CondGivenUpon : (cond, given, upon : UDS) -> (king_must_sing : UDFragment)  -> UDFragment ;
    -- -- if the prime minister is happy, and the queen had a nice breakfast, [the king must sing] upon opening the door

    -- GF funs that correspond to LS.Types constructors
    -- Deontic
    DMust, DMay, DShant : UDS -> UDS ;

    -- MyToken
    Every,
    TokAll,
    Party : NP -> NP ;

    -- RPRel
    RPis,
    RPeq,
    RPlt,
    RPlte,
    RPgt,
    RPgte,
    RPelem,
    RPnotElem : NP -> UDS -> UDFragment ; -- TODO: types?

    Means : NP -> UDS -> UDFragment ; -- foo MEANS bar -- what types to use??? "eyes IS (left IS blue AND right IS brown)"
    HornClause2 : (breach_is_severe : UDFragment) -> (data_is_lost : S) -> UDFragment ; -- breach is severe WHEN data is lost

    Who : UDS -> NP -> NP ; -- EVERY king WHO is a singer
    subjAction : NP -> UDS -> UDFragment ;

  -- AnyAll library in GF
    CN_AP_Conj_CNs_of_NP : AP -> Conj -> [CN] -> NP -> CN ; -- unauthorised access or copying of personal data

  -- Aarne
    Adv_no_later_than_Num_calendar_days_after_the_day_UDS : Numeral -> UDS -> UDFragment ;

}

{-
In NLG.hs:

subjA' <- parseSubj (subj rl) -- "qualifying person"

We can already assume that this is a noun phrase kind of thing
(despite that we parsed it into UDS)

We pattern match the constructors root_only and rootN_
and whatever is the argument of rootN_, is an actual NP

root_only (rootN_ (MassNP (AdjCN (PresPartAP (UseV qualify_V))) (UseN person_N)))

-}
