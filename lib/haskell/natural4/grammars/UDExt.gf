

abstract UDExt = UDApp ** {

  cat
    UDFragment ;
  fun
    Upon : (becoming_aware : UDS) ->
           (king_may_sing : UDFragment) ->
           UDFragment ; -- Upon becoming aware, the king may sing

    Must, May, Shant : UDS -> UDS ;

    Who : UDS -> NP -> NP ; -- EVERY king WHO is a singer
    Every : NP -> NP ;

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
