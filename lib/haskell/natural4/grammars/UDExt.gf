

abstract UDExt = UDApp ** {

  cat
    UDFragment ;
  fun
    Upon : UDS -> UDFragment ; -- the UDS is like "become aware …", this becomes "upon becoming aware"
    -- TODO: what if we already parsed "becoming aware"? can disable ProgrVP in UDAppEng.labels
    -- so it never becomes "becoming aware"?

    subjAction : NP -> UDS -> UDFragment ;

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
