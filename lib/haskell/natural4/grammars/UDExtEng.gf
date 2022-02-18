

concrete UDExtEng of UDExt = UDAppEng ** open Prelude, SyntaxEng, (P=ParadigmsEng), ExtendEng in {

  lin
    -- : UDS -> UDFragment ; -- the UDS is like "become aware …", this becomes "upon becoming aware"
    Upon uds = PredVPS (mkNP (P.mkPN "upon")) uds.pred.fin ;

    -- : NP -> UDS -> UDFragment ;
    subjAction subj uds = PredVPS subj uds.pred.fin ;


-- Aarne
    -- : Num -> UDS -> UDFragment ;
    Adv_no_later_than_Num_calendar_days_after_the_day_UDS num uds =
      mkAdv
        (P.mkPrep "no later than")
        (mkNP (mkNP num (P.mkN "calendar day"))
              (lin Adv {s = "after the day" ++ linUDS uds})) ;



}
