

concrete UDExtEng of UDExt = UDAppEng ** open Prelude, SyntaxEng, (P=ParadigmsEng), ExtendEng in {

  lin
    -- : UDS -> UDFragment ; -- the UDS is like "become aware …", this becomes "upon becoming aware"
    Upon uds = PredVPS (mkNP (P.mkPN "upon")) uds.pred.fin ;

    -- : NP -> UDS -> UDFragment ;
    subjAction subj uds = PredVPS subj uds.pred.fin ;



}
