concrete NL4BaseMay of NL4Base =
     CustomSyntaxMay
  ** NL4BaseFunctor
  with
      (Syntax=SyntaxMay)
    , (Extend=ExtendMay)
    , (Symbolic=SymbolicMay)
    , (Lexicon=LexiconMay)
    , (Construction=ConstructionMay)
    , (CustomSyntax=CustomSyntaxMay)
   ** open Coordination, Prelude, ParadigmsMay, (R=ResMay) in {


  lin RPConstraint cond on date =
      let onDate : Adv = SyntaxMay.mkAdv on date ;
       in {s = mkS onDate cond.s ; qs = lin QS (mkS onDate <lin S cond.qs : S>)} ;

  oper
    linWho : ExtendMay.VPS -> Str = \vps -> vps.s ;

-----------------------------------------------------------------------------
-- Month names
lin
    Jan = ss "Jan" ; Feb = ss "Feb" ; Mar = ss "Mar" ; Apr = ss "Apr" ;
    May = ss "Mai" ; Jun = ss "Jun" ; Jul = ss "Jul" ; Aug = ss "Ogos" ;
    Sep = ss "Sep" ; Oct = ss "Oct" ; Nov = ss "Nov" ; Dec = ss "Dis" ;

-----------------------------------------------------------------------------
-- Instead of crashing, every category should have a dummy constructor where to put a string

  lin
    recoverUnparsedPrePost string = {
      s = "·" ++ string.s ; -- if PrePost isn't parsed, use the original string
      qs = "Adakah perkara berikut berlaku:" ++ string.s -- make a question in an awkward way
      } ;

    -- : String -> String -> Constraint ;
    recoverRPis damage toContents = {
      s = "·" ++ damage.s ++ "is" ++ toContents.s ; -- if constraint isn't parsed, use the original string
      qs = "Is" ++ damage.s ++ toContents.s ++ bindQM
      } ;
    recoverUnparsedConstraint string = recoverUnparsedPrePost string ;

    recoverUnparsedWho string = MkVPS presSimul POS (mkVP (invarV string.s)) ;

    recoverUnparsedCond string = {
      s = lin S string ;
      qs = lin QS string
      } ;

    recoverUnparsedUpon string = mkVP (invarV string.s) ;

    recoverUnparsedSubj string = symb string ;

    recoverUnparsedAction string = MkVPI (mkVP (invarV string.s)) ;

    recoverUnparsedTimeUnit string = mkCN <LexiconMay.day_N ** {s = \\_ => "·" ++ string.s} : N> ;

}
