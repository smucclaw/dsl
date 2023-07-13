concrete NL4BaseEng of NL4Base =
     CustomSyntaxEng
  ** NL4BaseFunctor - [You, APWho, AdvWho]
  with
      (Syntax=SyntaxEng)
    , (Extend=ExtendEng)
    , (Symbolic=SymbolicEng)
    , (Lexicon=LexiconEng)
    , (CustomSyntax=CustomSyntaxEng)
   ** open Coordination, Prelude, ParadigmsEng, (R=ResEng) in {


  lin You = you_NP | mkNP (ParadigmsEng.mkN "You" "You" "Your" "Your") ;

  lin RPConstraint cond on date =
      let onDate : Adv = SyntaxEng.mkAdv on date in {
        s = CustomSyntaxEng.postAdvS cond.s onDate ;
        qs = CustomSyntaxEng.postAdvQS cond.qs onDate
        } ;


  oper
    linWho : ExtendEng.VPS -> Str = \vps ->
    let vpss = vps.s ! R.ODir False ! R.agrP3 R.Sg
      in vpss.fin ++ vpss.inf ;

-----------------------------------------------------------------------------
-- Month names

  lin
    Jan = ss "Jan" ; Feb = ss "Feb" ; Mar = ss "Mar" ; Apr = ss "Apr" ;
    May = ss "May" ; Jun = ss "Jun" ; Jul = ss "Jul" ; Aug = ss "Aug" ;
    Sep = ss "Sep" ; Oct = ss "Oct" ; Nov = ss "Nov" ; Dec = ss "Dec" ;

-----------------------------------------------------------------------------
-- Hack to get AP parsed into VPS
    -- : AP -> Who ; -- hack
    APWho alcoholic = lin VPS {s = \\_o,a => {fin = alcoholic.s ! a ; inf = []}} ;
    AdvWho in_part = lin VPS {s = \\_o,a => {fin = in_part.s ; inf = []}} ;

-----------------------------------------------------------------------------
-- Instead of crashing, every category should have a dummy constructor where to put a string


  lin recoverUnparsedPrePost string = {
      s = "·" ++ string.s ; -- if PrePost isn't parsed, use the original string
      qs = string.s -- rely on the the reader to connect the dots and make a question of the original string
      } ;

    -- : String -> String -> Constraint ;
  lin recoverRPis damage toContents = {
      s = "·" ++ damage.s ++ "is" ++ toContents.s ; -- if constraint isn't parsed, use the original string
      qs = "Is" ++ damage.s ++ toContents.s ++ bindQM
      } ;
  lin recoverUnparsedConstraint string = recoverUnparsedPrePost string ;

  lin recoverUnparsedWho string = MkVPS presSimul POS (mkVP (invarV string.s)) ;

  lin recoverUnparsedCond string = {
      s = lin S string ;
      qs = lin QS {s = \\_ => string.s}
      } ;

  lin recoverUnparsedUpon string = mkVP (invarV string.s) ;

  lin recoverUnparsedSubj string = symb string ;

  lin recoverUnparsedAction string = MkVPI (mkVP (invarV string.s)) ;

  lin recoverUnparsedTimeUnit string = mkCN <LexiconEng.day_N ** {s = \\_,_ => "·" ++ string.s} : N> ;

}

