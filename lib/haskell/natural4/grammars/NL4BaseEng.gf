concrete NL4BaseEng of NL4Base =
     CustomSyntaxEng
  ** NL4BaseFunctor - [You]
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
-- Instead of crashing, every category should have a dummy constructor where to put a string


  lin recoverUnparsedPrePost string = {
      s = "·" ++ string.s ; -- if PrePost isn't parsed, use the original string
      qs = "Did the following happen:" ++ string.s -- make a question in an awkward way
      } ;

    -- : String -> String -> Constraint ;
  lin recoverRPis damage toContents = {
      s = "·" ++ damage.s ++ "is" ++ toContents.s ; -- if constraint isn't parsed, use the original string
      qs = "Is" ++ damage.s ++ toContents.s ++ "?"
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



}

