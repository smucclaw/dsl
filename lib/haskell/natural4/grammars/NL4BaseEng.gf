concrete NL4BaseEng of NL4Base =
     CustomSyntaxEng
  ** NL4BaseFunctor - [You, APWho, AdvWho]
  with
      (Syntax=SyntaxEng)
    , (Extend=ExtendEng)
    , (Symbolic=SymbolicEng)
    , (Lexicon=LexiconEng)
    , (Construction=ConstructionEng)
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
      qs = "Does the following hold:" ++ string.s -- make a question in an awkward way
      } ;

    -- : String -> String -> Constraint ;
  lin recoverRPis damage toContents = {
      s = "·" ++ damage.s ++ "is" ++ toContents.s ; -- if constraint isn't parsed, use the original string
      qs = "Is" ++ damage.s ++ toContents.s ++ bindQM
      } ;
  lin recoverRPmath gt age fifty = {
      s = "·" ++ age.s ++ gt.s ++ fifty.s ; -- if constraint isn't parsed, use the original string
      qs = "Is" ++ age.s ++ gt.s ++ fifty.s ++ bindQM
      } ;
  lin recoverUnparsedConstraint string = {
      s = "·" ++ string.s ;
      qs = string.s ++ bindQM
      } ;

  lin recoverUnparsedWho string = MkVPS presSimul POS (mkVP (invarUnparsedV string)) ;

  lin recoverUnparsedCond string = {
      s = lin S string ;
      qs = lin QS {s = \\_ => string.s}
      } ;

  lin recoverUnparsedUpon string = mkVP (invarUnparsedV string) ;

  lin recoverUnparsedAction string = MkVPI (mkVP (invarUnparsedV string)) ;

  lin recoverUnparsedTimeUnit string = mkCN <LexiconEng.day_N ** {s = \\_,_ => "·" ++ string.s} : N> ;

  oper invarUnparsedV : SS -> V = \ss -> invarV ("·" ++ ss.s) ;

}

