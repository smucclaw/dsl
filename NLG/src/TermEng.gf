concrete TermEng of Term = open
  SyntaxEng,
  ParadigmsEng,
  (R=ResEng),
  ParamX,
  Prelude in {
  lincat
    Kind = LinKind;
    Term = NP ;
    [Term] = ListNP ;
    Conjunction = Conj ;
    Determiner = LinDet ;

  linref
    -- To make discontinuous categories show properly in the shell
    Kind = \x -> (mkUtt (merge x)).s ;
    Term = \x -> (mkUtt (np x)).s ;

  lin
        -- Conjunctions
    And = and_Conj ;
    Or = or_Conj ;
    BaseTerm = mkListNP ; -- : NP -> NP -> ListNP ;
    ConsTerm = mkListNP ; -- : NP -> ListNP -> ListNP ;
    ConjTerm = mkNP ;     -- : Conj -> ListNP -> NP

    -- Determiners
    ASg = table {
      Mass => emptyDet ;
      Count => aSg_Det ;
      Plural => aPl_Det
      } ;
    APl = table {
      Mass => aPl_Det ; -- or emptyDet ;
      Count => aPl_Det ;
      Plural => aPl_Det
      } ;
    TheSg = table {
      Mass => theSg_Det ;
      Count => theSg_Det ;
      Plural => thePl_Det
      } ;
    ThePl = table {
      Mass => thePl_Det ;
      Count => thePl_Det ;
      Plural => thePl_Det
      } ;
    Any = table {
      Mass => anySg_Det ;
      Count => anySg_Det ;
      Plural => anyPl_Det
      } ;
    All = table {
      Mass => allSg_Det ;
      Count => all_Det ;
      Plural => all_Det
      } ;

    -- Kinds, Terms and Properties
    -- : Determiner -> Kind -> Term
    TDet = term ; -- using our oper 'term', defined at the end of file

    -- Kinds with complements
    -- : Kind -> Term -> Kind ;    -- liquidation of the company
    -- Complement goes to cn field, not to adv field.
    ComplKind kind term = linkind (mkCN (merge kind) (adv part_Prep (np term))) ;

  param
    KType = Mass | Count | Plural ;

  oper
    -- Adv
    -- shorthand: mkAdv is imported from two modules, so it has to be qualified
    adv : Prep -> NP -> Adv = SyntaxEng.mkAdv ;

    -- Kind
    LinKind : Type = {
      cn : CN ;
      adv : Adv ;
      k : KType
      } ;

    mkKind : N -> LinKind = \n -> linkind (mkCN n) ;

    linkind : CN -> LinKind = \cn -> {
      cn = cn ;
      adv = emptyAdv ;
      k = Count
      } ;

    -- Term
    LinTerm : Type = NP ;

    -- Determiner
    -- We don't need the full category of Det from RGL
    DetLite : Type = {s : Str ;  n : ParamX.Number} ;
    LinDet : Type = KType => DetLite ;

    ----------------
    -- Empty phrases

    emptyAdv : Adv = ParadigmsEng.mkAdv [] ;
    emptyAP : AP = <mkAP (mkA []) : AP> ** {s = \\_ => []} ;
    emptyNP : NP = <mkNP (mkN []) : NP> ** {s = \\_ => []} ;

    ---------------
    -- Determiners

    -- Resource Grammar Library doesn't have any_Det, so we make it ourselves.
    -- Determiners are supposed to be closed class, so the constructor isn't
    -- exported in the API. (Silly, if you ask me.)
    -- The options are: open a low-level module and use the hidden constructor, or do this hack.
    anySg_Det : Det = a_Det ** { -- Extend a_Det: keyword ** is record extension
      s = "any"                -- Keep other fields from a_Det, but replace s with "any"
      } ;
    anyPl_Det : Det = aPl_Det ** {
      s = "any"
      } ;
    all_Det : Det = aPl_Det ** {
      s = "all"
      } ;
    allSg_Det : Det = a_Det ** {
      s = "all"
      } ;
    emptyDet : Det = a_Det ** {
      s = []
      } ;


    neither7nor_DConj : Conj = mkConj "neither" "nor" singular ;

    -------------------------------
    -- Other useful syntactic opers

    -- Default use for NPs
    np : LinTerm -> NP = id NP ;

    -- copied from RGL to work with DetLite
    -- This is BAD PRACTICE !!! TODO: add a strategic oper to English RG
    detCNLite : DetLite -> CN -> NP = \det,cn -> lin NP {
      s = \\c => det.s ++ cn.s ! det.n ! R.npcase2case c ;
      a = R.agrgP3 det.n cn.g
      } ;

    -- Combine Determiner and Kind into a Term
    term : LinDet -> LinKind -> LinTerm = \dets,kind ->
      detCNLite (dets ! kind.k) (merge kind) ;

    defTerm : LinKind -> NP = \k -> mkNP (merge k) ;

    -- Merge the discontinuous Kind into a single CN
    merge : LinKind -> CN = \kind -> mkCN kind.cn kind.adv ;

}
