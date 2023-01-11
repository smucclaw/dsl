concrete TermEng of Term = open
  SyntaxEng,
  ParadigmsEng,
  AdjectiveEng,
  (R=ResEng),
  (C=ConjunctionEng),
  Prelude in {
  lincat
    Kind = LinKind;
    [Kind] = ListLinKind ;
    Term = NP ;
    [Term] = ListNP ;
    Conjunction = Conj ;
    Determiner = LinDet ;
    Property = LinProp ;
    [Property] = ListLinProp ;

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

    BaseProperty = base AP ListAP mkListAP ; -- : AP -> AP -> ListAP
    ConsProperty = cons AP ListAP mkListAP ; -- : AP -> ListAP -> ListAP
    ConjProperty co ps = \\pol =>            -- : Conj -> ListAP -> AP
      mkAP co (ps ! pol) ; -- conjunctions don't change, because negations can be lexical.
                           -- e.g. "involuntary and unjustified"

    BaseKind k l = {
      cn = C.BaseCN (merge k) (merge l) ;
      k = case <k.k, l.k> of {
        <Plural,_>|<_,Plural> => Plural ;
        <Mass,Mass> => Mass ;
        _ => Count }
      } ;

    ConsKind k l = {
      cn = C.ConsCN (merge k) l.cn ;
      k = case <k.k, l.k> of {
        <Plural,_>|<_,Plural> => Plural ;
        <Mass,Mass> => Mass ;
        _ => Count }
      } ;

    ConjKind co ks = {
      cn = C.ConjCN co ks.cn ;
      adv = emptyAdv ;
      k = ks.k
      } ;

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
    AnyOther = table {
      Plural => any_otherPl_Det ;
      _ => any_other_Det
      } ;
    This = table {
      Plural => these_Det ;
      _ => this_Det
      } ;
    These = table {
      Mass => this_Det ;
      _ => these_Det
      } ;
    That = table {
      Plural => those_Det ;
      _ => that_Det
      } ;
    Those = table {
      Mass => that_Det ;
      _ => those_Det
      } ;
    Certain = table {
      Plural => certainPl_Det ;
      _ => certain_Det
      } ;


    -- Kinds, Terms and Properties
    -- : Determiner -> Kind -> Term
    TDet = term ; -- using our oper 'term', defined at the end of file

    PNeg prop = table {
      --Neg => prop ! Pos  -- double negation = positive
      _ => prop ! Neg
      } ;

    -- : Property -> Kind -> Kind ;
    KProperty props kind = let prop : AP = ap props in
      case prop.isPre of {
        True => kind ** { -- voluntary termination
          cn = mkCN prop kind.cn
          } ;
        False => kind ** { -- termination for the benefit of the Company
          adv = cc2 kind.adv (ap2adv prop)
          }
      } ;

    -- Kinds with complements
    -- : Kind -> Term -> Kind ;    -- liquidation of the company
    -- Complement goes to cn field, not to adv field.
    ComplKind kind term = kind ** {
      cn = mkCN (merge kind) (adv part_Prep (np term)) ;
      adv = emptyAdv
      } ;

  param
    KType = Mass | Count | Plural ;
    Polarity = Pos | Neg ;
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
    kind : Str -> LinKind = \str -> mkKind (mkN str) ;
    adjkind : Str -> Str -> LinKind =
      \a,n -> linkind (mkCN (mkAP (mkA a)) (mkN n)) ;
    ofkind : Str -> Str -> LinKind =
      \n,p -> linkind (mkCN (mkN n) (adv part_Prep (mkNP (mkN p)))) ;
    linkind : CN -> LinKind = \cn -> {
      cn = cn ;
      adv = emptyAdv ;
      k = Count
      } ;

    -- Conjunctions of Kinds
    ListLinKind : Type = {
      cn : C.ListCN ;
      k : KType ;
      } ;

    -- Term
    LinTerm : Type = NP ;

    -- Determiner
    -- We don't need the full category of Det from RGL
    DetLite : Type = {s : Str ; n : R.Number} ;
    LinDet : Type = KType => DetLite ;

    -- Property
    LinProp : Type = Polarity => AP ;
      -- TODO: see if needed {vps : VPSlash ; arg : NP ; qualif : AP ; adv : Adv ; predType : PredType} ;
    prop = overload {
      prop : Str -> Str -> LinProp = \pos,neg -> table {
        Pos => mkAP (mkA pos) ;
        Neg => mkAP (mkA neg)
        } ;
      prop : Str -> LinProp = \pos -> table {
        Pos => mkAP (mkA pos) ;
        Neg => mkAP (mkA ("not" ++ pos))
        }
      } ;

    -- Conjunctions
    ListLinProp : Type = Polarity => ListAP ;

    -- This is how you do polymorphism in GF.
    -- A bit unwieldy.

    base : (A : Type) -> (ListA : Type) ->
      (A -> A -> ListA) ->
      (a, b : Polarity => A) ->
      Polarity => ListA =
      \_,_,mkList,a,b -> \\pol => mkList (a ! pol) (b ! pol) ;

    cons : (A : Type) -> (ListA : Type) ->
      (A -> ListA -> ListA) ->
      (a : Polarity => A) ->
      (as : Polarity => ListA) ->
      Polarity => ListA =
      \_,_,mkList,a,as -> \\pol => mkList (a ! pol) (as ! pol) ;

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

    -- Generalize this into opers
    plDet : Str -> Det = \str -> aPl_Det ** {s = str} ;
    sgDet : Str -> Det = \str -> a_Det ** {s = str} ;

    anyPl_Det : Det = plDet "any" ;
    all_Det : Det = plDet "all" ;
    allSg_Det : Det = sgDet "all" ;
    emptyDet : Det = sgDet [] ;
    any_other_Det : Det = sgDet "any other" ;
    any_otherPl_Det : Det = plDet "any other" ;
    certain_Det : Det = sgDet "certain" ;
    certainPl_Det : Det = plDet "certain" ;

    neither7nor_DConj : Conj = mkConj "neither" "nor" singular ;

    -------------------------------
    -- Other useful syntactic opers

    -- Default use for NPs
    np : LinTerm -> NP = id NP ;

    -- Default use for most APs: pick the positive version (e.g. "voluntary", not "involuntary")
    ap : LinProp -> AP = \lp -> lp ! Pos ;

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

    ap2adv : AP -> Adv = \ap -> lin Adv (mkUtt ap) ; -- RGL has no AP->Adv fun
    adv2ap : Adv -> AP = AdjectiveEng.AdvAP emptyAP ; -- RGL has no Adv->AP fun
}
