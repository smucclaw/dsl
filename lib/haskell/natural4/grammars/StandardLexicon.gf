abstract StandardLexicon = NL4Base ** {

  -- Collection of "basic" words that we expect to appear across multiple documents in legal domain
  -- Very ad hoc at the moment, should consult a proper legal corpus, analyse valencies and complement types etc.
  -- This module should be the really high quality stuff

  -- Words from closed classes, like prepositions etc.
  fun
    within_Prep : Prep ;
    only_AdA : AdA ; -- within -> only within

  -- Open classes, like nouns, verbs etc.
  fun
    organisation
    , agency
    , loss : CN ;

    demand
    , perform
    , become : V2 ;

    assess : VS ;
    become_aware : VS ;

    apply
    , occur
    , respond : VP ; -- in corpus, takes oblique complements
                    -- TODO: create verb subcats from lexicon based on in which context they appear
                    -- "respond" :| []  -> respond : VP
                    -- "demand" :| [ "an explanation for your inaction" ] -> demand : V2, NP complement
                    -- "assess" :| [ "if it is a Notifiable Data Breach" ] -> assess : VS, S complement
                    -- TODO: is it overkill to have keywords in language? assess,IF,it is a NDB
    covered : AP ;
    ensuing
    , caused_by : NP -> AP ;

    NP_caused_by_PrePost : NP -> PrePost ;
    NP_caused_NP_to_VP_Prep_PrePost : (animal : NP) -> (water : NP) -> (escape : VP) -> (from : Prep) -> PrePost ;

}