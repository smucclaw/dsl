abstract StandardLexicon = NL4Base ** {

  -- Collection of "basic" words that we expect to appear across multiple documents in legal domain
  -- Very ad hoc at the moment, should consult a proper legal corpus, analyse valencies and complement types etc.
  -- This module should be the really high quality stuff
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

-----------------------------------------------------------------------------
-- Time units, currencies, …

    Day_Unit
    , Month_Unit
    , Year_Unit : TimeUnit ;

    Day1, Day2, Day3, Day4, Day5, Day6, Day7, Day8, Day9, Day10, Day11, Day12, Day13, Day14, Day15, Day16, Day17, Day18, Day19, Day20, Day21, Day22, Day23, Day24, Day25, Day26, Day27, Day28, Day29, Day30, Day31 : Day ;

    Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec : Month ;
    
    Y0, Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10 : YearComponent ;

    MkYear : (x1,_,_,x4: YearComponent) -> Year ;


}