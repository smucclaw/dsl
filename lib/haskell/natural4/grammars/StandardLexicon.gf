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

    Day1, Day2, Day3, Day4, Day5, Day6, Day7, Day8, Day9, Day10,
      Day11, Day12, Day13, Day14, Day15, Day16, Day17, Day18, Day19, Day20,
      Day21, Day22, Day23, Day24, Day25, Day26, Day27, Day28, Day29, Day30, Day31 : Day ;

   Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec : Month ;

   Year1950, Year1951, Year1952, Year1953, Year1954, Year1955, Year1956, Year1957, Year1958, Year1959,
      Year1960, Year1961, Year1962, Year1963, Year1964, Year1965, Year1966, Year1967, Year1968, Year1969,
      Year1970, Year1971, Year1972, Year1973, Year1974, Year1975, Year1976, Year1977, Year1978, Year1979,
      Year1980, Year1981, Year1982, Year1983, Year1984, Year1985, Year1986, Year1987, Year1988, Year1989,
      Year1990, Year1991, Year1992, Year1993, Year1994, Year1995, Year1996, Year1997, Year1998, Year1999,
      Year2000, Year2001, Year2002, Year2003, Year2004, Year2005, Year2006, Year2007, Year2008, Year2009,
      Year2010, Year2011, Year2012, Year2013, Year2014, Year2015, Year2016, Year2017, Year2018, Year2019,
      Year2020, Year2021, Year2022, Year2023, Year2024, Year2025, Year2026, Year2027, Year2028, Year2029,
      Year2030, Year2031, Year2032, Year2033, Year2034, Year2035, Year2036, Year2037, Year2038, Year2039,
      Year2040, Year2041, Year2042, Year2043, Year2044, Year2045, Year2046, Year2047, Year2048, Year2049,
      Year2050, Year2051, Year2052, Year2053, Year2054, Year2055, Year2056, Year2057, Year2058, Year2059 : Year ;


}