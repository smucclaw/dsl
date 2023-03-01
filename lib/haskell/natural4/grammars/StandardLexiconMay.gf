concrete StandardLexiconMay of StandardLexicon = NL4BaseMay **
  open
    SyntaxMay
  , ParadigmsMay
  , (R=ResMay)
  , Prelude
  in {

  -- Collection of "basic" words that we expect to appear across multiple documents in legal domain
  -- Very ad hoc at the moment, should consult a proper legal corpus, analyse valencies and complement types etc.
  -- This module should be the really high quality stuff
  lin
    organisation = mkCN (mkN ("organisasi"|"Organisasi")) ;
    agency = mkCN (mkN ("agensi"|"Agensi")) ;
    loss = mkCN (mkN "kerugian") ;

    demand = mkV2 "tuntutan" ;
    perform = mkV2 "melaksanakan" ;
    become = mkV2 "menjadi" ;
    assess = mkVS (mkV "menilai") ;

    apply = mkVP (mkV "memohon") ;
    occur = mkVP (mkV "berlaku") ;
    respond = mkVP (mkV "membalas") ;

    covered = mkAP (mkA ("dilindungi"|"Dilindungi")) ;
    ensuing np = mkAP (strA2 "seterusnya") <lin NP np : NP>  ;
    caused_by np = mkAP (mkA2 (mkA "disebabkan") by8agent_Prep) <lin NP np : NP> ;

    NP_caused_by_PrePost np = {
      s = npStr np ++ "disebabkan oleh" ;
      qs = "Adakah" ++ npStr np ++ "disebabkan oleh"
      } ;

    NP_caused_NP_to_VP_Prep_PrePost np water escape from =
      let cl : Cl = mkCl <np : NP> (mkVP cause_V2V <water : NP> <escape : VP>) ;
          cls : ClSlash = mkClSlash cl <from : Prep> ;
          qcl : QCl = hackQCl cls ;
          ss : SSlash = mkSSlash (mkTemp pastTense simultaneousAnt) positivePol cls ;
          qs : QS = mkQS pastTense qcl ;
      in {s = ss.s ; qs = (mkUtt qs).s} ;

  oper
    cause_V2V : V2V = mkV2V "sebab" ;

-- hack: just to get "does NP cause water to escape from", not "whom does NP cause water to escape from"
    hackQCl : ClSlash -> QCl = \clSlash -> mkQCl emptyIP clSlash ;

    emptyIP : IP = whatSg_IP ** {s = \\_ => []} ;

-----------------------------------------------------------------------------
-- Time units, currencies, …
lin
    Jan = ss "Jan" ;
    Feb = ss "Feb" ;
    Mar = ss "Mar" ;
    Apr = ss "Apr" ;
    May = ss "Mai" ;
    Jun = ss "Jun" ;
    Jul = ss "Jul" ;
    Aug = ss "Ogos" ;
    Sep = ss "Sep" ;
    Oct = ss "Oct" ;
    Nov = ss "Nov" ;
    Dec = ss "Dis" ;

  lin Day1 = ss "1" ;
      Day2 = ss "2" ;
      Day3 = ss "3" ;
      Day4 = ss "4" ;
      Day5 = ss "5" ;
      Day6 = ss "6" ;
      Day7 = ss "7" ;
      Day8 = ss "8" ;
      Day9 = ss "9" ;
      Day10 = ss "10" ;
      Day11 = ss "11" ;
      Day12 = ss "12" ;
      Day13 = ss "13" ;
      Day14 = ss "14" ;
      Day15 = ss "15" ;
      Day16 = ss "16" ;
      Day17 = ss "17" ;
      Day18 = ss "18" ;
      Day19 = ss "19" ;
      Day20 = ss "20" ;
      Day21 = ss "21" ;
      Day22 = ss "22" ;
      Day23 = ss "23" ;
      Day24 = ss "24" ;
      Day25 = ss "25" ;
      Day26 = ss "26" ;
      Day27 = ss "27" ;
      Day28 = ss "28" ;
      Day29 = ss "29" ;
      Day30 = ss "30" ;
      Day31 = ss "31" ;
  lin Year1950 = ss "1950" ; Year1951 = ss "1951" ; Year1952 = ss "1952" ; Year1953 = ss "1953" ; Year1954 = ss "1954" ; Year1955 = ss "1955" ; Year1956 = ss "1956" ; Year1957 = ss "1957" ; Year1958 = ss "1958" ; Year1959 = ss "1959" ;
      Year1960 = ss "1960" ; Year1961 = ss "1961" ; Year1962 = ss "1962" ; Year1963 = ss "1963" ; Year1964 = ss "1964" ; Year1965 = ss "1965" ; Year1966 = ss "1966" ; Year1967 = ss "1967" ; Year1968 = ss "1968" ; Year1969 = ss "1969" ;
      Year1970 = ss "1970" ; Year1971 = ss "1971" ; Year1972 = ss "1972" ; Year1973 = ss "1973" ; Year1974 = ss "1974" ; Year1975 = ss "1975" ; Year1976 = ss "1976" ; Year1977 = ss "1977" ; Year1978 = ss "1978" ; Year1979 = ss "1979" ;
      Year1980 = ss "1980" ; Year1981 = ss "1981" ; Year1982 = ss "1982" ; Year1983 = ss "1983" ; Year1984 = ss "1984" ; Year1985 = ss "1985" ; Year1986 = ss "1986" ; Year1987 = ss "1987" ; Year1988 = ss "1988" ; Year1989 = ss "1989" ;
      Year1990 = ss "1990" ; Year1991 = ss "1991" ; Year1992 = ss "1992" ; Year1993 = ss "1993" ; Year1994 = ss "1994" ; Year1995 = ss "1995" ; Year1996 = ss "1996" ; Year1997 = ss "1997" ; Year1998 = ss "1998" ; Year1999 = ss "1999" ;
      Year2000 = ss "2000" ; Year2001 = ss "2001" ; Year2002 = ss "2002" ; Year2003 = ss "2003" ; Year2004 = ss "2004" ; Year2005 = ss "2005" ; Year2006 = ss "2006" ; Year2007 = ss "2007" ; Year2008 = ss "2008" ; Year2009 = ss "2009" ;
      Year2010 = ss "2010" ; Year2011 = ss "2011" ; Year2012 = ss "2012" ; Year2013 = ss "2013" ; Year2014 = ss "2014" ; Year2015 = ss "2015" ; Year2016 = ss "2016" ; Year2017 = ss "2017" ; Year2018 = ss "2018" ; Year2019 = ss "2019" ;
      Year2020 = ss "2020" ; Year2021 = ss "2021" ; Year2022 = ss "2022" ; Year2023 = ss "2023" ; Year2024 = ss "2024" ; Year2025 = ss "2025" ; Year2026 = ss "2026" ; Year2027 = ss "2027" ; Year2028 = ss "2028" ; Year2029 = ss "2029" ;
      Year2030 = ss "2030" ; Year2031 = ss "2031" ; Year2032 = ss "2032" ; Year2033 = ss "2033" ; Year2034 = ss "2034" ; Year2035 = ss "2035" ; Year2036 = ss "2036" ; Year2037 = ss "2037" ; Year2038 = ss "2038" ; Year2039 = ss "2039" ;
      Year2040 = ss "2040" ; Year2041 = ss "2041" ; Year2042 = ss "2042" ; Year2043 = ss "2043" ; Year2044 = ss "2044" ; Year2045 = ss "2045" ; Year2046 = ss "2046" ; Year2047 = ss "2047" ; Year2048 = ss "2048" ; Year2049 = ss "2049" ;
      Year2050 = ss "2050" ; Year2051 = ss "2051" ; Year2052 = ss "2052" ; Year2053 = ss "2053" ; Year2054 = ss "2054" ; Year2055 = ss "2055" ; Year2056 = ss "2056" ; Year2057 = ss "2057" ; Year2058 = ss "2058" ; Year2059 = ss "2059" ;

    Day_Unit = mkCN (mkN "hari") ;
    Month_Unit = mkCN (mkN "bulan") ;
    Year_Unit = mkCN (mkN "tahun") ;

}