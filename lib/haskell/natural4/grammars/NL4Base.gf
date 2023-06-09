abstract NL4Base = CustomSyntax ** {
  flags startcat = Text ;
  cat
    Text ; -- for fancy NLG and web forms

    -- Any structure that is using BoolStruct needs to prepare for PrePost
    PrePost ; -- "Loss or Damage caused by", "an animal caused water to escape from"

    -- Regulative
    Cond ;
    [Cond]{2} ;
    Action ;
    Who ;
    [Who]{2} ;
    Subj ;
    Deontic ;
    Upon ;
  fun
    -- for fancy NLG
    Regulative : Subj -> Deontic -> Action -> Text ;
    advUPON : Upon -> Text ; -- actually include the word Upon

    -- for web forms
    qWHO,
    sWHO : Subj -> Who -> Text ;
    qUPON,  -- TODO rethink types when adding more langs
            -- TODO2 do we allow upon to take full sentence or just VP*?
    sUPON : Subj -> Upon -> Text ;
    qCOND,
    sCOND : Cond -> Text ;

    -- general Regulative stuff
    EVERY,
    PARTY,
    AN, THE : CN -> Subj ; -- EVERY Person
    WHO : Temp -> Pol -> VP -> Who ;    -- WHO walks
    ACTION : VP -> Action ;

    MUST, MAY, SHANT : Deontic ;
    AND, OR : Conj ;

    SubjWho : Subj -> Who -> Subj ;
    ConjWho : Conj -> [Who] -> Who ;
    ConjPreWho : PrePost -> Conj -> [Who] -> Who ; -- TODO need to find examples in the wild
    ConjPrePostWho : (_,_ : PrePost) -> Conj -> [Who] -> Who ;

    You : Subj ;

    UPON : VP -> Upon ; -- upon becoming

    WHEN : NP -> Temp -> Pol -> VP -> Cond ;
    ConjCond : Conj -> [Cond] -> Cond ;
    ConjPreCond : PrePost -> Conj -> [Cond] -> Cond ; -- TODO need to find examples in the wild
    ConjPrePostCond : (_,_ : PrePost) -> Conj -> [Cond] -> Cond ;


-- General BoolStruct stuff, just first sketch — should be handled more structurally in HS
    cat
      Constraint ;
      [Constraint]{2} ;
    --   IncompleteConstraint ;
    --   [IncompleteConstraint]{2} ;
    fun

      RPleafS : NP -> VPS -> Constraint ;
      RPleafNP : NP -> Constraint ; -- to pair with PrePost to get a full sentence ???
      ConjConstraint : Conj -> [Constraint] -> Constraint ;
      ConjPreConstraint : PrePost -> Conj -> [Constraint] -> Constraint ;
      ConjPrePostConstraint : PrePost -> PrePost -> Conj -> [Constraint] -> Constraint ;

      qPREPOST : PrePost -> Text ;
      qCONSTR : Constraint -> Text ;

-----------------------------------------------------------------------------
-- Time expressions

  cat
    Temporal ;
    TimeUnit ; -- day, month, year …

  fun
    TemporalConstraint
      : TComparison   -- isomorphic to TComparison in Types.hs, defined below
      -> Digits       -- from RGL Numeral module
      -> TimeUnit     -- ad hoc, defined below (TODO: should this )
      -> Temporal ;

    TemporalConstraintNoDigits  -- TemporalConstraint in Haskell has Maybe Int
      : TComparison
      -> TimeUnit
      -> Temporal ;

    Day_Unit
    , Month_Unit
    , Year_Unit : TimeUnit ;

  cat
    Date ;
      Day ; Month ; Year ;
      YearComponent ; -- to make up a year: 4 digits
    TComparison ;
    [TComparison]{2} ;

  fun
    RPConstraint :
      Cond -> TComparison -- ON , AFTER, …
           -> Date        -- 1 Feb 2022
           -> Cond ;
    BEFORE, AFTER, BY, ON, VAGUE : TComparison ;
    ConjTComparison : Conj -> [TComparison] -> TComparison ;

    MkDate : Day -> Month -> Year -> Date ;

    Day1, Day2, Day3, Day4, Day5, Day6, Day7, Day8, Day9, Day10, Day11, Day12, Day13, Day14, Day15, Day16, Day17, Day18, Day19, Day20, Day21, Day22, Day23, Day24, Day25, Day26, Day27, Day28, Day29, Day30, Day31 : Day ;

    Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec : Month ;

    MkYear : (x1,_,_,x4: YearComponent) -> Year ;
    Y0, Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9 : YearComponent ;

-----------------------------------------------------------------------------
-- Very specific things, yet uncategorised
    V2_PrePost : V2 -> PrePost ; -- consumes
    NP_PrePost : NP -> PrePost ; -- beverage
    APWho : AP -> Who ; -- alcoholic into a VP

-----------------------------------------------------------------------------
-- Instead of crashing, every category should have a dummy constructor where to put a string

    fun
      recoverUnparsedPrePost : String -> PrePost ; -- Workaround if PrePost not parsed (since they are not full constituents)
      recoverUnparsedConstraint : String -> Constraint ;
      recoverUnparsedWho : String -> Who ;
      recoverUnparsedCond : String -> Cond ;
      recoverUnparsedUpon : String -> Upon ;
      recoverUnparsedSubj : String -> Subj ;
      recoverUnparsedAction : String -> Action ;
      recoverUnparsedTimeUnit : String -> TimeUnit ;

      recoverRPis : String -> String -> Constraint ;

}