# mp

## L4 handles Legal Spreadsheets

This codebase reads CSV, interprets the L4 in it, and outputs various transformations intended for ingestion by related tools.

## Demo: parsing a Legal Spreadsheet as CSV

The "must sing" tab is available as a CSV, thanks to Google Sheets. (Publish to the web, tab as CSV.)

We save the CSV to disk:

    wget https://docs.google.com/spreadsheets/d/e/2PACX-1vSbknuGFkvvk_Pv9e97iDW9BInxd-Cj27-xC8d_SwveP1PxERV7ZmiAIU7PWXas2CEMH2bl8PyyD6X3/pub\?gid\=1043543357\&single\=true\&output\=csv -O sing.csv

Then we feed it to `stack run`.

    stack run < sing.csv

The output is a list of "rule" data structures parsed from the CSV.

    ┌─[mengwong@solo-7] - [~/src/smucclaw/sandbox/mengwong/mp] - [2021-10-31 06:34:04]
    └─[0] <git:(default b1acb42✈) > stack run < sing.csv
    [ Regulative
        { every = "Person"
        , who = Just
            ( All
                ( Pre "Who" )
                [ Leaf "walks"
                , Any
                    ( Pre "any of:" )
                    [ Leaf "eats"
                    , Leaf "drinks"
                    ]
                ]
            )
        , cond = Nothing
        , deontic = DMust
        , action =
            ( "sing"
            , []
            )
        , temporal = Nothing
        , hence = Nothing
        , lest = Nothing
        , rlabel = Nothing
        , lsource = Nothing
        , srcref = Nothing
        , upon = Nothing
        , given = Nothing
        }
    ]
    [ Regulative
        { every = "Person"
        , who = Just
            ( All
                ( Pre "Who" )
                [ Leaf "walks"
                , Any
                    ( Pre "any of:" )
                    [ Leaf "eats"
                    , Leaf "drinks"
                    ]
                ]
            )
        , cond = Nothing
        , deontic = DMust
        , action =
            ( "sing"
            , []
            )
        , temporal = Nothing
        , hence = Nothing
        , lest = Nothing
        , rlabel = Nothing
        , lsource = Nothing
        , srcref = Nothing
        , upon = Nothing
        , given = Nothing
        }
    ]
    [ Constitutive
        { term = "The rule-level checkbox is checked"
        , cond = Just
            ( Any
                ( Pre "any of:" )
                [ Leaf "the conditions do not hold"
                , All
                    ( Pre "all of:" )
                    [ Leaf "the conditions do hold"
                    , Leaf "the action is satisfied"
                    ]
                ]
            )
        , rlabel = Nothing
        , lsource = Nothing
        , srcref = Nothing
        }
    ]
    [ Regulative
        { every = "Person"
        , who = Just
            ( Leaf "Qualifies" )
        , cond = Nothing
        , deontic = DMust
        , action =
            ( "sing"
            , []
            )
        , temporal = Nothing
        , hence = Nothing
        , lest = Nothing
        , rlabel = Nothing
        , lsource = Nothing
        , srcref = Nothing
        , upon = Nothing
        , given = Nothing
        }
    , Constitutive
        { term = "Qualifies"
        , cond = Just
            ( All
                ( Pre "all of:" )
                [ Leaf "walks"
                , Any
                    ( Pre "any of:" )
                    [ Leaf "eats"
                    , Leaf "drinks"
                    ]
                ]
            )
        , rlabel = Nothing
        , lsource = Nothing
        , srcref = Nothing
        }
    ]

### We transform it for downstream consumption

This is all work in progress...

#### Output: JSON suitable for constructing a Web App

See vue-pure-pdpa

    stack run -- --as json

#### Output: JSON suitable for D3

    stack run -- --as d3

#### Output: An SVG

    stack run -- --as svg

### We evaluate it against some input

If a Legal Spreadsheet contains a test case, we can run those tests.

    stack run -- --tests

### We explain how we arrived at results

    stack run -- --tests --explain
