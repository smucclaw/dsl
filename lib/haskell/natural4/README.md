# Natural L4

This repository contains a higher-level variant of the core language, called Natural L4. It is implemented in the form of spreadsheets, and read as CSV files.

This documentation is being reworked and will end up at the top level README of this repo.

#### Output: JSON suitable for constructing a Web App

We'll add proper option handling soon, using environment variables is too much of a hack.

See also the vue-pure-pdpa repo.

    ┌─[mengwong@solo-7] - [~/src/smucclaw/sandbox/mengwong/mp] - [2021-10-31 06:43:53]
    └─[0] <git:(default b399530✱✈) > tail -20 sing.csv | MP_JSON=True stack run
    [
        {
            "tag": "Regulative",
            "every": "Person",
            "who": {
                "tag": "Leaf",
                "contents": "Qualifies"
            },
            "srcref": null,
            "action": [
                "sing",
                []
            ],
            "deontic": "DMust",
            "cond": null,
            "temporal": null,
            "upon": null,
            "rlabel": null,
            "hence": null,
            "given": null,
            "lest": null,
            "lsource": null
        },
        {
            "tag": "Constitutive",
            "srcref": null,
            "term": "Qualifies",
            "cond": {
                "tag": "All",
                "contents": [
                    {
                        "tag": "Pre",
                        "contents": "all of:"
                    },
                    [
                        {
                            "tag": "Leaf",
                            "contents": "walks"
                        },
                        {
                            "tag": "Any",
                            "contents": [
                                {
                                    "tag": "Pre",
                                    "contents": "any of:"
                                },
                                [
                                    {
                                        "tag": "Leaf",
                                        "contents": "eats"
                                    },
                                    {
                                        "tag": "Leaf",
                                        "contents": "drinks"
                                    }
                                ]
                            ]
                        }
                    ]
                ]
            },
            "rlabel": null,
            "lsource": null
        }
    ]

#### Output: JSON suitable for D3

    stack run -- --as d3

#### Output: An SVG

    stack run -- --as svg

### We evaluate it against some input

If a Legal Spreadsheet contains a test case, we can run those tests.

    stack run -- --tests

### We explain how we arrived at results

    stack run -- --tests --explain
