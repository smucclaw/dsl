-- original rules:

[ Hornlike
    { name =
        [ "mustPayCorpTax"
        , "p"
        , "c"
        ]
    , super = Nothing
    , keyword = Decide
    , given = Just
        (
            ( "p" :| []
            , Just
                ( SimpleType TOne "Person" )
            ) :|
            [
                ( "c" :| []
                , Just
                    ( SimpleType TOne "Corporation" )
                )
            ]
        )
    , upon = Nothing
    , clauses =
        [ HC2
            { hHead = RPMT
                [ "mustPayCorpTax"
                , "p"
                , "c"
                ]
            , hBody = Just
                ( All Nothing
                    [ Leaf
                        ( RPParamText
                            (
                                ( "x" :| []
                                , Just
                                    ( SimpleType TOne "City" )
                                ) :| []
                            )
                        )
                    , Leaf
                        ( RPMT
                            [ "owner"
                            , "p"
                            , "c"
                            ]
                        )
                    , Leaf
                        ( RPMT
                            [ "profitable"
                            , "c"
                            ]
                        )
                    , Leaf
                        ( RPMT
                            [ "hasHQ"
                            , "c"
                            , "x"
                            ]
                        )
                    , Not
                        ( Leaf
                            ( RPMT
                                [ "isTaxHaven"
                                , "x"
                                ]
                            )
                        )
                    ]
                )
            }
        ]
    , rlabel = Just
        ( "§"
        , 1
        , "mustPayCorpTax"
        )
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "/Users/avishkarmahajan/Downloads/LegalSSv0.9.4.3 - natural-asp.csv"
            , short = "/Users/avishkarmahajan/Downloads/LegalSSv0.9.4.3 - natural-asp.csv"
            , srcrow = 2
            , srccol = 1
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
, TypeDecl
    { name = [ "Person" ]
    , super = Nothing
    , has =
        [ TypeDecl
            { name = [ "Name" ]
            , super = Just
                ( SimpleType TOne "String" )
            , has = []
            , enums = Nothing
            , given = Nothing
            , upon = Nothing
            , rlabel = Nothing
            , lsource = Nothing
            , srcref = Nothing
            , defaults = []
            , symtab = []
            }
        ]
    , enums = Nothing
    , given = Nothing
    , upon = Nothing
    , rlabel = Nothing
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "/Users/avishkarmahajan/Downloads/LegalSSv0.9.4.3 - natural-asp.csv"
            , short = "/Users/avishkarmahajan/Downloads/LegalSSv0.9.4.3 - natural-asp.csv"
            , srcrow = 2
            , srccol = 20
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
, TypeDecl
    { name = [ "Corporation" ]
    , super = Nothing
    , has =
        [ TypeDecl
            { name = [ "hasHQ" ]
            , super = Just
                ( SimpleType TOne "City" )
            , has = []
            , enums = Nothing
            , given = Nothing
            , upon = Nothing
            , rlabel = Nothing
            , lsource = Nothing
            , srcref = Nothing
            , defaults = []
            , symtab = []
            }
        , TypeDecl
            { name = [ "Owner" ]
            , super = Just
                ( SimpleType TOne "Person" )
            , has = []
            , enums = Nothing
            , given = Nothing
            , upon = Nothing
            , rlabel = Nothing
            , lsource = Nothing
            , srcref = Nothing
            , defaults = []
            , symtab = []
            }
        , TypeDecl
            { name = [ "profitable" ]
            , super = Just
                ( SimpleType TOne "Boolean" )
            , has = []
            , enums = Nothing
            , given = Nothing
            , upon = Nothing
            , rlabel = Nothing
            , lsource = Nothing
            , srcref = Nothing
            , defaults = []
            , symtab = []
            }
        ]
    , enums = Nothing
    , given = Nothing
    , upon = Nothing
    , rlabel = Nothing
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "/Users/avishkarmahajan/Downloads/LegalSSv0.9.4.3 - natural-asp.csv"
            , short = "/Users/avishkarmahajan/Downloads/LegalSSv0.9.4.3 - natural-asp.csv"
            , srcrow = 2
            , srccol = 23
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
, TypeDecl
    { name = [ "City" ]
    , super = Nothing
    , has =
        [ TypeDecl
            { name = [ "Name" ]
            , super = Just
                ( SimpleType TOne "String" )
            , has = []
            , enums = Nothing
            , given = Nothing
            , upon = Nothing
            , rlabel = Nothing
            , lsource = Nothing
            , srcref = Nothing
            , defaults = []
            , symtab = []
            }
        , TypeDecl
            { name = [ "isTaxhaven" ]
            , super = Just
                ( SimpleType TOne "Boolean" )
            , has = []
            , enums = Nothing
            , given = Nothing
            , upon = Nothing
            , rlabel = Nothing
            , lsource = Nothing
            , srcref = Nothing
            , defaults = []
            , symtab = []
            }
        ]
    , enums = Nothing
    , given = Nothing
    , upon = Nothing
    , rlabel = Nothing
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "/Users/avishkarmahajan/Downloads/LegalSSv0.9.4.3 - natural-asp.csv"
            , short = "/Users/avishkarmahajan/Downloads/LegalSSv0.9.4.3 - natural-asp.csv"
            , srcrow = 2
            , srccol = 28
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
]
-- variable-substitution expanded AnyAll rules

[ Hornlike
    { name =
        [ "mustPayCorpTax"
        , "p"
        , "c"
        ]
    , super = Nothing
    , keyword = Decide
    , given = Just
        (
            ( "p" :| []
            , Just
                ( SimpleType TOne "Person" )
            ) :|
            [
                ( "c" :| []
                , Just
                    ( SimpleType TOne "Corporation" )
                )
            ]
        )
    , upon = Nothing
    , clauses =
        [ HC2
            { hHead = RPMT
                [ "mustPayCorpTax"
                , "p"
                , "c"
                ]
            , hBody = Just
                ( All Nothing
                    [ Leaf
                        ( RPParamText
                            (
                                ( "x" :| []
                                , Just
                                    ( SimpleType TOne "City" )
                                ) :| []
                            )
                        )
                    , Leaf
                        ( RPMT
                            [ "owner"
                            , "p"
                            , "c"
                            ]
                        )
                    , Leaf
                        ( RPMT
                            [ "profitable"
                            , "c"
                            ]
                        )
                    , Leaf
                        ( RPMT
                            [ "hasHQ"
                            , "c"
                            , "x"
                            ]
                        )
                    , Not
                        ( Leaf
                            ( RPMT
                                [ "isTaxHaven"
                                , "x"
                                ]
                            )
                        )
                    ]
                )
            }
        ]
    , rlabel = Just
        ( "§"
        , 1
        , "mustPayCorpTax"
        )
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "/Users/avishkarmahajan/Downloads/LegalSSv0.9.4.3 - natural-asp.csv"
            , short = "/Users/avishkarmahajan/Downloads/LegalSSv0.9.4.3 - natural-asp.csv"
            , srcrow = 2
            , srccol = 1
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
]
-- getAndOrTrees

-- ["mustPayCorpTax"]
Just
    ( All Nothing
        [ Leaf "x"
        , Leaf "owner p c"
        , Leaf "profitable c"
        , Leaf "hasHQ c x"
        , Not
            ( Leaf "isTaxHaven x" )
        ]
    )

-- ["Person"]
Nothing

-- ["Corporation"]
Nothing

-- ["City"]
Nothing



-- class hierarchy:

CT
    ( fromList
        [
            ( "City"
            ,
                (
                    ( Nothing
                    , []
                    )
                , CT
                    ( fromList
                        [
                            ( "Name"
                            ,
                                (
                                    ( Just
                                        ( SimpleType TOne "String" )
                                    , []
                                    )
                                , CT
                                    ( fromList [] )
                                )
                            )
                        ,
                            ( "isTaxhaven"
                            ,
                                (
                                    ( Just
                                        ( SimpleType TOne "Boolean" )
                                    , []
                                    )
                                , CT
                                    ( fromList [] )
                                )
                            )
                        ]
                    )
                )
            )
        ,
            ( "Corporation"
            ,
                (
                    ( Nothing
                    , []
                    )
                , CT
                    ( fromList
                        [
                            ( "Owner"
                            ,
                                (
                                    ( Just
                                        ( SimpleType TOne "Person" )
                                    , []
                                    )
                                , CT
                                    ( fromList [] )
                                )
                            )
                        ,
                            ( "hasHQ"
                            ,
                                (
                                    ( Just
                                        ( SimpleType TOne "City" )
                                    , []
                                    )
                                , CT
                                    ( fromList [] )
                                )
                            )
                        ,
                            ( "profitable"
                            ,
                                (
                                    ( Just
                                        ( SimpleType TOne "Boolean" )
                                    , []
                                    )
                                , CT
                                    ( fromList [] )
                                )
                            )
                        ]
                    )
                )
            )
        ,
            ( "Person"
            ,
                (
                    ( Nothing
                    , []
                    )
                , CT
                    ( fromList
                        [
                            ( "Name"
                            ,
                                (
                                    ( Just
                                        ( SimpleType TOne "String" )
                                    , []
                                    )
                                , CT
                                    ( fromList [] )
                                )
                            )
                        ]
                    )
                )
            )
        ]
    )


-- symbol table:

fromList
    [
        ( [ "mustPayCorpTax" ]
        , fromList
            [
                ( [ "c" ]
                ,
                    (
                        ( Just
                            ( SimpleType TOne "Corporation" )
                        , []
                        )
                    , []
                    )
                )
            ,
                (
                    [ "mustPayCorpTax"
                    , "p"
                    , "c"
                    ]
                ,
                    (
                        ( Nothing
                        , []
                        )
                    ,
                        [ HC2
                            { hHead = RPMT
                                [ "mustPayCorpTax"
                                , "p"
                                , "c"
                                ]
                            , hBody = Just
                                ( All Nothing
                                    [ Leaf
                                        ( RPParamText
                                            (
                                                ( "x" :| []
                                                , Just
                                                    ( SimpleType TOne "City" )
                                                ) :| []
                                            )
                                        )
                                    , Leaf
                                        ( RPMT
                                            [ "owner"
                                            , "p"
                                            , "c"
                                            ]
                                        )
                                    , Leaf
                                        ( RPMT
                                            [ "profitable"
                                            , "c"
                                            ]
                                        )
                                    , Leaf
                                        ( RPMT
                                            [ "hasHQ"
                                            , "c"
                                            , "x"
                                            ]
                                        )
                                    , Not
                                        ( Leaf
                                            ( RPMT
                                                [ "isTaxHaven"
                                                , "x"
                                                ]
                                            )
                                        )
                                    ]
                                )
                            }
                        ]
                    )
                )
            ,
                ( [ "p" ]
                ,
                    (
                        ( Just
                            ( SimpleType TOne "Person" )
                        , []
                        )
                    , []
                    )
                )
            ]
        )
    ]
