Node
    { rootLabel = Q
        { shouldView = View
        , andOr = And
        , prePost = Nothing
        , mark = Default { getDefault = Left Nothing }
        }
    , subForest =
        [ Node
            { rootLabel = Q
                { shouldView = Ask
                , andOr = Simply "x"
                , prePost = Nothing
                , mark = Default { getDefault = Left Nothing }
                }
            , subForest = []
            }
        , Node
            { rootLabel = Q
                { shouldView = Ask
                , andOr = Simply "owner p c"
                , prePost = Nothing
                , mark = Default { getDefault = Left Nothing }
                }
            , subForest = []
            }
        , Node
            { rootLabel = Q
                { shouldView = Ask
                , andOr = Simply "profitable c"
                , prePost = Nothing
                , mark = Default { getDefault = Left Nothing }
                }
            , subForest = []
            }
        , Node
            { rootLabel = Q
                { shouldView = Ask
                , andOr = Simply "hasHQ c x"
                , prePost = Nothing
                , mark = Default { getDefault = Left Nothing }
                }
            , subForest = []
            }
        , Node
            { rootLabel = Q
                { shouldView = View
                , andOr = Neg
                , prePost = Nothing
                , mark = Default { getDefault = Left Nothing }
                }
            , subForest =
                [ Node
                    { rootLabel = Q
                        { shouldView = Ask
                        , andOr = Simply "isTaxHaven x"
                        , prePost = Nothing
                        , mark = Default { getDefault = Left Nothing }
                        }
                    , subForest = []
                    }
                ]
            }
        ]
    }