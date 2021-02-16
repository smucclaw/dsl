{-
Run as follows:
* Parsing with BNFC: dsl/bnfc> cp l4/deon_bike_meng_detail.l4 l4/test.l4  ; stack exec l4 l4/test.l4 > out/test.out
* In ghci:
  :l RuleToTa
  writeFile "/home/strecker/Systems/Uppaal/Examples/deon_bike_gen.xta" (ta_sys_to_uppaal (sersToTASys (toplevelsToSERs rulelist)))

-}

module ExampleInput where
import Data.List
import AbsL



{-
usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

parse_file fn = do
  s <- readFile fn
  lrt <- pTops (myLLexer s)
  (case lrt of
    Left _ -> error "parse error"
    Right tree -> do
      showTree 2 tree
      exitSuccess
    )
-}

{-
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pTops
    "-s":fs -> mapM_ (runFile 0 pTops) fs
    fs -> mapM_ (runFile 2 pTops) fs
-}

----------------------------------------------------------------------
-- Old ???
----------------------------------------------------------------------


rulelist :: [Toplevels]
rulelist       = [ ToplevelsRule
        ( Rule
            ( RID
                ( OA_dots
                    [ ObjAttrElemIdent ( Ident "mkContract" ) ]
                )
            ) ( RName OptLangStrings1 ) AsofNull Meta0
            ( RuleDeem ( GUGiven GivenLimb0 UponLimb1 )
                [ DefLimb DefDefine
                    [ CComma
                        ( Op2E
                            ( BRel_Isa
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Buyer" ) )
                                        ]
                                    )
                                )
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Person" ) )
                                        ]
                                    )
                                )
                            )
                        )
                    ] WithLimb1 AsofNull
                , DefLimb DefDefine
                    [ CComma
                        ( Op2E
                            ( BRel_Isa
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Seller" ) )
                                        ]
                                    )
                                )
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Person" ) )
                                        ]
                                    )
                                )
                            )
                        )
                    ]
                    ( WithLimb2 WithHas_WITH
                        [ WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "inventory" ) )
                                            ]
                                        )
                                    )
                                    ( ListE
                                        ( ListComma
                                            [ BracesE
                                                ( BraceList1
                                                    [ CComma
                                                        ( Op2E
                                                            ( BCmp_Eq1
                                                                ( UnifyE
                                                                    ( UnifyExpr1
                                                                        [ UnifyElemObjAttrElem
                                                                            ( ObjAttrElemIdent ( Ident "amount" ) )
                                                                        ]
                                                                    )
                                                                )
                                                                ( UnifyE
                                                                    ( UnifyExpr1
                                                                        [ UnifyElemObjAttrElem
                                                                            ( ObjAttrElemUIdent ( UIdent "Money" ) )
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    , CComma
                                                        ( Op2E
                                                            ( BCmp_Eq1
                                                                ( UnifyE
                                                                    ( UnifyExpr1
                                                                        [ UnifyElemObjAttrElem
                                                                            ( ObjAttrElemIdent ( Ident "item" ) )
                                                                        ]
                                                                    )
                                                                )
                                                                ( UnifyE
                                                                    ( UnifyExpr1
                                                                        [ UnifyElemObjAttrElem
                                                                            ( ObjAttrElemUIdent ( UIdent "Int" ) )
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    ]
                                                )
                                            ]
                                        )
                                    )
                                )
                            )
                        ]
                    ) AsofNull
                , DefLimb DefDefine
                    [ CComma
                        ( Op2E
                            ( BRel_Isa
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Event" ) )
                                        ]
                                    )
                                )
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Record" ) )
                                        ]
                                    )
                                )
                            )
                        )
                    ]
                    ( WithLimb2 WithHas_WITH
                        [ WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "time" ) )
                                            ]
                                        )
                                    )
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "Time" ) )
                                            ]
                                        )
                                    )
                                )
                            )
                        ]
                    ) AsofNull
                , DefLimb DefDefine
                    [ CComma
                        ( Op2E
                            ( BRel_Isa
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Message" ) )
                                        ]
                                    )
                                )
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Event" ) )
                                        ]
                                    )
                                )
                            )
                        )
                    ]
                    ( WithLimb2 WithHas_WITH
                        [ WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "from" ) )
                                            ]
                                        )
                                    )
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "Person" ) )
                                            ]
                                        )
                                    )
                                )
                            )
                        , WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "to" ) )
                                            ]
                                        )
                                    )
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "Person" ) )
                                            ]
                                        )
                                    )
                                )
                            )
                        ]
                    ) AsofNull
                , DefLimb DefDefine
                    [ CComma
                        ( Op2E
                            ( BRel_Isa
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Delivery" ) )
                                        ]
                                    )
                                )
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Event" ) )
                                        ]
                                    )
                                )
                            )
                        )
                    ]
                    ( WithLimb2 WithHas_WITH
                        [ WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "from" ) )
                                            ]
                                        )
                                    )
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "Person" ) )
                                            ]
                                        )
                                    )
                                )
                            )
                        , WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "to" ) )
                                            ]
                                        )
                                    )
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "Person" ) )
                                            ]
                                        )
                                    )
                                )
                            )
                        , WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "item" ) )
                                            ]
                                        )
                                    )
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "Item" ) )
                                            ]
                                        )
                                    )
                                )
                            )
                        ]
                    ) AsofNull
                , DefLimb DefDefine
                    [ CComma
                        ( Op2E
                            ( BRel_Isa
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Payment" ) )
                                        ]
                                    )
                                )
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Event" ) )
                                        ]
                                    )
                                )
                            )
                        )
                    ]
                    ( WithLimb2 WithHas_WITH
                        [ WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "from" ) )
                                            ]
                                        )
                                    )
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "Person" ) )
                                            ]
                                        )
                                    )
                                )
                            )
                        , WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "to" ) )
                                            ]
                                        )
                                    )
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "Person" ) )
                                            ]
                                        )
                                    )
                                )
                            )
                        , WithInExp
                            ( Op2E
                                ( BRel_Isa
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "amount" ) )
                                            ]
                                        )
                                    )
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "CurrencyAmount" ) )
                                            ]
                                        )
                                    )
                                )
                            )
                        ]
                    ) AsofNull
                , DefLimb DefDefine
                    [ CComma
                        ( Op2E
                            ( BRel_Isa
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Order" ) )
                                        ]
                                    )
                                )
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Message" ) )
                                        ]
                                    )
                                )
                            )
                        )
                    ]
                    ( WithLimb2 WithHas_WITH
                        [ WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "amount" ) )
                                            ]
                                        )
                                    )
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "CurrencyAmount" ) )
                                            ]
                                        )
                                    )
                                )
                            )
                        , WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "item" ) )
                                            ]
                                        )
                                    )
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "Item" ) )
                                            ]
                                        )
                                    )
                                )
                            )
                        ]
                    ) AsofNull
                , DefLimb DefDefine
                    [ CComma
                        ( Op2E
                            ( BRel_Isa
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemIdent ( Ident "wantBike" ) )
                                        ]
                                    )
                                )
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Order" ) )
                                        ]
                                    )
                                )
                            )
                        )
                    ]
                    ( WithLimb2 WithHas_WITH
                        [ WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "amount" ) )
                                            ]
                                        )
                                    )
                                    ( Op1E
                                        ( UCurr
                                            ( CurrCode ( UIdent "EUR" ) )
                                        )
                                        ( ConstE ( IntV 100 ) )
                                    )
                                )
                            )
                        , WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "item" ) )
                                            ]
                                        )
                                    )
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "bicycle2000" ) )
                                            ]
                                        )
                                    )
                                )
                            )
                        ]
                    ) AsofNull
                , DefLimb DefDefine
                    [ CComma
                        ( Op2E
                            ( BRel_Is
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemIdent ( Ident "maxDays" ) )
                                        ]
                                    )
                                )
                                ( ConstE ( IntV 5 ) )
                            )
                        )
                    ] WithLimb1 AsofNull
                , DefLimb DefDefine
                    [ CComma
                        ( Op2E
                            ( BRel_Isa
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemIdent ( Ident "bicycle2000" ) )
                                        ]
                                    )
                                )
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemIdent ( Ident "item" ) )
                                        ]
                                    )
                                )
                            )
                        )
                    ]
                    ( WithLimb2 WithHas_WITH
                        [ WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "name" ) )
                                            ]
                                        )
                                    )
                                    ( ConstE ( StringV "HotWheels 2000" ) )
                                )
                            )
                        ]
                    ) AsofNull
                , DefLimb DefEntity
                    [ CComma
                        ( Op2E
                            ( BRel_Isa
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemIdent ( Ident "alice" ) )
                                        ]
                                    )
                                )
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Person" ) )
                                        ]
                                    )
                                )
                            )
                        )
                    ]
                    ( WithLimb2 WithHas_WITH
                        [ WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "name" ) )
                                            ]
                                        )
                                    )
                                    ( ConstE ( StringV "Alice Apple" ) )
                                )
                            )
                        ]
                    ) AsofNull
                , DefLimb DefEntity
                    [ CComma
                        ( Op2E
                            ( BRel_Isa
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemIdent ( Ident "bob" ) )
                                        ]
                                    )
                                )
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Person" ) )
                                        ]
                                    )
                                )
                            )
                        )
                    ]
                    ( WithLimb2 WithHas_WITH
                        [ WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "name" ) )
                                            ]
                                        )
                                    )
                                    ( ConstE ( StringV "Bob Banana" ) )
                                )
                            )
                        ]
                    ) AsofNull
                , DefLimb DefDeclare
                    [ CComma
                        ( Op2E
                            ( BRel_Is
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Buyer" ) )
                                        ]
                                    )
                                )
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemIdent ( Ident "alice" ) )
                                        ]
                                    )
                                )
                            )
                        )
                    ] WithLimb1 AsofNull
                , DefLimb DefDeclare
                    [ CComma
                        ( Op2E
                            ( BRel_Is
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Seller" ) )
                                        ]
                                    )
                                )
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemIdent ( Ident "bob" ) )
                                        ]
                                    )
                                )
                            )
                        )
                    ]
                    ( WithLimb2 WithHas_WITH
                        [ WithInExp
                            ( Op2E
                                ( BCmp_Eq1
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "inventory" ) )
                                            ]
                                        )
                                    )
                                    ( ListE
                                        ( ListComma
                                            [ BracesE
                                                ( BraceList1
                                                    [ CComma
                                                        ( Op2E
                                                            ( BCmp_GTE
                                                                ( UnifyE
                                                                    ( UnifyExpr1
                                                                        [ UnifyElemObjAttrElem
                                                                            ( ObjAttrElemIdent ( Ident "amount" ) )
                                                                        ]
                                                                    )
                                                                )
                                                                ( Op1E
                                                                    ( UCurr
                                                                        ( CurrCode ( UIdent "EUR" ) )
                                                                    )
                                                                    ( ConstE ( IntV 100 ) )
                                                                )
                                                            )
                                                        )
                                                    , CComma
                                                        ( Op2E
                                                            ( BCmp_Eq1
                                                                ( UnifyE
                                                                    ( UnifyExpr1
                                                                        [ UnifyElemObjAttrElem
                                                                            ( ObjAttrElemIdent ( Ident "item" ) )
                                                                        ]
                                                                    )
                                                                )
                                                                ( UnifyE
                                                                    ( UnifyExpr1
                                                                        [ UnifyElemObjAttrElem
                                                                            ( ObjAttrElemIdent ( Ident "bicycle2000" ) )
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    ]
                                                )
                                            , BracesE
                                                ( BraceList1
                                                    [ CComma
                                                        ( Op2E
                                                            ( BCmp_GTE
                                                                ( UnifyE
                                                                    ( UnifyExpr1
                                                                        [ UnifyElemObjAttrElem
                                                                            ( ObjAttrElemIdent ( Ident "amount" ) )
                                                                        ]
                                                                    )
                                                                )
                                                                ( Op1E
                                                                    ( UCurr
                                                                        ( CurrCode ( UIdent "EUR" ) )
                                                                    )
                                                                    ( ConstE ( IntV 50 ) )
                                                                )
                                                            )
                                                        )
                                                    , CComma
                                                        ( Op2E
                                                            ( BCmp_Eq1
                                                                ( UnifyE
                                                                    ( UnifyExpr1
                                                                        [ UnifyElemObjAttrElem
                                                                            ( ObjAttrElemIdent ( Ident "item" ) )
                                                                        ]
                                                                    )
                                                                )
                                                                ( UnifyE
                                                                    ( UnifyExpr1
                                                                        [ UnifyElemObjAttrElem
                                                                            ( ObjAttrElemIdent ( Ident "coldWheels" ) )
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    ]
                                                )
                                            , BracesE
                                                ( BraceList1
                                                    [ CComma
                                                        ( Op2E
                                                            ( BCmp_GTE
                                                                ( UnifyE
                                                                    ( UnifyExpr1
                                                                        [ UnifyElemObjAttrElem
                                                                            ( ObjAttrElemIdent ( Ident "amount" ) )
                                                                        ]
                                                                    )
                                                                )
                                                                ( Op1E
                                                                    ( UCurr
                                                                        ( CurrCode ( UIdent "USD" ) )
                                                                    )
                                                                    ( ConstE ( IntV 200 ) )
                                                                )
                                                            )
                                                        )
                                                    , CComma
                                                        ( Op2E
                                                            ( BCmp_Eq1
                                                                ( UnifyE
                                                                    ( UnifyExpr1
                                                                        [ UnifyElemObjAttrElem
                                                                            ( ObjAttrElemIdent ( Ident "item" ) )
                                                                        ]
                                                                    )
                                                                )
                                                                ( UnifyE
                                                                    ( UnifyExpr1
                                                                        [ UnifyElemObjAttrElem
                                                                            ( ObjAttrElemIdent ( Ident "popAwheelie" ) )
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    ]
                                                )
                                            ]
                                        )
                                    )
                                )
                            )
                        ]
                    ) AsofNull
                ] ( WHW WhenLimb0 DNoHence WhereLimb0 )
            )
        )
    , ToplevelsRule
        ( Rule
            ( RID
                ( OA_dots
                    [ ObjAttrElemUIdent ( UIdent "OrderBike" ) ]
                )
            ) ( RName OptLangStrings1 ) AsofNull Meta0
            ( RModal
                ( GUGiven
                    ( GivenLimb1
                        ( GivenExpr1
                            [ UnifyE
                                ( UnifyExpr1
                                    [ UnifyElemObjAttrElem
                                        ( ObjAttrElemUIdent ( UIdent "Start" ) )
                                    ]
                                )
                            ]
                        )
                    ) UponLimb1
                )
                ( MD1
                    ( PartyLimb
                        ( PSome
                            ( OA_dots
                                [ ObjAttrElemUIdent ( UIdent "Buyer" ) ]
                            )
                        ) OptAsAlias0
                    )
                    ( DeonticLimb1 DEMay OptLangStrings1
                        ( ActionSingle
                            ( Op2E
                                ( BRel_Fat
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "send" ) )
                                            ]
                                        )
                                    )
                                    ( ObjME
                                        ( ObjMethod1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "Order" ) )
                                            ]
                                            ( Args2
                                                [ CComma
                                                    ( Op2E
                                                        ( BCmp_Eq1
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemIdent ( Ident "to" ) )
                                                                    ]
                                                                )
                                                            )
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemUIdent ( UIdent "Seller" ) )
                                                                    ]
                                                                )
                                                            )
                                                        )
                                                    )
                                                ]
                                            ) OptLangStrings1
                                        )
                                    )
                                )
                            ) [] OptAsAlias0
                        )
                    ) DL0
                )
                ( WHW WhenLimb0
                    ( DHence
                        ( RGotoOne
                            ( RID
                                ( OA_dots
                                    [ ObjAttrElemUIdent ( UIdent "AwaitDeliverBike" ) ]
                                )
                            )
                        ) Args1 OptLangStrings1
                    ) WhereLimb0
                )
            )
        )
    , ToplevelsRule
        ( Rule
            ( RID
                ( OA_dots
                    [ ObjAttrElemUIdent ( UIdent "BuyerPays" ) ]
                )
            ) ( RName OptLangStrings1 ) AsofNull Meta0
            ( RModal
                ( GUGiven
                    ( GivenLimb1
                        ( GivenExpr1
                            [ UnifyE
                                ( UnifyExpr1
                                    [ UnifyElemObjAttrElem
                                        ( ObjAttrElemUIdent ( UIdent "AwaitDeliverBike" ) )
                                    ]
                                )
                            ]
                        )
                    )
                    ( UponLimb2 Upon0
                        ( GivenExpr1
                            [ Op2E
                                ( BRel_Fat
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "recv" ) )
                                            ]
                                        )
                                    )
                                    ( ObjME
                                        ( ObjMethod1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "Delivery" ) )
                                            ]
                                            ( Args2
                                                [ CComma
                                                    ( Op2E
                                                        ( BCmp_Eq1
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemIdent ( Ident "from" ) )
                                                                    ]
                                                                )
                                                            )
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemIdent ( Ident "seller" ) )
                                                                    ]
                                                                )
                                                            )
                                                        )
                                                    )
                                                , CComma
                                                    ( Op2E
                                                        ( BCmp_Eq1
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemIdent ( Ident "item" ) )
                                                                    ]
                                                                )
                                                            )
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemIdent ( Ident "bikeSale" ) )
                                                                    , UnifyElemObjAttrElem
                                                                        ( ObjAttrElemIdent ( Ident "item" ) )
                                                                    ]
                                                                )
                                                            )
                                                        )
                                                    )
                                                ]
                                            ) OptLangStrings1
                                        )
                                    )
                                )
                            ]
                        )
                    )
                )
                ( MD1
                    ( PartyLimb
                        ( PSome
                            ( OA_dots
                                [ ObjAttrElemUIdent ( UIdent "Buyer" ) ]
                            )
                        ) OptAsAlias0
                    )
                    ( DeonticLimb1 DEMust OptLangStrings1
                        ( ActionSingle
                            ( Op2E
                                ( BRel_Fat
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "send" ) )
                                            ]
                                        )
                                    )
                                    ( ObjME
                                        ( ObjMethod1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "Payment" ) )
                                            ]
                                            ( Args2
                                                [ CComma
                                                    ( Op2E
                                                        ( BCmp_Eq1
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemIdent ( Ident "to" ) )
                                                                    ]
                                                                )
                                                            )
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemIdent ( Ident "seller" ) )
                                                                    ]
                                                                )
                                                            )
                                                        )
                                                    )
                                                , CComma
                                                    ( Op2E
                                                        ( BCmp_Eq1
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemIdent ( Ident "amount" ) )
                                                                    ]
                                                                )
                                                            )
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemIdent ( Ident "bikeSale" ) )
                                                                    , UnifyElemObjAttrElem
                                                                        ( ObjAttrElemIdent ( Ident "amount" ) )
                                                                    ]
                                                                )
                                                            )
                                                        )
                                                    )
                                                ]
                                            ) OptLangStrings1
                                        )
                                    )
                                )
                            ) [] OptAsAlias0
                        )
                    )
                    ( DLLimb TRBefore
                        ( TemporalExpr2
                            ( OA_dots
                                [ ObjAttrElemIdent ( Ident "maxDays" ) ]
                            ) DurationExpr1
                        ) OptAsAlias0
                    )
                )
                ( WHW WhenLimb0 ( DHence RFulfilled Args1 OptLangStrings1 ) WhereLimb0 )
            )
        )
    , ToplevelsRule
        ( Rule
            ( RID
                ( OA_dots
                    [ ObjAttrElemUIdent ( UIdent "DeliverBike" ) ]
                )
            ) ( RName OptLangStrings1 ) AsofNull Meta0
            ( RModal
                ( GUGiven
                    ( GivenLimb1
                        ( GivenExpr1
                            [ UnifyE
                                ( UnifyExpr1
                                    [ UnifyElemObjAttrElem
                                        ( ObjAttrElemUIdent ( UIdent "Start" ) )
                                    ]
                                )
                            ]
                        )
                    )
                    ( UponLimb2 Upon0
                        ( GivenExpr1
                            [ Op2E
                                ( BRel_Fat
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "recv" ) )
                                            ]
                                        )
                                    )
                                    ( ObjME
                                        ( ObjMethod1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "Order" ) )
                                            ]
                                            ( Args2
                                                [ CComma
                                                    ( Op2E
                                                        ( BCmp_Eq1
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemIdent ( Ident "from" ) )
                                                                    ]
                                                                )
                                                            )
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemUIdent ( UIdent "Buyer" ) )
                                                                    ]
                                                                )
                                                            )
                                                        )
                                                    )
                                                ]
                                            ) OptLangStrings1
                                        )
                                    )
                                )
                            ]
                        )
                    )
                )
                ( MD1
                    ( PartyLimb
                        ( PSome
                            ( OA_dots
                                [ ObjAttrElemUIdent ( UIdent "Seller" ) ]
                            )
                        ) OptAsAlias0
                    )
                    ( DeonticLimb1 DEMust OptLangStrings1
                        ( ActionSingle
                            ( Op2E
                                ( BRel_Fat
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "send" ) )
                                            ]
                                        )
                                    )
                                    ( ObjME
                                        ( ObjMethod1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "Delivery" ) )
                                            ]
                                            ( Args2
                                                [ CComma
                                                    ( Op2E
                                                        ( BCmp_Eq1
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemIdent ( Ident "to" ) )
                                                                    ]
                                                                )
                                                            )
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemUIdent ( UIdent "Buyer" ) )
                                                                    ]
                                                                )
                                                            )
                                                        )
                                                    )
                                                , CComma
                                                    ( Op2E
                                                        ( BCmp_Eq1
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemIdent ( Ident "item" ) )
                                                                    ]
                                                                )
                                                            )
                                                            ( UnifyE
                                                                ( UnifyExpr1
                                                                    [ UnifyElemObjAttrElem
                                                                        ( ObjAttrElemUIdent ( UIdent "Order" ) )
                                                                    , UnifyElemObjAttrElem
                                                                        ( ObjAttrElemIdent ( Ident "item" ) )
                                                                    ]
                                                                )
                                                            )
                                                        )
                                                    )
                                                ]
                                            ) OptLangStrings1
                                        )
                                    )
                                )
                            ) [] OptAsAlias0
                        )
                    )
                    ( DLLimb TRBefore
                        ( TemporalExpr2
                            ( OA_dots
                                [ ObjAttrElemIdent ( Ident "maxDays" ) ]
                            ) DurationExpr1
                        ) OptAsAlias0
                    )
                )
                ( WHW
                    ( WhenLimb1
                        ( Op2E
                            ( BL_In
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Order" ) )
                                        ]
                                    )
                                )
                                ( UnifyE
                                    ( UnifyExpr1
                                        [ UnifyElemObjAttrElem
                                            ( ObjAttrElemUIdent ( UIdent "Seller" ) )
                                        , UnifyElemObjAttrElem
                                            ( ObjAttrElemIdent ( Ident "inventory" ) )
                                        ]
                                    )
                                )
                            )
                        )
                    )
                    ( DHence
                        ( RGotoOne
                            ( RID
                                ( OA_dots
                                    [ ObjAttrElemUIdent ( UIdent "AwaitBuyerPays" ) ]
                                )
                            )
                        ) Args1 OptLangStrings1
                    ) WhereLimb0
                )
            )
        )
    , ToplevelsRule
        ( Rule
            ( RID
                ( OA_dots
                    [ ObjAttrElemUIdent ( UIdent "ReceivesPay" ) ]
                )
            ) ( RName OptLangStrings1 ) AsofNull Meta0
            ( RModal
                ( GUGiven
                    ( GivenLimb1
                        ( GivenExpr1
                            [ UnifyE
                                ( UnifyExpr1
                                    [ UnifyElemObjAttrElem
                                        ( ObjAttrElemUIdent ( UIdent "AwaitBuyerPays" ) )
                                    ]
                                )
                            ]
                        )
                    )
                    ( UponLimb2 Upon0
                        ( GivenExpr1
                            [ Op2E
                                ( BRel_Fat
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemIdent ( Ident "recv" ) )
                                            ]
                                        )
                                    )
                                    ( UnifyE
                                        ( UnifyExpr1
                                            [ UnifyElemObjAttrElem
                                                ( ObjAttrElemUIdent ( UIdent "Payment" ) )
                                            ]
                                        )
                                    )
                                )
                            ]
                        )
                    )
                )
                ( MD1
                    ( PartyLimb
                        ( PSome
                            ( OA_dots
                                [ ObjAttrElemUIdent ( UIdent "Seller" ) ]
                            )
                        ) OptAsAlias0
                    )
                    ( DeonticLimb1 DEMust OptLangStrings1
                        ( ActionSingle
                            ( UnifyE
                                ( UnifyExpr1
                                    [ UnifyElemObjAttrElem
                                        ( ObjAttrElemIdent ( Ident "end" ) )
                                    ]
                                )
                            ) [] OptAsAlias0
                        )
                    ) DL0
                )
                ( WHW WhenLimb0 ( DHence RFulfilled Args1 OptLangStrings1 ) WhereLimb0 )
            )
        )
    ]
                 

