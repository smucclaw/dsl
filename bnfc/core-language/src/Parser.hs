{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseProgram
--  , parseTokens,
) where

import Lexer
import Syntax

import Prelude
import Control.Monad.Except
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,329) ([0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,16,0,0,0,512,0,0,0,0,0,0,32768,0,0,0,0,0,0,4096,0,0,0,0,1,0,256,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,32768,0,0,0,16,0,32,0,0,0,0,2,0,0,0,256,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,16,0,0,8,0,0,0,0,0,256,0,0,0,0,0,0,49152,227,6273,0,0,48,0,66,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,192,0,264,0,0,0,0,0,0,0,65024,15,0,0,32768,1,98,0,0,0,256,0,0,0,0,0,0,0,58304,33024,24,0,0,0,16384,0,0,0,0,256,0,0,14576,8256,6,0,0,0,0,0,0,0,0,0,0,0,0,2048,1,0,61440,16440,1568,0,0,58304,33024,24,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,1,0,0,64,0,0,0,61440,16440,1568,0,0,0,64,0,0,0,0,20480,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,63488,63,0,0,0,0,0,0,0,14576,8256,6,0,3072,0,4224,0,0,0,0,64,0,0,57344,255,0,0,0,0,72,0,0,0,0,0,0,0,0,256,0,0,0,0,256,0,0,0,0,0,0,0,65028,15,0,0,0,0,1,0,0,0,1024,0,0,0,64512,3,0,0,0,0,16,0,0,0,64,0,0,15360,4110,392,0,0,14576,8256,6,0,49152,227,6273,0,0,36608,1027,98,0,0,3644,34832,1,0,61440,16440,1568,0,0,58304,33024,24,0,0,911,25092,0,0,15360,4110,392,0,0,14576,8256,6,0,49152,227,6273,0,0,0,32768,4,0,0,4096,0,0,0,768,0,1056,0,0,0,1,0,0,12288,0,16896,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,224,0,0,0,32768,3,0,0,0,3968,0,0,0,0,62,0,0,0,63488,0,0,0,0,1022,0,0,0,64512,15,0,0,0,16376,0,0,0,0,0,0,0,768,0,1056,0,0,12,32768,16,0,0,911,25092,0,0,0,0,18,0,0,0,0,0,0,3072,0,4224,0,0,36608,1027,98,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,63488,63,0,0,0,0,4,0,0,0,0,5,0,0,0,0,0,0,48,0,66,0,0,57344,255,0,0,0,64,0,0,0,0,0,16,0,0,0,0,0,0,32768,65504,0,0,0,16384,1024,0,0,0,256,16,0,0,0,4,0,0,0,3644,34832,1,0,61440,16440,1568,0,0,58304,33024,24,0,0,0,0,0,0,15360,4110,392,0,0,16384,0,0,0,0,65024,15,0,0,0,16376,0,0,0,57344,255,0,0,0,65408,3,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_program","Program","ClassDecls","ClassDecl","ClassDef","FieldDecls","FieldDecl","GlobalVarDecls","GlobalVarDecl","VarDeclsCommaSep","VarDecl","Assertions","Assertion","ATp","TpsCommaSep","Tp","Pattern","VarsCommaSep","Expr","App","Acc","Atom","ExprsCommaSep","Rules","Rule","RuleName","RuleVarDecls","RulePrecond","RuleConcl","Annot","assert","class","decl","defn","extends","rule","Bool","Int","let","in","not","all","ex","if","then","else","for","true","false","'\\\\'","'->'","'-->'","'||'","'&&'","'='","'<'","'>'","'+'","'-'","'*'","'/'","'%'","'.'","','","':'","'('","')'","'{'","'}'","NUM","VAR","%eof"]
        bit_start = st * 74
        bit_end = (st + 1) * 74
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..73]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (34) = happyShift action_6
action_2 (6) = happyGoto action_4
action_2 (10) = happyGoto action_5
action_2 _ = happyReduce_10

action_3 (74) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 (35) = happyShift action_10
action_5 (11) = happyGoto action_8
action_5 (26) = happyGoto action_9
action_5 _ = happyReduce_63

action_6 (73) = happyShift action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (68) = happyShift action_17
action_7 (32) = happyGoto action_16
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_11

action_9 (38) = happyShift action_15
action_9 (14) = happyGoto action_12
action_9 (27) = happyGoto action_13
action_9 (28) = happyGoto action_14
action_9 _ = happyReduce_16

action_10 (73) = happyShift action_11
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (67) = happyShift action_27
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (33) = happyShift action_26
action_12 (15) = happyGoto action_25
action_12 _ = happyReduce_1

action_13 _ = happyReduce_64

action_14 (49) = happyShift action_24
action_14 (29) = happyGoto action_23
action_14 _ = happyReduce_67

action_15 (58) = happyShift action_22
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (37) = happyShift action_20
action_16 (70) = happyShift action_21
action_16 (7) = happyGoto action_19
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (72) = happyShift action_18
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (69) = happyShift action_57
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_4

action_20 (73) = happyShift action_56
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (8) = happyGoto action_55
action_21 _ = happyReduce_7

action_22 (73) = happyShift action_54
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (46) = happyShift action_53
action_23 (30) = happyGoto action_52
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (73) = happyShift action_51
action_24 (12) = happyGoto action_49
action_24 (13) = happyGoto action_50
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_17

action_26 (43) = happyShift action_38
action_26 (44) = happyShift action_39
action_26 (45) = happyShift action_40
action_26 (46) = happyShift action_41
action_26 (50) = happyShift action_42
action_26 (51) = happyShift action_43
action_26 (52) = happyShift action_44
action_26 (61) = happyShift action_45
action_26 (68) = happyShift action_46
action_26 (72) = happyShift action_47
action_26 (73) = happyShift action_48
action_26 (21) = happyGoto action_34
action_26 (22) = happyGoto action_35
action_26 (23) = happyGoto action_36
action_26 (24) = happyGoto action_37
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (39) = happyShift action_30
action_27 (40) = happyShift action_31
action_27 (68) = happyShift action_32
action_27 (73) = happyShift action_33
action_27 (16) = happyGoto action_28
action_27 (18) = happyGoto action_29
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_26

action_29 (53) = happyShift action_93
action_29 _ = happyReduce_12

action_30 _ = happyReduce_19

action_31 _ = happyReduce_20

action_32 (39) = happyShift action_30
action_32 (40) = happyShift action_31
action_32 (68) = happyShift action_32
action_32 (73) = happyShift action_33
action_32 (16) = happyGoto action_28
action_32 (17) = happyGoto action_91
action_32 (18) = happyGoto action_92
action_32 _ = happyReduce_23

action_33 _ = happyReduce_21

action_34 (54) = happyShift action_80
action_34 (55) = happyShift action_81
action_34 (56) = happyShift action_82
action_34 (57) = happyShift action_83
action_34 (58) = happyShift action_84
action_34 (59) = happyShift action_85
action_34 (60) = happyShift action_86
action_34 (61) = happyShift action_87
action_34 (62) = happyShift action_88
action_34 (63) = happyShift action_89
action_34 (64) = happyShift action_90
action_34 _ = happyReduce_18

action_35 (50) = happyShift action_42
action_35 (51) = happyShift action_43
action_35 (68) = happyShift action_46
action_35 (72) = happyShift action_47
action_35 (73) = happyShift action_48
action_35 (23) = happyGoto action_79
action_35 (24) = happyGoto action_37
action_35 _ = happyReduce_50

action_36 (65) = happyShift action_78
action_36 _ = happyReduce_52

action_37 _ = happyReduce_54

action_38 (43) = happyShift action_38
action_38 (44) = happyShift action_39
action_38 (45) = happyShift action_40
action_38 (46) = happyShift action_41
action_38 (50) = happyShift action_42
action_38 (51) = happyShift action_43
action_38 (52) = happyShift action_44
action_38 (61) = happyShift action_45
action_38 (68) = happyShift action_46
action_38 (72) = happyShift action_47
action_38 (73) = happyShift action_48
action_38 (21) = happyGoto action_77
action_38 (22) = happyGoto action_35
action_38 (23) = happyGoto action_36
action_38 (24) = happyGoto action_37
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (73) = happyShift action_76
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (73) = happyShift action_75
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (43) = happyShift action_38
action_41 (44) = happyShift action_39
action_41 (45) = happyShift action_40
action_41 (46) = happyShift action_41
action_41 (50) = happyShift action_42
action_41 (51) = happyShift action_43
action_41 (52) = happyShift action_44
action_41 (61) = happyShift action_45
action_41 (68) = happyShift action_46
action_41 (72) = happyShift action_47
action_41 (73) = happyShift action_48
action_41 (21) = happyGoto action_74
action_41 (22) = happyGoto action_35
action_41 (23) = happyGoto action_36
action_41 (24) = happyGoto action_37
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_58

action_43 _ = happyReduce_59

action_44 (68) = happyShift action_72
action_44 (73) = happyShift action_73
action_44 (19) = happyGoto action_71
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (43) = happyShift action_38
action_45 (44) = happyShift action_39
action_45 (45) = happyShift action_40
action_45 (46) = happyShift action_41
action_45 (50) = happyShift action_42
action_45 (51) = happyShift action_43
action_45 (52) = happyShift action_44
action_45 (61) = happyShift action_45
action_45 (68) = happyShift action_46
action_45 (72) = happyShift action_47
action_45 (73) = happyShift action_48
action_45 (21) = happyGoto action_70
action_45 (22) = happyGoto action_35
action_45 (23) = happyGoto action_36
action_45 (24) = happyGoto action_37
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (43) = happyShift action_38
action_46 (44) = happyShift action_39
action_46 (45) = happyShift action_40
action_46 (46) = happyShift action_41
action_46 (50) = happyShift action_42
action_46 (51) = happyShift action_43
action_46 (52) = happyShift action_44
action_46 (61) = happyShift action_45
action_46 (68) = happyShift action_46
action_46 (72) = happyShift action_47
action_46 (73) = happyShift action_48
action_46 (21) = happyGoto action_68
action_46 (22) = happyGoto action_35
action_46 (23) = happyGoto action_36
action_46 (24) = happyGoto action_37
action_46 (25) = happyGoto action_69
action_46 _ = happyReduce_60

action_47 _ = happyReduce_56

action_48 _ = happyReduce_57

action_49 (66) = happyShift action_67
action_49 _ = happyReduce_68

action_50 _ = happyReduce_13

action_51 (67) = happyShift action_66
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (47) = happyShift action_65
action_52 (31) = happyGoto action_64
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (43) = happyShift action_38
action_53 (44) = happyShift action_39
action_53 (45) = happyShift action_40
action_53 (46) = happyShift action_41
action_53 (50) = happyShift action_42
action_53 (51) = happyShift action_43
action_53 (52) = happyShift action_44
action_53 (61) = happyShift action_45
action_53 (68) = happyShift action_46
action_53 (72) = happyShift action_47
action_53 (73) = happyShift action_48
action_53 (21) = happyGoto action_63
action_53 (22) = happyGoto action_35
action_53 (23) = happyGoto action_36
action_53 (24) = happyGoto action_37
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (59) = happyShift action_62
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (71) = happyShift action_60
action_55 (73) = happyShift action_61
action_55 (9) = happyGoto action_59
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (70) = happyShift action_58
action_56 _ = happyFail (happyExpListPerState 56)

action_57 _ = happyReduce_71

action_58 (8) = happyGoto action_121
action_58 _ = happyReduce_7

action_59 _ = happyReduce_8

action_60 _ = happyReduce_5

action_61 (68) = happyShift action_17
action_61 (32) = happyGoto action_120
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_66

action_63 (54) = happyShift action_80
action_63 (55) = happyShift action_81
action_63 (56) = happyShift action_82
action_63 (57) = happyShift action_83
action_63 (58) = happyShift action_84
action_63 (59) = happyShift action_85
action_63 (60) = happyShift action_86
action_63 (61) = happyShift action_87
action_63 (62) = happyShift action_88
action_63 (63) = happyShift action_89
action_63 (64) = happyShift action_90
action_63 _ = happyReduce_69

action_64 _ = happyReduce_65

action_65 (43) = happyShift action_38
action_65 (44) = happyShift action_39
action_65 (45) = happyShift action_40
action_65 (46) = happyShift action_41
action_65 (50) = happyShift action_42
action_65 (51) = happyShift action_43
action_65 (52) = happyShift action_44
action_65 (61) = happyShift action_45
action_65 (68) = happyShift action_46
action_65 (72) = happyShift action_47
action_65 (73) = happyShift action_48
action_65 (21) = happyGoto action_119
action_65 (22) = happyGoto action_35
action_65 (23) = happyGoto action_36
action_65 (24) = happyGoto action_37
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (39) = happyShift action_30
action_66 (40) = happyShift action_31
action_66 (68) = happyShift action_32
action_66 (73) = happyShift action_33
action_66 (16) = happyGoto action_28
action_66 (18) = happyGoto action_118
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (73) = happyShift action_51
action_67 (13) = happyGoto action_117
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (54) = happyShift action_80
action_68 (55) = happyShift action_81
action_68 (56) = happyShift action_82
action_68 (57) = happyShift action_83
action_68 (58) = happyShift action_84
action_68 (59) = happyShift action_85
action_68 (60) = happyShift action_86
action_68 (61) = happyShift action_87
action_68 (62) = happyShift action_88
action_68 (63) = happyShift action_89
action_68 (64) = happyShift action_90
action_68 _ = happyReduce_61

action_69 (66) = happyShift action_115
action_69 (69) = happyShift action_116
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_46

action_71 (67) = happyShift action_114
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (73) = happyShift action_113
action_72 (20) = happyGoto action_112
action_72 _ = happyReduce_30

action_73 _ = happyReduce_28

action_74 (47) = happyShift action_111
action_74 (54) = happyShift action_80
action_74 (55) = happyShift action_81
action_74 (56) = happyShift action_82
action_74 (57) = happyShift action_83
action_74 (58) = happyShift action_84
action_74 (59) = happyShift action_85
action_74 (60) = happyShift action_86
action_74 (61) = happyShift action_87
action_74 (62) = happyShift action_88
action_74 (63) = happyShift action_89
action_74 (64) = happyShift action_90
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (67) = happyShift action_110
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (67) = happyShift action_109
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (57) = happyShift action_83
action_77 (58) = happyShift action_84
action_77 (59) = happyShift action_85
action_77 (60) = happyShift action_86
action_77 (61) = happyShift action_87
action_77 (62) = happyShift action_88
action_77 (63) = happyShift action_89
action_77 (64) = happyShift action_90
action_77 _ = happyReduce_40

action_78 (73) = happyShift action_108
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (65) = happyShift action_78
action_79 _ = happyReduce_51

action_80 (43) = happyShift action_38
action_80 (44) = happyShift action_39
action_80 (45) = happyShift action_40
action_80 (46) = happyShift action_41
action_80 (50) = happyShift action_42
action_80 (51) = happyShift action_43
action_80 (52) = happyShift action_44
action_80 (61) = happyShift action_45
action_80 (68) = happyShift action_46
action_80 (72) = happyShift action_47
action_80 (73) = happyShift action_48
action_80 (21) = happyGoto action_107
action_80 (22) = happyGoto action_35
action_80 (23) = happyGoto action_36
action_80 (24) = happyGoto action_37
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (43) = happyShift action_38
action_81 (44) = happyShift action_39
action_81 (45) = happyShift action_40
action_81 (46) = happyShift action_41
action_81 (50) = happyShift action_42
action_81 (51) = happyShift action_43
action_81 (52) = happyShift action_44
action_81 (61) = happyShift action_45
action_81 (68) = happyShift action_46
action_81 (72) = happyShift action_47
action_81 (73) = happyShift action_48
action_81 (21) = happyGoto action_106
action_81 (22) = happyGoto action_35
action_81 (23) = happyGoto action_36
action_81 (24) = happyGoto action_37
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (43) = happyShift action_38
action_82 (44) = happyShift action_39
action_82 (45) = happyShift action_40
action_82 (46) = happyShift action_41
action_82 (50) = happyShift action_42
action_82 (51) = happyShift action_43
action_82 (52) = happyShift action_44
action_82 (61) = happyShift action_45
action_82 (68) = happyShift action_46
action_82 (72) = happyShift action_47
action_82 (73) = happyShift action_48
action_82 (21) = happyGoto action_105
action_82 (22) = happyGoto action_35
action_82 (23) = happyGoto action_36
action_82 (24) = happyGoto action_37
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (43) = happyShift action_38
action_83 (44) = happyShift action_39
action_83 (45) = happyShift action_40
action_83 (46) = happyShift action_41
action_83 (50) = happyShift action_42
action_83 (51) = happyShift action_43
action_83 (52) = happyShift action_44
action_83 (61) = happyShift action_45
action_83 (68) = happyShift action_46
action_83 (72) = happyShift action_47
action_83 (73) = happyShift action_48
action_83 (21) = happyGoto action_104
action_83 (22) = happyGoto action_35
action_83 (23) = happyGoto action_36
action_83 (24) = happyGoto action_37
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (43) = happyShift action_38
action_84 (44) = happyShift action_39
action_84 (45) = happyShift action_40
action_84 (46) = happyShift action_41
action_84 (50) = happyShift action_42
action_84 (51) = happyShift action_43
action_84 (52) = happyShift action_44
action_84 (61) = happyShift action_45
action_84 (68) = happyShift action_46
action_84 (72) = happyShift action_47
action_84 (73) = happyShift action_48
action_84 (21) = happyGoto action_103
action_84 (22) = happyGoto action_35
action_84 (23) = happyGoto action_36
action_84 (24) = happyGoto action_37
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (43) = happyShift action_38
action_85 (44) = happyShift action_39
action_85 (45) = happyShift action_40
action_85 (46) = happyShift action_41
action_85 (50) = happyShift action_42
action_85 (51) = happyShift action_43
action_85 (52) = happyShift action_44
action_85 (61) = happyShift action_45
action_85 (68) = happyShift action_46
action_85 (72) = happyShift action_47
action_85 (73) = happyShift action_48
action_85 (21) = happyGoto action_102
action_85 (22) = happyGoto action_35
action_85 (23) = happyGoto action_36
action_85 (24) = happyGoto action_37
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (43) = happyShift action_38
action_86 (44) = happyShift action_39
action_86 (45) = happyShift action_40
action_86 (46) = happyShift action_41
action_86 (50) = happyShift action_42
action_86 (51) = happyShift action_43
action_86 (52) = happyShift action_44
action_86 (61) = happyShift action_45
action_86 (68) = happyShift action_46
action_86 (72) = happyShift action_47
action_86 (73) = happyShift action_48
action_86 (21) = happyGoto action_101
action_86 (22) = happyGoto action_35
action_86 (23) = happyGoto action_36
action_86 (24) = happyGoto action_37
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (43) = happyShift action_38
action_87 (44) = happyShift action_39
action_87 (45) = happyShift action_40
action_87 (46) = happyShift action_41
action_87 (50) = happyShift action_42
action_87 (51) = happyShift action_43
action_87 (52) = happyShift action_44
action_87 (61) = happyShift action_45
action_87 (68) = happyShift action_46
action_87 (72) = happyShift action_47
action_87 (73) = happyShift action_48
action_87 (21) = happyGoto action_100
action_87 (22) = happyGoto action_35
action_87 (23) = happyGoto action_36
action_87 (24) = happyGoto action_37
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (43) = happyShift action_38
action_88 (44) = happyShift action_39
action_88 (45) = happyShift action_40
action_88 (46) = happyShift action_41
action_88 (50) = happyShift action_42
action_88 (51) = happyShift action_43
action_88 (52) = happyShift action_44
action_88 (61) = happyShift action_45
action_88 (68) = happyShift action_46
action_88 (72) = happyShift action_47
action_88 (73) = happyShift action_48
action_88 (21) = happyGoto action_99
action_88 (22) = happyGoto action_35
action_88 (23) = happyGoto action_36
action_88 (24) = happyGoto action_37
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (43) = happyShift action_38
action_89 (44) = happyShift action_39
action_89 (45) = happyShift action_40
action_89 (46) = happyShift action_41
action_89 (50) = happyShift action_42
action_89 (51) = happyShift action_43
action_89 (52) = happyShift action_44
action_89 (61) = happyShift action_45
action_89 (68) = happyShift action_46
action_89 (72) = happyShift action_47
action_89 (73) = happyShift action_48
action_89 (21) = happyGoto action_98
action_89 (22) = happyGoto action_35
action_89 (23) = happyGoto action_36
action_89 (24) = happyGoto action_37
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (43) = happyShift action_38
action_90 (44) = happyShift action_39
action_90 (45) = happyShift action_40
action_90 (46) = happyShift action_41
action_90 (50) = happyShift action_42
action_90 (51) = happyShift action_43
action_90 (52) = happyShift action_44
action_90 (61) = happyShift action_45
action_90 (68) = happyShift action_46
action_90 (72) = happyShift action_47
action_90 (73) = happyShift action_48
action_90 (21) = happyGoto action_97
action_90 (22) = happyGoto action_35
action_90 (23) = happyGoto action_36
action_90 (24) = happyGoto action_37
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (66) = happyShift action_95
action_91 (69) = happyShift action_96
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (53) = happyShift action_93
action_92 _ = happyReduce_24

action_93 (39) = happyShift action_30
action_93 (40) = happyShift action_31
action_93 (68) = happyShift action_32
action_93 (73) = happyShift action_33
action_93 (16) = happyGoto action_28
action_93 (18) = happyGoto action_94
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (53) = happyShift action_93
action_94 _ = happyReduce_27

action_95 (39) = happyShift action_30
action_95 (40) = happyShift action_31
action_95 (68) = happyShift action_32
action_95 (73) = happyShift action_33
action_95 (16) = happyGoto action_28
action_95 (18) = happyGoto action_131
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_22

action_97 _ = happyReduce_49

action_98 _ = happyReduce_48

action_99 _ = happyReduce_47

action_100 (62) = happyShift action_88
action_100 (63) = happyShift action_89
action_100 (64) = happyShift action_90
action_100 _ = happyReduce_45

action_101 (62) = happyShift action_88
action_101 (63) = happyShift action_89
action_101 (64) = happyShift action_90
action_101 _ = happyReduce_44

action_102 (57) = happyFail []
action_102 (58) = happyFail []
action_102 (59) = happyFail []
action_102 (60) = happyShift action_86
action_102 (61) = happyShift action_87
action_102 (62) = happyShift action_88
action_102 (63) = happyShift action_89
action_102 (64) = happyShift action_90
action_102 _ = happyReduce_42

action_103 (57) = happyFail []
action_103 (58) = happyFail []
action_103 (59) = happyFail []
action_103 (60) = happyShift action_86
action_103 (61) = happyShift action_87
action_103 (62) = happyShift action_88
action_103 (63) = happyShift action_89
action_103 (64) = happyShift action_90
action_103 _ = happyReduce_41

action_104 (57) = happyFail []
action_104 (58) = happyFail []
action_104 (59) = happyFail []
action_104 (60) = happyShift action_86
action_104 (61) = happyShift action_87
action_104 (62) = happyShift action_88
action_104 (63) = happyShift action_89
action_104 (64) = happyShift action_90
action_104 _ = happyReduce_43

action_105 (56) = happyShift action_82
action_105 (57) = happyShift action_83
action_105 (58) = happyShift action_84
action_105 (59) = happyShift action_85
action_105 (60) = happyShift action_86
action_105 (61) = happyShift action_87
action_105 (62) = happyShift action_88
action_105 (63) = happyShift action_89
action_105 (64) = happyShift action_90
action_105 _ = happyReduce_38

action_106 (55) = happyShift action_81
action_106 (56) = happyShift action_82
action_106 (57) = happyShift action_83
action_106 (58) = happyShift action_84
action_106 (59) = happyShift action_85
action_106 (60) = happyShift action_86
action_106 (61) = happyShift action_87
action_106 (62) = happyShift action_88
action_106 (63) = happyShift action_89
action_106 (64) = happyShift action_90
action_106 _ = happyReduce_37

action_107 (54) = happyShift action_80
action_107 (55) = happyShift action_81
action_107 (56) = happyShift action_82
action_107 (57) = happyShift action_83
action_107 (58) = happyShift action_84
action_107 (59) = happyShift action_85
action_107 (60) = happyShift action_86
action_107 (61) = happyShift action_87
action_107 (62) = happyShift action_88
action_107 (63) = happyShift action_89
action_107 (64) = happyShift action_90
action_107 _ = happyReduce_36

action_108 _ = happyReduce_53

action_109 (39) = happyShift action_30
action_109 (40) = happyShift action_31
action_109 (68) = happyShift action_32
action_109 (73) = happyShift action_33
action_109 (16) = happyGoto action_28
action_109 (18) = happyGoto action_130
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (39) = happyShift action_30
action_110 (40) = happyShift action_31
action_110 (68) = happyShift action_32
action_110 (73) = happyShift action_33
action_110 (16) = happyGoto action_28
action_110 (18) = happyGoto action_129
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (43) = happyShift action_38
action_111 (44) = happyShift action_39
action_111 (45) = happyShift action_40
action_111 (46) = happyShift action_41
action_111 (50) = happyShift action_42
action_111 (51) = happyShift action_43
action_111 (52) = happyShift action_44
action_111 (61) = happyShift action_45
action_111 (68) = happyShift action_46
action_111 (72) = happyShift action_47
action_111 (73) = happyShift action_48
action_111 (21) = happyGoto action_128
action_111 (22) = happyGoto action_35
action_111 (23) = happyGoto action_36
action_111 (24) = happyGoto action_37
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (66) = happyShift action_126
action_112 (69) = happyShift action_127
action_112 _ = happyFail (happyExpListPerState 112)

action_113 _ = happyReduce_31

action_114 (39) = happyShift action_30
action_114 (40) = happyShift action_31
action_114 (68) = happyShift action_32
action_114 (73) = happyShift action_33
action_114 (16) = happyGoto action_125
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (43) = happyShift action_38
action_115 (44) = happyShift action_39
action_115 (45) = happyShift action_40
action_115 (46) = happyShift action_41
action_115 (50) = happyShift action_42
action_115 (51) = happyShift action_43
action_115 (52) = happyShift action_44
action_115 (61) = happyShift action_45
action_115 (68) = happyShift action_46
action_115 (72) = happyShift action_47
action_115 (73) = happyShift action_48
action_115 (21) = happyGoto action_124
action_115 (22) = happyGoto action_35
action_115 (23) = happyGoto action_36
action_115 (24) = happyGoto action_37
action_115 _ = happyFail (happyExpListPerState 115)

action_116 _ = happyReduce_55

action_117 _ = happyReduce_14

action_118 (53) = happyShift action_93
action_118 _ = happyReduce_15

action_119 (54) = happyShift action_80
action_119 (55) = happyShift action_81
action_119 (56) = happyShift action_82
action_119 (57) = happyShift action_83
action_119 (58) = happyShift action_84
action_119 (59) = happyShift action_85
action_119 (60) = happyShift action_86
action_119 (61) = happyShift action_87
action_119 (62) = happyShift action_88
action_119 (63) = happyShift action_89
action_119 (64) = happyShift action_90
action_119 _ = happyReduce_70

action_120 (67) = happyShift action_123
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (71) = happyShift action_122
action_121 (73) = happyShift action_61
action_121 (9) = happyGoto action_59
action_121 _ = happyFail (happyExpListPerState 121)

action_122 _ = happyReduce_6

action_123 (39) = happyShift action_30
action_123 (40) = happyShift action_31
action_123 (68) = happyShift action_32
action_123 (73) = happyShift action_33
action_123 (16) = happyGoto action_28
action_123 (18) = happyGoto action_137
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (54) = happyShift action_80
action_124 (55) = happyShift action_81
action_124 (56) = happyShift action_82
action_124 (57) = happyShift action_83
action_124 (58) = happyShift action_84
action_124 (59) = happyShift action_85
action_124 (60) = happyShift action_86
action_124 (61) = happyShift action_87
action_124 (62) = happyShift action_88
action_124 (63) = happyShift action_89
action_124 (64) = happyShift action_90
action_124 _ = happyReduce_62

action_125 (53) = happyShift action_136
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (73) = happyShift action_135
action_126 _ = happyFail (happyExpListPerState 126)

action_127 _ = happyReduce_29

action_128 (48) = happyShift action_134
action_128 (54) = happyShift action_80
action_128 (55) = happyShift action_81
action_128 (56) = happyShift action_82
action_128 (57) = happyShift action_83
action_128 (58) = happyShift action_84
action_128 (59) = happyShift action_85
action_128 (60) = happyShift action_86
action_128 (61) = happyShift action_87
action_128 (62) = happyShift action_88
action_128 (63) = happyShift action_89
action_128 (64) = happyShift action_90
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (53) = happyShift action_93
action_129 (65) = happyShift action_133
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (53) = happyShift action_93
action_130 (65) = happyShift action_132
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (53) = happyShift action_93
action_131 _ = happyReduce_25

action_132 (43) = happyShift action_38
action_132 (44) = happyShift action_39
action_132 (45) = happyShift action_40
action_132 (46) = happyShift action_41
action_132 (50) = happyShift action_42
action_132 (51) = happyShift action_43
action_132 (52) = happyShift action_44
action_132 (61) = happyShift action_45
action_132 (68) = happyShift action_46
action_132 (72) = happyShift action_47
action_132 (73) = happyShift action_48
action_132 (21) = happyGoto action_141
action_132 (22) = happyGoto action_35
action_132 (23) = happyGoto action_36
action_132 (24) = happyGoto action_37
action_132 _ = happyFail (happyExpListPerState 132)

action_133 (43) = happyShift action_38
action_133 (44) = happyShift action_39
action_133 (45) = happyShift action_40
action_133 (46) = happyShift action_41
action_133 (50) = happyShift action_42
action_133 (51) = happyShift action_43
action_133 (52) = happyShift action_44
action_133 (61) = happyShift action_45
action_133 (68) = happyShift action_46
action_133 (72) = happyShift action_47
action_133 (73) = happyShift action_48
action_133 (21) = happyGoto action_140
action_133 (22) = happyGoto action_35
action_133 (23) = happyGoto action_36
action_133 (24) = happyGoto action_37
action_133 _ = happyFail (happyExpListPerState 133)

action_134 (43) = happyShift action_38
action_134 (44) = happyShift action_39
action_134 (45) = happyShift action_40
action_134 (46) = happyShift action_41
action_134 (50) = happyShift action_42
action_134 (51) = happyShift action_43
action_134 (52) = happyShift action_44
action_134 (61) = happyShift action_45
action_134 (68) = happyShift action_46
action_134 (72) = happyShift action_47
action_134 (73) = happyShift action_48
action_134 (21) = happyGoto action_139
action_134 (22) = happyGoto action_35
action_134 (23) = happyGoto action_36
action_134 (24) = happyGoto action_37
action_134 _ = happyFail (happyExpListPerState 134)

action_135 _ = happyReduce_32

action_136 (43) = happyShift action_38
action_136 (44) = happyShift action_39
action_136 (45) = happyShift action_40
action_136 (46) = happyShift action_41
action_136 (50) = happyShift action_42
action_136 (51) = happyShift action_43
action_136 (52) = happyShift action_44
action_136 (61) = happyShift action_45
action_136 (68) = happyShift action_46
action_136 (72) = happyShift action_47
action_136 (73) = happyShift action_48
action_136 (21) = happyGoto action_138
action_136 (22) = happyGoto action_35
action_136 (23) = happyGoto action_36
action_136 (24) = happyGoto action_37
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (53) = happyShift action_93
action_137 _ = happyReduce_9

action_138 (54) = happyShift action_80
action_138 (55) = happyShift action_81
action_138 (56) = happyShift action_82
action_138 (57) = happyShift action_83
action_138 (58) = happyShift action_84
action_138 (59) = happyShift action_85
action_138 (60) = happyShift action_86
action_138 (61) = happyShift action_87
action_138 (62) = happyShift action_88
action_138 (63) = happyShift action_89
action_138 (64) = happyShift action_90
action_138 _ = happyReduce_33

action_139 (54) = happyShift action_80
action_139 (55) = happyShift action_81
action_139 (56) = happyShift action_82
action_139 (57) = happyShift action_83
action_139 (58) = happyShift action_84
action_139 (59) = happyShift action_85
action_139 (60) = happyShift action_86
action_139 (61) = happyShift action_87
action_139 (62) = happyShift action_88
action_139 (63) = happyShift action_89
action_139 (64) = happyShift action_90
action_139 _ = happyReduce_39

action_140 (54) = happyShift action_80
action_140 (55) = happyShift action_81
action_140 (56) = happyShift action_82
action_140 (57) = happyShift action_83
action_140 (58) = happyShift action_84
action_140 (59) = happyShift action_85
action_140 (60) = happyShift action_86
action_140 (61) = happyShift action_87
action_140 (62) = happyShift action_88
action_140 (63) = happyShift action_89
action_140 (64) = happyShift action_90
action_140 _ = happyReduce_35

action_141 (54) = happyShift action_80
action_141 (55) = happyShift action_81
action_141 (56) = happyShift action_82
action_141 (57) = happyShift action_83
action_141 (58) = happyShift action_84
action_141 (59) = happyShift action_85
action_141 (60) = happyShift action_86
action_141 (61) = happyShift action_87
action_141 (62) = happyShift action_88
action_141 (63) = happyShift action_89
action_141 (64) = happyShift action_90
action_141 _ = happyReduce_34

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	(HappyAbsSyn26  happy_var_3) `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Program (reverse happy_var_1)  (reverse happy_var_2) (reverse happy_var_3) (reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 : happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	(HappyAbsSyn32  happy_var_3) `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (ClassDecl (AClsNm happy_var_2 happy_var_3) happy_var_4
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  7 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (ClassDef (Just (ClsNm "Object")) (reverse happy_var_2)
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 5 7 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (ClassDef (Just (ClsNm happy_var_2)) (reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_0  8 happyReduction_7
happyReduction_7  =  HappyAbsSyn8
		 ([]
	)

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_2 : happy_var_1
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 4 9 happyReduction_9
happyReduction_9 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_2) `HappyStk`
	(HappyTerminal (TokenSym happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (FieldDecl (AFldNm happy_var_1 happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_0  10 happyReduction_10
happyReduction_10  =  HappyAbsSyn10
		 ([]
	)

happyReduce_11 = happySpecReduce_2  10 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_2 : happy_var_1
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 4 11 happyReduction_12
happyReduction_12 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (VarDecl happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_1  12 happyReduction_13
happyReduction_13 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  12 happyReduction_14
happyReduction_14 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_3 : happy_var_1
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  13 happyReduction_15
happyReduction_15 (HappyAbsSyn18  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn13
		 (VarDecl happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_0  14 happyReduction_16
happyReduction_16  =  HappyAbsSyn14
		 ([]
	)

happyReduce_17 = happySpecReduce_2  14 happyReduction_17
happyReduction_17 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_2 : happy_var_1
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  15 happyReduction_18
happyReduction_18 (HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (Assertion happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  16 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn16
		 (BoolT
	)

happyReduce_20 = happySpecReduce_1  16 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn16
		 (IntT
	)

happyReduce_21 = happySpecReduce_1  16 happyReduction_21
happyReduction_21 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn16
		 (ClassT (ClsNm happy_var_1)
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  16 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (let tcs = happy_var_2 in if length tcs == 1 then head tcs else TupleT (reverse tcs)
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_0  17 happyReduction_23
happyReduction_23  =  HappyAbsSyn17
		 ([]
	)

happyReduce_24 = happySpecReduce_1  17 happyReduction_24
happyReduction_24 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  17 happyReduction_25
happyReduction_25 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_3 : happy_var_1
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  18 happyReduction_26
happyReduction_26 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  18 happyReduction_27
happyReduction_27 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (FunT happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  19 happyReduction_28
happyReduction_28 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn19
		 (VarP happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  19 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (let vcs = happy_var_2 in if length vcs == 1 then VarP (head vcs) else VarListP (reverse vcs)
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_0  20 happyReduction_30
happyReduction_30  =  HappyAbsSyn20
		 ([]
	)

happyReduce_31 = happySpecReduce_1  20 happyReduction_31
happyReduction_31 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn20
		 ([happy_var_1]
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  20 happyReduction_32
happyReduction_32 (HappyTerminal (TokenSym happy_var_3))
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_3 : happy_var_1
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happyReduce 6 21 happyReduction_33
happyReduction_33 ((HappyAbsSyn21  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (FunE () happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 6 21 happyReduction_34
happyReduction_34 ((HappyAbsSyn21  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (QuantifE () All happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_35 = happyReduce 6 21 happyReduction_35
happyReduction_35 ((HappyAbsSyn21  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (QuantifE () Ex happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_3  21 happyReduction_36
happyReduction_36 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOpE () (BBool BBimpl) happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  21 happyReduction_37
happyReduction_37 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOpE () (BBool BBor) happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  21 happyReduction_38
happyReduction_38 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOpE () (BBool BBand) happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happyReduce 6 21 happyReduction_39
happyReduction_39 ((HappyAbsSyn21  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (IfThenElseE () happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_2  21 happyReduction_40
happyReduction_40 (HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (UnaOpE () (UBool UBneg) happy_var_2
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  21 happyReduction_41
happyReduction_41 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOpE () (BCompar BClt) happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  21 happyReduction_42
happyReduction_42 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOpE () (BCompar BCgt) happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  21 happyReduction_43
happyReduction_43 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOpE () (BCompar BCeq) happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  21 happyReduction_44
happyReduction_44 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOpE () (BArith BAadd) happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  21 happyReduction_45
happyReduction_45 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOpE () (BArith BAsub) happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  21 happyReduction_46
happyReduction_46 (HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (UnaOpE () (UArith UAminus) happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  21 happyReduction_47
happyReduction_47 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOpE () (BArith BAmul) happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  21 happyReduction_48
happyReduction_48 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOpE () (BArith BAdiv) happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  21 happyReduction_49
happyReduction_49 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOpE () (BArith BAmod) happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  21 happyReduction_50
happyReduction_50 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_2  22 happyReduction_51
happyReduction_51 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (AppE () happy_var_1 happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  22 happyReduction_52
happyReduction_52 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  23 happyReduction_53
happyReduction_53 (HappyTerminal (TokenSym happy_var_3))
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (FldAccE () happy_var_1 (FldNm happy_var_3)
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  23 happyReduction_54
happyReduction_54 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  24 happyReduction_55
happyReduction_55 _
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (let ecs = happy_var_2 in if length ecs == 1 then head ecs else TupleE () (reverse ecs)
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  24 happyReduction_56
happyReduction_56 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn24
		 (ValE () (IntV happy_var_1)
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  24 happyReduction_57
happyReduction_57 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn24
		 (VarE () happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  24 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn24
		 (ValE () (BoolV True)
	)

happyReduce_59 = happySpecReduce_1  24 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn24
		 (ValE () (BoolV False)
	)

happyReduce_60 = happySpecReduce_0  25 happyReduction_60
happyReduction_60  =  HappyAbsSyn25
		 ([]
	)

happyReduce_61 = happySpecReduce_1  25 happyReduction_61
happyReduction_61 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn25
		 ([happy_var_1]
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  25 happyReduction_62
happyReduction_62 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_3 : happy_var_1
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_0  26 happyReduction_63
happyReduction_63  =  HappyAbsSyn26
		 ([]
	)

happyReduce_64 = happySpecReduce_2  26 happyReduction_64
happyReduction_64 (HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_2 : happy_var_1
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happyReduce 4 27 happyReduction_65
happyReduction_65 ((HappyAbsSyn31  happy_var_4) `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	(HappyAbsSyn28  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Rule happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_66 = happyReduce 4 28 happyReduction_66
happyReduction_66 (_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (happy_var_3
	) `HappyStk` happyRest

happyReduce_67 = happySpecReduce_0  29 happyReduction_67
happyReduction_67  =  HappyAbsSyn29
		 ([]
	)

happyReduce_68 = happySpecReduce_2  29 happyReduction_68
happyReduction_68 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn29
		 (reverse happy_var_2
	)
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_2  30 happyReduction_69
happyReduction_69 (HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (happy_var_2
	)
happyReduction_69 _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_2  31 happyReduction_70
happyReduction_70 (HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (happy_var_2
	)
happyReduction_70 _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  32 happyReduction_71
happyReduction_71 _
	(HappyTerminal (TokenNum happy_var_2))
	_
	 =  HappyAbsSyn32
		 (GFAnnot happy_var_2
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 74 74 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenAssert -> cont 33;
	TokenClass -> cont 34;
	TokenDecl -> cont 35;
	TokenDefn -> cont 36;
	TokenExtends -> cont 37;
	TokenRule -> cont 38;
	TokenBool -> cont 39;
	TokenInt -> cont 40;
	TokenLet -> cont 41;
	TokenIn -> cont 42;
	TokenNot -> cont 43;
	TokenAll -> cont 44;
	TokenEx -> cont 45;
	TokenIf -> cont 46;
	TokenThen -> cont 47;
	TokenElse -> cont 48;
	TokenFor -> cont 49;
	TokenTrue -> cont 50;
	TokenFalse -> cont 51;
	TokenLambda -> cont 52;
	TokenArrow -> cont 53;
	TokenImpl -> cont 54;
	TokenOr -> cont 55;
	TokenAnd -> cont 56;
	TokenEq -> cont 57;
	TokenLt -> cont 58;
	TokenGt -> cont 59;
	TokenAdd -> cont 60;
	TokenSub -> cont 61;
	TokenMul -> cont 62;
	TokenDiv -> cont 63;
	TokenMod -> cont 64;
	TokenDot -> cont 65;
	TokenComma -> cont 66;
	TokenColon -> cont 67;
	TokenLParen -> cont 68;
	TokenRParen -> cont 69;
	TokenLBrace -> cont 70;
	TokenRBrace -> cont 71;
	TokenNum happy_dollar_dollar -> cont 72;
	TokenSym happy_dollar_dollar -> cont 73;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 74 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Except String a -> (a -> Except String b) -> Except String b
happyThen = ((>>=))
happyReturn :: () => a -> Except String a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Except String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Except String a
happyError' = (\(tokens, _) -> parseError tokens)
program tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseProgram:: String -> Either String (Program (Maybe ClassName) ())
parseProgram input = runExcept $ do
  tokenStream <- scanTokens input
  program tokenStream

-- still needed ???
-- parseTokens :: String -> Either String [Token]
-- parseTokens = runExcept . scanTokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
