{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module TextuaL4.ParTextuaL
  ( happyError
  , myLexer
  , pRule
  ) where

import Prelude

import qualified TextuaL4.AbsTextuaL
import TextuaL4.LexTextuaL
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap4 = HappyWrap4 (Double)
happyIn4 :: (Double) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap4 x)
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> HappyWrap4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 (Integer)
happyIn5 :: (Integer) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 (TextuaL4.AbsTextuaL.Text)
happyIn6 :: (TextuaL4.AbsTextuaL.Text) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 (TextuaL4.AbsTextuaL.Rule)
happyIn7 :: (TextuaL4.AbsTextuaL.Rule) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 (TextuaL4.AbsTextuaL.IsA)
happyIn8 :: (TextuaL4.AbsTextuaL.IsA) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 ([TextuaL4.AbsTextuaL.IsA])
happyIn9 :: ([TextuaL4.AbsTextuaL.IsA]) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 (TextuaL4.AbsTextuaL.Fields)
happyIn10 :: (TextuaL4.AbsTextuaL.Fields) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 (TextuaL4.AbsTextuaL.Deontic)
happyIn11 :: (TextuaL4.AbsTextuaL.Deontic) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 (TextuaL4.AbsTextuaL.Who)
happyIn12 :: (TextuaL4.AbsTextuaL.Who) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 (TextuaL4.AbsTextuaL.InlineHornlike)
happyIn13 :: (TextuaL4.AbsTextuaL.InlineHornlike) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 (TextuaL4.AbsTextuaL.RelationalPredicate)
happyIn14 :: (TextuaL4.AbsTextuaL.RelationalPredicate) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 (TextuaL4.AbsTextuaL.MTExpr)
happyIn15 :: (TextuaL4.AbsTextuaL.MTExpr) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 (TextuaL4.AbsTextuaL.Bool)
happyIn16 :: (TextuaL4.AbsTextuaL.Bool) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 ([TextuaL4.AbsTextuaL.MTExpr])
happyIn17 :: ([TextuaL4.AbsTextuaL.MTExpr]) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 (TextuaL4.AbsTextuaL.BoolStruct)
happyIn18 :: (TextuaL4.AbsTextuaL.BoolStruct) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 ([TextuaL4.AbsTextuaL.BoolStruct])
happyIn19 :: ([TextuaL4.AbsTextuaL.BoolStruct]) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 (TextuaL4.AbsTextuaL.RPRel)
happyIn20 :: (TextuaL4.AbsTextuaL.RPRel) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 (TextuaL4.AbsTextuaL.TComparison)
happyIn21 :: (TextuaL4.AbsTextuaL.TComparison) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x60\x1a\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\xd0\x01\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x0a\x08\x00\x04\x74\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x69\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x40\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xd6\x0c\x3d\xf7\x06\x00\x00\x00\x00\x00\x00\x20\x84\x10\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x01\x80\x80\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x05\x04\x00\x02\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x01\x80\x80\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x10\x00\x08\xe8\x00\x00\x00\x00\x0a\x08\x00\x04\x74\x00\x00\x00\x00\x05\x04\x00\x02\x3a\x00\x00\x00\x00\x00\x00\x8c\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x20\x00\x10\xd0\x01\x00\x00\x00\x14\x10\x00\x08\xe8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x10\x00\x00\xe8\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x80\x00\x40\x40\x07\x00\x00\x00\x50\x40\x00\x20\xa0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x02\x00\x01\x1d\x00\x00\x00\x00\x00\x00\x42\x08\x00\x00\x00\x00\xa0\x80\x00\x40\x40\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x40\x00\x20\xa0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x10\x00\x08\xe8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x01\x80\x80\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pRule","Double","Integer","Text","Rule","IsA","ListIsA","Fields","Deontic","Who","InlineHornlike","RelationalPredicate","MTExpr","Bool","ListMTExpr","BoolStruct","ListBoolStruct","RPRel","TComparison","'('","')'","','","';'","'<'","'<='","'>'","'>='","'A'","'ABOUT'","'AFTER'","'ALL'","'AND'","'ANY'","'BEFORE'","'BY'","'DECIDE'","'DECLARE'","'DIVIDE'","'EQUALS'","'EVERY'","'False'","'GIVEN'","'GIVETH'","'HAS'","'IF'","'IN'","'IS'","'MAP'","'MAX'","'MAY'","'MEANS'","'MIN'","'MINUS'","'MODULO'","'MUST'","'NOT'","'ON'","'OR'","'PRODUCT'","'SHANT'","'SUBJECT'","'SUM'","'TO'","'True'","'WHO'","L_doubl","L_integ","L_Text","%eof"]
        bit_start = st Prelude.* 71
        bit_end = (st Prelude.+ 1) Prelude.* 71
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..70]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x17\x00\xd6\xff\x00\x00\xea\xff\xdc\xff\x0a\x00\xeb\xff\x05\x00\xeb\xff\xeb\xff\x00\x00\x09\x00\x48\x00\x5a\x00\x17\x00\x00\x00\x00\x00\x08\x00\x00\x00\x0a\x00\x00\x00\xfc\xff\x14\x00\x6f\x00\x73\x00\x00\x00\x05\x00\x00\x00\x00\x00\x70\x00\x00\x00\x6e\x00\x05\x00\x00\x00\x05\x00\x00\x00\x66\x00\x00\x00\x05\x00\x05\x00\x05\x00\xda\x00\x00\x00\x00\x00\x00\x00\x05\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7d\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x00\x00\x9b\x00\x9c\x00\x00\x00\x7b\x00\x0a\x00\xa7\x00\x8e\x00\x97\x00\x00\x00\x05\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\xe9\xff\x05\x00\x00\x00\xbd\x00\xc2\x00\xc3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\xd1\x00\xd2\x00\x05\x00\x00\x00\x00\x00\xb6\x00\xb6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\x00\x29\x00\x71\x00\xfb\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x00\x00\x00\x00\x00\x00\x00\x00\xeb\x00\x00\x00\x5e\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\xd3\x00\x00\x00\x00\x00\x85\x00\x00\x00\x94\x00\x00\x00\x00\x01\x00\x00\x3f\x00\x45\x00\x99\x00\x51\x00\x00\x00\x00\x00\x00\x00\xa8\x00\xad\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\xe8\x00\x00\x00\xd6\x00\x00\x00\x00\x00\x55\x00\x5b\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbc\x00\xf4\x00\xc1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6b\x00\x00\x00\xd0\x00\x00\x00\x00\x00\x00\x00\xd5\x00\x00\x00\x00\x00\xfa\x00\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xfe\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xf0\xff\x00\x00\xef\xff\x00\x00\xe2\xff\xe3\xff\xe4\xff\xd5\xff\xde\xff\xe1\xff\xe6\xff\x00\x00\x00\x00\x00\x00\xdf\xff\x00\x00\xe0\xff\xfd\xff\xec\xff\xe4\xff\xf5\xff\x00\x00\xf6\xff\x00\x00\xfb\xff\x00\x00\xd6\xff\x00\x00\x00\x00\x00\x00\x00\x00\xea\xff\xeb\xff\xe9\xff\x00\x00\x00\x00\xbd\xff\xcf\xff\xce\xff\xcd\xff\xcc\xff\xb8\xff\xbb\xff\xc8\xff\xbc\xff\xba\xff\xc3\xff\xd0\xff\xd1\xff\xcb\xff\xd2\xff\xbe\xff\xbf\xff\xc0\xff\xc4\xff\xc2\xff\xc9\xff\xb9\xff\xc7\xff\xc5\xff\x00\x00\xc6\xff\xdd\xff\x00\x00\x00\x00\xfa\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf3\xff\xee\xff\x00\x00\x00\x00\xc1\xff\xca\xff\xe5\xff\xe8\xff\x00\x00\x00\x00\x00\x00\xf9\xff\xd4\xff\x00\x00\x00\x00\xed\xff\xf4\xff\xdc\xff\xd9\xff\x00\x00\xe7\xff\x00\x00\xf8\xff\x00\x00\x00\x00\x00\x00\xf1\xff\xf2\xff\xda\xff\xd8\xff\xf7\xff\xd3\xff\xd7\xff\xdb\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x05\x00\x06\x00\x07\x00\x08\x00\x2f\x00\x0a\x00\x0b\x00\x1f\x00\x0d\x00\x20\x00\x0f\x00\x10\x00\x24\x00\x32\x00\x13\x00\x14\x00\x0c\x00\x29\x00\x0e\x00\x0c\x00\x19\x00\x0e\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x16\x00\x31\x00\x21\x00\x22\x00\x23\x00\x16\x00\x25\x00\x26\x00\x27\x00\x28\x00\x1c\x00\x2a\x00\x2b\x00\x11\x00\x12\x00\x25\x00\x02\x00\x15\x00\x04\x00\x17\x00\x18\x00\x02\x00\x03\x00\x2d\x00\x1f\x00\x2f\x00\x30\x00\x31\x00\x2d\x00\x24\x00\x2f\x00\x30\x00\x31\x00\x02\x00\x29\x00\x04\x00\x00\x00\x01\x00\x02\x00\x2e\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x31\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x00\x00\x01\x00\x02\x00\x07\x00\x11\x00\x09\x00\x00\x00\x01\x00\x02\x00\x04\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x00\x00\x01\x00\x02\x00\x10\x00\x11\x00\x01\x00\x00\x00\x01\x00\x02\x00\x01\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x07\x00\x08\x00\x00\x00\x01\x00\x02\x00\x1a\x00\x19\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x31\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x01\x00\x01\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x2c\x00\x31\x00\x00\x00\x01\x00\x02\x00\x09\x00\x1a\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x31\x00\x03\x00\x00\x00\x01\x00\x02\x00\x02\x00\x02\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x02\x00\x02\x00\x00\x00\x01\x00\x02\x00\x02\x00\x06\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x31\x00\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0b\x00\x0c\x00\x0d\x00\x1f\x00\x20\x00\x07\x00\x02\x00\x02\x00\x24\x00\x04\x00\x05\x00\x02\x00\x02\x00\x29\x00\x04\x00\x05\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x03\x00\x35\x00\x36\x00\x2b\x00\x37\x00\x21\x00\x38\x00\x39\x00\x2c\x00\xff\xff\x3a\x00\x3b\x00\x18\x00\x2d\x00\x19\x00\x4b\x00\x3c\x00\x4c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x1a\x00\x0b\x00\x41\x00\x42\x00\x43\x00\x1a\x00\x44\x00\x45\x00\x46\x00\x47\x00\x50\x00\x48\x00\x49\x00\x06\x00\x07\x00\x1b\x00\x0b\x00\x08\x00\x1d\x00\x09\x00\x0a\x00\x03\x00\x04\x00\x1c\x00\x2b\x00\x03\x00\x1d\x00\x0b\x00\x1c\x00\x2c\x00\x03\x00\x1d\x00\x0b\x00\x0b\x00\x2d\x00\x0c\x00\x0f\x00\x10\x00\x11\x00\x2e\x00\x03\x00\x4c\x00\x0f\x00\x10\x00\x11\x00\x0b\x00\x12\x00\x13\x00\x14\x00\x15\x00\x5d\x00\x5f\x00\x12\x00\x13\x00\x14\x00\x15\x00\x5d\x00\x5e\x00\x0f\x00\x10\x00\x11\x00\x59\x00\x4f\x00\x5a\x00\x0f\x00\x10\x00\x11\x00\x4e\x00\x12\x00\x13\x00\x14\x00\x15\x00\x5d\x00\x69\x00\x12\x00\x13\x00\x14\x00\x15\x00\x5d\x00\x68\x00\x0f\x00\x10\x00\x11\x00\x2e\x00\x2f\x00\x28\x00\x0f\x00\x10\x00\x11\x00\x27\x00\x12\x00\x13\x00\x14\x00\x15\x00\x5d\x00\x70\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x0f\x00\x10\x00\x11\x00\x28\x00\x29\x00\x0f\x00\x10\x00\x11\x00\x23\x00\x25\x00\x12\x00\x13\x00\x14\x00\x15\x00\x25\x00\x12\x00\x13\x00\x14\x00\x15\x00\x21\x00\x0f\x00\x10\x00\x11\x00\x0b\x00\x57\x00\x0f\x00\x10\x00\x11\x00\x55\x00\x54\x00\x12\x00\x13\x00\x14\x00\x15\x00\x61\x00\x12\x00\x13\x00\x14\x00\x15\x00\x5c\x00\x0f\x00\x10\x00\x11\x00\x56\x00\x0b\x00\x0f\x00\x10\x00\x11\x00\x51\x00\x6b\x00\x12\x00\x13\x00\x14\x00\x15\x00\x58\x00\x12\x00\x13\x00\x14\x00\x15\x00\x57\x00\x0f\x00\x10\x00\x11\x00\x0b\x00\x65\x00\x0f\x00\x10\x00\x11\x00\x64\x00\x63\x00\x12\x00\x13\x00\x14\x00\x15\x00\x67\x00\x12\x00\x13\x00\x14\x00\x15\x00\x65\x00\x0f\x00\x10\x00\x11\x00\x6f\x00\x6e\x00\x0f\x00\x10\x00\x11\x00\x6b\x00\x23\x00\x12\x00\x13\x00\x14\x00\x15\x00\x6f\x00\x12\x00\x13\x00\x14\x00\x15\x00\x6c\x00\x0f\x00\x10\x00\x1e\x00\x0b\x00\x0f\x00\x10\x00\x1e\x00\x0f\x00\x10\x00\x1e\x00\x1f\x00\x13\x00\x14\x00\x15\x00\x51\x00\x13\x00\x14\x00\x15\x00\x13\x00\x14\x00\x49\x00\x2b\x00\x5c\x00\x66\x00\x72\x00\x0b\x00\x2c\x00\x0d\x00\x0e\x00\x71\x00\x0b\x00\x2d\x00\x0d\x00\x60\x00\x0b\x00\x00\x00\x0d\x00\x52\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 71) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71)
	]

happy_n_terms = 51 :: Prelude.Int
happy_n_nonterms = 18 :: Prelude.Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TD happy_var_1)) -> 
	happyIn4
		 ((read happy_var_1) :: Double
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn5
		 ((read happy_var_1) :: Integer
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_Text happy_var_1)) -> 
	happyIn6
		 (TextuaL4.AbsTextuaL.Text happy_var_1
	)}

happyReduce_4 = happySpecReduce_3  3# happyReduction_4
happyReduction_4 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_2 of { (HappyWrap8 happy_var_2) -> 
	case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
	happyIn7
		 (TextuaL4.AbsTextuaL.TypeDecl happy_var_2 happy_var_3
	)}}

happyReduce_5 = happySpecReduce_3  3# happyReduction_5
happyReduction_5 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	case happyOut7 happy_x_3 of { (HappyWrap7 happy_var_3) -> 
	happyIn7
		 (TextuaL4.AbsTextuaL.Given happy_var_2 happy_var_3
	)}}

happyReduce_6 = happyReduce 4# 3# happyReduction_6
happyReduction_6 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { (HappyWrap18 happy_var_2) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	case happyOut18 happy_x_4 of { (HappyWrap18 happy_var_4) -> 
	happyIn7
		 (TextuaL4.AbsTextuaL.RegSimple happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_7 = happyReduce 5# 3# happyReduction_7
happyReduction_7 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { (HappyWrap18 happy_var_2) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	case happyOut11 happy_x_4 of { (HappyWrap11 happy_var_4) -> 
	case happyOut18 happy_x_5 of { (HappyWrap18 happy_var_5) -> 
	happyIn7
		 (TextuaL4.AbsTextuaL.RegWho happy_var_2 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest}}}}

happyReduce_8 = happyReduce 6# 3# happyReduction_8
happyReduction_8 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { (HappyWrap18 happy_var_2) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	case happyOut13 happy_x_4 of { (HappyWrap13 happy_var_4) -> 
	case happyOut11 happy_x_5 of { (HappyWrap11 happy_var_5) -> 
	case happyOut18 happy_x_6 of { (HappyWrap18 happy_var_6) -> 
	happyIn7
		 (TextuaL4.AbsTextuaL.RegWhoInline happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}

happyReduce_9 = happySpecReduce_3  3# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut18 happy_x_3 of { (HappyWrap18 happy_var_3) -> 
	happyIn7
		 (TextuaL4.AbsTextuaL.HornlikeMeans happy_var_1 happy_var_3
	)}}

happyReduce_10 = happySpecReduce_2  3# happyReduction_10
happyReduction_10 happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_2 of { (HappyWrap14 happy_var_2) -> 
	happyIn7
		 (TextuaL4.AbsTextuaL.HornlikeDecide happy_var_2
	)}

happyReduce_11 = happyReduce 4# 3# happyReduction_11
happyReduction_11 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut14 happy_x_2 of { (HappyWrap14 happy_var_2) -> 
	case happyOut18 happy_x_4 of { (HappyWrap18 happy_var_4) -> 
	happyIn7
		 (TextuaL4.AbsTextuaL.HornlikeDecideIf happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_12 = happyReduce 4# 3# happyReduction_12
happyReduction_12 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_2 of { (HappyWrap8 happy_var_2) -> 
	case happyOut14 happy_x_4 of { (HappyWrap14 happy_var_4) -> 
	happyIn7
		 (TextuaL4.AbsTextuaL.HlikeGiveth happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_13 = happyReduce 6# 3# happyReduction_13
happyReduction_13 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_2 of { (HappyWrap8 happy_var_2) -> 
	case happyOut14 happy_x_4 of { (HappyWrap14 happy_var_4) -> 
	case happyOut18 happy_x_6 of { (HappyWrap18 happy_var_6) -> 
	happyIn7
		 (TextuaL4.AbsTextuaL.HlikeGivethIf happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_14 = happyReduce 4# 4# happyReduction_14
happyReduction_14 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut6 happy_x_4 of { (HappyWrap6 happy_var_4) -> 
	happyIn8
		 (TextuaL4.AbsTextuaL.IsAType happy_var_1 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_15 = happySpecReduce_1  4# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	happyIn8
		 (TextuaL4.AbsTextuaL.IsANoType happy_var_1
	)}

happyReduce_16 = happySpecReduce_1  5# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	happyIn9
		 ((:[]) happy_var_1
	)}

happyReduce_17 = happySpecReduce_3  5# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut9 happy_x_3 of { (HappyWrap9 happy_var_3) -> 
	happyIn9
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_18 = happySpecReduce_2  6# happyReduction_18
happyReduction_18 happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	happyIn10
		 (TextuaL4.AbsTextuaL.Has happy_var_2
	)}

happyReduce_19 = happySpecReduce_0  6# happyReduction_19
happyReduction_19  =  happyIn10
		 (TextuaL4.AbsTextuaL.EmptyFields
	)

happyReduce_20 = happySpecReduce_1  7# happyReduction_20
happyReduction_20 happy_x_1
	 =  happyIn11
		 (TextuaL4.AbsTextuaL.Deontic_MUST
	)

happyReduce_21 = happySpecReduce_1  7# happyReduction_21
happyReduction_21 happy_x_1
	 =  happyIn11
		 (TextuaL4.AbsTextuaL.Deontic_MAY
	)

happyReduce_22 = happySpecReduce_1  7# happyReduction_22
happyReduction_22 happy_x_1
	 =  happyIn11
		 (TextuaL4.AbsTextuaL.Deontic_SHANT
	)

happyReduce_23 = happySpecReduce_2  8# happyReduction_23
happyReduction_23 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_2 of { (HappyWrap18 happy_var_2) -> 
	happyIn12
		 (TextuaL4.AbsTextuaL.WhoSimple happy_var_2
	)}

happyReduce_24 = happySpecReduce_2  9# happyReduction_24
happyReduction_24 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_2 of { (HappyWrap18 happy_var_2) -> 
	happyIn13
		 (TextuaL4.AbsTextuaL.MeansInline happy_var_2
	)}

happyReduce_25 = happySpecReduce_1  10# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	happyIn14
		 (TextuaL4.AbsTextuaL.RPMT happy_var_1
	)}

happyReduce_26 = happySpecReduce_3  10# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	case happyOut18 happy_x_3 of { (HappyWrap18 happy_var_3) -> 
	happyIn14
		 (TextuaL4.AbsTextuaL.RPBoolStructR happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_27 = happySpecReduce_1  11# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	happyIn15
		 (TextuaL4.AbsTextuaL.MTT happy_var_1
	)}

happyReduce_28 = happySpecReduce_1  11# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn15
		 (TextuaL4.AbsTextuaL.MTI happy_var_1
	)}

happyReduce_29 = happySpecReduce_1  11# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	happyIn15
		 (TextuaL4.AbsTextuaL.MTF happy_var_1
	)}

happyReduce_30 = happySpecReduce_1  11# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	happyIn15
		 (TextuaL4.AbsTextuaL.MTB happy_var_1
	)}

happyReduce_31 = happySpecReduce_1  12# happyReduction_31
happyReduction_31 happy_x_1
	 =  happyIn16
		 (TextuaL4.AbsTextuaL.Bool_True
	)

happyReduce_32 = happySpecReduce_1  12# happyReduction_32
happyReduction_32 happy_x_1
	 =  happyIn16
		 (TextuaL4.AbsTextuaL.Bool_False
	)

happyReduce_33 = happySpecReduce_1  13# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	happyIn17
		 ((:[]) happy_var_1
	)}

happyReduce_34 = happySpecReduce_2  13# happyReduction_34
happyReduction_34 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
	happyIn17
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_35 = happyReduce 4# 14# happyReduction_35
happyReduction_35 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut19 happy_x_3 of { (HappyWrap19 happy_var_3) -> 
	happyIn18
		 (TextuaL4.AbsTextuaL.Any happy_var_3
	) `HappyStk` happyRest}

happyReduce_36 = happyReduce 6# 14# happyReduction_36
happyReduction_36 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut19 happy_x_4 of { (HappyWrap19 happy_var_4) -> 
	case happyOut6 happy_x_6 of { (HappyWrap6 happy_var_6) -> 
	happyIn18
		 (TextuaL4.AbsTextuaL.AnyPrePost happy_var_1 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_37 = happyReduce 5# 14# happyReduction_37
happyReduction_37 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut19 happy_x_4 of { (HappyWrap19 happy_var_4) -> 
	happyIn18
		 (TextuaL4.AbsTextuaL.AnyPre happy_var_1 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_38 = happyReduce 4# 14# happyReduction_38
happyReduction_38 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut19 happy_x_3 of { (HappyWrap19 happy_var_3) -> 
	happyIn18
		 (TextuaL4.AbsTextuaL.All happy_var_3
	) `HappyStk` happyRest}

happyReduce_39 = happyReduce 5# 14# happyReduction_39
happyReduction_39 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut19 happy_x_4 of { (HappyWrap19 happy_var_4) -> 
	happyIn18
		 (TextuaL4.AbsTextuaL.AllPre happy_var_1 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_40 = happyReduce 6# 14# happyReduction_40
happyReduction_40 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut19 happy_x_4 of { (HappyWrap19 happy_var_4) -> 
	case happyOut6 happy_x_6 of { (HappyWrap6 happy_var_6) -> 
	happyIn18
		 (TextuaL4.AbsTextuaL.AllPrePost happy_var_1 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_41 = happySpecReduce_2  14# happyReduction_41
happyReduction_41 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_2 of { (HappyWrap18 happy_var_2) -> 
	happyIn18
		 (TextuaL4.AbsTextuaL.Not happy_var_2
	)}

happyReduce_42 = happySpecReduce_1  14# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
	happyIn18
		 (TextuaL4.AbsTextuaL.Leaf happy_var_1
	)}

happyReduce_43 = happySpecReduce_1  15# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	happyIn19
		 ((:[]) happy_var_1
	)}

happyReduce_44 = happySpecReduce_3  15# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut19 happy_x_3 of { (HappyWrap19 happy_var_3) -> 
	happyIn19
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_45 = happySpecReduce_1  16# happyReduction_45
happyReduction_45 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPis
	)

happyReduce_46 = happySpecReduce_1  16# happyReduction_46
happyReduction_46 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPhas
	)

happyReduce_47 = happySpecReduce_1  16# happyReduction_47
happyReduction_47 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPeq
	)

happyReduce_48 = happySpecReduce_1  16# happyReduction_48
happyReduction_48 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPlt
	)

happyReduce_49 = happySpecReduce_1  16# happyReduction_49
happyReduction_49 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPlte
	)

happyReduce_50 = happySpecReduce_1  16# happyReduction_50
happyReduction_50 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPgt
	)

happyReduce_51 = happySpecReduce_1  16# happyReduction_51
happyReduction_51 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPgte
	)

happyReduce_52 = happySpecReduce_1  16# happyReduction_52
happyReduction_52 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPelem
	)

happyReduce_53 = happySpecReduce_2  16# happyReduction_53
happyReduction_53 happy_x_2
	happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPnotElem
	)

happyReduce_54 = happySpecReduce_1  16# happyReduction_54
happyReduction_54 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPnot
	)

happyReduce_55 = happySpecReduce_1  16# happyReduction_55
happyReduction_55 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPand
	)

happyReduce_56 = happySpecReduce_1  16# happyReduction_56
happyReduction_56 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPor
	)

happyReduce_57 = happySpecReduce_1  16# happyReduction_57
happyReduction_57 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPsum
	)

happyReduce_58 = happySpecReduce_1  16# happyReduction_58
happyReduction_58 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPproduct
	)

happyReduce_59 = happySpecReduce_1  16# happyReduction_59
happyReduction_59 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPminus
	)

happyReduce_60 = happySpecReduce_1  16# happyReduction_60
happyReduction_60 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPdivide
	)

happyReduce_61 = happySpecReduce_1  16# happyReduction_61
happyReduction_61 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPmodulo
	)

happyReduce_62 = happySpecReduce_2  16# happyReduction_62
happyReduction_62 happy_x_2
	happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPsubjectTo
	)

happyReduce_63 = happySpecReduce_1  16# happyReduction_63
happyReduction_63 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPmin
	)

happyReduce_64 = happySpecReduce_1  16# happyReduction_64
happyReduction_64 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPmax
	)

happyReduce_65 = happySpecReduce_1  16# happyReduction_65
happyReduction_65 happy_x_1
	 =  happyIn20
		 (TextuaL4.AbsTextuaL.RPmap
	)

happyReduce_66 = happySpecReduce_1  16# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut21 happy_x_1 of { (HappyWrap21 happy_var_1) -> 
	happyIn20
		 (TextuaL4.AbsTextuaL.RPTC happy_var_1
	)}

happyReduce_67 = happySpecReduce_1  17# happyReduction_67
happyReduction_67 happy_x_1
	 =  happyIn21
		 (TextuaL4.AbsTextuaL.TBefore
	)

happyReduce_68 = happySpecReduce_1  17# happyReduction_68
happyReduction_68 happy_x_1
	 =  happyIn21
		 (TextuaL4.AbsTextuaL.TAfter
	)

happyReduce_69 = happySpecReduce_1  17# happyReduction_69
happyReduction_69 happy_x_1
	 =  happyIn21
		 (TextuaL4.AbsTextuaL.TBy
	)

happyReduce_70 = happySpecReduce_1  17# happyReduction_70
happyReduction_70 happy_x_1
	 =  happyIn21
		 (TextuaL4.AbsTextuaL.TOn
	)

happyReduce_71 = happySpecReduce_1  17# happyReduction_71
happyReduction_71 happy_x_1
	 =  happyIn21
		 (TextuaL4.AbsTextuaL.TVague
	)

happyNewToken action sts stk [] =
	happyDoAction 50# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TS _ 46) -> cont 46#;
	PT _ (TD happy_dollar_dollar) -> cont 47#;
	PT _ (TI happy_dollar_dollar) -> cont 48#;
	PT _ (T_Text happy_dollar_dollar) -> cont 49#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 50# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pRule tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap7 x') = happyOut7 x} in x'))

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































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
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
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
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

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
