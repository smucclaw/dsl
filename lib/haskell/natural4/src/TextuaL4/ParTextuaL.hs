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
newtype HappyWrap6 = HappyWrap6 (String)
happyIn6 :: (String) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 (TextuaL4.AbsTextuaL.Token)
happyIn7 :: (TextuaL4.AbsTextuaL.Token) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 (TextuaL4.AbsTextuaL.Rule)
happyIn8 :: (TextuaL4.AbsTextuaL.Rule) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 (TextuaL4.AbsTextuaL.IsA)
happyIn9 :: (TextuaL4.AbsTextuaL.IsA) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 ([TextuaL4.AbsTextuaL.IsA])
happyIn10 :: ([TextuaL4.AbsTextuaL.IsA]) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 (TextuaL4.AbsTextuaL.Fields)
happyIn11 :: (TextuaL4.AbsTextuaL.Fields) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 (TextuaL4.AbsTextuaL.Deontic)
happyIn12 :: (TextuaL4.AbsTextuaL.Deontic) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 (TextuaL4.AbsTextuaL.Who)
happyIn13 :: (TextuaL4.AbsTextuaL.Who) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 (TextuaL4.AbsTextuaL.InlineHornlike)
happyIn14 :: (TextuaL4.AbsTextuaL.InlineHornlike) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 (TextuaL4.AbsTextuaL.RelationalPredicate)
happyIn15 :: (TextuaL4.AbsTextuaL.RelationalPredicate) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 (TextuaL4.AbsTextuaL.MTExpr)
happyIn16 :: (TextuaL4.AbsTextuaL.MTExpr) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 (TextuaL4.AbsTextuaL.Bool)
happyIn17 :: (TextuaL4.AbsTextuaL.Bool) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 (TextuaL4.AbsTextuaL.Text)
happyIn18 :: (TextuaL4.AbsTextuaL.Text) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 ([TextuaL4.AbsTextuaL.MTExpr])
happyIn19 :: ([TextuaL4.AbsTextuaL.MTExpr]) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 (TextuaL4.AbsTextuaL.BoolStruct)
happyIn20 :: (TextuaL4.AbsTextuaL.BoolStruct) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 ([TextuaL4.AbsTextuaL.BoolStruct])
happyIn21 :: ([TextuaL4.AbsTextuaL.BoolStruct]) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 (TextuaL4.AbsTextuaL.RPRel)
happyIn22 :: (TextuaL4.AbsTextuaL.RPRel) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 (TextuaL4.AbsTextuaL.TComparison)
happyIn23 :: (TextuaL4.AbsTextuaL.TComparison) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x80\x69\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x7a\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x50\x40\x00\x20\xa0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa6\x01\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\xa0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x78\x6b\x86\x9e\x7b\x03\x00\x00\x00\x00\x00\x00\x80\x10\x42\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x40\x00\x20\xa0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x04\x00\x02\x7a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x40\x00\x20\xa0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x40\x00\x20\xa0\x07\x00\x00\x00\x40\x01\x01\x80\x80\x1e\x00\x00\x00\x00\x05\x04\x00\x02\x7a\x00\x00\x00\x00\x00\x00\x60\x84\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x10\x00\x08\xe8\x01\x00\x00\x00\x50\x40\x00\x20\xa0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x80\x1e\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x01\x80\x80\x1e\x00\x00\x00\x00\x05\x04\x00\x02\x7a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x10\x00\x08\xe8\x01\x00\x00\x00\x00\x00\x80\x10\x02\x00\x00\x00\x00\x40\x01\x01\x80\x80\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x04\x00\x02\x7a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x40\x00\x20\xa0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x01\x80\x80\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pRule","Double","Integer","String","Token","Rule","IsA","ListIsA","Fields","Deontic","Who","InlineHornlike","RelationalPredicate","MTExpr","Bool","Text","ListMTExpr","BoolStruct","ListBoolStruct","RPRel","TComparison","'('","')'","','","';'","'<'","'<='","'>'","'>='","'A'","'ABOUT'","'AFTER'","'ALL'","'AND'","'ANY'","'BEFORE'","'BY'","'DECIDE'","'DECLARE'","'DIVIDE'","'EQUALS'","'EVERY'","'False'","'GIVEN'","'GIVETH'","'HAS'","'IF'","'IN'","'IS'","'MAP'","'MAX'","'MAY'","'MEANS'","'MIN'","'MINUS'","'MODULO'","'MUST'","'NOT'","'ON'","'OR'","'PRODUCT'","'SHANT'","'SUBJECT'","'SUM'","'TO'","'True'","'WHO'","L_doubl","L_integ","L_quoted","L_Token","%eof"]
        bit_start = st Prelude.* 74
        bit_end = (st Prelude.+ 1) Prelude.* 74
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..73]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x17\x00\xd6\xff\x00\x00\x00\x00\x00\x00\xd7\xff\xee\xff\x0f\x00\xff\xff\x05\x00\xff\xff\xff\xff\x00\x00\x00\x00\x0b\x00\x04\x00\x2f\x00\x17\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x08\x00\xfc\xff\x04\x01\x46\x00\x49\x00\x00\x00\x05\x00\x00\x00\x00\x00\x3f\x00\x40\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\xff\xff\x00\x00\x05\x00\x05\x00\x05\x00\x30\x01\x00\x00\x00\x00\x00\x00\x05\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3e\x00\x00\x00\x00\x00\x00\x00\x45\x00\x00\x00\x71\x00\x72\x00\x00\x00\x00\x00\xff\xff\x81\x00\x0f\x00\x76\x00\xff\xff\x00\x00\x05\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\xe9\xff\x05\x00\x00\x00\x88\x00\x9f\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\xa5\x00\xb6\x00\x00\x00\x05\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x3b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\x00\x38\x00\x86\x00\x29\x01\x40\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x01\x00\x00\x00\x00\x00\x00\x13\x01\x00\x00\x00\x00\x26\x00\x43\x00\x00\x00\x00\x00\x00\x00\x8c\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x9d\x00\x00\x00\xa3\x00\x00\x00\x2e\x01\x00\x00\x42\x00\x54\x00\xb4\x00\x23\x00\x00\x00\x00\x00\x00\x00\xba\x00\xcb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x01\x00\x00\x0f\x01\x00\x00\x16\x01\x00\x00\x5b\x00\x6d\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\x00\xc7\x00\xe2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x74\x00\x00\x00\xe8\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf9\x00\x00\x00\x48\x01\x4a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xfe\xff\xdd\xff\xdc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xfb\xff\x00\x00\xef\xff\xee\xff\x00\x00\xe1\xff\xe2\xff\xd2\xff\xdb\xff\xe0\xff\xe3\xff\xe5\xff\x00\x00\x00\x00\x00\x00\xde\xff\x00\x00\xdf\xff\xfd\xff\xeb\xff\xf4\xff\xe3\xff\x00\x00\xf5\xff\x00\x00\xfa\xff\x00\x00\xd3\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe9\xff\xea\xff\xe8\xff\x00\x00\x00\x00\xba\xff\xcc\xff\xcb\xff\xca\xff\xc9\xff\xb5\xff\xb8\xff\xc5\xff\xb9\xff\xb7\xff\xc0\xff\xcd\xff\xce\xff\xc8\xff\xcf\xff\xbb\xff\xbc\xff\xbd\xff\xc1\xff\xbf\xff\xc6\xff\xb6\xff\xc4\xff\xc2\xff\x00\x00\xc3\xff\x00\x00\x00\x00\xda\xff\xf9\xff\x00\x00\x00\x00\x00\x00\xf2\xff\x00\x00\xed\xff\x00\x00\x00\x00\xbe\xff\xc7\xff\xe4\xff\xe7\xff\x00\x00\x00\x00\x00\x00\xf8\xff\xd1\xff\x00\x00\x00\x00\xec\xff\xf3\xff\xd9\xff\xd6\xff\x00\x00\xe6\xff\x00\x00\xf7\xff\x00\x00\x00\x00\xf0\xff\x00\x00\xf1\xff\xd7\xff\xd5\xff\xf6\xff\xd0\xff\xd4\xff\xd8\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x05\x00\x06\x00\x07\x00\x08\x00\x2f\x00\x0a\x00\x0b\x00\x1f\x00\x0d\x00\x33\x00\x0f\x00\x10\x00\x24\x00\x20\x00\x13\x00\x14\x00\x0c\x00\x29\x00\x0e\x00\x0c\x00\x19\x00\x0e\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x16\x00\x11\x00\x21\x00\x22\x00\x23\x00\x1c\x00\x25\x00\x26\x00\x27\x00\x28\x00\x16\x00\x2a\x00\x2b\x00\x11\x00\x12\x00\x25\x00\x08\x00\x15\x00\x0a\x00\x17\x00\x18\x00\x31\x00\x32\x00\x2d\x00\x04\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x12\x00\x13\x00\x02\x00\x03\x00\x2d\x00\x05\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0e\x00\x01\x00\x31\x00\x32\x00\x01\x00\x08\x00\x09\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x01\x00\x02\x00\x03\x00\x19\x00\x1b\x00\x1a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x01\x00\x02\x00\x03\x00\x2c\x00\x01\x00\x01\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x01\x00\x02\x00\x03\x00\x09\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\x02\x00\x02\x00\x00\x00\x01\x00\x02\x00\x03\x00\x02\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\x02\x00\x07\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\x08\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x02\x00\x03\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x1f\x00\x0e\x00\xff\xff\xff\xff\xff\xff\x24\x00\xff\xff\xff\xff\x02\x00\x03\x00\x29\x00\x05\x00\x06\x00\x02\x00\x03\x00\x2e\x00\x05\x00\x06\x00\x02\x00\x03\x00\x0e\x00\x05\x00\x06\x00\xff\xff\xff\xff\x0e\x00\x02\x00\x03\x00\x04\x00\xff\xff\x0e\x00\x02\x00\x03\x00\xff\xff\x05\x00\x02\x00\x03\x00\x04\x00\x0e\x00\x02\x00\x03\x00\x02\x00\x03\x00\x0e\x00\x1f\x00\x20\x00\xff\xff\x0e\x00\xff\xff\x24\x00\xff\xff\x0e\x00\xff\xff\x0e\x00\x29\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x34\x00\x35\x00\x36\x00\x37\x00\x03\x00\x38\x00\x39\x00\x2e\x00\x3a\x00\xff\xff\x3b\x00\x3c\x00\x2f\x00\x24\x00\x3d\x00\x3e\x00\x1b\x00\x30\x00\x1c\x00\x4d\x00\x3f\x00\x4e\x00\x40\x00\x41\x00\x42\x00\x43\x00\x1d\x00\x53\x00\x44\x00\x45\x00\x46\x00\x52\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x1d\x00\x4b\x00\x4c\x00\x08\x00\x09\x00\x1e\x00\x5c\x00\x0a\x00\x5d\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x1f\x00\x51\x00\x03\x00\x20\x00\x0d\x00\x0e\x00\x31\x00\x32\x00\x03\x00\x04\x00\x1f\x00\x20\x00\x03\x00\x20\x00\x0d\x00\x0e\x00\x12\x00\x13\x00\x03\x00\x04\x00\x0f\x00\x2b\x00\x0d\x00\x0e\x00\x2a\x00\x2b\x00\x2c\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x60\x00\x62\x00\x12\x00\x13\x00\x03\x00\x04\x00\x28\x00\x5a\x00\x26\x00\x12\x00\x13\x00\x03\x00\x04\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x60\x00\x61\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x60\x00\x6c\x00\x12\x00\x13\x00\x03\x00\x04\x00\x59\x00\x58\x00\x57\x00\x12\x00\x13\x00\x03\x00\x04\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x60\x00\x6b\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x60\x00\x73\x00\x12\x00\x13\x00\x03\x00\x04\x00\x55\x00\x68\x00\x12\x00\x13\x00\x03\x00\x04\x00\x6f\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x28\x00\x12\x00\x13\x00\x03\x00\x04\x00\x67\x00\x66\x00\x12\x00\x13\x00\x03\x00\x04\x00\x72\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x24\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x64\x00\x12\x00\x13\x00\x03\x00\x04\x00\x71\x00\x26\x00\x12\x00\x13\x00\x03\x00\x04\x00\x00\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x5f\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x5b\x00\x12\x00\x13\x00\x03\x00\x04\x00\x69\x00\x00\x00\x12\x00\x13\x00\x03\x00\x04\x00\x00\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x5a\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x6a\x00\x12\x00\x13\x00\x03\x00\x04\x00\x00\x00\x00\x00\x12\x00\x13\x00\x03\x00\x04\x00\x00\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x68\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x72\x00\x12\x00\x13\x00\x03\x00\x04\x00\x00\x00\x00\x00\x12\x00\x13\x00\x03\x00\x04\x00\x00\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x6f\x00\x21\x00\x15\x00\x16\x00\x22\x00\x18\x00\x12\x00\x13\x00\x03\x00\x04\x00\x12\x00\x13\x00\x03\x00\x04\x00\x00\x00\x03\x00\x04\x00\x53\x00\x15\x00\x16\x00\x22\x00\x18\x00\x15\x00\x16\x00\x22\x00\x4e\x00\x2e\x00\x6d\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x03\x00\x04\x00\x30\x00\x10\x00\x11\x00\x03\x00\x04\x00\x31\x00\x10\x00\x63\x00\x03\x00\x04\x00\x0f\x00\x10\x00\x55\x00\x00\x00\x00\x00\x0f\x00\x03\x00\x04\x00\x05\x00\x00\x00\x0f\x00\x03\x00\x04\x00\x00\x00\x0e\x00\x03\x00\x04\x00\x4f\x00\x06\x00\x03\x00\x04\x00\x03\x00\x04\x00\x0f\x00\x2e\x00\x5f\x00\x00\x00\x06\x00\x00\x00\x2f\x00\x00\x00\x75\x00\x00\x00\x74\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 74) [
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
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74)
	]

happy_n_terms = 52 :: Prelude.Int
happy_n_nonterms = 20 :: Prelude.Int

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
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn6
		 (happy_var_1
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_Token happy_var_1)) -> 
	happyIn7
		 (TextuaL4.AbsTextuaL.Token happy_var_1
	)}

happyReduce_5 = happySpecReduce_3  4# happyReduction_5
happyReduction_5 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	happyIn8
		 (TextuaL4.AbsTextuaL.TypeDecl happy_var_2 happy_var_3
	)}}

happyReduce_6 = happySpecReduce_3  4# happyReduction_6
happyReduction_6 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	case happyOut8 happy_x_3 of { (HappyWrap8 happy_var_3) -> 
	happyIn8
		 (TextuaL4.AbsTextuaL.Given happy_var_2 happy_var_3
	)}}

happyReduce_7 = happyReduce 4# 4# happyReduction_7
happyReduction_7 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	case happyOut20 happy_x_4 of { (HappyWrap20 happy_var_4) -> 
	happyIn8
		 (TextuaL4.AbsTextuaL.RegSimple happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_8 = happyReduce 5# 4# happyReduction_8
happyReduction_8 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	case happyOut13 happy_x_3 of { (HappyWrap13 happy_var_3) -> 
	case happyOut12 happy_x_4 of { (HappyWrap12 happy_var_4) -> 
	case happyOut20 happy_x_5 of { (HappyWrap20 happy_var_5) -> 
	happyIn8
		 (TextuaL4.AbsTextuaL.RegWho happy_var_2 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest}}}}

happyReduce_9 = happyReduce 6# 4# happyReduction_9
happyReduction_9 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	case happyOut13 happy_x_3 of { (HappyWrap13 happy_var_3) -> 
	case happyOut14 happy_x_4 of { (HappyWrap14 happy_var_4) -> 
	case happyOut12 happy_x_5 of { (HappyWrap12 happy_var_5) -> 
	case happyOut20 happy_x_6 of { (HappyWrap20 happy_var_6) -> 
	happyIn8
		 (TextuaL4.AbsTextuaL.RegWhoInline happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}

happyReduce_10 = happySpecReduce_3  4# happyReduction_10
happyReduction_10 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut20 happy_x_3 of { (HappyWrap20 happy_var_3) -> 
	happyIn8
		 (TextuaL4.AbsTextuaL.HornlikeMeans happy_var_1 happy_var_3
	)}}

happyReduce_11 = happySpecReduce_2  4# happyReduction_11
happyReduction_11 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_2 of { (HappyWrap15 happy_var_2) -> 
	happyIn8
		 (TextuaL4.AbsTextuaL.HornlikeDecide happy_var_2
	)}

happyReduce_12 = happyReduce 4# 4# happyReduction_12
happyReduction_12 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_2 of { (HappyWrap15 happy_var_2) -> 
	case happyOut20 happy_x_4 of { (HappyWrap20 happy_var_4) -> 
	happyIn8
		 (TextuaL4.AbsTextuaL.HornlikeDecideIf happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_13 = happyReduce 4# 4# happyReduction_13
happyReduction_13 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	case happyOut15 happy_x_4 of { (HappyWrap15 happy_var_4) -> 
	happyIn8
		 (TextuaL4.AbsTextuaL.HlikeGiveth happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_14 = happyReduce 6# 4# happyReduction_14
happyReduction_14 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	case happyOut15 happy_x_4 of { (HappyWrap15 happy_var_4) -> 
	case happyOut20 happy_x_6 of { (HappyWrap20 happy_var_6) -> 
	happyIn8
		 (TextuaL4.AbsTextuaL.HlikeGivethIf happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_15 = happyReduce 4# 5# happyReduction_15
happyReduction_15 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut18 happy_x_4 of { (HappyWrap18 happy_var_4) -> 
	happyIn9
		 (TextuaL4.AbsTextuaL.IsAType happy_var_1 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_16 = happySpecReduce_1  5# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	happyIn9
		 (TextuaL4.AbsTextuaL.IsANoType happy_var_1
	)}

happyReduce_17 = happySpecReduce_1  6# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut9 happy_x_1 of { (HappyWrap9 happy_var_1) -> 
	happyIn10
		 ((:[]) happy_var_1
	)}

happyReduce_18 = happySpecReduce_3  6# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { (HappyWrap9 happy_var_1) -> 
	case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
	happyIn10
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_19 = happySpecReduce_2  7# happyReduction_19
happyReduction_19 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	happyIn11
		 (TextuaL4.AbsTextuaL.Has happy_var_2
	)}

happyReduce_20 = happySpecReduce_0  7# happyReduction_20
happyReduction_20  =  happyIn11
		 (TextuaL4.AbsTextuaL.EmptyFields
	)

happyReduce_21 = happySpecReduce_1  8# happyReduction_21
happyReduction_21 happy_x_1
	 =  happyIn12
		 (TextuaL4.AbsTextuaL.Deontic_MUST
	)

happyReduce_22 = happySpecReduce_1  8# happyReduction_22
happyReduction_22 happy_x_1
	 =  happyIn12
		 (TextuaL4.AbsTextuaL.Deontic_MAY
	)

happyReduce_23 = happySpecReduce_1  8# happyReduction_23
happyReduction_23 happy_x_1
	 =  happyIn12
		 (TextuaL4.AbsTextuaL.Deontic_SHANT
	)

happyReduce_24 = happySpecReduce_2  9# happyReduction_24
happyReduction_24 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	happyIn13
		 (TextuaL4.AbsTextuaL.WhoSimple happy_var_2
	)}

happyReduce_25 = happySpecReduce_2  10# happyReduction_25
happyReduction_25 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	happyIn14
		 (TextuaL4.AbsTextuaL.MeansInline happy_var_2
	)}

happyReduce_26 = happySpecReduce_1  11# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut19 happy_x_1 of { (HappyWrap19 happy_var_1) -> 
	happyIn15
		 (TextuaL4.AbsTextuaL.RPMT happy_var_1
	)}

happyReduce_27 = happySpecReduce_3  11# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { (HappyWrap19 happy_var_1) -> 
	case happyOut22 happy_x_2 of { (HappyWrap22 happy_var_2) -> 
	case happyOut20 happy_x_3 of { (HappyWrap20 happy_var_3) -> 
	happyIn15
		 (TextuaL4.AbsTextuaL.RPBoolStructR happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_28 = happySpecReduce_1  12# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	happyIn16
		 (TextuaL4.AbsTextuaL.MTT happy_var_1
	)}

happyReduce_29 = happySpecReduce_1  12# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn16
		 (TextuaL4.AbsTextuaL.MTI happy_var_1
	)}

happyReduce_30 = happySpecReduce_1  12# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	happyIn16
		 (TextuaL4.AbsTextuaL.MTF happy_var_1
	)}

happyReduce_31 = happySpecReduce_1  12# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	happyIn16
		 (TextuaL4.AbsTextuaL.MTB happy_var_1
	)}

happyReduce_32 = happySpecReduce_1  13# happyReduction_32
happyReduction_32 happy_x_1
	 =  happyIn17
		 (TextuaL4.AbsTextuaL.Bool_True
	)

happyReduce_33 = happySpecReduce_1  13# happyReduction_33
happyReduction_33 happy_x_1
	 =  happyIn17
		 (TextuaL4.AbsTextuaL.Bool_False
	)

happyReduce_34 = happySpecReduce_1  14# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	happyIn18
		 (TextuaL4.AbsTextuaL.TextString happy_var_1
	)}

happyReduce_35 = happySpecReduce_1  14# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	happyIn18
		 (TextuaL4.AbsTextuaL.TextToken happy_var_1
	)}

happyReduce_36 = happySpecReduce_1  15# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	happyIn19
		 ((:[]) happy_var_1
	)}

happyReduce_37 = happySpecReduce_2  15# happyReduction_37
happyReduction_37 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	case happyOut19 happy_x_2 of { (HappyWrap19 happy_var_2) -> 
	happyIn19
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_38 = happyReduce 4# 16# happyReduction_38
happyReduction_38 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_3 of { (HappyWrap21 happy_var_3) -> 
	happyIn20
		 (TextuaL4.AbsTextuaL.Any happy_var_3
	) `HappyStk` happyRest}

happyReduce_39 = happyReduce 6# 16# happyReduction_39
happyReduction_39 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut21 happy_x_4 of { (HappyWrap21 happy_var_4) -> 
	case happyOut18 happy_x_6 of { (HappyWrap18 happy_var_6) -> 
	happyIn20
		 (TextuaL4.AbsTextuaL.AnyPrePost happy_var_1 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_40 = happyReduce 5# 16# happyReduction_40
happyReduction_40 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut21 happy_x_4 of { (HappyWrap21 happy_var_4) -> 
	happyIn20
		 (TextuaL4.AbsTextuaL.AnyPre happy_var_1 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_41 = happyReduce 4# 16# happyReduction_41
happyReduction_41 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_3 of { (HappyWrap21 happy_var_3) -> 
	happyIn20
		 (TextuaL4.AbsTextuaL.All happy_var_3
	) `HappyStk` happyRest}

happyReduce_42 = happyReduce 5# 16# happyReduction_42
happyReduction_42 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut21 happy_x_4 of { (HappyWrap21 happy_var_4) -> 
	happyIn20
		 (TextuaL4.AbsTextuaL.AllPre happy_var_1 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_43 = happyReduce 6# 16# happyReduction_43
happyReduction_43 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut21 happy_x_4 of { (HappyWrap21 happy_var_4) -> 
	case happyOut18 happy_x_6 of { (HappyWrap18 happy_var_6) -> 
	happyIn20
		 (TextuaL4.AbsTextuaL.AllPrePost happy_var_1 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_44 = happySpecReduce_2  16# happyReduction_44
happyReduction_44 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	happyIn20
		 (TextuaL4.AbsTextuaL.Not happy_var_2
	)}

happyReduce_45 = happySpecReduce_1  16# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	happyIn20
		 (TextuaL4.AbsTextuaL.Leaf happy_var_1
	)}

happyReduce_46 = happySpecReduce_1  17# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	happyIn21
		 ((:[]) happy_var_1
	)}

happyReduce_47 = happySpecReduce_3  17# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	case happyOut21 happy_x_3 of { (HappyWrap21 happy_var_3) -> 
	happyIn21
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_48 = happySpecReduce_1  18# happyReduction_48
happyReduction_48 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPis
	)

happyReduce_49 = happySpecReduce_1  18# happyReduction_49
happyReduction_49 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPhas
	)

happyReduce_50 = happySpecReduce_1  18# happyReduction_50
happyReduction_50 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPeq
	)

happyReduce_51 = happySpecReduce_1  18# happyReduction_51
happyReduction_51 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPlt
	)

happyReduce_52 = happySpecReduce_1  18# happyReduction_52
happyReduction_52 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPlte
	)

happyReduce_53 = happySpecReduce_1  18# happyReduction_53
happyReduction_53 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPgt
	)

happyReduce_54 = happySpecReduce_1  18# happyReduction_54
happyReduction_54 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPgte
	)

happyReduce_55 = happySpecReduce_1  18# happyReduction_55
happyReduction_55 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPelem
	)

happyReduce_56 = happySpecReduce_2  18# happyReduction_56
happyReduction_56 happy_x_2
	happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPnotElem
	)

happyReduce_57 = happySpecReduce_1  18# happyReduction_57
happyReduction_57 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPnot
	)

happyReduce_58 = happySpecReduce_1  18# happyReduction_58
happyReduction_58 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPand
	)

happyReduce_59 = happySpecReduce_1  18# happyReduction_59
happyReduction_59 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPor
	)

happyReduce_60 = happySpecReduce_1  18# happyReduction_60
happyReduction_60 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPsum
	)

happyReduce_61 = happySpecReduce_1  18# happyReduction_61
happyReduction_61 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPproduct
	)

happyReduce_62 = happySpecReduce_1  18# happyReduction_62
happyReduction_62 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPminus
	)

happyReduce_63 = happySpecReduce_1  18# happyReduction_63
happyReduction_63 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPdivide
	)

happyReduce_64 = happySpecReduce_1  18# happyReduction_64
happyReduction_64 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPmodulo
	)

happyReduce_65 = happySpecReduce_2  18# happyReduction_65
happyReduction_65 happy_x_2
	happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPsubjectTo
	)

happyReduce_66 = happySpecReduce_1  18# happyReduction_66
happyReduction_66 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPmin
	)

happyReduce_67 = happySpecReduce_1  18# happyReduction_67
happyReduction_67 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPmax
	)

happyReduce_68 = happySpecReduce_1  18# happyReduction_68
happyReduction_68 happy_x_1
	 =  happyIn22
		 (TextuaL4.AbsTextuaL.RPmap
	)

happyReduce_69 = happySpecReduce_1  18# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	happyIn22
		 (TextuaL4.AbsTextuaL.RPTC happy_var_1
	)}

happyReduce_70 = happySpecReduce_1  19# happyReduction_70
happyReduction_70 happy_x_1
	 =  happyIn23
		 (TextuaL4.AbsTextuaL.TBefore
	)

happyReduce_71 = happySpecReduce_1  19# happyReduction_71
happyReduction_71 happy_x_1
	 =  happyIn23
		 (TextuaL4.AbsTextuaL.TAfter
	)

happyReduce_72 = happySpecReduce_1  19# happyReduction_72
happyReduction_72 happy_x_1
	 =  happyIn23
		 (TextuaL4.AbsTextuaL.TBy
	)

happyReduce_73 = happySpecReduce_1  19# happyReduction_73
happyReduction_73 happy_x_1
	 =  happyIn23
		 (TextuaL4.AbsTextuaL.TOn
	)

happyReduce_74 = happySpecReduce_1  19# happyReduction_74
happyReduction_74 happy_x_1
	 =  happyIn23
		 (TextuaL4.AbsTextuaL.TVague
	)

happyNewToken action sts stk [] =
	happyDoAction 51# notHappyAtAll action sts stk []

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
	PT _ (TL happy_dollar_dollar) -> cont 49#;
	PT _ (T_Token happy_dollar_dollar) -> cont 50#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 51# tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap8 x') = happyOut8 x} in x'))

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
