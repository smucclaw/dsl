-- File generated by the BNF Converter (bnfc 2.9.5).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TextuaL4.Transform where

import qualified TextuaL4.AbsTextuaL as TL4
import LS.Rule
import LS.Types
import AnyAll (BoolStruct(..), Label(..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String.Interpolate (i)
import Data.Text qualified as Text

transText :: TL4.Text -> Text.Text
transText x = case x of
  TL4.TextString string -> Text.pack string
  TL4.TextToken (TL4.Token string) -> Text.pack string

transRule :: TL4.Rule -> Rule
transRule x = case x of
-- Regulative
  TL4.RegSimple sbj deont act ->
    defaultReg {
      subj = transBoolStructP sbj
    , deontic = transDeontic deont
    , action = transBoolStructP act
    }
  TL4.RegWho text who deontic action ->
    let simple = transRule $ TL4.RegSimple text deontic action
    in simple {who = Just $ transWho who}
  TL4.RegWhoInline text who hlike deontic action ->
    let simple = transRule $ TL4.RegWho text who deontic action
    in simple {wwhere = [transInlineHornlike who hlike]}

-- Hornlike
  -- text MEANS boolstruct
  TL4.HornlikeMeans name hhead
    -> mkHlike Means (transText name) (bsr2rp' name hhead)

  -- DECIDE relpred(foo IS bar)
  TL4.HornlikeDecide [] -> error [i|transRule: empty clauses in Hornlike|]
  TL4.HornlikeDecide hcs@(hc:_) -> defaultHorn {
      keyword = Decide
    , name = nameFromHC hc
    , clauses = transHornClause <$> hcs
    }

  -- Only Hornlikes have GIVETH
  -- GIVETH foo (IS A bar) DECIDE relpred
  TL4.HlikeGiveth giveth hhead ->
    let simple = transRule $ TL4.HornlikeDecide hhead
    in simple {
        giveth = Just $ isa2pt giveth
      }

-- TypeDecl
  -- DECLARE Foo (IS A Bar) (HAS …)?
  TL4.TypeDecl isa fields ->
    let isaRule = transIsA isa
    in isaRule {
      has = transFields fields
    }

-- Any rule can have a GIVEN
  TL4.Given (isa:isas) rule -> (transRule rule) {
      given = Just $ fmap isa2tm (isa :| isas)
    }
  -- Empty list shouldn't happen because we have separator nonempty in the grammar
  -- But if someone changes the grammar later, this is totally valid case anyway:
  -- if no givens, then just use the rule as is.
  TL4.Given _ rule -> transRule rule

-- Deviation from og grammar: we allow a list of HornClauses in WHERE, not arbitrary Rules
  TL4.Where rule hclauses -> case transRule rule of
    TypeDecl{} -> error [i|transRule: no wwhere allowed in TypeDecl|]
    rl -> rl {
      wwhere = [transRule $ TL4.HornlikeDecide hclauses]
      }


nameFromHC :: TL4.HornClause -> [MTExpr]
nameFromHC hc = case hc of
  TL4.HeadOnly hhead   -> nameFromRP hhead
  TL4.HeadBody hhead _ -> nameFromRP hhead

nameFromRP :: TL4.RelationalPredicate -> [MTExpr]
nameFromRP rp = case rp of
  TL4.RPMT mtes -> transMTExpr <$> mtes
  TL4.RPBoolStructR mtes _ _ -> nameFromRP $ TL4.RPMT mtes
  TL4.RPnary rprel (rp:_) -> nameFromRP rp
  TL4.RPnary rprel [] -> error [i|nameFromRP: empty RPnary|]

nameFromBS :: TL4.BoolStruct -> Text.Text
nameFromBS bs = case bs of
  TL4.Leaf rp -> mt2text $ nameFromRP rp
  _ -> bsr2text $ transBoolStructR bs

mkSimpleType :: ParamType -> TL4.Text -> TypeSig
mkSimpleType pt = SimpleType pt . transText

transIsA :: TL4.IsA -> Rule
transIsA x = case x of
  TL4.IsANoType tname -> isaNotype tname
  TL4.IsAType tname typesig -> isaType TOne tname typesig
  TL4.IsAnType tname typesig -> isaType TOne tname typesig
  TL4.IsAOptional tname typesig -> isaType TOptional tname typesig
  TL4.IsAList tname typesig -> isaType TList1 tname typesig
  TL4.IsASet tname typesig -> isaType TSet1 tname typesig
  TL4.IsAEnum tname enums ->
    let pt = transParamText $ TL4.RPMT $ fmap TL4.MTT enums
    in (isaNotype tname) {
        super = Just $ InlineEnum TOne pt
        }
  where
    isaNotype tname = defaultTypeDecl {
      name = [MTT $ transText tname]
    , super = Nothing
    }
    isaType pt tname typesig = (isaNotype tname) {
      super = Just $ mkSimpleType pt typesig
    }

-- translates an IsA to ParamText, used in GIVEN and GIVETH
-- type ParamText = NonEmpty (NonEmpty MTExpr, Maybe TypeSig)
isa2pt :: TL4.IsA -> ParamText
isa2pt isa = isa2tm isa :| []

isa2tm :: TL4.IsA -> TypedMulti
isa2tm isa = (nm :| [], sup)
  where
    rl = transIsA isa
    nm:_ = name rl
    sup = super rl

transFields :: TL4.Fields -> [Rule]
transFields x = case x of
  TL4.Has isas -> transIsA <$> isas
  TL4.EmptyFields -> []

mkHlike :: MyToken -> Text.Text -> RelationalPredicate -> Rule
mkHlike kw text rp = defaultHorn {
    keyword = kw
  , name = [MTT text]
  , clauses = [HC rp Nothing]
  }

transHornClause :: TL4.HornClause -> HornClause2
transHornClause x = case x of
  TL4.HeadOnly hhead
   -> HC (transRelationalPredicate hhead) Nothing
  TL4.HeadBody hhead hbody
   -> HC (transRelationalPredicate hhead) (Just $ transBoolStructR hbody)
  TL4.HeadOtherwise hhead
   -> HC (transRelationalPredicate hhead) (Just otherwiseBSR)

bsr2rp :: Text.Text -> TL4.BoolStruct -> RelationalPredicate
bsr2rp text bsr = RPBoolStructR [MTT text] RPis (transBoolStructR bsr)

bsr2rp' :: TL4.Text -> TL4.BoolStruct -> RelationalPredicate
bsr2rp' t = bsr2rp (transText t)

transDeontic :: TL4.Deontic -> Deontic
transDeontic x = case x of
  TL4.Deontic_MUST -> DMust
  TL4.Deontic_MAY -> DMay
  TL4.Deontic_SHANT -> DShant

transWho :: TL4.Who -> BoolStructR
transWho x = case x of
  TL4.WhoSimple bsr -> transBoolStructR bsr

transInlineHornlike :: TL4.Who -> TL4.InlineHornlike -> Rule
transInlineHornlike (TL4.WhoSimple whoBSR) (TL4.MeansInline meansBSR) =
  mkHlike Means whoText (bsr2rp whoText meansBSR)
  where
    whoText = bsr2text $ transBoolStructR whoBSR

transRelationalPredicate :: TL4.RelationalPredicate -> RelationalPredicate
transRelationalPredicate x = case x of
  TL4.RPMT mtes
    -> RPMT (transMTExpr <$> mtes)
  TL4.RPBoolStructR xs rprel (TL4.Leaf (TL4.RPMT ys))
    -> RPConstraint (transMTExpr <$> xs) (transRPRel rprel) (transMTExpr <$> ys)
  TL4.RPBoolStructR xs rprel (TL4.Leaf rp)
    -> RPnary (transRPRel rprel) [RPMT $ fmap transMTExpr xs, transRelationalPredicate rp]
  TL4.RPBoolStructR mtes rprel bsr
    -> RPBoolStructR (transMTExpr <$> mtes) (transRPRel rprel) (transBoolStructR bsr)
  TL4.RPnary rprel rps
    -> RPnary (transRPRel rprel) (transRelationalPredicate <$> rps)

-- type ParamText = NonEmpty (NonEmpty MTExpr, Maybe TypeSig)
transParamText :: TL4.RelationalPredicate -> ParamText
transParamText x = case x of
  TL4.RPMT (mte:mtes)
    -> (transMTExpr mte :| fmap transMTExpr mtes, Nothing) :| []
  _ -> error [i|transParamText: #{x} not supported as ParamText|]

transMTExpr :: TL4.MTExpr -> MTExpr
transMTExpr x = case x of
  TL4.MTT text -> MTT $ transText text
  TL4.MTI integer -> MTI integer
  TL4.MTF double -> MTF double
  TL4.MTB bool -> MTB $ transBool bool

transBool :: TL4.Bool -> Bool
transBool x = case x of
  TL4.Bool_True -> True
  TL4.Bool_False -> False

transBoolStructP :: TL4.BoolStruct -> BoolStructP
transBoolStructP x = case x of
  TL4.Leaf rp -> Leaf $ transParamText rp
  TL4.Not bs -> Not $ transBoolStructP bs
  TL4.Any bss
    -> Any Nothing (transBoolStructP <$> bss)
  TL4.AnyPre text bss
    -> Any (Just $ Pre $ transText text) (transBoolStructP <$> bss)
  TL4.AnyPrePost pr bss pst
    -> Any (Just $ PrePost (transText pr) (transText pst)) (transBoolStructP <$> bss)
  TL4.All bss
    -> All Nothing (transBoolStructP <$> bss)
  TL4.AllPre text bss
    -> All (Just $ Pre $ transText text) (transBoolStructP <$> bss)
  TL4.AllPrePost pr bss pst
    -> All (Just $ PrePost (transText pr) (transText pst)) (transBoolStructP <$> bss)
  TL4.Unless bs1 bs2 -> transBoolStructP $ TL4.All [bs1, TL4.Not bs2]

transBoolStructR :: TL4.BoolStruct -> BoolStructR
transBoolStructR x = case x of
  TL4.Any bsrs
    -> Any Nothing (transBoolStructR <$> bsrs)
  TL4.AnyPre text bsrs
    -> Any (Just $ Pre $ transText text) (transBoolStructR <$> bsrs)
  TL4.AnyPrePost pr bsrs pst
    -> Any (Just $ PrePost (transText pr) (transText pst)) (transBoolStructR <$> bsrs)
  TL4.All bsrs
    -> All Nothing (transBoolStructR <$> bsrs)
  TL4.AllPre text bsrs
    -> All (Just $ Pre $ transText text) (transBoolStructR <$> bsrs)
  TL4.AllPrePost pr bsrs pst
    -> All (Just $ PrePost (transText pr) (transText pst)) (transBoolStructR <$> bsrs)
  TL4.Not bsr -> Not $ transBoolStructR bsr
  TL4.Leaf rp -> Leaf $ transRelationalPredicate rp
  TL4.Unless bs1 bs2 -> transBoolStructR $ TL4.All [bs1, TL4.Not bs2]

-- in case we switch to a special construct for OTHERWISE, change this
otherwiseBSR :: BoolStructR
otherwiseBSR = Leaf (RPMT [MTT "OTHERWISE"])

transRPRel :: TL4.RPRel -> RPRel
transRPRel x = case x of
  TL4.RPis -> RPis
  TL4.RPhas -> RPhas
  TL4.RPeq -> RPeq
  TL4.RPlt -> RPlt
  TL4.RPlte -> RPlte
  TL4.RPgt -> RPgt
  TL4.RPgte -> RPgte
  TL4.RPelem -> RPelem
  TL4.RPnotElem -> RPnotElem
  TL4.RPnot -> RPnot
  TL4.RPand -> RPand
  TL4.RPor -> RPor
  TL4.RPsum -> RPsum
  TL4.RPproduct -> RPproduct
  TL4.RPminus -> RPminus
  TL4.RPdivide -> RPdivide
  TL4.RPmodulo -> RPmodulo
  TL4.RPsubjectTo -> RPsubjectTo
  TL4.RPmin -> RPmin
  TL4.RPmax -> RPmax
  TL4.RPmap -> RPmap
  TL4.RPTC tc -> RPTC $ transTComparison tc

transTComparison :: TL4.TComparison -> TComparison
transTComparison x = case x of
  TL4.TBefore -> TBefore
  TL4.TAfter -> TAfter
  TL4.TBy -> TBy
  TL4.TOn -> TOn
  TL4.TVague -> TVague
