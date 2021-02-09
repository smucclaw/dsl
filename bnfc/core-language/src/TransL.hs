-- Copied from SkelL.hs

module TransL where

import qualified AbsL
import Syntax

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: AbsL.Ident -> String
transIdent x = case x of
  AbsL.Ident string -> string
transTrueBool :: AbsL.TrueBool -> Result
transTrueBool x = case x of
  AbsL.TrueBool string -> failure x
transFalseBool :: AbsL.FalseBool -> Result
transFalseBool x = case x of
  AbsL.FalseBool string -> failure x
transYYYYMMDD :: AbsL.YYYYMMDD -> Result
transYYYYMMDD x = case x of
  AbsL.YYYYMMDD string -> failure x
transYYYYMMDDTHHMM :: AbsL.YYYYMMDDTHHMM -> Result
transYYYYMMDDTHHMM x = case x of
  AbsL.YYYYMMDDTHHMM string -> failure x
transUIdent :: AbsL.UIdent -> Result
transUIdent x = case x of
  AbsL.UIdent string -> failure x
transRule :: AbsL.Rule -> Result
transRule x = case x of
  AbsL.Rule ruledef rulename rulebody -> failure x
transRuleDef :: AbsL.RuleDef -> Result
transRuleDef x = case x of
  AbsL.RID objattr -> failure x
  AbsL.RNumID integer objattr -> failure x
  AbsL.RNum integer -> failure x
transRuleName :: AbsL.RuleName -> Result
transRuleName x = case x of
  AbsL.RName optlangstrings -> failure x
transOptLangStrings :: AbsL.OptLangStrings -> Result
transOptLangStrings x = case x of
  AbsL.OptLangStrings1 -> failure x
  AbsL.OptLangStringsLangStrings langstrings -> failure x
transRuleBody :: AbsL.RuleBody -> Result
transRuleBody x = case x of
  AbsL.RBNoop -> failure x
  AbsL.RuleDeem givenlimb constraints withlimb whenlimb wherelimb -> failure x
  AbsL.RModal givenlimb partylimb deonticlimb whenlimb deadlinelimb hencelimb wherelimb -> failure x
  AbsL.RuleBody1 matchvarss -> failure x
transPartyLimb :: AbsL.PartyLimb -> Result
transPartyLimb x = case x of
  AbsL.PartyLimb1 partydef asalias -> failure x
transPartyDef :: AbsL.PartyDef -> Result
transPartyDef x = case x of
  AbsL.PSome objattr -> failure x
  AbsL.PEvery pevery -> failure x
  AbsL.PNobody pnobody -> failure x
transPEvery :: AbsL.PEvery -> Result
transPEvery x = case x of
  AbsL.PEvery_EVERYBODY -> failure x
  AbsL.PEvery_ANYBODY -> failure x
  AbsL.PEvery_EVERYONE -> failure x
  AbsL.PEvery_ANYONE -> failure x
transPNobody :: AbsL.PNobody -> Result
transPNobody x = case x of
  AbsL.PNobody_NOBODY -> failure x
  AbsL.PNobody1 -> failure x
  AbsL.PNobody_NOONE -> failure x
  AbsL.PNobody_NONE -> failure x
transAsAlias :: AbsL.AsAlias -> Result
transAsAlias x = case x of
  AbsL.AsAlias1 -> failure x
  AbsL.AsAlias2 objattr -> failure x
transDeonticLimb :: AbsL.DeonticLimb -> Result
transDeonticLimb x = case x of
  AbsL.DeonticLimb1 deonticexpr actionlimb -> failure x
transDeonticExpr :: AbsL.DeonticExpr -> Result
transDeonticExpr x = case x of
  AbsL.DEMust -> failure x
  AbsL.DEMay -> failure x
  AbsL.DEShant -> failure x
transActionLimb :: AbsL.ActionLimb -> Result
transActionLimb x = case x of
  AbsL.ActionLimb1 objmethod blahs asalias -> failure x
transDeadlineLimb :: AbsL.DeadlineLimb -> Result
transDeadlineLimb x = case x of
  AbsL.DeadlineLimb1 -> failure x
  AbsL.DeadlineLimb2 temporalexpr asalias -> failure x
transTemporalExpr :: AbsL.TemporalExpr -> Result
transTemporalExpr x = case x of
  AbsL.TemporalExpr1 datetime durationexpr -> failure x
  AbsL.TemporalExpr2 objattr durationexpr -> failure x
  AbsL.TemporalExpr3 junctionlist -> failure x
  AbsL.TemporalExpr4 junctionlist -> failure x
transDurationExpr :: AbsL.DurationExpr -> Result
transDurationExpr x = case x of
  AbsL.DurationExpr1 -> failure x
  AbsL.DurationExpr2 duration -> failure x
transDuration :: AbsL.Duration -> Result
transDuration x = case x of
  AbsL.TDYM integer -> failure x
  AbsL.TDW integer -> failure x
  AbsL.TDD integer -> failure x
  AbsL.TDDH integer1 integer2 -> failure x
transHenceLimb :: AbsL.HenceLimb -> Result
transHenceLimb x = case x of
  AbsL.DNoHence -> failure x
  AbsL.DHence goto args optlangstrings -> failure x
  AbsL.DHeLe goto1 args1 optlangstrings1 goto2 args2 optlangstrings2 -> failure x
  AbsL.DLest goto args optlangstrings -> failure x
transGoto :: AbsL.Goto -> Result
transGoto x = case x of
  AbsL.GotoRuleDef ruledef -> failure x
  AbsL.Goto_FULFILLED -> failure x
  AbsL.Goto_BREACH -> failure x
transMatchVars :: AbsL.MatchVars -> Result
transMatchVars x = case x of
  AbsL.MatchVars1 mathexpr -> failure x
  AbsL.MatchVars2 matchexpr -> failure x
  AbsL.MatchVars3 relbool -> failure x
  AbsL.MatchVars4 varexpr -> failure x
  AbsL.MatchVars5 constraint -> failure x
  AbsL.MatchVars6 numberlike -> failure x
  AbsL.MatchVars7 boolexp -> failure x
  AbsL.MatchVars8 objmethod -> failure x
  AbsL.MatchVars9 objattr -> failure x
  AbsL.MatchVars10 objattrelem -> failure x
  AbsL.MatchVars11 unifyexpr -> failure x
  AbsL.MatchVars12 unifyelems -> failure x
  AbsL.MatchVars13 whenlimb -> failure x
  AbsL.MatchVars14 wherelimb -> failure x
  AbsL.MatchVars15 deonticlimb -> failure x
  AbsL.MatchVars16 bracelist -> failure x
  AbsL.MatchVars17 arrayofbraces -> failure x
  AbsL.MatchVars18 hencelimb -> failure x
transModule :: AbsL.Module -> Result
transModule x = case x of
  AbsL.ModuleDecl objattr -> failure x
transTops :: AbsL.Tops -> Result
transTops x = case x of
  AbsL.Toplevel toplevelss -> failure x
transToplevels :: AbsL.Toplevels -> Result
transToplevels x = case x of
  AbsL.ToplevelsModule module_ -> failure x
  AbsL.ToplevelsImport import_ -> failure x
  AbsL.ToplevelsPragma pragma -> failure x
  AbsL.ToplevelsRule rule -> failure x
  AbsL.ToplevelsEntity entity -> failure x
  AbsL.ToplevelsScenario scenario -> failure x
transPragma :: AbsL.Pragma -> Result
transPragma x = case x of
  AbsL.Pragma1 commalist -> failure x
transImport :: AbsL.Import -> Result
transImport x = case x of
  AbsL.Import objattr -> failure x
transWhenLimb :: AbsL.WhenLimb -> Result
transWhenLimb x = case x of
  AbsL.WhenLimb1 -> failure x
  AbsL.WhenLimb2 boolexp unlessexpr -> failure x
transUnlessExpr :: AbsL.UnlessExpr -> Result
transUnlessExpr x = case x of
  AbsL.UnlessExpr1 -> failure x
  AbsL.UnlessExpr2 boolexp -> failure x
transTypeUnify :: AbsL.TypeUnify -> Result
transTypeUnify x = case x of
  AbsL.TU -> failure x
transTypeString :: AbsL.TypeString -> Result
transTypeString x = case x of
  AbsL.TS -> failure x
transTypeBool :: AbsL.TypeBool -> Result
transTypeBool x = case x of
  AbsL.TB -> failure x
transTypeMath :: AbsL.TypeMath -> Result
transTypeMath x = case x of
  AbsL.TM -> failure x
transTypeObjMethod :: AbsL.TypeObjMethod -> Result
transTypeObjMethod x = case x of
  AbsL.TOM -> failure x
transTypeTemporal :: AbsL.TypeTemporal -> Result
transTypeTemporal x = case x of
  AbsL.TT -> failure x
transVarExpr :: AbsL.VarExpr -> Exp ()
transVarExpr x = case x of
  AbsL.VarExprMathExpr mathexpr -> transMathExpr mathexpr
  AbsL.VarExprBoolExp boolexp -> transBoolExp boolexp
  -- AbsL.VarExprPlainVal plainval -> failure x
transPlainVal :: AbsL.PlainVal -> Result
transPlainVal x = case x of
  AbsL.PlainValString string -> failure x
  AbsL.PlainValBraceList bracelist -> failure x
  AbsL.PlainValArrayOfBraces arrayofbraces -> failure x
  AbsL.PlainValDateTime datetime -> failure x
  AbsL.PlainVal1 datetime1 datetime2 -> failure x
transMathExpr :: AbsL.MathExpr -> Exp ()
transMathExpr x = case x of
  AbsL.MathExpr1 mathexpr1 mathexpr2 -> BinOpE () (BArith BAadd) (transMathExpr mathexpr1) (transMathExpr mathexpr2)
  AbsL.MathExpr2 mathexpr1 mathexpr2 -> BinOpE () (BArith BAsub) (transMathExpr mathexpr1) (transMathExpr mathexpr2)
  AbsL.MathExpr11 mathexpr1 mathexpr2 -> BinOpE () (BArith BAmul) (transMathExpr mathexpr1) (transMathExpr mathexpr2)
  AbsL.MathExpr12 mathexpr1 mathexpr2 -> BinOpE () (BArith BAdiv) (transMathExpr mathexpr1) (transMathExpr mathexpr2)
  AbsL.MathExpr21 currencyprefix mathexpr -> transMathExpr mathexpr
  AbsL.MathExpr3Numberlike numberlike -> transNumberlike numberlike
  -- AbsL.MathExpr31 objmethod typemath -> 
  AbsL.MathExpr3ObjMethod objmethod -> transObjMethod objmethod
transNumberlike :: AbsL.Numberlike -> Exp ()
transNumberlike x = case x of
  AbsL.NumberlikeNumeric numeric -> ValE () (IntV (transNumeric numeric))
  AbsL.Numberlike1 numeric -> ValE () (IntV (transNumeric numeric))
transNumeric :: AbsL.Numeric -> Integer
transNumeric x = case x of
  AbsL.NumericInteger integer -> integer
  -- AbsL.NumericDouble double -> failure x
transCurrencyPrefix :: AbsL.CurrencyPrefix -> Result
transCurrencyPrefix x = case x of
  AbsL.NoCurrency -> failure x
  AbsL.CurrCode uident -> failure x
  AbsL.CurrDollar -> failure x
transBoolExp :: AbsL.BoolExp -> Exp ()
transBoolExp x = case x of
  AbsL.BAnd1 boolexp1 boolexp2 -> BinOpE () (BBool BBand) (transBoolExp boolexp1) (transBoolExp boolexp2)
  AbsL.BAnd2 boolexp1 boolexp2 -> BinOpE () (BBool BBand) (transBoolExp boolexp1) (transBoolExp boolexp2)
  AbsL.BOr1 boolexp1 boolexp2 -> BinOpE () (BBool BBor) (transBoolExp boolexp1) (transBoolExp boolexp2)
  AbsL.BOr2 boolexp1 boolexp2 -> BinOpE () (BBool BBor) (transBoolExp boolexp1) (transBoolExp boolexp2)
  AbsL.BLikely likelihood boolexp -> (transBoolExp boolexp)
  AbsL.BNot1 boolexp -> UnaOpE () (UBool UBneg) (transBoolExp boolexp)
  AbsL.BNot2 boolexp -> UnaOpE () (UBool UBneg) (transBoolExp boolexp)
  AbsL.ObjEq boolexp1 boolexp2 -> BinOpE () (BCompar BCeq) (transBoolExp boolexp1) (transBoolExp boolexp2)
  AbsL.MathLT mathexpr1 mathexpr2 -> BinOpE () (BCompar BClt) (transMathExpr mathexpr1) (transMathExpr mathexpr2)
  AbsL.MathLTE mathexpr1 mathexpr2 -> BinOpE () (BCompar BClte) (transMathExpr mathexpr1) (transMathExpr mathexpr2)
  AbsL.MathGT mathexpr1 mathexpr2 -> BinOpE () (BCompar BCgt) (transMathExpr mathexpr1) (transMathExpr mathexpr2)
  AbsL.MathGTE mathexpr1 mathexpr2 -> BinOpE () (BCompar BCgte) (transMathExpr mathexpr1) (transMathExpr mathexpr2)
  AbsL.MathEq mathexpr1 mathexpr2 -> BinOpE () (BCompar BCeq) (transMathExpr mathexpr1) (transMathExpr mathexpr2)
  -- AbsL.StrEq1 string objmethod -> failure x
  -- AbsL.StrEq2 objmethod string -> failure x
  -- AbsL.BMatch matchexpr -> failure x
  AbsL.BTrue truebool -> ValE () (BoolV True)
  AbsL.BFalse falsebool -> ValE () (BoolV False)
transLikelihood :: AbsL.Likelihood -> Result
transLikelihood x = case x of
  AbsL.Likely -> failure x
  AbsL.Unlikely -> failure x
transMatchExpr :: AbsL.MatchExpr -> Result
transMatchExpr x = case x of
  AbsL.ME_rbjl matchexpr1 relbool matchexpr2 -> failure x
  AbsL.MatchExpr1ObjMethod objmethod -> failure x
  AbsL.MatchExpr11 objmethod typebool -> failure x
  AbsL.MatchExpr1JunctionList junctionlist -> failure x
transRelBool :: AbsL.RelBool -> Result
transRelBool x = case x of
  AbsL.RelBool_IS -> failure x
  AbsL.RelBool_ISA -> failure x
  AbsL.RelBool_ARE -> failure x
  AbsL.RelBool_HAS -> failure x
  AbsL.RelBool1 -> failure x
transObjMethod :: AbsL.ObjMethod -> Exp ()
transObjMethod x = case x of
  AbsL.ObjMethod1 unifyelems args -> head (map transUnifyElem unifyelems)    -- TODO: incomplete treatment
transArgs :: AbsL.Args -> Result
transArgs x = case x of
  AbsL.Args1 -> failure x
  AbsL.Args2 constraints -> failure x
transUnifyExpr :: AbsL.UnifyExpr -> Result
transUnifyExpr x = case x of
  AbsL.UnifyExpr1 objattrelem unifyelems -> failure x
transUnifyElem :: AbsL.UnifyElem -> Exp ()
transUnifyElem x = case x of
  AbsL.UnifyElemObjAttrElem objattrelem -> transObjAttrElem objattrelem
  -- AbsL.UnifyElemUnifyBracket unifybracket -> failure x
  -- AbsL.UnifyElemUnifyStar unifystar -> failure x
transUnifyStar :: AbsL.UnifyStar -> Result
transUnifyStar x = case x of
  AbsL.UnifyStar1 -> failure x
transUnifyBracket :: AbsL.UnifyBracket -> Result
transUnifyBracket x = case x of
  AbsL.UnifyBracket1 commaelems -> failure x
transJunctionList :: AbsL.JunctionList -> Result
transJunctionList x = case x of
  AbsL.JL_And andlist -> failure x
  AbsL.JL_Or orlist -> failure x
  AbsL.JL_Xor xorlist -> failure x
  AbsL.JL_Comma commalist -> failure x
transCommaList :: AbsL.CommaList -> Result
transCommaList x = case x of
  AbsL.CommaList commaelems -> failure x
transCommaElem :: AbsL.CommaElem -> Result
transCommaElem x = case x of
  AbsL.CommaElemObjMethod objmethod -> failure x
  AbsL.CommaElemString string -> failure x
transAndList :: AbsL.AndList -> Result
transAndList x = case x of
  AbsL.AndList andelem andelems -> failure x
transAndElem :: AbsL.AndElem -> Result
transAndElem x = case x of
  AbsL.AndElemObjMethod objmethod -> failure x
  AbsL.AndElemString string -> failure x
transOrList :: AbsL.OrList -> Result
transOrList x = case x of
  AbsL.OrList orelem orelems -> failure x
transOrElem :: AbsL.OrElem -> Result
transOrElem x = case x of
  AbsL.OrElemObjMethod objmethod -> failure x
  AbsL.OrElemString string -> failure x
transXorList :: AbsL.XorList -> Result
transXorList x = case x of
  AbsL.XorList xorelem xorelems -> failure x
transXorElem :: AbsL.XorElem -> Result
transXorElem x = case x of
  AbsL.XorElemObjMethod objmethod -> failure x
  AbsL.XorElemString string -> failure x
transObjAttrElem :: AbsL.ObjAttrElem -> Exp ()
transObjAttrElem x = case x of
  AbsL.ObjAttrElemIdent ident -> VarE () (VarNm (transIdent ident))
  -- AbsL.ObjAttrElemUIdent uident -> failure x
transObjAttr :: AbsL.ObjAttr -> Result
transObjAttr x = case x of
  AbsL.OA_dots objattrelems -> failure x
transEntity :: AbsL.Entity -> Result
transEntity x = case x of
  AbsL.REntity objattr1 givenlimb objattr2 withlimb asof whenlimb wherelimb -> failure x
transGivenLimb :: AbsL.GivenLimb -> Result
transGivenLimb x = case x of
  AbsL.GivenLimb1 -> failure x
  AbsL.GivenLimb2 givenexpr -> failure x
transGivenExpr :: AbsL.GivenExpr -> Result
transGivenExpr x = case x of
  AbsL.GivenExpr1 objattrs -> failure x
  AbsL.GivenExpr2 objattrs havinglimb -> failure x
transHavingLimb :: AbsL.HavingLimb -> Result
transHavingLimb x = case x of
  AbsL.HavingLimb1 havingboolexps -> failure x
transHavingBoolExp :: AbsL.HavingBoolExp -> Result
transHavingBoolExp x = case x of
  AbsL.HavingBoolExpBoolExp boolexp -> failure x
transWithLimb :: AbsL.WithLimb -> Result
transWithLimb x = case x of
  AbsL.WithLimb1 -> failure x
  AbsL.WithLimb2 withins -> failure x
transWithIn :: AbsL.WithIn -> Result
transWithIn x = case x of
  AbsL.WithIn1 constraints -> failure x
  AbsL.WithInTraceExpr traceexpr -> failure x
transTraceExpr :: AbsL.TraceExpr -> Result
transTraceExpr x = case x of
  AbsL.TraceExpr1 logevents -> failure x
transLogEvent :: AbsL.LogEvent -> Result
transLogEvent x = case x of
  AbsL.LogEvent1 iso objattr1 objattr2 blahs -> failure x
transBlah :: AbsL.Blah -> Result
transBlah x = case x of
  AbsL.BlahVarExpr varexpr -> failure x
transWhereLimb :: AbsL.WhereLimb -> Result
transWhereLimb x = case x of
  AbsL.WhereLimb1 -> failure x
  AbsL.WhereLimb2 whereexps -> failure x
transWhereExp :: AbsL.WhereExp -> Result
transWhereExp x = case x of
  AbsL.WhereExp1 constraint withlimb whenlimb wherelimb -> failure x
transAsof :: AbsL.Asof -> Result
transAsof x = case x of
  AbsL.Asof datetime -> failure x
  AbsL.AsofNull -> failure x
transDateTime :: AbsL.DateTime -> Result
transDateTime x = case x of
  AbsL.DateTimeIso8601 iso -> failure x
  AbsL.DateTime_PRESENT -> failure x
  AbsL.DateTime_NOW -> failure x
transIso :: AbsL.Iso -> Result
transIso x = case x of
  AbsL.Iso8601YYYYMMDD yyyymmdd -> failure x
  AbsL.Iso8601YYYYMMDDTHHMM yyyymmddthhmm -> failure x
transUIdentList :: AbsL.UIdentList -> Result
transUIdentList x = case x of
  AbsL.UIdentList1 uidentelems -> failure x
transUIdentElem :: AbsL.UIdentElem -> Result
transUIdentElem x = case x of
  AbsL.UIdentElemUIdent uident -> failure x
transBraceList :: AbsL.BraceList -> Result
transBraceList x = case x of
  AbsL.BraceList1 constraints -> failure x
transConstraint :: AbsL.Constraint -> Result
transConstraint x = case x of
  AbsL.Constraint1 objattr constraintbinop varexpr -> failure x
transArrayOfBraces :: AbsL.ArrayOfBraces -> Result
transArrayOfBraces x = case x of
  AbsL.ArrayOfBraces1 bracelists -> failure x
transConstraintBinOp :: AbsL.ConstraintBinOp -> Result
transConstraintBinOp x = case x of
  AbsL.ConstraintBinOp1 -> failure x
  AbsL.ConstraintBinOp2 -> failure x
  AbsL.ConstraintBinOp3 -> failure x
  AbsL.ConstraintBinOp4 -> failure x
  AbsL.ConstraintBinOp5 -> failure x
  AbsL.ConstraintBinOp6 -> failure x
  AbsL.ConstraintBinOp_IS -> failure x
  AbsL.ConstraintBinOp_ISA -> failure x
  AbsL.ConstraintBinOp_ARE -> failure x
  AbsL.ConstraintBinOp_HAS -> failure x
transScenario :: AbsL.Scenario -> Result
transScenario x = case x of
  AbsL.RScenario objattr withlimb traceexpr asof wherelimb -> failure x
transNormalString :: AbsL.NormalString -> Result
transNormalString x = case x of
  AbsL.EString string -> failure x
transLangStrings :: AbsL.LangStrings -> Result
transLangStrings x = case x of
  AbsL.ELangStrings langstrings -> failure x
transLangString :: AbsL.LangString -> Result
transLangString x = case x of
  AbsL.ELangString langid normalstring -> failure x
transLangID :: AbsL.LangID -> Result
transLangID x = case x of
  AbsL.LangID1 langlabels -> failure x
transLangLabel :: AbsL.LangLabel -> Result
transLangLabel x = case x of
  AbsL.LangLabelIdent ident -> failure x
transCurrID :: AbsL.CurrID -> Result
transCurrID x = case x of
  AbsL.CurrID1 currlabel -> failure x
transCurrLabel :: AbsL.CurrLabel -> Result
transCurrLabel x = case x of
  AbsL.CurrLabelUIdent uident -> failure x

-- $> transVarExpr ( VarExprBoolExp ( MathLTE ( MathExpr1 ( MathExpr21 CurrDollar ( MathExpr3Numberlike ( NumberlikeNumeric ( NumericInteger 50 ) ) ) ) ( MathExpr11 ( MathExpr1 ( MathExpr3Numberlike ( NumberlikeNumeric ( NumericInteger 90 ) ) ) ( MathExpr3Numberlike ( NumberlikeNumeric ( NumericInteger 4 ) ) ) ) ( MathExpr3ObjMethod ( ObjMethod1 [ UnifyElemObjAttrElem ( ObjAttrElemIdent ( Ident "foo" ) ) ] Args1 ) ) ) ) ( MathExpr11 ( MathExpr3Numberlike ( NumberlikeNumeric ( NumericInteger 2 ) ) ) ( MathExpr3Numberlike ( NumberlikeNumeric ( NumericInteger 5 ) ) ) ) ) )
