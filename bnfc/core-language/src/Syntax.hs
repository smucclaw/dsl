
module Syntax where

----------------------------------------------------------------------
-- Definition of expressions
----------------------------------------------------------------------

-- GF Annotations to names
newtype GFAnnot = GFAnnot Integer
  deriving (Eq, Ord, Show, Read)

----- Names 
newtype VarName = VarNm String
  deriving (Eq, Ord, Show, Read)
data AnnotClassName = AClsNm String GFAnnot
  deriving (Eq, Ord, Show, Read)
newtype ClassName = ClsNm String
  deriving (Eq, Ord, Show, Read)
newtype FieldName = FldNm String
  deriving (Eq, Ord, Show, Read)
data AnnotFieldName = AFldNm String GFAnnot
  deriving (Eq, Ord, Show, Read)
newtype RuleName = RlNm String
  deriving (Eq, Ord, Show, Read)
newtype PartyName = PtNm String
  deriving (Eq, Ord, Show, Read)


annotClassName2ClassName :: AnnotClassName -> ClassName
annotClassName2ClassName (AClsNm cn a) = ClsNm cn

annotFieldName2FieldName :: AnnotFieldName -> FieldName
annotFieldName2FieldName (AFldNm fn a) = FldNm fn



----- Program

data Program ct at = Program [ClassDecl ct] [VarDecl] [Assertion at]
  deriving (Eq, Ord, Show, Read)

----- Types 
data Tp
  = BoolT
  | IntT
  | ClassT ClassName
  | FunT Tp Tp
  | ErrT
  deriving (Eq, Ord, Show, Read)

data VarDecl = VarDecl VarName Tp
  deriving (Eq, Ord, Show, Read)

-- Field attributes: for example cardinality restrictions
-- data FieldAttribs = FldAtt
data FieldDecl = FieldDecl AnnotFieldName Tp -- FieldAttribs
  deriving (Eq, Ord, Show, Read)

-- superclass, list of field declarations
data ClassDef t = ClassDef t [FieldDecl]
  deriving (Eq, Ord, Show, Read)

-- declares class with ClassName and definition as of ClassDef
data ClassDecl t = ClassDecl AnnotClassName (ClassDef t)
  deriving (Eq, Ord, Show, Read)

name_of_class_decl :: ClassDecl t -> AnnotClassName
name_of_class_decl (ClassDecl cn _) = cn

def_of_class_decl :: ClassDecl t -> ClassDef t
def_of_class_decl (ClassDecl _ cd) = cd

fields_of_class_def :: ClassDef t -> [FieldDecl]
fields_of_class_def (ClassDef scn fds) = fds

data GeneralRule = TBD
  deriving (Eq, Ord, Show, Read)
data Module t = Mdl [ClassDecl t] [GeneralRule]
  deriving (Eq, Ord, Show, Read)

class_decls_of_module :: Module t -> [ClassDecl t]
class_decls_of_module (Mdl cds _) = cds

rules_of_module :: Module t -> [GeneralRule]
rules_of_module (Mdl _ rls) = rls

-- Custom Classes and Preable Module
-- some custom classes - should eventually go into a prelude and not be hard-wired
objectC = ClassDecl (AClsNm "Object" (GFAnnot 0)) (ClassDef Nothing [])

{-
TODO: the following should be defined in concrete syntax in a preamble.

-- QualifiedNumeric class with val field
-- TODO: should its type be IntT or a FloatT?
qualifNumC = ClassDecl (ClsNm "QualifiedNumeric")
                    (ClassDef (Just (ClsNm "Object"))
                            [FieldDecl (FldNm "val") IntT])

-- Currency as QualifiedNumeric, with specific currencies (SGD, USD) as subclasses
currencyC = ClassDecl (ClsNm "Currency")
                    (ClassDef (Just (ClsNm "QualifiedNumeric")) [])
currencyCs = [ClassDecl (ClsNm "SGD") (ClassDef (Just (ClsNm "Currency")) []),
              ClassDecl (ClsNm "USD") (ClassDef (Just (ClsNm "Currency")) [])]
  
-- Time as QualifiedNumeric, with Year, Month, Day etc. as subclasses
-- TODO: treatment of time needs a second thought
--       (so far no distinction between time point and duration)
timeC = ClassDecl (ClsNm "Time")
                    (ClassDef (Just (ClsNm "QualifiedNumeric")) [])
timeCs = [ClassDecl (ClsNm "Year") (ClassDef (Just (ClsNm "Time")) []),
          ClassDecl (ClsNm "Day") (ClassDef (Just (ClsNm "Time")) [])]

eventC  = ClassDecl (ClsNm "Event")
                  (ClassDef (Just (ClsNm "Object"))
                   [FieldDecl (FldNm "time") (ClassT (ClsNm "Time"))])
customCs = [objectC, qualifNumC, currencyC] ++ currencyCs ++ [timeC] ++ timeCs ++ [eventC]
-}

customCs = [objectC]
preambleMdl = Mdl customCs []

----- Expressions 
data Val
    = BoolV Bool
    | IntV Integer
    | RecordV ClassName [(FieldName, Val)]
    | ErrV
  deriving (Eq, Ord, Show, Read)

-- unary arithmetic operators
data UArithOp = UAminus
  deriving (Eq, Ord, Show, Read)

-- unary boolean operators
data UBoolOp = UBneg
  deriving (Eq, Ord, Show, Read)

-- unary operators (union of the above)
data UnaOp
    = UArith UArithOp
    | UBool UBoolOp
  deriving (Eq, Ord, Show, Read)

-- binary arithmetic operators
data BArithOp = BAadd | BAsub | BAmul | BAdiv | BAmod
  deriving (Eq, Ord, Show, Read)

-- binary comparison operators
data BComparOp = BCeq | BClt | BClte | BCgt | BCgte | BCne
  deriving (Eq, Ord, Show, Read)

-- binary boolean operators
data BBoolOp = BBand | BBor
  deriving (Eq, Ord, Show, Read)

-- binary operators (union of the above)
data BinOp
    = BArith BArithOp
    | BCompar BComparOp
    | BBool BBoolOp
  deriving (Eq, Ord, Show, Read)

-- operators for combining list elements
data ListOp = AndList | OrList | XorList | CommaList
    deriving (Eq, Ord, Show, Read)

-- Exp t is an expression of type t (to be determined during type checking / inference)
data Exp t
    = ValE t Val                                -- value
    | VarE t VarName                            -- variable
    | UnaOpE t UnaOp (Exp t)                    -- unary operator
    | BinOpE t BinOp (Exp t) (Exp t)            -- binary operator
    | IfThenElseE t (Exp t) (Exp t) (Exp t)     -- conditional
    | AppE t (Exp t) (Exp t)                    -- function application
    | FunE t VarName Tp (Exp t)                 -- function abstraction
    | ClosE t [(VarName, Exp t)] (Exp t)        -- closure  (not externally visible)
    | FldAccE t (Exp t) FieldName               -- field access
    | CastE t Tp (Exp t)                        -- cast to type
    | ListE t ListOp [Exp t]                    -- list expression
    deriving (Eq, Ord, Show, Read)


-- Cmd t is a command of type t
data Cmd t
    = Skip                                      -- Do nothing
    | VAssign VarName (Exp t)                   -- Assignment to variable
    | FAssign (Exp t) FieldName (Exp t)         -- Assignment to field
  deriving (Eq, Ord, Show, Read)


data Assertion t = Assertion (Exp t)
  deriving (Eq, Ord, Show, Read)

----------------------------------------------------------------------
-- Definition of Timed Automata
----------------------------------------------------------------------

data Clock = Cl String
  deriving (Eq, Ord, Show, Read)

-- Clock constraint of the form x < c.
-- TODO: Reconsider the Integer. Might also be a subclass of the "Time" class
data ClConstr = ClCn Clock BComparOp Integer
  deriving (Eq, Ord, Show, Read)

data Loc = Lc String
  deriving (Eq, Ord, Show, Read)

-- Synchronization type: send or receive
data Sync = Snd | Rec
  deriving (Eq, Ord, Show, Read)

-- Action in a transition: the string is the ClassName of a subclass of Event
data Action
  = Internal
  | Act ClassName Sync
  deriving (Eq, Ord, Show, Read)

action_name :: Action -> [ClassName]
action_name Internal = []
action_name (Act cn s) = [cn]

-- Transition condition: clock constraints and Boolean expression
data TransitionCond t = TransCond [ClConstr] (Exp t)
  deriving (Eq, Ord, Show, Read)

-- Transition action: synchronization action; clock resets; and execution of command (typically assignments)
data TransitionAction t = TransAction Action [Clock] (Cmd t)
  deriving (Eq, Ord, Show, Read)

transition_action_name :: TransitionAction t -> [ClassName]
transition_action_name (TransAction act _ _) = action_name act

-- Transition relation from location to location via Action,
-- provided [ClConstr] are satisfied; and resetting [Clock]
data Transition t = Trans Loc (TransitionCond t) (TransitionAction t) Loc
  deriving (Eq, Ord, Show, Read)

-- Timed Automaton having:
-- a name
-- a set of locations,
-- a set of channel types (subclasses of Event),
-- a set of clocks,
-- a transition relation,
-- a set of initial locations,
-- an invariant per location and
-- a labelling (an expression true in the location).
-- Major extension: "Labeling function" which is typically taken to be Loc -> AP -> Bool
-- for AP a type of atomic propositioons and which is here taken to be [(Loc, Exp t)].
-- Note: the set of locations, actions, clocks could in principle be inferred from the remaining info.
-- Type parameter t: type of expressions: () or Tp, see function Typing/tp_expr
data TA t = TmdAut String [Loc] [ClassName] [Clock] [Transition t] [Loc] [(Loc, [ClConstr])] [(Loc, Exp t)]
  deriving (Eq, Ord, Show, Read)

-- Timed Automata System: a set of TAs running in parallel
-- Type parameter ext: Environment-specific extension
data TASys t ext = TmdAutSys [TA t] ext
  deriving (Eq, Ord, Show, Read)

name_of_ta :: TA t -> String
name_of_ta (TmdAut nm ta_locs ta_act_clss ta_clks trans init_locs invs lbls) = nm

channels_of_ta :: TA t -> [ClassName]
channels_of_ta (TmdAut nm ta_locs ta_act_clss ta_clks trans init_locs invs lbls) = ta_act_clss


----------------------------------------------------------------------
-- L4 Event Rules
----------------------------------------------------------------------

-- CURRENTLY NOT USED, rather see the translations in RuleToTa.hs

-- NB: Event rules as opposed to rules defining terminology etc.


data Event t
  = EventClConstr ClConstr
  | EventCond (Exp t)
  deriving (Eq, Ord, Show, Read)

-- only Must and May, assuming that Shant can be compiled away during syntax analysis
data Modality = Must | May
  deriving (Eq, Ord, Show, Read)

-- EventRule with components:
-- rule name
-- event list (interpreted conjunctively, all events of the list have to be satisfied)
-- modality
-- a non-empthy list of Parties (and not a single one). The first in the list is the one initiating the action
-- (i.e., sender), the other ones are the receivers (if any)
-- action
-- clock constraints valid in the state corresponding to the name of this rule
-- rule name corresponding to HENCE clause
-- rule name (optional) corresponding to LEST clause

data EventRule t = EvRule RuleName [Event t] Modality [PartyName] Action [ClConstr] RuleName (Maybe RuleName)
  deriving (Eq, Ord, Show, Read)
