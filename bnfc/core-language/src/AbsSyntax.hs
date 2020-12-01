
module AbsSyntax where

----------------------------------------------------------------------
-- Definition of expressions
----------------------------------------------------------------------

----- Names 
newtype VarName = VarNm String
  deriving (Eq, Ord, Show, Read)
newtype ClassName = ClsNm String
  deriving (Eq, Ord, Show, Read)
newtype FieldName = FldNm String
  deriving (Eq, Ord, Show, Read)
newtype RuleName = RlNm String
  deriving (Eq, Ord, Show, Read)
newtype PartyName = PtNm String
  deriving (Eq, Ord, Show, Read)


----- Types 
data Tp
  = BoolT
  | IntT
  | FunT Tp Tp
  | ClassT ClassName
  | ErrT
  deriving (Eq, Ord, Show, Read)

-- Field attributes: for example cardinality restrictions
-- data FieldAttribs = FldAtt
data FieldDecl = FldDecl FieldName Tp -- FieldAttribs
  deriving (Eq, Ord, Show, Read)

-- superclass, list of field declarations
data ClassDef t = ClsDef t [FieldDecl] 
  deriving (Eq, Ord, Show, Read)

-- declares class with ClassName and definition as of ClassDef
data ClassDecl t = ClsDecl ClassName (ClassDef t)
  deriving (Eq, Ord, Show, Read)

name_of_class_decl :: ClassDecl t -> ClassName
name_of_class_decl (ClsDecl cn _) = cn

def_of_class_decl :: ClassDecl t -> ClassDef t
def_of_class_decl (ClsDecl _ cd) = cd

fields_of_class_def :: ClassDef t -> [FieldDecl]
fields_of_class_def (ClsDef scn fds) = fds

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
objectC = ClsDecl (ClsNm "Object") (ClsDef Nothing [])

-- QualifiedNumeric class with val field
-- TODO: should its type be IntT or a FloatT?
qualifNumC = ClsDecl (ClsNm "QualifiedNumeric")
                    (ClsDef (Just (ClsNm "Object"))
                            [FldDecl (FldNm "val") IntT])

-- Currency as QualifiedNumeric, with specific currencies (SGD, USD) as subclasses
currencyC = ClsDecl (ClsNm "Currency")
                    (ClsDef (Just (ClsNm "QualifiedNumeric")) [])
currencyCs = [ClsDecl (ClsNm "SGD") (ClsDef (Just (ClsNm "Currency")) []),
              ClsDecl (ClsNm "USD") (ClsDef (Just (ClsNm "Currency")) [])]
  
-- Time as QualifiedNumeric, with Year, Month, Day etc. as subclasses
-- TODO: treatment of time needs a second thought
--       (so far no distinction between time point and duration)
timeC = ClsDecl (ClsNm "Time")
                    (ClsDef (Just (ClsNm "QualifiedNumeric")) [])
timeCs = [ClsDecl (ClsNm "Year") (ClsDef (Just (ClsNm "Time")) []),
          ClsDecl (ClsNm "Day") (ClsDef (Just (ClsNm "Time")) [])]

eventC  = ClsDecl (ClsNm "Event")
                  (ClsDef (Just (ClsNm "Object"))
                   [FldDecl (FldNm "time") (ClassT (ClsNm "Time"))])
customCs = [objectC, qualifNumC, currencyC] ++ currencyCs ++ [timeC] ++ timeCs ++ [eventC]

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

-- Transition relation from location to location via Action,
-- provided [ClConstr] are satisfied; and resetting [Clock]
data Transition = Trans Loc [ClConstr] Action [Clock] Loc
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
-- Type parameter e: 
data TA t = TmdAut String [Loc] [ClassName] [Clock] [Transition] [Loc] [(Loc, [ClConstr])] [(Loc, Exp t)]
  deriving (Eq, Ord, Show, Read)

-- Timed Automata System: a set of TAs running in parallel
data TASys t = TmdAutSys [TA t]
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
