{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| transpiler to ProB \/ EventB \/ Rodin

Some decisions we make along the way:

- [@timing model@]: what is the timing model -- discrete-time or real-time?

  Provisionally, our answer is: discrete-time.

  So, if we have a 30-day deadline, we define a tick as a day, and we
  partition the world into 31 chunks; before the deadline passes, we
  are in one region; after the deadline passes, we are in another
  region. There are two regions.

  This is important because there might be other events that could
  occur before or after the deadline: if a payment event happens
  before the deadline, great. If a payment event after the deadline,
  not so great.

  So there are now four regions:

  +--------------------------------------+-----------------------------------+
  |            before deadline       | DEADLINE |      after deadline        |
  +--------------------------------------+-----------------------------------+
  |   before    | PAYMENT |   after  |                                       |
  +--------------------------------------+-----------------------------------+
  |                        before    | PAYMENT |   after                     |
  +--------------------------------------+-----------------------------------+
  |                                         before    | PAYMENT |   after    |
  +--------------------------------------+-----------------------------------+

  This discrete-time model reduces to 3 possible worlds.

- do we aim to generate a restricted set of assertions?

- [@temporal logic@]: are we using LTL or CTL?

  Answer: Let's try both to start?

- what is the conversion model?
  - do we want each automaton to represent a rule?
  - do we want each automaton to represent an actor?
  - do we want the overall automaton to represent the contract?

= Outputs

== Assertions in CTL and LTL

One of the concrete outputs we want to generate is for PDPA. These
particular CTL/LTL expressions test the race condition, which we
already discovered by thinking about it. To demonstrate the system, we
want these outputs to be automatically generated based on the ruleset
alone. Perhaps these outputs are generated as part of a larger set of
CTL\/LTL outputs; but these are the ones we expect to produce an
interesting result.

== CTL output

@
  EF EX[notify_indiv_happened(org)]
  EF EX[prohibit_notify_indiv_happened(pdpc)]
  EG {breached_by = None}
@

== LTL output

@
  (G {breached_by = {}}) &
  G ([prohibit_notify_indiv_happened(_)] =>
    H not [notify_indiv_happened(_)])
@


== The overall ruleset translated to B

Please see this other file in the sandbox repo:

@
sandbox/joe/bmethod/contract/Pdpa.mch
@

This is a rendering of the rules in B-machine format.

We would want to output that as well.

Joe mentioned it might be possible to generate the above LTL/CTL test cases from the B machine directly! But let's represent it by hand using an embedded Haskell eDSL for now.

Avishkar points out that one way of finding inconsistencies in rules, is to find models of @ r1 && r2 && (! r3) @.

-}

module LS.XPile.B where

import LS
import AnyAll.BoolStruct (alwaysLabeled)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Prettyprinter
import Text.Pretty.Simple (pShowNoColor)
import qualified AnyAll as AA
import qualified Data.Map as Map

data LTLunary  = Lnot       -- ^ unary negation
               | Lbrace     -- ^ state formula
               | Lbracket   -- ^ actions
               | G -- ^ globally
               | F -- ^ finally
               | R -- ^ release
               | W -- ^ weak until
               | M -- ^ mighty release
               | X -- ^ next, aka O and N   -- this is one of the big ones
               | U -- ^ until               -- this is the other big one
               | H -- ^ past tense
           deriving (Eq, Read, Show)
data LTLbinary = Land
               | Lor
               | Limplies
           deriving (Eq, Read, Show)

data LTL a = L1   LTLunary  (LTL a)          -- ^ unary
           | L2   LTLbinary (LTL a) (LTL a)  -- ^ binary
           | Le   a                          -- ^ base element
           | Lb   Bool                       -- ^ bool special case
           deriving (Eq, Read, Show)

type Expr = String
             
sample1 :: LTL Expr
sample1 = (L2 Land
            (L1 G (L1 Lbracket (Le "breached_by = {}")))
            (L1 G (L2 Limplies
                   (L1 Lbracket (Le"prohibit_notify_indiv_happened(_)"))
                   (L1 H (L1 Lnot (L1 Lbrace (Le "notify_indiv_happened(_)")))))))
                       
instance Pretty a => Pretty (LTL a) where
  pretty (Le x)                    = pretty x
  pretty (Lb b)                    = pretty b -- True / False
  pretty (L1 Lnot   ltlx       ) = "not" <+> pretty ltlx
  pretty (L1 Lbrace ltlx       ) = braces   $ pretty ltlx
  pretty (L1 Lbracket ltlx     ) = brackets $ pretty ltlx
  pretty (L1 G        ltlx     ) = viaShow G <+> pretty ltlx
  pretty (L1 F        ltlx     ) = viaShow F <+> pretty ltlx
  pretty (L1 R        ltlx     ) = viaShow R <+> pretty ltlx
  pretty (L1 W        ltlx     ) = viaShow W <+> pretty ltlx
  pretty (L1 M        ltlx     ) = viaShow M <+> pretty ltlx
  pretty (L1 X        ltlx     ) = viaShow X <+> pretty ltlx
  pretty (L1 U        ltlx     ) = viaShow U <+> pretty ltlx
  pretty (L1 H        ltlx     ) = viaShow H <+> pretty ltlx
  pretty (L2 Land     ltlx ltly) = parens ( pretty ltlx ) <+> "&"  <> nest 2 (line <> parens (pretty ltly))
  pretty (L2 Lor      ltlx ltly) = parens ( pretty ltlx ) <+> "or" <+> parens (pretty ltly)
  pretty (L2 Limplies ltlx ltly) = parens ( pretty ltlx ) <+> "=>" <> line <> nest 4 ( parens (pretty ltly))

-- | extract the tree-structured rules from Interpreter

asB :: Interpreted -> String
asB l4i =
  show (pretty sample1)
  <> "\n" <>

  -- [TOOD] the hard part now is the conversion from the Interpreted AST to the above embedded language
  
     show (vsep
           [ "toplevelDecisions :: Map.Map (String) (Item String)"
           , "toplevelDecisions = Map.fromFoldable " <>
             (pretty $ TL.unpack (
                 pShowNoColor
                   [( T.intercalate " / " (T.unwords <$> names)
                    , alwaysLabeled bs)
                   | (names,bs) <- qaHornsT l4i
                   ]
                 )
             )
           , "toplevelDefaultMarking :: Marking"
           , "toplevelDefaultMarking = Marking $ Map.fromFoldable " <>
             (pretty . TL.unpack
              . TL.replace "False" "false"
              . TL.replace "True" "true"
              . pShowNoColor $ 
              Map.toList . AA.getMarking $
              getMarkings l4i
             )
           ]
          )
