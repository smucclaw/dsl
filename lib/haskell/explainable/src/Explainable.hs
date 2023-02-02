{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Explainable where

import qualified Data.Map as Map
import Control.Monad.Trans.RWS
import Data.Tree
import Data.List ( intercalate )
import Data.Ord ()
import Data.Maybe (mapMaybe)
import Control.Monad (forM_, mapAndUnzipM)
import Data.Bifunctor (first, second)

-- | Our Explainable monad supports evaluation-with-explanation of expressions in our DSL.
-- Normally, a DSL is /evaluated/: its expressions are reduced from types in the DSL to types in the host language.
-- for example, a DSL @eval@ would turn @(MathBin Plus (Val 1) (Val 2))@ into a simple Haskell @3 :: Float@.
--
-- But what if we need to show our work? We don't just want evaluation, we want explanation.
--
-- How do we augment evaluation to provide explanation?
--
-- Evaluation operates by recursive evaluation of the expression tree.
-- This monad offers transparency into evaluation, by constructing an
-- explanation tree which parallels the operation of the recursive
-- evaluation. Each eval inside the Explainable monad returns an `XP`
-- explanation of the "reasoning" behind that step, and that
-- explanation is (manually) stitched into the larger XP tree as it
-- builds up.
--
-- For convenience, we use Reader, Writer, and State to set up an
-- environment that `eval` functions can take advantage of. Typically
-- the scenario is passed in the Reader, and available via @asks
-- origReader@. Variable symbol tables can be kept in State, for
-- reading and writing. The Writer is a list of Strings, and can be
-- appended to for logging purposes, in a stackwise fashion for
-- efficiency; later entries get prepended to the head.
--
-- A little unusually, the explainable monad returns @(a,XP)@. So to get the actual value from a return, you should @fst@ it.
--
-- Our RWST wraps IO so you have the opportunity to do some actual STDOUT anytime by `liftIO`ing your @putStrLn@.
--
-- The Reader supports environmental context for a given evaluation.
-- The Reader type is user-specified; in our primary app, the Reader is a Scenario.
--
-- We tack on a bit of useful infrastructure of our own: basically, a call stack, and a history trace of previous execution,
-- in the form of a `HistoryPath`.
type Explainable r a = RWST         (HistoryPath,r) [String] MyState IO (a,XP)

-- | As we evaluate down from the root to the leaves,
--
-- * we record the output of previous evaluations in the first part, @History@
-- * we record the current path from the root in the second part, @Path@
--
-- in case a particular function needs to justify its return value by referring to some previous computation.
-- Technically this makes computation impure, so it is generally preferable to not do that.
type HistoryPath = ([String], [String])

-- | utility functions to return the historypath and origreader elemetns
historypath :: (hp,r) -> hp
origReader  :: (hp,r) ->    r
(historypath,origReader) = (fst,snd)

-- | The Writer supports logging, basically. We set it as @[String]@ in case you actually do want to use it.
-- Our primary app doen't actually use the Writer, but returns XP as the @snd@ part in our return value instead.
-- 
-- The @XP@ explanation is designed to be readable as an Org-mode file.
-- Inspired by Literate Programming we use the idea of Literate Outputting.
-- We separate our output into two parts:
--
-- - The @snd@ "Stdexp" component gets rendered as the heading followed by whatever body.
-- - The @fst@ "Stdout" component gets rendered within an Example block.
--
-- And then there are the children to a given node, which are rendered as sub-entries under the current output node.
-- @return@s are required to construct the @(a,XP)@ by hand, assembling the sub-entries in whatever way makes the most sense.
--
-- If you want to return multiple lines of commentary, the head of the `Stdexp` is the heading, and subsequent lines are provided
-- without decoration so they form the text block under the heading.
type XP = Tree (Stdout, Stdexp)
type Stdout = [String]
type Stdexp = [String]

-- | Helper function to make a child-free explanation node
mkNod :: a -> Tree ([b],[a])
mkNod x = Node ([],[x]) []
  
-- | The State supports a symbol table in which variables and functions are tracked.
-- We have a couple different symbol tables, one for numeric and one for boolean functions.
-- In future we will have as many symbol tables as there are Expr types in our DSL.
-- Grab the `MyState` by calling @<- get@. If you know which symtab you want, you can call
-- @<- gets symtabX@.
type SymTab = Map.Map String
data MyState = MyState { symtabF :: SymTab (Expr     Float)
                       , symtabP :: SymTab (Pred     Float)
                       , symtabL :: SymTab (ExprList Float)
                       }
  deriving (Show, Eq)
emptyState :: MyState
emptyState = MyState Map.empty Map.empty Map.empty

-- * Utility functions

-- | utility function to format the call stack  
pathSpec :: [String] -> String
pathSpec = intercalate " / " . reverse


-- | Explain an arbitrary @Explainable@; it's up to the input expr to run eval whatever
xplainE :: (Show e) => r -> Explainable r e -> IO (e, XP, MyState, [String])
xplainE r expr = do
  ((val,xpl), stab, wlog) <- runRWST
                             expr
                             (([],["toplevel"]),r)
                             emptyState
  putStrLn $ "** xplainE: " ++ show val
 -- putStrLn $ "** toplevel: val = "    ++ show val 
 --  putStrLn $ "** toplevel: symtab = " ++ show stab
 -- putStrLn $ "** toplevel: log = "    ++ show wlog
  putStrLn $ "*** toplevel: xpl = " ++ show val ++ "\n" ++ drawTreeOrg 3 xpl

  return (val, xpl, stab, wlog)

