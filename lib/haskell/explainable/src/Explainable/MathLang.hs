{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Explainable.MathLang where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Control.Monad (forM_, mapAndUnzipM)
import Control.Monad.Trans.RWS
import Data.Tree
import Data.Bifunctor
import Explainable

-- * Now we do a deepish embedding of an eDSL.
-- See:
-- - https://drive.google.com/file/d/1QMR2vJRDrnef_PaE1rGLfPOcrFAFkOxD/view?usp=share_link
-- - https://drive.google.com/file/d/1ETMkvgrAC6RCyJgF8np3vci5u9gwG9jS/view?usp=share_link
-- for theoretical background on how to embed a simple DSL in Haskell.

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


-- ** Simple Float Expressions

-- | Numeric expressions are things that evaluate to a number.
-- The @a@ here is pretty much always a @Float@.
data Expr a = Val a                                -- ^ simple value
            | Parens            (Expr a)           -- ^ parentheses for grouping
            | MathBin MathBinOp (Expr a) (Expr a)  -- ^ binary arithmetic operation
            | MathVar String                       -- ^ variable reference
            | MathSet String    (Expr a)           -- ^ variable assignment
            | MathITE  (Pred a) (Expr a) (Expr a)  -- ^ if-then-else
            | MathMax           (Expr a) (Expr a)  -- ^ max of two expressions
            | MathMin           (Expr a) (Expr a)  -- ^ min of two expressions
            | ListFold SomeFold (ExprList a)       -- ^ fold a list of expressions into a single expr value
            deriving (Eq, Show)

-- | basic binary operator for arithmetic
(|+),(|-),(|*),(|/) :: Expr Float -> Expr Float -> Expr Float
x |+ y = MathBin Plus   x y
x |- y = MathBin Minus  x y
x |* y = MathBin Times  x y
x |/ y = MathBin Divide x y

infix 5 |*, |/
infix 4 |+, |-

-- | fmap.
-- 
-- In Haskell, we would say @(+2) <$> [1,2,3]@
--
-- Here, we would say @2 +| [1,2,3]@
--
-- But this is crude. There's an alternative way to say it, using MathSections in an ExprList.

(+|),(-|),(*|),(/|) :: Expr Float -> [Expr Float] -> ExplainableIO r MyState [Float]
x +| ys = second (Node ([],["mapping + " ++ show x ++ " over a list"])) <$> mapAndUnzipM (eval . MathBin Plus   x) ys
x -| ys = second (Node ([],["mapping - " ++ show x ++ " over a list"])) <$> mapAndUnzipM (eval . MathBin Minus  x) ys
x *| ys = second (Node ([],["mapping * " ++ show x ++ " over a list"])) <$> mapAndUnzipM (eval . MathBin Times  x) ys
x /| ys = second (Node ([],["mapping / " ++ show x ++ " over a list"])) <$> mapAndUnzipM (eval . MathBin Divide x) ys

-- ** Function Sections and infrastructure for folds

-- | a function section is a unary math operator constructed from partial application of a binary.
-- for example, if we wanted to do a Haskell @(2*)@ we would say @MathSection Times (Val 2)@
data MathSection a
  = Id
  | MathSection MathBinOp (Expr a)
  deriving (Eq, Show)

data MathBinOp = Plus | Minus | Times | Divide
  deriving (Eq, Show)

-- | we can reduce a list of expressions to a single value...
data SomeFold = FoldSum      -- ^ by taking the sum
              | FoldProduct  -- ^ by taking the product
              | FoldMax      -- ^ by taking the maximum
              | FoldMin      -- ^ by taking the minimum
              deriving (Eq, Show)

-- ** Lists

-- | We can filter, map, and mapIf over lists of expressions. Here, @a@ is pretty much always a @Float@.
data ExprList a
  = MathList [Expr a]                                    -- ^ a basic list of `Expr` expressions
  | ListMap   (MathSection a)               (ExprList a) -- ^ apply the function to everything
  | ListFilt                  (Expr a) Comp (ExprList a) -- ^ eliminate the unwanted elements
  | ListMapIf (MathSection a) (Expr a) Comp (ExprList a) -- ^ leaving the unwanted elements unchanged
  deriving (Eq, Show)

-- * Some sugary constructors for expressions in our math language.

-- | An ExprList contains expressions which have been filtered by being less or greater than some threshold.
--
-- In Haskell, we would say @ filter (>0) [-2,-1,0,1,2] @
--
-- Here, we would say @ 0 <| [-2,-1,0,1,2] @

(<|),(|>) :: Expr Float -> ExprList Float -> ExprList Float
x <| ys = ListFilt x CLT ys
x |> ys = ListFilt x CGT ys

-- | To support our notion of Data.Ord
data Comp = CEQ | CGT | CLT | CGTE | CLTE
  deriving (Eq, Show)

-- | @show@ for comparisons
shw :: Comp -> String
shw CEQ  = "=="
shw CGT  = ">"
shw CGTE = ">="
shw CLT  = "<"
shw CLTE = "<="

-- * Syntactic Sugar

(+||),sumOf,productOf,(*||) :: ExprList a -> Expr a
(+||)     = ListFold FoldSum
sumOf     = ListFold FoldSum
(*||)     = ListFold FoldProduct
productOf = ListFold FoldProduct

negativeElementsOf :: [Float] -> ExprList Float
negativeElementsOf xs = Val 0 |> MathList (Val <$> xs)

positiveElementsOf :: [Float] -> ExprList Float
positiveElementsOf xs = Val 0 <| MathList (Val <$> xs)

timesEach :: Float -> ExprList Float -> ExprList Float
timesEach n = ListMap (MathSection Times (Val n))

timesPositives' :: Float -> ExprList Float -> ExprList Float
timesPositives' n = ListMapIf (MathSection Times (Val n)) (Val 0) CLT

timesPositives :: Float -> [Float] -> ExprList Float
timesPositives n ns = timesPositives' n (MathList (Val <$> ns))

-- | logical not

(!|) :: Pred a -> Pred a
(!|) = PredNot

-- * Booleans

-- | A list of predicates

type PredList a = [Pred a]

-- | conditional predicates: things that evaluate to a boolean
data Pred a
  = PredVal Bool
  | PredNot (Pred a)                       -- ^ boolean not
  | PredComp Comp (Expr a) (Expr a)        -- ^ Ord comparisions: x < y
  | PredVar String                         -- ^ boolean variable name
  | PredITE (Pred a) (Pred a) (Pred a)     -- ^ if then else, booleans
  deriving (Eq, Show)

-- | variables
data Var a
  = VarMath String (Expr a)
  | VarPred String (Pred a)
    deriving (Eq, Show)

-- * Evaluations

-- | Evaluate floats

eval :: Expr Float -> ExplainableIO r MyState Float
eval (Val x) = do
  (history,path) <- asks historypath
  return (x, Node ([unlines history ++ pathSpec path ++ ": " ++ show x]
                  ,[show x ++ ": a leaf value"]) [])
eval (MathBin Plus   x y) = binEval "addition"       (+) x y
eval (MathBin Minus  x y) = binEval "subtraction"    (-) x y
eval (MathBin Times  x y) = binEval "multiplication" (*) x y
eval (MathBin Divide x y) = binEval "division"       (/) x y
eval (Parens x)           = unaEval "parentheses"    id  x
eval (MathITE p x y)      = evalFP eval  p x y
eval (MathMax x y)        = eval (ListFold FoldMax (MathList [x,y]))
eval (MathMin x y)        = eval (ListFold FoldMin (MathList [x,y]))
eval (MathVar str) =
  let title = "variable expansion"
      (lhs,rhs) = verbose title
  in retitle (title <> " " <> show str) $ do
    (xvar, xpl1) <- getvarF str
    (xval, xpl2) <- eval xvar
    return (xval, Node ([], [show xval ++ ": " ++ lhs ++ " " ++ str]) [xpl1, xpl2])
eval (MathSet str x) =
  let title = "variable assignment"
  in retitle (title <> " " <> show str <> " := " <> show x) $ do
    symtab <- gets symtabF
    let newmap = Map.union (Map.singleton str x) symtab
    modify (\ms -> ms { symtabF = newmap })
    (xval,xpl) <- eval x
    return (xval, Node ([], [show xval ++ ": " ++ " saved to " ++ str]) [xpl])

eval (ListFold FoldMin     xs) = doFold "min" minimum xs
eval (ListFold FoldMax     xs) = doFold "max" maximum xs
eval (ListFold FoldSum     xs) = doFold "sum" sum xs
eval (ListFold FoldProduct xs) = doFold "product" product xs                              

-- | do a fold over an `ExprList`
doFold :: String -> ([Float] -> Float) -> ExprList Float -> ExplainableIO r MyState Float
doFold str f xs = retitle ("listfold " <> str) $ do
  (MathList yvals,yexps) <- evalList xs
  zs <- mapM eval yvals
  let toreturn = f (fst <$> zs)
  return (toreturn
         , Node ([],(show toreturn ++ " = " ++ str ++ " of " ++ show (length zs) ++ " elements")
                  : [ "- " ++ show e | e <- fst <$> zs ])
           (yexps : (snd <$> zs)))
  
-- | helper function, Unary evaluation of an `Expr` `Float` to some `Float`
unaEval :: String -> (Float -> Float) -> Expr Float -> ExplainableIO r MyState Float
unaEval title f x =
  let (lhs,_rhs) = verbose title
  in retitle title $ do
    (xval, xpl) <- eval x
    let toreturn = f xval
    return (toreturn, Node ([], [show toreturn ++ ": " ++ lhs]) [xpl])

-- | helper function, Binary evaluation
binEval :: String -> (Float -> Float -> Float) -> Expr Float -> Expr Float -> ExplainableIO r MyState Float
binEval title f x y = retitle title $ do
  -- liftIO putStrLn should be treated as more of a Debug.Trace.
  -- "normal" output gets returned in the fst part of the Node.
  -- normal output then gets output inside a #+begin_example/#+end_example block.
  -- liftIO $ putStrLn $ "eval " ++ title ++ ": path is " ++ intercalate " / " (reverse path)
  (xval, xpl) <- eval x
  (yval, ypl) <- local (\((h,p),r) -> ((h ++ [show xval],p),r)) (eval y)
   -- we sneak in monadic history of the upper evaluations
  let toreturn = f xval yval
      (lhs,rhs) = verbose title
  return (toreturn, Node (fst (rootLabel xpl) ++ fst (rootLabel ypl)
                         , [show toreturn ++ ": " ++ lhs])
                    [xpl, mkNod rhs, ypl] )

-- | Evaluate predicates

evalP :: Pred Float -> ExplainableIO r MyState Bool
evalP (PredVal x) = do
  return (x, Node ([],[show x ++ ": a leaf value"]) [])
evalP (PredNot x) = do
  (xval,xpl) <- retitle "not" (evalP x)
  let toreturn = not xval
  return (toreturn, Node ([] ,[show toreturn ++ ": logical not of"]) [xpl])
evalP (PredComp c x y) =
  let title = "comparison"
  in retitle (title <> " " <> shw c) $ do
    (xval, xpl) <- eval x
    (yval, ypl) <- eval y
    let c' = compare xval yval
        toreturn = case c of
          CEQ  | c' == EQ           -> True
          CGT  | c' == GT           -> True
          CLT  | c' == LT           -> True
          CGTE | c' `elem` [GT, EQ] -> True
          CLTE | c' `elem` [LT, EQ] -> True
          _                         -> False
        (lhs,rhs) = verbose title
    return (toreturn, (Node ([]
                            ,[show toreturn ++ " " ++ lhs ++ " (" ++ shw c ++ ")"])
                       [ xpl
                       , mkNod rhs
                       , ypl ]))

evalP (PredVar str) =
  let title = "variable expansion"
      (lhs,rhs) = verbose title
  in retitle (title <> " " <> show str) $ do
    (xvar, xpl1) <- getvarP str
    (xval, xpl2) <- evalP xvar
    return (xval, Node ([], [show xval ++ ": " ++ lhs ++ " " ++ str]) [xpl1, xpl2])

evalP (PredITE p x y) = evalFP evalP p x y

-- | Evaluate If-Then-Else by first evaluating the conditional, and then evaluating the chosen branch.
-- This works for both boolean Predicates and float Exprs.
evalFP :: Show t
       => (t -> ExplainableIO r MyState a)
       -> Pred Float
       -> t
       -> t
       -> ExplainableIO r MyState a
evalFP evf p x y = retitle "if-then-else" $ do
  (pval,pxpl) <- evalP p
  if pval
    then do
    (xval,xxpl) <- evf x
    return (xval, Node ([],["if " ++ show p ++ " then " ++ show x ++ " else " ++ show y] ) [pxpl, mkNod "thus we choose the then branch", xxpl])
    else do
    (yval,yxpl) <- evf y
    return (yval, Node ([],["if-then-else false"] ) [pxpl, mkNod "thus we choose the else branch", yxpl])

-- | Evaluate an `ExprList`

evalList :: ExprList Float -> ExplainableIO r MyState (ExprList Float)
evalList (MathList a) = return (MathList a, Node (show <$> a,["base MathList with " ++ show (length a) ++ " elements"]) [])
evalList (ListFilt x comp (MathList ys)) = do
  origs <- mapM eval ys
  round1 <- mapM (evalP . PredComp comp x) ys
  let round2 = [ if not r1
                 then (Nothing, Node ([show xval]
                                     , ["excluded " ++ show xval ++
                                        " due to failing comparison test"]) [xpl])
                 else (Just xval, Node ([show xval]
                                       , ["included " ++ show xval ++
                                          " due to passing comparison test"]) [xpl])
               | ((r1,xpl), xval) <- zip round1 ys
               ]
      round3 = mapMaybe fst round2
  return ( MathList round3
         , Node ([]
                , (show (length round3) ++ " elements were reduced from an original " ++ show (length round1))
                  : ["- " ++ show (fst o) | o <- origs])
           $ fmap snd round2)
evalList (ListFilt x comp lf2) = do
  (lf2val, lf2xpl) <- evalList lf2
  (lf3val, lf3xpl) <- evalList (ListFilt x comp lf2val)
  return (lf3val, Node ([],["recursing RHS ListFilt"]) [lf2xpl, mkNod "becomes", lf3xpl])

evalList (ListMap Id ylist) = return (ylist, mkNod "id on ExprList")
evalList (ListMap (MathSection binop x) ylist) = retitle "fmap mathsection" $ do
  (MathList ylist', yxpl) <- evalList ylist
  return ( MathList [ MathBin binop x y | y <- ylist' ]
         , Node ([],["fmap mathsection " ++ show binop ++ show x ++ " over " ++ show (length ylist') ++ " elements"]) [yxpl] )

evalList (ListMapIf Id _c _comp ylist) = retitle "fmap mathsection id" $ evalList ylist
evalList (ListMapIf (MathSection binop x) c comp ylist) = retitle "fmap mathsection if" $ do
  (MathList ylist', yxpl) <- evalList ylist
  liveElements <- mapM (evalP . PredComp comp c) ylist'

  return ( MathList [ if tf then MathBin binop x y else y
                    | (y,tf) <- zip ylist' (fst <$> liveElements) ]
         , Node ([],["fmap mathsection " ++ show binop ++ show x ++ " over " ++ show (length (filter id (fst <$> liveElements))) ++ " relevant elements (" ++
                    "who pass " ++ show c ++ " " ++ show comp ++ ")"])
           [ yxpl , Node ([],["selection of relevant elements"]) (snd <$> liveElements) ] )

-- | deepEvalList means we reduce the `ExprList` to a plain haskell list of floats.
-- I supposed this is analogous to "unboxed" types.

deepEvalList :: (ExprList Float,XP) -> ExplainableIO r MyState [Float]
deepEvalList (MathList xs,xp) = do
  vals <- mapM eval xs
  return (fst <$> vals, Node ([],["deep evaluation to floats"]) (xp : (snd <$> vals)))
deepEvalList (other,_xp) = deepEvalList =<< evalList other


-- * Variable retrieval and assignment into the symbol table.
-- At present we have no notion of scope.

-- | Get an @Expr Float@ variable

getvarF :: String -> ExplainableIO r MyState (Expr Float)
getvarF x = do
  symtab <- gets symtabF
  return (symtab Map.! x, Node ([show $ symtab Map.! x], ["looked up " ++ x]) [])

-- | Get a @Pred Float@ variable
  
getvarP :: String -> ExplainableIO r MyState (Pred Float)
getvarP x = do
  symtab <- gets symtabP
  return (symtab Map.! x, Node ([show $ symtab Map.! x], ["looked up " ++ x]) [])

-- [TODO] ExprLists too, i suppose

-- | given a title string, provide a generic template for its explanation
verbose :: String -> (String, String)
verbose "addition"       = ("which we obtain by adding", "to")
verbose "subtraction"    = ("which we obtain by taking", "minus")
verbose "division"       = ("which we obtain by dividing", "by")
verbose "multiplication" = ("which we obtain by multiplying", "by")
verbose "parentheses"    = ("which is a parenthesized", "")
verbose "negation"       = ("which is logical negation of", "")
verbose "comparison"     = ("is the result of comparing", "with")
verbose "variable expansion" = ("which comes from the variable", "")
verbose x                = (x, x ++ " argument")

-- | some example runs
toplevel :: IO ()
toplevel = forM_ [ Val 2 |+ (Val 5 |- Val 1)
                 , ListFold FoldSum $ Val 2 <| MathList [Val 1, Val 2, Val 3, Val 4]
                 , ListFold FoldSum $ Val 0 <| MathList [Val (-2), Val (-1), Val 0, Val 1, Val 2, Val 3]
                 , ListFold FoldSum $ Val 0 <| MathList [Val (-2), Val (-1), Val 0, Val 1, Val 2, Val 3]
                 ] $ \topexpr -> do
  (val, xpl, stab, wlog) <- xplainF () topexpr
  return ()

-- * Explainers  

-- | Explain an @Expr Float@
xplainF :: r -> Expr Float -> IO (Float, XP, MyState, [String])
xplainF r expr = do
  ((val,xpl), stab, wlog) <- runRWST
                             (eval expr)
                             (([],["toplevel"]),r)         -- reader: HistoryPath, actualReader
                             emptyState -- state: MyState
  putStrLn $ "* xplainF"
  putStrLn $ "#+begin_src haskell\n" ++ show expr ++ "\n#+end_src"
  putStrLn $ "** toplevel: val = "    ++ show val
  putStrLn $ "** toplevel: log = "    ++ show wlog
  putStrLn $ "** toplevel: xpl = " ++ show val ++ "\n" ++ drawTreeOrg 3 xpl

  return (val, xpl, stab, wlog)

-- | Explain an @ExprList Float@
xplainL :: r -> ExprList Float -> IO ([Float], XP, MyState, [String])
xplainL r exprList = do
  ((xl,xp), stab, wlog) <- runRWST
                           (deepEvalList (exprList,mkNod "deep eval"))
                           (([],["toplevel"]),r)         -- reader: HistoryPath, actualReader
                           emptyState -- state: MyState
  putStrLn $ "* xplainL"
  putStrLn $ "#+begin_src haskell\n" ++ show xl ++ "\n#+end_src"
  putStrLn $ "** toplevel: val = "    ++ show xl
  putStrLn $ "** toplevel: log = "    ++ show wlog
  putStrLn $ "** toplevel: xpl = " ++ show xl ++ "\n" ++ drawTreeOrg 3 xp
  return (xl, xp, stab, wlog) -- [TODO] note the explanation result from xs is discarded

unMathList :: Show a => ExprList a -> [Expr a]
unMathList (MathList xs) = xs
unMathList x             = error $ "unMathList: expected exprList to be fully evaluated, but got " ++ show x
