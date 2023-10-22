module Explainable.MathLang where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Monad (forM_, mapAndUnzipM, unless)
import Control.Monad.Trans (liftIO)
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
data Expr a = Val      ExprLabel a                            -- ^ simple value
            | Parens   ExprLabel           (Expr a)           -- ^ parentheses for grouping
            | MathBin  ExprLabel MathBinOp (Expr a) (Expr a)  -- ^ binary arithmetic operation
            | MathVar  ExprLabel String                       -- ^ variable reference
            | MathSet  ExprLabel String    (Expr a)           -- ^ variable assignment
            | MathITE  ExprLabel  (Pred a) (Expr a) (Expr a)  -- ^ if-then-else
            | MathMax  ExprLabel           (Expr a) (Expr a)  -- ^ max of two expressions
            | MathMin  ExprLabel           (Expr a) (Expr a)  -- ^ min of two expressions
            | ListFold ExprLabel SomeFold (ExprList a)        -- ^ fold a list of expressions into a single expr value
            deriving (Eq, Show)

type ExprLabel = Maybe String
showlbl :: ExprLabel -> String
showlbl Nothing  = mempty
showlbl (Just l) = " (" ++ l ++ ")"

getExprLabel :: Expr a -> ExprLabel
getExprLabel ( Val      lbl  x     ) = lbl
getExprLabel ( Parens   lbl  x     ) = lbl
getExprLabel ( MathBin  lbl  x y z ) = lbl
getExprLabel ( MathVar  lbl  x     ) = lbl
getExprLabel ( MathSet  lbl  x y   ) = lbl
getExprLabel ( MathITE  lbl  x y z ) = lbl
getExprLabel ( MathMax  lbl  x y   ) = lbl
getExprLabel ( MathMin  lbl  x y   ) = lbl
getExprLabel ( ListFold lbl  x y   ) = lbl


(<++>) :: Maybe String -> Maybe String -> Maybe String
(<++>) Nothing Nothing  = Nothing
(<++>) Nothing (Just y) = Just y
(<++>) (Just x) Nothing = Just x
(<++>) (Just x) (Just y) = Just (x ++ ", " ++ y)

-- | basic binary operator for arithmetic
(|+),(|-),(|*),(|/) :: Expr Float -> Expr Float -> Expr Float
x |+ y = MathBin Nothing Plus   x y
x |- y = MathBin Nothing Minus  x y
x |* y = MathBin Nothing Times  x y
x |/ y = MathBin Nothing Divide x y

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
x +| ys = second (Node ([],["mapping + " ++ show x ++ " over a list"])) <$> mapAndUnzipM (eval . MathBin Nothing Plus   x) ys
x -| ys = second (Node ([],["mapping - " ++ show x ++ " over a list"])) <$> mapAndUnzipM (eval . MathBin Nothing Minus  x) ys
x *| ys = second (Node ([],["mapping * " ++ show x ++ " over a list"])) <$> mapAndUnzipM (eval . MathBin Nothing Times  x) ys
x /| ys = second (Node ([],["mapping / " ++ show x ++ " over a list"])) <$> mapAndUnzipM (eval . MathBin Nothing Divide x) ys

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
  = MathList  ExprLabel [Expr a]                                    -- ^ a basic list of `Expr` expressions
  | ListMap   ExprLabel (MathSection a)               (ExprList a) -- ^ apply the function to everything
  | ListFilt  ExprLabel                 (Expr a) Comp (ExprList a) -- ^ eliminate the unwanted elements
  | ListMapIf ExprLabel (MathSection a) (Expr a) Comp (ExprList a) -- ^ leaving the unwanted elements unchanged
  deriving (Eq, Show)

-- * Some sugary constructors for expressions in our math language.

-- | An ExprList contains expressions which have been filtered by being less or greater than some threshold.
--
-- In Haskell, we would say @ filter (>0) [-2,-1,0,1,2] @
--
-- Here, we would say @ 0 <| [-2,-1,0,1,2] @

(<|),(|>) :: Expr Float -> ExprList Float -> ExprList Float
x <| ys = ListFilt Nothing x CLT ys
x |> ys = ListFilt Nothing x CGT ys

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
(+||)     = ListFold Nothing FoldSum
sumOf     = ListFold Nothing FoldSum
(*||)     = ListFold Nothing FoldProduct
productOf = ListFold Nothing FoldProduct

negativeElementsOf :: [Float] -> ExprList Float
negativeElementsOf xs = Val Nothing 0 |> MathList Nothing (Val Nothing <$> xs)

positiveElementsOf :: [Float] -> ExprList Float
positiveElementsOf xs = Val Nothing 0 <| MathList Nothing (Val Nothing <$> xs)

timesEach :: Float -> ExprList Float -> ExprList Float
timesEach n = ListMap Nothing (MathSection Times (Val Nothing n))

timesPositives' :: Float -> ExprList Float -> ExprList Float
timesPositives' n = ListMapIf Nothing (MathSection Times (Val Nothing n)) (Val Nothing 0) CLT

timesPositives :: Float -> [Float] -> ExprList Float
timesPositives n ns = timesPositives' n (MathList Nothing (Val Nothing <$> ns))

-- | logical not

(!|) :: Pred a -> Pred a
(!|) = PredNot (Just "logical negation")

-- * Booleans

-- | A list of predicates

type PredList a = [Pred a]

-- | conditional predicates: things that evaluate to a boolean
data Pred a
  = PredVal  ExprLabel Bool
  | PredNot  ExprLabel (Pred a)                       -- ^ boolean not
  | PredComp ExprLabel Comp (Expr a) (Expr a)         -- ^ Ord comparisions: x < y
  | PredVar  ExprLabel String                         -- ^ boolean variable name
  | PredITE  ExprLabel (Pred a) (Pred a) (Pred a)     -- ^ if then else, booleans
  deriving (Eq, Show)

getPredLabel :: Pred a -> ExprLabel
getPredLabel ( PredVal      lbl  _     ) = lbl
getPredLabel ( PredNot      lbl  _     ) = lbl
getPredLabel ( PredComp     lbl  _ _ _ ) = lbl
getPredLabel ( PredVar      lbl  _     ) = lbl
getPredLabel ( PredITE      lbl  _ _ _ ) = lbl


-- | variables
data Var a
  = VarMath String (Expr a)
  | VarPred String (Pred a)
    deriving (Eq, Show)

-- * Evaluations

-- | Evaluate floats

eval,eval' :: Expr Float -> ExplainableIO r MyState Float
eval exprfloat = do
  (x, result) <- eval' exprfloat
  let lbl = getExprLabel exprfloat
  -- liftIO $ putStrLn $ "lbl = " ++ show lbl
  -- i really need to learn Lens / Optics
  unless (null lbl) $ modify (\mystate -> mystate { symtabF = Map.insert (fromMaybe "" lbl) (Val lbl x) (symtabF mystate) })
  -- liftIO . print =<< get
  return (x, result)

eval' (Val lbl x) = do
  (history,path) <- asks historypath
  return (x, Node ([unlines history ++ pathSpec path ++ ": " ++ show x]
                  ,[show x ++ ": " ++ fromMaybe "a leaf value" lbl]) [])
eval' (MathBin _lbl Plus   x y) = binEval "addition"       (+) x y
eval' (MathBin _lbl Minus  x y) = binEval "subtraction"    (-) x y
eval' (MathBin _lbl Times  x y) = binEval "multiplication" (*) x y
eval' (MathBin _lbl Divide x y) = binEval "division"       (/) x y
eval' (Parens  _lbl x)          = unaEval "parentheses"    id  x
eval' (MathITE _lbl p x y)      = evalFP eval  p x y
eval' (MathMax  lbl x y)        = eval (ListFold lbl FoldMax (MathList lbl [x,y]))
eval' (MathMin  lbl x y)        = eval (ListFold lbl FoldMin (MathList lbl [x,y]))
eval' (MathVar _lbl str) =
  let title = "variable expansion: " ++ str
      (lhs,rhs) = verbose title
  in retitle (title <> " " <> show str) $ do
    (xvar, xpl1) <- getvarF str
    (xval, xpl2) <- eval xvar
    return (xval, Node ([], [show xval ++ ": " ++ lhs ++ " " ++ str]) [xpl1, xpl2])
eval' (MathSet _lbl str x) =
  let title = "variable assignment:" ++ str
  in retitle (title <> " " <> show str <> " := " <> show x) $ do
    symtab <- gets symtabF
    let newmap = Map.union (Map.singleton str x) symtab
    modify (\ms -> ms { symtabF = newmap })
    (xval,xpl) <- eval x
    return (xval, Node ([], [show xval ++ ": " ++ " saved to " ++ str]) [xpl])

eval' (ListFold _lbl FoldMin     xs) = doFold "min" minimum xs
eval' (ListFold _lbl FoldMax     xs) = doFold "max" maximum xs
eval' (ListFold _lbl FoldSum     xs) = doFold "sum" sum xs
eval' (ListFold _lbl FoldProduct xs) = doFold "product" product xs                              

-- | do a fold over an `ExprList`
doFold :: String -> ([Float] -> Float) -> ExprList Float -> ExplainableIO r MyState Float
doFold str f xs = retitle ("listfold " <> str) $ do
  (MathList ylbl yvals,yexps) <- evalList xs
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

evalP,evalP' :: Pred Float -> ExplainableIO r MyState Bool
evalP pred = do
  (x, result) <- evalP' pred
  let lbl = getPredLabel pred
  unless (null lbl) $ modify (\mystate -> mystate { symtabP = Map.insert (fromMaybe "" lbl) (PredVal lbl x) (symtabP mystate) })
  return (x, result)

evalP' (PredVal lbl x) = do
  return (x, Node ([],[show x ++ ": a leaf value" ++ showlbl lbl]) [])
evalP' (PredNot lbl x) = do
  (xval,xpl) <- retitle "not" (evalP x)
  let toreturn = not xval
  return (toreturn, Node ([] ,[show toreturn ++ ": logical not of"]) [xpl])
evalP' (PredComp lbl c x y) =
  let title = "comparison" ++ showlbl lbl
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

evalP' (PredVar lbl str) =
  let title = "variable expansion" ++ showlbl lbl
      (lhs,rhs) = verbose title
  in retitle (title <> " " <> show str) $ do
    (xvar, xpl1) <- getvarP str
    (xval, xpl2) <- evalP xvar
    return (xval, Node ([], [show xval ++ ": " ++ lhs ++ " " ++ str]) [xpl1, xpl2])

evalP' (PredITE lbl p x y) = evalFP evalP p x y

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
evalList (MathList lbl a) = return (MathList lbl a, Node (show <$> a,["base MathList with " ++ show (length a) ++ " elements"]) [])
evalList (ListFilt lbl1 x comp (MathList lbl2 ys)) = do
  origs <- mapM eval ys
  round1 <- mapM (evalP . PredComp lbl1 comp x) ys
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
  return ( MathList (lbl1 <++> lbl2) round3
         , Node ([]
                , (show (length round3) ++ " elements were reduced from an original " ++ show (length round1))
                  : ["- " ++ show (fst o) | o <- origs])
           $ fmap snd round2)
evalList (ListFilt lbl x comp lf2) = do
  (lf2val, lf2xpl) <- evalList lf2
  (lf3val, lf3xpl) <- evalList (ListFilt lbl x comp lf2val)
  return (lf3val, Node ([],["recursing RHS ListFilt"]) [lf2xpl, mkNod "becomes", lf3xpl])

evalList (ListMap lbl Id ylist) = return (ylist, mkNod ("id on ExprList" ++ showlbl lbl))
evalList (ListMap lbl1 (MathSection binop x) ylist) = retitle "fmap mathsection" $ do
  (MathList lbl2 ylist', yxpl) <- evalList ylist
  return ( MathList lbl2 [ MathBin Nothing binop x y | y <- ylist' ]
         , Node ([],["fmap mathsection " ++ show binop ++ show x ++ " over " ++ show (length ylist') ++ " elements"]) [yxpl] )

evalList (ListMapIf lbl Id _c _comp ylist) = retitle ("fmap mathsection id" ++ showlbl lbl) $ evalList ylist
evalList (ListMapIf lbl1 (MathSection binop x) c comp ylist) = retitle ("fmap mathsection if" ++ showlbl lbl1) $ do
  (MathList lbl2 ylist', yxpl) <- evalList ylist
  liveElements <- mapM (evalP . PredComp (lbl1 <++> lbl2) comp c) ylist'

  return ( MathList (Just "evaled list") [ if tf then MathBin (Just "tf true") binop x y else y
                                         | (y,tf) <- zip ylist' (fst <$> liveElements) ]
         , Node ([],["fmap mathsection " ++ show binop ++ show x ++ " over " ++ show (length (filter id (fst <$> liveElements))) ++ " relevant elements (" ++
                    "who pass " ++ show c ++ " " ++ show comp ++ ")"])
           [ yxpl , Node ([],["selection of relevant elements"]) (snd <$> liveElements) ] )

-- | deepEvalList means we reduce the `ExprList` to a plain haskell list of floats.
-- I supposed this is analogous to "unboxed" types.

deepEvalList :: (ExprList Float,XP) -> ExplainableIO r MyState [Float]
deepEvalList (MathList lbl xs,xp) = do
  vals <- mapM eval xs
  return (fst <$> vals, Node ([],["deep evaluation to floats"]) (xp : (snd <$> vals)))
deepEvalList (other,_xp) = deepEvalList =<< evalList other


-- * Variable retrieval and assignment into the symbol table.
-- At present we have no notion of scope.

-- | Get an @Expr Float@ variable

getvarF :: String -> ExplainableIO r MyState (Expr Float)
getvarF x = do
  symtab <- gets symtabF
  case x `Map.lookup` symtab of
    Nothing -> liftIO $ do
      putStrLn ("getvarF: unable to find variable `" ++ x ++ "` in symbol table")
      error "MathLang fatal error in getvarF"
    Just v  -> return (v, Node ([show v], ["variable `" ++ x ++ "` has value " ++ show v]) [])

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


(@|+),(@|-),(@|*),(@|/) :: String -> Expr Float -> Expr Float -> Expr Float
(@|+) lbl = MathBin (Just lbl) Plus
(@|-) lbl = MathBin (Just lbl) Minus
(@|*) lbl = MathBin (Just lbl) Times
(@|/) lbl = MathBin (Just lbl) Divide

class Exprlbl expr a where
  (@|=) :: String -> expr a -> expr a

instance Exprlbl Expr a where
  (@|=) lbl ( Val      Nothing x     ) = Val      (Just lbl) x     
  (@|=) lbl ( Parens   Nothing x     ) = Parens   (Just lbl) x     
  (@|=) lbl ( MathBin  Nothing x y z ) = MathBin  (Just lbl) x y z 
  (@|=) lbl ( MathVar  Nothing x     ) = MathVar  (Just lbl) x     
  (@|=) lbl ( MathSet  Nothing x y   ) = MathSet  (Just lbl) x y   
  (@|=) lbl ( MathITE  Nothing x y z ) = MathITE  (Just lbl) x y z 
  (@|=) lbl ( MathMax  Nothing x y   ) = MathMax  (Just lbl) x y   
  (@|=) lbl ( MathMin  Nothing x y   ) = MathMin  (Just lbl) x y   
  (@|=) lbl ( ListFold Nothing x y   ) = ListFold (Just lbl) x y   
  (@|=) lbl ( Val      (Just old) x     ) = Val      (Just lbl <++> Just ("previously " ++ old)) x     
  (@|=) lbl ( Parens   (Just old) x     ) = Parens   (Just lbl <++> Just ("previously " ++ old)) x     
  (@|=) lbl ( MathBin  (Just old) x y z ) = MathBin  (Just lbl <++> Just ("previously " ++ old)) x y z 
  (@|=) lbl ( MathVar  (Just old) x     ) = MathVar  (Just lbl <++> Just ("previously " ++ old)) x     
  (@|=) lbl ( MathSet  (Just old) x y   ) = MathSet  (Just lbl <++> Just ("previously " ++ old)) x y   
  (@|=) lbl ( MathITE  (Just old) x y z ) = MathITE  (Just lbl <++> Just ("previously " ++ old)) x y z 
  (@|=) lbl ( MathMax  (Just old) x y   ) = MathMax  (Just lbl <++> Just ("previously " ++ old)) x y   
  (@|=) lbl ( MathMin  (Just old) x y   ) = MathMin  (Just lbl <++> Just ("previously " ++ old)) x y   
  (@|=) lbl ( ListFold (Just old) x y   ) = ListFold (Just lbl <++> Just ("previously " ++ old)) x y   

(@|.) :: String -> a -> Expr a
(@|.) = Val . Just
infix 6 @|., @|..

infix 1 @|=
infix 4 @|+, @|-
infix 5 @|*, @|/

-- | syntactic sugar for ternary syntax. The trick is to join up the branches into a single thing
data TernaryRHS a = TRHS a a deriving (Eq, Show)

(@|:) :: expr -> expr -> TernaryRHS expr
(@|:) tbranch fbranch = TRHS tbranch fbranch

class ExprTernary expr a where
  (@|?) :: Pred a -> TernaryRHS (expr a) -> expr a

infixr 2 @|?
infix  3 @|:

instance ExprTernary Expr a where
  (@|?) pred (TRHS tbranch fbranch) = MathITE Nothing pred tbranch fbranch

instance ExprTernary Pred a where
  (@|?) pred (TRHS tbranch fbranch) = PredITE Nothing pred tbranch fbranch

-- | syntactic sugar for the predicate expressions
instance Exprlbl Pred a where
  (@|=) lbl ( PredVal  Nothing    x )     = PredVal  (Just lbl) x
  (@|=) lbl ( PredNot  Nothing    x )     = PredNot  (Just lbl) x
  (@|=) lbl ( PredComp Nothing    x y z ) = PredComp (Just lbl) x y z
  (@|=) lbl ( PredVar  Nothing    x )     = PredVar  (Just lbl) x
  (@|=) lbl ( PredITE  Nothing    x y z ) = PredITE  (Just lbl) x y z
  (@|=) lbl ( PredVal  (Just old) x )     = PredVal  (Just lbl <++> Just ("previously " ++ old)) x
  (@|=) lbl ( PredNot  (Just old) x )     = PredNot  (Just lbl <++> Just ("previously " ++ old)) x
  (@|=) lbl ( PredComp (Just old) x y z ) = PredComp (Just lbl <++> Just ("previously " ++ old)) x y z
  (@|=) lbl ( PredVar  (Just old) x )     = PredVar  (Just lbl <++> Just ("previously " ++ old)) x
  (@|=) lbl ( PredITE  (Just old) x y z ) = PredITE  (Just lbl <++> Just ("previously " ++ old)) x y z

(@|..) :: String -> Bool -> Pred a
(@|..) = PredVal . Just

-- | some example runs
toplevel :: IO ()
toplevel = forM_ [ Val (Just "two") 2 |+ (Val (Just "five") 5 |- Val (Just "one") 1)
                 , "my sum" @|= "two" @|. 2 |+ "three" @|. 3
                 , ListFold (Just "greater than 2") FoldSum $ Val (Just "two") 2 <| MathList (Just "one to four")
                   [Val Nothing 1, Val Nothing 2, Val Nothing 3, Val Nothing 4]
                 , ListFold (Just "positive 1") FoldSum $ Val (Just "zero") 0 <| ml23
                 , ListFold (Just "positive 2") FoldSum $ Val (Just "zero") 0 <| ml23
                 ] $ \topexpr -> do
  (val, xpl, stab, wlog) <- xplainF () emptyState topexpr
  return ()
  where ml23 = MathList (Just "minus two to three")
               [Val Nothing (-2), Val Nothing (-1), Val Nothing 0, Val Nothing 1, Val Nothing 2, Val Nothing 3]
-- * Explainers  

-- | Explain an @Expr Float@
xplainF :: r -> MyState -> Expr Float -> IO (Float, XP, MyState, [String])
xplainF r s expr = do
  ((val,xpl), stab, wlog) <- runRWST
                             (eval expr)
                             (([],["toplevel"]),r)         -- reader: HistoryPath, actualReader
                             s
  putStrLn $ "#+begin_src haskell\n" ++ show expr ++ "\n#+end_src"
  putStrLn $ "- val :: "    ++ show val
  putStrLn $ "- log :: "    ++ show wlog
  putStrLn $ "- xpl :: " ++ show val ++ "\n" ++ drawTreeOrg 3 xpl

  return (val, xpl, stab, wlog)

-- | Explain an @ExprList Float@
xplainL :: r -> ExprList Float -> IO ([Float], XP, MyState, [String])
xplainL r exprList = do
  ((xl,xp), stab, wlog) <- runRWST
                           (deepEvalList (exprList,mkNod "deep eval"))
                           (([],["toplevel"]),r)         -- reader: HistoryPath, actualReader
                           emptyState -- state: MyState
  putStrLn $ "#+begin_src haskell\n" ++ show xl ++ "\n#+end_src"
  putStrLn $ "- val :: "    ++ show xl
  putStrLn $ "- log :: "    ++ show wlog
  putStrLn $ "- xpl :: " ++ show xl ++ "\n" ++ drawTreeOrg 3 xp
  return (xl, xp, stab, wlog) -- [TODO] note the explanation result from xs is discarded

unMathList :: Show a => ExprList a -> [Expr a]
unMathList (MathList lbl xs) = xs
unMathList x                 = error $ "unMathList: expected exprList to be fully evaluated, but got " ++ show x

-- | dump an explanation of a mathlang expression
dumpExplanationF :: Int -> MyState -> Expr Float -> IO ()
dumpExplanationF depth s f = do
  (val, xpl, stab, wlog) <- xplainF () s f
  putStrLn (stars ++ " val" ); print val
  putStrLn (stars ++ " xpl" ); print xpl
  putStrLn (stars ++ " stab"); print stab
  putStrLn (stars ++ " wlog"); print wlog

  where stars = replicate depth '*'
  

