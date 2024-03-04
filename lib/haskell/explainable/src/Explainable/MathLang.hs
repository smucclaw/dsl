{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Explainable.MathLang where

import Control.Monad (mapAndUnzipM, unless)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.RWS
  ( RWST (runRWST),
    asks,
    gets,
    local,
    modify,
  )
import Data.Bifunctor (Bifunctor (second))
-- import Data.Text qualified as T

import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Interpolate (i, __i)
import Data.Tree (Tree (..))
import Explainable
  ( ExplainableIO,
    XP,
    drawTreeOrg,
    emptyXP,
    historypath,
    mkNod,
    pathSpec,
    retitle,
  )
import Numeric.Extras (fmod)
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    colon,
    comma,
    dquotes,
    encloseSep,
    hang,
    indent,
    lbrace,
    line,
    list,
    lparen,
    parens,
    rbrace,
    rparen,
    tupled,
    viaShow,
    (<+>),
  )
import Prettyprinter.Interpolate (__di)
import Prelude hiding (pred)
import Debug.Trace (trace)

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
type SymTab = Map.HashMap String
data MyState = MyState { symtabF :: SymTab (Expr     Double) -- numbers (numeric expressions)
                       , symtabP :: SymTab (Pred     Double) -- booleans (propositional expressions)
                       , symtabL :: SymTab (ExprList Double) -- lists of numeric expressions
                       , symtabS :: SymTab String
                       }
  deriving (Show, Eq)

emptyState :: MyState
emptyState = MyState Map.empty Map.empty Map.empty Map.empty

instance Semigroup MyState where
  (MyState f p l s) <> (MyState f' p' l' s') =
    MyState (f <> f') (p <> p') (l <> l') (s <> s')

instance Monoid MyState where
  mempty = emptyState


-- ** Simple Double Expressions

-- | Numeric expressions are things that evaluate to a number.
-- The @a@ here is pretty much always a @Double@.
data Expr a = Val      ExprLabel a                            -- ^ simple value
            | Parens   ExprLabel           (Expr a)           -- ^ parentheses for grouping
            | MathBin  ExprLabel MathBinOp (Expr a) (Expr a)  -- ^ binary arithmetic operation
            | MathVar            String                       -- ^ variable reference
            | MathSet            String    (Expr a)           -- ^ variable assignment
            | MathITE  ExprLabel  (Pred a) (Expr a) (Expr a)  -- ^ if-then-else
            | MathMax  ExprLabel           (Expr a) (Expr a)  -- ^ max of two expressions
            | MathMin  ExprLabel           (Expr a) (Expr a)  -- ^ min of two expressions
            | ListFold ExprLabel SomeFold (ExprList a)        -- ^ fold a list of expressions into a single expr value
            | Undefined ExprLabel -- ^ we realize, too late, that we needed an Expr ( Maybe Double ) or perhaps a Maybe (Expr Double)
            deriving (Eq)

instance (Show a) => Show (Expr a) where
  show (Val Nothing a) = [i|Val #{parensIfNeg (show a)}|]
  show (Val lbl a) = [i|Val #{showlbl lbl} #{parensIfNeg (show a)}|]
  show (Parens lbl e) = [i|Parens #{showlbl lbl} #{e}|]
  show (MathBin lbl binop e1 e2) = [i|MathBinOp #{showlbl lbl} #{binop} #{e1} #{e2}|]
  show (MathVar str) = [i|MathVar #{str}|]
  show (MathSet str e) = [i|MathSet #{str} #{e}|]
  show (MathITE lbl pred e1 e2) = [i|MathITE #{showlbl lbl} #{pred} #{e1} #{e2}|]
  show (MathMax lbl e1 e2) = [i|MathMax #{showlbl lbl} #{e1} #{e2}|]
  show (MathMin lbl e1 e2) = [i|MathMin #{showlbl lbl} #{e1} #{e2}|]
  show (ListFold lbl f el) = [i|ListFold #{showlbl lbl} #{f} #{el}|]
  show (Undefined lbl) = [i|Undefined #{showlbl lbl}|]

type ExprLabel = Maybe String

showlbl :: ExprLabel -> String
showlbl Nothing  = mempty
showlbl (Just l) = [i|(#{l})|]

parensIfNeg :: String -> String
parensIfNeg str@('-':_) = [i|(#{str})|]
parensIfNeg str = str

cappedBy :: Expr a -> Expr a -> Expr a
cappedBy = MathMin $ Just "capped by"

discountedBy :: Expr Double -> Expr Double -> Expr Double
discountedBy x y = x |* ("one hundred percent" @|. 1 |- y)

-- | shouldn't this move into the Exprlbl class?
getExprLabel :: Expr a -> ExprLabel
getExprLabel ( Undefined lbl       ) = lbl
getExprLabel ( Val      lbl  _     ) = lbl
getExprLabel ( Parens   lbl  _     ) = lbl
getExprLabel ( MathVar       lbl   ) = Just lbl
getExprLabel ( MathSet       lbl _ ) = Just lbl
getExprLabel ( MathMax  lbl  _ _   ) = lbl
getExprLabel ( MathMin  lbl  _ _   ) = lbl
getExprLabel ( ListFold lbl  _ _   ) = lbl
getExprLabel ( MathBin  lbl  _ _ _ ) = lbl
getExprLabel ( MathITE  lbl  _ _ _ ) = lbl


(<++>) :: Maybe String -> Maybe String -> Maybe String
(<++>) Nothing Nothing  = Nothing
(<++>) Nothing (Just y) = Just y
(<++>) (Just x) Nothing = Just x
(<++>) (Just x) (Just y) = Just [i|#{x}, #{y}|]

-- | basic binary operator for arithmetic
(|+),(|-),(|*),(|/),(|%) :: Expr Double -> Expr Double -> Expr Double
x |+ y = MathBin Nothing Plus   x y
x |- y = MathBin Nothing Minus  x y
x |* y = MathBin Nothing Times  x y
x |/ y = MathBin Nothing Divide x y
x |% y = MathBin Nothing Modulo x y

infixl 5 |*, |/
infixl 4 |+, |-

less :: Expr Double -> Expr Double -> Expr Double
less = (|-)

-- | fmap.
--
-- In Haskell, we would say @(+2) <$> [1,2,3]@
--
-- Here, we would say @2 +| [1,2,3]@
--
-- But this is crude. There's an alternative way to say it, using MathSections in an ExprList.

(+|),(-|),(*|),(/|),(%|) :: Expr Double -> [Expr Double] -> ExplainableIO r MyState [Double]
x +| ys = second (Node ([],[[i|mapping + #{x} over a list|]])) <$> mapAndUnzipM (eval . MathBin Nothing Plus   x) ys
x -| ys = second (Node ([],[[i|mapping - #{x} over a list|]])) <$> mapAndUnzipM (eval . MathBin Nothing Minus  x) ys
x *| ys = second (Node ([],[[i|mapping * #{x} over a list|]])) <$> mapAndUnzipM (eval . MathBin Nothing Times  x) ys
x /| ys = second (Node ([],[[i|mapping / #{x} over a list|]])) <$> mapAndUnzipM (eval . MathBin Nothing Divide x) ys
x %| ys = second (Node ([],[[i|mapping / #{x} over a list|]])) <$> mapAndUnzipM (eval . MathBin Nothing Modulo x) ys

-- ** Function Sections and infrastructure for folds

-- | a function section is a unary math operator constructed from partial application of a binary.
-- for example, if we wanted to do a Haskell @(2*)@ we would say @MathSection Times (Val 2)@
data MathSection a
  = Id
  | MathSection MathBinOp (Expr a)
  deriving (Eq, Show)

data MathBinOp = Plus | Minus | Times | Divide | Modulo
  deriving (Eq, Show)

-- | we can reduce a list of expressions to a single value...
data SomeFold = FoldSum      -- ^ by taking the sum
              | FoldProduct  -- ^ by taking the product
              | FoldMax      -- ^ by taking the maximum
              | FoldMin      -- ^ by taking the minimum
              deriving (Eq, Show)

-- ** Lists

-- | We can filter, map, and mapIf over lists of expressions. Here, @a@ is pretty much always a @Double@.
data ExprList a
  = MathList  ExprLabel [Expr a]                                    -- ^ a basic list of `Expr` expressions
  | ListMap   ExprLabel (MathSection a)               (ExprList a) -- ^ apply the function to everything
  | ListFilt  ExprLabel                 (Expr a) Comp (ExprList a) -- ^ eliminate the unwanted elements
  | ListMapIf ExprLabel (MathSection a) (Expr a) Comp (ExprList a) -- ^ leaving the unwanted elements unchanged
  | ListConcat ExprLabel [ExprList a] -- ^ [[a]] -> [a]
  | ListITE    ExprLabel (Pred a) (ExprList a) (ExprList a)        -- ^ if-then-else for expr lists
  deriving (Eq, Show)

-- * Some sugary constructors for expressions in our math language.

-- | An ExprList contains expressions which have been filtered by being less or greater than some threshold.
--
-- In Haskell, we would say @ filter (>0) [-2,-1,0,1,2] @
--
-- Here, we would say @ 0 <| [-2,-1,0,1,2] @

(<|),(|>) :: Expr Double -> ExprList Double -> ExprList Double
x <| ys = ListFilt Nothing x CLT ys
x |> ys = ListFilt Nothing x CGT ys

-- | To support our notion of Data.Ord and Eq
data Comp = CEQ | CGT | CLT | CGTE | CLTE | CNEQ
  deriving (Eq, Show)

-- | @show@ for comparisons
shw :: Comp -> String
shw CEQ  = "=="
shw CNEQ = "!="
shw CGT  = ">"
shw CGTE = ">="
shw CLT  = "<"
shw CLTE = "<="

-- * Syntactic Sugar

(+||),(*||),(<||),(>||) :: ExprList a -> Expr a
(+||)     = ListFold Nothing FoldSum
(*||)     = ListFold Nothing FoldProduct
(<||)     = ListFold Nothing FoldMin
(>||)     = ListFold Nothing FoldMax

sumOf,productOf,minOf,maxOf :: [Expr a] -> Expr a
sumOf     = (+||) . MathList Nothing
productOf = (*||) . MathList Nothing
minOf     = (<||) . MathList Nothing
maxOf     = (>||) . MathList Nothing

negativeElementsOf :: [Double] -> ExprList Double
negativeElementsOf xs = Val Nothing 0 |> MathList Nothing (Val Nothing <$> xs)

positiveElementsOf :: [Double] -> ExprList Double
positiveElementsOf xs = Val Nothing 0 <| MathList Nothing (Val Nothing <$> xs)

timesEach :: Double -> ExprList Double -> ExprList Double
timesEach n = ListMap Nothing $ MathSection Times (Val Nothing n)

timesPositives' :: Double -> ExprList Double -> ExprList Double
timesPositives' n = ListMapIf Nothing (MathSection Times (Val Nothing n)) (Val Nothing 0) CLT

timesPositives :: Double -> [Double] -> ExprList Double
timesPositives n ns = timesPositives' n $ MathList Nothing (Val Nothing <$> ns)

-- | logical not

(|!) :: Pred a -> Pred a
(|!) = PredNot $ Just "logical negation"

-- * Booleans

-- | A list of predicates

type PredList a = [Pred a]

-- | conditional predicates: things that evaluate to a boolean
data Pred a
  = PredVal  ExprLabel Bool
  | PredNot  ExprLabel (Pred a)                       -- ^ boolean not
  | PredComp ExprLabel Comp (Expr a) (Expr a)         -- ^ Ord comparisions: x < y
  | PredBin  ExprLabel PredBinOp (Pred a) (Pred a)    -- ^ predicate and / or / eq / ne
  | PredVar  String                                   -- ^ boolean variable retrieval
  | PredSet  String (Pred a)                          -- ^ boolean variable assignment
  | PredITE  ExprLabel (Pred a) (Pred a) (Pred a)     -- ^ if then else, booleans
  | PredFold ExprLabel AndOr (PredList a)             -- ^ and / or a list
  deriving (Eq, Show)

data AndOr = PLAnd | PLOr
  deriving (Eq, Show)

getPredLabel :: Pred a -> ExprLabel
getPredLabel ( PredVal      lbl  _     ) = lbl
getPredLabel ( PredNot      lbl  _     ) = lbl
getPredLabel ( PredBin      lbl  _ _ _ ) = lbl
getPredLabel ( PredComp     lbl  _ _ _ ) = lbl
getPredLabel ( PredVar      lbl        ) = Just lbl
getPredLabel ( PredSet      lbl  _     ) = Just lbl
getPredLabel ( PredITE      lbl  _ _ _ ) = lbl
getPredLabel ( PredFold     lbl  _ _   ) = lbl


-- | variables
data Var a
  = VarMath String (Expr a)
  | VarPred String (Pred a)
    deriving (Eq, Show)

-- * Evaluations

-- | Evaluate floats

eval,eval' :: Expr Double -> ExplainableIO r MyState Double
eval exprfloat = do
  (x, result) <- eval' exprfloat
  let lbl = getExprLabel exprfloat
  -- liftIO $ putStrLn $ "lbl = " ++ show lbl
  -- i really need to learn Lens / Optics
  unless (null lbl) $ modify \mystate -> mystate { symtabF = Map.insert (fromMaybe "" lbl) (Val lbl x) (symtabF mystate) }
  -- liftIO . print =<< get
  pure (x, result)

-- in the Haskell we default undefined to zero. In the typescript output we preserve undefined.
eval' (Undefined lbl) = do
  (history,path) <- asks historypath
  return (0, Node ([[i|#{unlines history}#{pathSpec path}: undefined as zero 0|]]
                  ,["undefined: " ++ fromMaybe "an explicit undefined value, defaulting to zero" lbl]) [])

eval' (Val lbl x) = do
  (history,path) <- asks historypath
  return (x, Node ([[i|#{unlines history}#{pathSpec path}: #{x}|]]
                  ,[[i|#{x}: #{fromMaybe "a leaf value" lbl}|]]) [])
eval' (MathBin _lbl Plus   x y) = binEval "addition"       (+) x y
eval' (MathBin _lbl Minus  x y) = binEval "subtraction"    (-) x y
eval' (MathBin _lbl Times  x y) = binEval "multiplication" (*) x y
eval' (MathBin _lbl Divide x y) = binEval "division"       (/) x y
eval' (MathBin _lbl Modulo x y) = binEval "modulo"         fmod x y
eval' (Parens  _lbl x)          = unaEval "parentheses"    id  x
eval' (MathITE _lbl p x y)      = evalFP eval  p x y
eval' (MathMax  lbl x y)        = eval (ListFold lbl FoldMax (MathList lbl [x,y]))
eval' (MathMin  lbl x y)        = eval (ListFold lbl FoldMin (MathList lbl [x,y]))
eval' (MathVar      str) =
  let title = [i|variable expansion: #{str}|]
      (lhs,_rhs) = verbose title
  in retitle [i|#{title} #{str}|] do
    (xvar, xpl1) <- getvarF str
    (xval, xpl2) <- eval xvar
    pure (xval, Node ([], [[i|#{xval}: #{lhs} #{str}|]]) [xpl1, xpl2])
eval' (MathSet     str x) =
  let title :: String = [i|variable assignment: #{str}|]
  in retitle [i|#{title} #{str} := #{x}|] do
    symtab <- gets symtabF
    let newmap = Map.union (Map.singleton str x) symtab
    modify \ms -> ms { symtabF = newmap }
    (xval,xpl) <- eval x
    return (xval, Node ([], [[i|#{xval}: saved to #{str}|]]) [xpl])

eval' (ListFold _lbl FoldMin     xs) = doFold "min" minimum xs
eval' (ListFold _lbl FoldMax     xs) = doFold "max" maximum xs
eval' (ListFold _lbl FoldSum     xs) = doFold "sum" sum xs
eval' (ListFold _lbl FoldProduct xs) = doFold "product" product xs

-- | do a fold over an `ExprList`
doFold :: String -> ([Double] -> Double) -> ExprList Double -> ExplainableIO r MyState Double
doFold str f xs = retitle [i|listfold #{str}|] do
  (MathList _ylbl yvals,yexps) <- evalList xs
  zs <- eval `traverse` yvals
  let toreturn = f $ fst <$> zs
  pure (toreturn
         , Node ([],[i|#{toreturn} = #{str} of #{length zs} elements|]
                  : [ [i|- #{e}|] | e <- fst <$> zs ])
           (yexps : (snd <$> zs)))

-- | helper function, Unary evaluation of an `Expr` `Double` to some `Double`
unaEval :: String -> (Double -> Double) -> Expr Double -> ExplainableIO r MyState Double
unaEval title f x =
  let (lhs,_rhs) = verbose title
  in retitle title do
    (xval, xpl) <- eval x
    let toreturn = f xval
    pure (toreturn, Node ([], [[i|#{toreturn}: #{lhs}|]]) [xpl])

-- | helper function, Binary evaluation
binEval :: String -> (Double -> Double -> Double) -> Expr Double -> Expr Double -> ExplainableIO r MyState Double
binEval title f x y = retitle title do
  -- liftIO putStrLn should be treated as more of a Debug.Trace.
  -- "normal" output gets returned in the fst part of the Node.
  -- normal output then gets output inside a #+begin_example/#+end_example block.
  -- liftIO $ putStrLn $ "eval " ++ title ++ ": path is " ++ intercalate " / " (reverse path)
  (xval, xpl) <- eval x
  (yval, ypl) <- local (\((h,p),r) -> ((h ++ [show xval],p),r)) (eval y)
   -- we sneak in monadic history of the upper evaluations
  let toreturn = f xval yval
      (lhs,rhs) = verbose title
  pure (toreturn, Node (fst (rootLabel xpl) ++ fst (rootLabel ypl)
                         , [[i|#{toreturn}: #{lhs}|]])
                    [xpl, mkNod rhs, ypl] )

-- | Evaluate predicates

evalP,evalP' :: Pred Double -> ExplainableIO r MyState Bool
evalP pred = do
  (x, result) <- evalP' pred
  let lbl = getPredLabel pred
  unless (null lbl) $
    modify \mystate -> mystate { symtabP = Map.insert (fromMaybe "" lbl) (PredVal lbl x) (symtabP mystate) }
  pure (x, result)

evalP' (PredVal lbl x) = do
  pure (x, Node ([], [[i|#{x}: a leaf value#{showlbl lbl}|]]) [])

evalP' (PredNot _lbl x) = do
  (xval,xpl) <- retitle "not" $ evalP x
  let toreturn = not xval
  pure (toreturn, Node ([] , [[i|#{toreturn}: logical not of|]]) [xpl])

evalP' (PredBin _lbl binop x y) = do
  (xval, xpl) <- evalP x
  (yval, ypl) <- evalP y
  let toreturn = case binop of
                   PredAnd -> xval && yval
                   PredOr  -> xval || yval
                   PredEq  -> xval == yval
                   PredNeq -> xval /= yval
  pure (toreturn, Node ([] , [[i|#{toreturn}: logical #{binop}|]]) [xpl, ypl])

evalP' (PredComp lbl c x y) =
  let title :: String = [i|comparison#{showlbl lbl}|]
  in retitle [i|#{title} #{shw c}|] do
    (xval, xpl) <- eval x
    (yval, ypl) <- eval y
    let c' = compare xval yval
        toreturn = case c of
          CEQ  | c' == EQ           -> True
          CGT  | c' == GT           -> True
          CLT  | c' == LT           -> True
          CGTE | c' `elem` [GT, EQ] -> True
          CLTE | c' `elem` [LT, EQ] -> True
          CNEQ | c' /= EQ           -> True
          _                         -> False
        (lhs,rhs) = verbose title
    pure (toreturn, Node ([]
                            ,[[i|#{toreturn} #{lhs} (#{shw c})|]])
                       [ xpl
                       , mkNod rhs
                       , ypl ])

evalP' (PredFold lbl andor ps) =
  let title :: String = [i|listfold#{showlbl lbl}|]
  in retitle [i|#{title} #{andor}|] do
    evalps <- evalP `traverse` ps
    let toreturn = case andor of
                     PLAnd -> all fst evalps
                     PLOr  -> any fst evalps
    pure (toreturn, Node ([]
                           , [[i|#{toreturn} #{andor}|]])
                      (snd <$> evalps ) )

evalP' (PredVar str) =
  let title :: String = [i|variable expansion: #{str}|]
      (lhs,_rhs) = verbose title
  in retitle [i|#{title} #{str}|] do
    (xvar, xpl1) <- getvarP str
    (xval, xpl2) <- evalP xvar
    pure (xval, Node ([], [[i|#{xval}: #{lhs} #{str}|]]) [xpl1, xpl2])

evalP' (PredSet str x) =
  let title :: String = [i|variable assignment: #{str}|]
  in retitle [i|#{title} #{str} := #{fromMaybe (show x) (getPredLabel x)}|] do
    symtab <- gets symtabP
    let newmap = Map.insert str x symtab
    modify \ms -> ms { symtabP = newmap }
    (xval,xpl) <- evalP x
    pure (xval, Node ([], [[i|#{xval}:  saved to #{str}|]]) [xpl])

evalP' (PredITE _lbl p x y) = evalFP evalP p x y

-- | Evaluate If-Then-Else by first evaluating the conditional, and then evaluating the chosen branch.
-- This works for both boolean Predicates and float Exprs.
evalFP :: Show t
       => (t -> ExplainableIO r MyState a)
       -> Pred Double
       -> t
       -> t
       -> ExplainableIO r MyState a
evalFP evf p x y = retitle "if-then-else" do
  (pval,pxpl) <- evalP p
  if pval
    then do
      (xval,xxpl) <- evf x
      pure (xval, Node ([],[[i|if #{p} then #{show x} else #{show y}|]]) [pxpl, mkNod "thus we choose the then branch", xxpl])
    else do
      (yval,yxpl) <- evf y
      pure (yval, Node ([],["if-then-else false"] ) [pxpl, mkNod "thus we choose the else branch", yxpl])

-- | Evaluate an `ExprList`

evalList :: ExprList Double -> ExplainableIO r MyState (ExprList Double)
evalList (MathList lbl a) = pure (MathList lbl a, Node (show <$> a,[[i|base MathList with #{length a} elements|]]) [])
evalList (ListFilt lbl1 x comp (MathList lbl2 ys)) = do
  origs <- eval `traverse` ys
  -- [TODO] exclude Undefined values from origs
  round1 <- (evalP . PredComp lbl1 comp x) `traverse` ys
  let round2 = [ if not r1
                 then (Nothing, Node ([show xval]
                                     , [[i|excluded #{xval} due to failing comparison test|]]) [xpl])
                 else (Just xval, Node ([show xval]
                                     , [[i|included #{xval} due to passing comparison test|]]) [xpl])
               | ((r1,xpl), xval) <- zip round1 ys
               ]
      round3 = mapMaybe fst round2
  pure ( MathList (lbl1 <++> lbl2) round3
         , Node ([]
                , [i|#{length round3} elements were reduced from an original #{length round1}|]
                  : [[i|- #{fst o}|] | o <- origs])
           $ fmap snd round2)
evalList (ListFilt lbl x comp lf2) = do
  (lf2val, lf2xpl) <- evalList lf2
  (lf3val, lf3xpl) <- evalList $ ListFilt lbl x comp lf2val
  pure (lf3val, Node ([],["recursing RHS ListFilt"]) [lf2xpl, mkNod "becomes", lf3xpl])

evalList (ListMap lbl Id ylist) = pure (ylist, mkNod ("id on ExprList" ++ showlbl lbl))

evalList (ListMap _lbl1 (MathSection binop x) ylist) = retitle "fmap mathsection" do
  (MathList lbl2 ylist', yxpl) <- evalList ylist
  pure ( MathList lbl2 [ MathBin Nothing binop x y | y <- ylist' ]
         , Node ([],[[i|fmap mathsection #{binop}#{x} over #{length ylist'} elements|]]) [yxpl] )

evalList (ListMapIf lbl Id _c _comp ylist) =
  retitle [i|fmap mathsection id#{showlbl lbl}|] $ evalList ylist

evalList (ListMapIf lbl1 (MathSection binop x) c comp ylist) =
  retitle ("fmap mathsection if" ++ showlbl lbl1) do
    (MathList lbl2 ylist', yxpl) <- evalList ylist
    liveElements <- (evalP . PredComp (lbl1 <++> lbl2) comp c) `traverse` ylist'

    pure ( MathList (Just "evaled list") [ if b then MathBin (Just "boolean true") binop x y else y
                                          | (y,b) <- zip ylist' (fst <$> liveElements) ]
          , Node ([],[[i|fmap mathsection #{binop}#{x} over #{length $ filter id (fst <$> liveElements)} relevant elements (who pass #{c} #{comp})|]])
            [ yxpl , Node ([],["selection of relevant elements"]) (snd <$> liveElements) ] )

evalList (ListConcat lbl xxs) = do
  mapped <- evalList `traverse` xxs
  pure ( MathList lbl (mconcat [ f | (MathList _lbl2 f,_) <- mapped ] )
         , Node ([], [[i|concatted #{length mapped} elements|]])
           (snd <$> mapped))

evalList (ListITE _lbl p y z) = evalFP evalList p y z

-- | deepEvalList means we reduce the `ExprList` to a plain haskell list of floats.
-- I supposed this is analogous to "unboxed" types.

deepEvalList :: (ExprList Double,XP) -> ExplainableIO r MyState [Double]
deepEvalList (MathList _lbl xs,xp) = do
  vals <- eval `traverse` xs
  pure (fst <$> vals, Node ([],["deep evaluation to floats"]) (xp : (snd <$> vals)))
deepEvalList (other,_xp) = deepEvalList =<< evalList other


-- * Variable retrieval and assignment into the symbol table.
-- At present we have no notion of scope.

-- | Get an @Expr Double@ variable

getvarF :: String -> ExplainableIO r MyState (Expr Double)
getvarF x = do
  symtab <- gets symtabF
  case symtab Map.!? x of
    Just v ->
      pure (v, Node ([show v], [[i|variable `#{x}` has value #{v}|]]) [])
    _ -> liftIO do
      putStrLn [__i|
        getvarF: unable to find variable `#{x}` in symbol table
        MathLang fatal error in getvarF
      |]
      pure (Val Nothing 0, emptyXP)

-- | Get a @Pred Double@ variable

getvarP :: String -> ExplainableIO r MyState (Pred Double)
getvarP x = do
  symtab <- gets symtabP
  case symtab Map.!? x of
    Just v -> pure (v, Node ([show v], [[i|looked up #{x}|]]) [])
    _ -> pure (PredVar "", emptyXP)

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
verbose x                = (x, [i|{x} argument|])

(@|+),(@|-),(@|*),(@|/),(@|%) :: String -> Expr Double -> Expr Double -> Expr Double
(@|+) lbl = MathBin (Just lbl) Plus
(@|-) lbl = MathBin (Just lbl) Minus
(@|*) lbl = MathBin (Just lbl) Times
(@|/) lbl = MathBin (Just lbl) Divide
(@|%) lbl = MathBin (Just lbl) Modulo

class Exprlbl expr a where
  (@|=) :: String -> expr a -> expr a
  getvar, (@|$<) :: String -> expr a
  (@|$>) :: String -> expr a -> expr a
  getvar = (@|$<)

instance Exprlbl Expr a where
  (@|=) lbl ( Undefined Nothing      ) = Undefined (Just lbl)
  (@|=) lbl ( Val      Nothing x     ) = Val      (Just lbl) x
  (@|=) lbl ( Parens   Nothing x     ) = Parens   (Just lbl) x
  (@|=) lbl ( MathBin  Nothing x y z ) = MathBin  (Just lbl) x y z
  (@|=) _   ( MathVar          _     ) = error "use @|$< to label a variable reference"
  (@|=) _   ( MathSet          _ _   ) = error "use @|$> to label a variable assignment"
  (@|=) lbl ( MathITE  Nothing x y z ) = MathITE  (Just lbl) x y z
  (@|=) lbl ( MathMax  Nothing x y   ) = MathMax  (Just lbl) x y
  (@|=) lbl ( MathMin  Nothing x y   ) = MathMin  (Just lbl) x y
  (@|=) lbl ( ListFold Nothing x y   ) = ListFold (Just lbl) x y
  (@|=) lbl ( Undefined (Just _old)       ) = Undefined (Just lbl {- <++> Just ("previously " ++ old) -} )
  (@|=) lbl ( Val       (Just _old) x     ) = Val       (Just lbl {- <++> Just ("previously " ++ old) -} ) x
  (@|=) lbl ( Parens    (Just _old) x     ) = Parens    (Just lbl {- <++> Just ("previously " ++ old) -} ) x
  (@|=) lbl ( MathBin   (Just _old) x y z ) = MathBin   (Just lbl {- <++> Just ("previously " ++ old) -} ) x y z
  (@|=) lbl ( MathITE   (Just _old) x y z ) = MathITE   (Just lbl {- <++> Just ("previously " ++ old) -} ) x y z
  (@|=) lbl ( MathMax   (Just _old) x y   ) = MathMax   (Just lbl {- <++> Just ("previously " ++ old) -} ) x y
  (@|=) lbl ( MathMin   (Just _old) x y   ) = MathMin   (Just lbl {- <++> Just ("previously " ++ old) -} ) x y
  (@|=) lbl ( ListFold  (Just _old) x y   ) = ListFold  (Just lbl {- <++> Just ("previously " ++ old) -} ) x y

  (@|$<) :: String -> Expr a
  (@|$<) = MathVar

  (@|$>) :: String -> Expr a -> Expr a
  (@|$>) = MathSet

(@|.) :: String -> a -> Expr a
(@|.) = Val . Just
infix 6 @|., @|.., @|??

undef,(@|??) :: String -> Expr Double
(@|??) = undef
undef = Undefined . Just

infixl 1 @|=
infixl 4 @|+, @|-
infixl 5 @|*, @|/

-- | syntactic sugar for ternary syntax. The trick is to join up the branches into a single thing
newtype TernaryRHS a = TRHS (a, a)
  deriving (Eq, Show)

(@|:) :: expr -> expr -> TernaryRHS expr
(@|:) = curry coerce

class ExprTernary expr a where
  (@|?) :: Pred a -> TernaryRHS (expr a) -> expr a

infixr 2 @|?
infixr 3 @|:

instance ExprTernary Expr a where
  (@|?) pred (coerce -> (tbranch, fbranch)) =
    MathITE Nothing pred tbranch fbranch

instance ExprTernary ExprList a where
  (@|?) pred (coerce -> (tbranch, fbranch)) =
    ListITE Nothing pred tbranch fbranch

instance ExprTernary Pred a where
  (@|?) pred (coerce -> (tbranch, fbranch)) =
    PredITE Nothing pred tbranch fbranch

-- | syntactic sugar for the predicate expressions
instance Exprlbl Pred a where
  (@|=) lbl ( PredVal  Nothing    x )     = PredVal  (Just lbl) x
  (@|=) lbl ( PredNot  Nothing    x )     = PredNot  (Just lbl) x
  (@|=) lbl ( PredBin  Nothing    x y z ) = PredBin  (Just lbl) x y z
  (@|=) lbl ( PredComp Nothing    x y z ) = PredComp (Just lbl) x y z
  (@|=) lbl ( PredFold Nothing    x y   ) = PredFold (Just lbl) x y
  (@|=) lbl ( PredITE  Nothing    x y z ) = PredITE  (Just lbl) x y z
  (@|=) _   ( PredVar             _ )     = error "use @|$< to label a variable reference"
  (@|=) _   ( PredSet             _ _ )   = error "use @|$> to label a variable assignment"
  (@|=) lbl ( PredVal  (Just old) x )     = PredVal  (Just lbl <++> Just ("previously " ++ old)) x
  (@|=) lbl ( PredNot  (Just old) x )     = PredNot  (Just lbl <++> Just ("previously " ++ old)) x
  (@|=) lbl ( PredBin  (Just old) x y z ) = PredBin  (Just lbl <++> Just ("previously " ++ old)) x y z
  (@|=) lbl ( PredComp (Just old) x y z ) = PredComp (Just lbl <++> Just ("previously " ++ old)) x y z
  (@|=) lbl ( PredFold (Just old) x y   ) = PredFold (Just lbl <++> Just ("previously " ++ old)) x y
  (@|=) lbl ( PredITE  (Just old) x y z ) = PredITE  (Just lbl <++> Just ("previously " ++ old)) x y z

  (@|$<) :: String -> Pred a
  (@|$<) = PredVar

  (@|$>) :: String -> Pred a -> Pred a
  (@|$>) = PredSet

(@|..) :: String -> Bool -> Pred a
(@|..) = PredVal . Just

(|&&),(|||),(|==),(|/=),(|!=) :: Pred a -> Pred a -> Pred a
(|&&) = PredBin Nothing PredAnd
(|||) = PredBin Nothing PredOr
(|==) = PredBin Nothing PredEq
(|!=) = PredBin Nothing PredNeq
(|/=) = PredBin Nothing PredNeq

infix 2 |||, @|||
infix 3 |&&, @|&&
infix 4 |==, |/=, |!=, @|==, @|!=, @|/=

(|===),(|!==),(|/==) :: Expr a -> Expr a -> Pred a
(|===) = PredComp Nothing CEQ
(|/==) = PredComp Nothing CNEQ
(|!==) = PredComp Nothing CNEQ

(@|<),(@|>),(@|<=),(@|>=),(@|===),(@|!==),(@|/==) :: String -> Expr a -> Expr a -> Pred a
(@|<)  s = PredComp (Just s) CLT
(@|<=) s = PredComp (Just s) CLTE
(@|>)  s = PredComp (Just s) CGT
(@|>=) s = PredComp (Just s) CGTE
(@|===) s = PredComp (Just s) CEQ
(@|/==) s = PredComp (Just s) CNEQ
(@|!==) s = PredComp (Just s) CNEQ

(@|&&),(@|||),(@|==),(@|!=),(@|/=) :: String -> Pred a -> Pred a -> Pred a
(@|&&) s = PredBin (Just s) PredAnd
(@|||) s =  PredBin (Just s) PredOr
(@|==) s = PredBin (Just s) PredEq
(@|!=) s = PredBin (Just s) PredNeq
(@|/=) s = PredBin (Just s) PredNeq

(@|!) :: String -> Pred a -> Pred a
(@|!) s = PredNot (Just s)

data PredBinOp = PredAnd | PredOr | PredEq | PredNeq
  deriving (Eq, Show)

-- | some example runs
toplevel :: IO ()
toplevel = for_ [ Val (Just "two") 2 |+ (Val (Just "five") 5 |- Val (Just "one") 1)
                 , "my sum" @|= "two" @|. 2 |+ "three" @|. 3
                 , ListFold (Just "greater than 2") FoldSum $ Val (Just "two") 2 <| MathList (Just "one to four")
                   [Val Nothing 1, Val Nothing 2, Val Nothing 3, Val Nothing 4]
                 , ListFold (Just "positive 1") FoldSum $ Val (Just "zero") 0 <| ml23
                 , ListFold (Just "positive 2") FoldSum $ Val (Just "zero") 0 <| ml23
                 ] \topexpr -> do
  (_val, _xpl, _stab, _wlog) <- xplainF () emptyState topexpr
  pure ()
  where ml23 = MathList (Just "minus two to three")
               [Val Nothing (-2), Val Nothing (-1), Val Nothing 0, Val Nothing 1, Val Nothing 2, Val Nothing 3]
-- * Explainers

-- | Explain an @Expr Double@
xplainF :: r -> MyState -> Expr Double -> IO (Double, XP, MyState, [String])
xplainF r s expr = do
  ((val,xpl), stab, wlog) <- runRWST
                             (eval expr)
                             (([],["toplevel"]),r)         -- reader: HistoryPath, actualReader
                             s

  putStrLn [__i|
    \#+begin_src haskell
    #{expr}
    \#+end_src
    - val :: #{val}
    - log :: #{wlog}
    - xpl :: #{val}
    #{drawTreeOrg 3 xpl}
  |]

  pure (val, xpl, stab, wlog)

-- | Explain an @ExprList Double@
xplainL :: r -> ExprList Double -> IO ([Double], XP, MyState, [String])
xplainL r exprList = do
  ((xl,xp), stab, wlog) <- runRWST
                           (deepEvalList (exprList,mkNod "deep eval"))
                           (([],["toplevel"]),r)         -- reader: HistoryPath, actualReader
                           emptyState -- state: MyState
  putStrLn [__i|
    \#+begin_src haskell
    #{xl}
    \#+end_src
    - val :: #{xl}
    - log :: #{wlog}
    - xpl :: #{xl}
    #{drawTreeOrg 3 xp}
  |]

  pure (xl, xp, stab, wlog) -- [TODO] note the explanation result from xs is discarded

unMathList :: Show a => ExprList a -> [Expr a]
unMathList (MathList _lbl xs) = xs
unMathList x                  =
  trace [i|unMathList: expected exprList to be fully evaluated, but got #{x}|] []

-- | dump an explanation of a mathlang expression
dumpExplanationF :: Int -> MyState -> Expr Double -> IO ()
dumpExplanationF depth s f = do
  (val, xpl, stab, wlog) <- xplainF () s f

  let stars = replicate depth '*'
  putStrLn [__i|
    #{stars} val
    #{val}
    #{stars} xpl
    #{xpl}
    #{stars} starting state
    #{s}
    #{stars} ending SymTab
    #{stab}
    #{stars} wlog
    #{wlog}
    #{stars} typescript
    \#+BEGIN_SRC typescript :tangle from-hs.ts
    #{dumpTypescript "" s f}
    \#+END_SRC
  |]

dumpTypescript :: Doc ann0 -> MyState -> Expr Double -> Doc ann1
dumpTypescript realign s f =
  [__di|
    // this is machine generated from explainable/src/Explainable/MathLang.hs and also ToMathlang.hs

    import * as tsm from './mathlang';
    export { exprReduce, asDot } from './mathlang';

    export function myshow(expr: tsm.Expr<any>) : tsm.Expr<any> {
      console.log("** " + Math.round(expr.val))
      tsm.explTrace(expr, 3)

      console.log("** JSON of symTab")
      console.log("\#+NAME symtab")
      console.log("\#+BEGIN_SRC json")
      console.log(JSON.stringify(tsm.symTab,null,2))
      console.log("\#+END_SRC")
      return expr
    }
    #{ppst s realign}
    export const maxClaim = () => {
      return #{pp f}
    }
  |]

-- * Prettty-printing to the Typescript version of the MathLang library
class ToTS expr a where
  pp :: (Show a) => expr a -> Doc ann


-- in future consider a new class tsm.Undefined -- would that more faithfully follow this representation?
instance ToTS Expr a where
  pp (Undefined lbl  )        = "new tsm.Num0"    <+> h0tupled [dquotes $ maybe ("undefined") pretty lbl, "undefined"]
  pp (Val      lbl x )        = "new tsm.Num0"    <+> h0tupled [dquotes $ maybe (viaShow x) pretty lbl, viaShow x]

  pp (Parens   _lbl x       ) = parens (pp x) -- discard the label, but [TODO] call SetVar to save it. TBH i don't think this ever actually gets used.
  pp (MathBin  lbl mbop x y ) = "new tsm.Num2"    <+> h0tupled [ dquotes $ maybe ("binop " <> viaShow mbop) pretty lbl
                                                                      , "tsm.NumBinOp." <> case mbop of { Plus -> "Add"; Minus -> "Sub"; Times -> "Mul"; Divide -> "Div"; Modulo -> "Mod" }
                                                                      , pp x , pp y ]
  pp (MathVar  str          ) = "new tsm.GetVar"  <+> h0parens ( dqpretty str)
  pp (MathSet  str x        ) = "new tsm.SetVar"  <+> h0tupled [ dqpretty str, parens (pp x) <> ".val" ]
  pp (MathITE  lbl p x y    ) = "new tsm.Bool3"   <+> h0tupled [ dquotes $ maybe "if-then-else" pretty lbl , "tsm.BoolTriOp.IfThenElse" , pp p, pp x, pp y ]
  pp (MathMax  lbl   x y    ) = "new tsm.Num2"    <+> h0tupled [ dquotes $ maybe "greater of"   pretty lbl , "tsm.NumBinOp.MaxOf2"      , pp x, pp y ]
  pp (MathMin  lbl   x y    ) = "new tsm.Num2"    <+> h0tupled [ dquotes $ maybe "lesser of"    pretty lbl , "tsm.NumBinOp.MinOf2"      , pp x, pp y ]
  pp (ListFold lbl f (MathList mlbl xs) ) = "new tsm.NumFold" <+> h0tupled [ dquotes $ maybe "list fold" pretty (lbl <++> mlbl)
                                                                           , "tsm.NumFoldOp." <> case f of { FoldSum -> "Sum"; FoldProduct -> "Product"; FoldMax -> "Max"; FoldMin -> "Min" }
                                                                           , hang 2 $ list (pp <$> xs) ]

  pp x = trace [i|MathLang:ToTS pp unimplemented; #{x}|] ""

instance ToTS Pred a where
  pp (PredVal  lbl a    ) = "new tsm.Bool0"      <+> h0tupled [ dquotes $ maybe (viaShow a) pretty lbl, tf a ]
  pp (PredNot  lbl x    ) = "new tsm.Bool1"      <+> h0tupled [ dquotes $ maybe (viaShow x) pretty lbl, "tsm.BoolUnaOp.BoolNot", pp x ]
  pp (PredComp lbl c x y) = "new tsm.NumToBool2" <+> h0tupled [ dquotes $ maybe (viaShow c) pretty lbl
                                                                     , "tsm.NumToBoolOp." <> case c of { CEQ -> "NBeq"; CNEQ -> "NBneq"; CLT -> "NBlt"; CLTE -> "NBlte"; CGT -> "NBgt"; CGTE -> "NBgte" }
                                                                     , pp x, pp y ]
  pp (PredBin  lbl o x y) = "new tsm.Bool2" <+> h0tupled [ dquotes $ maybe (viaShow o) pretty lbl
                                                                , "tsm.BoolBinOp." <> case o of { PredAnd -> "And"; PredOr -> "Or"; PredEq -> "BoolEq"; PredNeq -> "BoolNeq" }
                                                                , pp x, pp y ]
  pp (PredVar  str      ) = "new tsm.GetVar" <+> h0parens ( dqpretty str )
  pp (PredSet  str x    ) = "new tsm.SetVar" <+> h0tupled [ dqpretty str, parens (pp x) <> ".val" ]
  pp (PredITE  lbl p x y) = "new tsm.Bool3"  <+> h0tupled [ dquotes $ maybe "if-then-else" pretty lbl , "tsm.BoolTriOp.IfThenElse" , pp p, pp x, pp y ]
  pp (PredFold lbl p xs)  = "new tsm.BoolFold" <+> h0tupled [ dquotes $ maybe "any/all" pretty lbl
                                                            , "tsm.BoolFoldOp." <> case p of { PLAnd -> "All"; PLOr -> "Any" }
                                                            , hang 2 $ list ( pp <$> xs ) ]



ppst :: MyState -> Doc ann -> Doc ann
ppst (MyState{..}) realign =
  [__di|
    function realign (form_data : any) {
      var toreturn = {...form_data}
  |] <>  realign <> line <> "  return toreturn;\n}" <> line <>
   "export function setup (symtab : any) {" <> line <> indent 2 (
  "const realigned = realign(symtab);" <> line <>
  "tsm.initSymTab" <> hang 1
  ( encloseSep lparen rparen comma
    [ "{ ..." <> encloseSep lbrace rbrace comma (
      [ dquotes keyString <> colon <+> valString
      | (k,Val _lbl v) <- Map.toList symtabF
      , let keyString = pretty k
            valString = pretty v
      ]
      <>
      [ dquotes keyString <> colon <+> valString
      | (k,PredVal _lbl v) <- Map.toList symtabP
      , let keyString = pretty k
            valString = if v then "true" else "false"
      ]
      <>
      [ dquotes keyString <> colon <+> dquotes valString
      | (k,v) <- Map.toList symtabS
      , let keyString = pretty k
            valString = pretty v
      ] )
    , "...realigned }"
    ]
  ) ) <> line <> "}" <> line


dqpretty :: (Pretty a) => a -> Doc ann
dqpretty = dquotes . pretty

tf :: Bool -> Doc ann
tf True = "true"
tf False = "false"

h0parens :: Doc ann -> Doc ann
h0parens = hang 0 . parens
h0tupled :: [Doc ann] -> Doc ann
h0tupled = hang 0 . tupled


-- * data flow extraction
-- we don't care about the value returned, just the state graph traversed
-- [TODO] this allows us to return a full default emptyState symtab that tells the expert system
-- waht to ask the end user for.

allVars :: Expr Double -> ExplainableIO r MyState (Expr Double)
allVars inexpr = pure (inexpr, emptyXP)
