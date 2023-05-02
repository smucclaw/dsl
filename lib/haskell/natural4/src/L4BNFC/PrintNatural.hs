-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for PrintNatural.

module PrintNatural where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsNatural

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsNatural.PIdent where
  prt _ (AbsNatural.PIdent (_,i)) = doc $ showString i
instance Print AbsNatural.PInteger where
  prt _ (AbsNatural.PInteger (_,i)) = doc $ showString i
instance Print AbsNatural.IfPart where
  prt i = \case
    AbsNatural.IfPart bsrany -> prPrec i 0 (concatD [doc (showString "IF"), prt 0 bsrany])

instance Print AbsNatural.BoolStructR where
  prt i = \case
    AbsNatural.BSRAny bsranys boolstructr -> prPrec i 0 (concatD [prt 0 bsranys, doc (showString "OR"), prt 0 boolstructr])
    AbsNatural.BSRLeaf bsrleaf -> prPrec i 2 (concatD [prt 0 bsrleaf])
    AbsNatural.BSRAll bsralls boolstructr -> prPrec i 1 (concatD [prt 0 bsralls, doc (showString "AND"), prt 0 boolstructr])

instance Print AbsNatural.BSRLeaf where
  prt i = \case
    AbsNatural.LeafNoLabel relationalpredicate -> prPrec i 2 (concatD [prt 0 relationalpredicate])
    AbsNatural.LeafPostLabel bsrleaf multiterm -> prPrec i 1 (concatD [prt 0 bsrleaf, doc (showString "POST"), prt 0 multiterm])
    AbsNatural.LeafPreLabel multiterm boolstructr -> prPrec i 0 (concatD [doc (showString "PRE"), prt 0 multiterm, prt 0 boolstructr])

instance Print AbsNatural.BSRAny where
  prt i = \case
    AbsNatural.BSRAnyList boolstructr -> prPrec i 0 (concatD [prt 0 boolstructr])

instance Print [AbsNatural.BSRAny] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "OR"), prt 0 xs]

instance Print AbsNatural.BSRAll where
  prt i = \case
    AbsNatural.BSRAllList boolstructr -> prPrec i 0 (concatD [prt 0 boolstructr])

instance Print [AbsNatural.BSRAll] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "AND"), prt 0 xs]

instance Print AbsNatural.RelationalPredicate where
  prt i = \case
    AbsNatural.RPConstraint multiterm1 multiterm2 -> prPrec i 0 (concatD [prt 0 multiterm1, doc (showString "IS"), prt 0 multiterm2])
    AbsNatural.RPBoolStructR multiterm boolstructr -> prPrec i 0 (concatD [prt 0 multiterm, doc (showString "IS"), prt 0 boolstructr])
    AbsNatural.RPMT multiterm -> prPrec i 0 (concatD [prt 0 multiterm])

instance Print AbsNatural.RPRel where
  prt i = \case
    AbsNatural.RPis -> prPrec i 0 (concatD [doc (showString "IS")])

instance Print AbsNatural.MultiTerm where
  prt i = \case
    AbsNatural.MultiTerm mtexprs -> prPrec i 0 (concatD [prt 0 mtexprs])

instance Print AbsNatural.MTExpr where
  prt i = \case
    AbsNatural.MTExpr mtt -> prPrec i 0 (concatD [prt 0 mtt])

instance Print AbsNatural.MTT where
  prt i = \case
    AbsNatural.MTT pident -> prPrec i 0 (concatD [prt 0 pident])

instance Print AbsNatural.MTN where
  prt i = \case
    AbsNatural.MTN pinteger -> prPrec i 0 (concatD [prt 0 pinteger])

instance Print [AbsNatural.MTExpr] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]
