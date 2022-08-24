{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module LS.XPile.Typescript where

-- | the job of this module is to output valid typescript class definitions, decision functions, and variable instances.

-- consider: https://hackage.haskell.org/package/aeson-typescript
-- not suitable because L4 allows users to define their own types which are not known to Haskell's type system.

import Debug.Trace
import Prettyprinter

import AnyAll
import LS.PrettyPrinter 
import L4.Syntax as CoreL4

import LS.Types as SFL4
import L4.Annotation
import LS.Interpreter

import Data.Functor ( (<&>) )

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.List.NonEmpty as NE


asTypescript :: [SFL4.Rule] -> Doc ann
asTypescript rs =
  let l4i = l4interpret defaultInterpreterOptions rs
  in
    vsep [ tsClasses l4i
         , jsInstances l4i
         ]

tsClasses :: Interpreted -> Doc ann
tsClasses l4i =
  let ct@(CT ch) = classtable l4i
  in
  vsep [ "class" <+> snake_case [className] <>
         case clsParent ct className of
           Nothing       -> mempty
           (Just parent) -> " extends" <+> pretty parent
         -- attributes of the class are shown as decls
         <+> lbrace <> Prettyprinter.line
         <> indent 2 ( vsep [ snake_case [attrname] <>
                              case attrType children attrname of
                                Just t -> " :" <+> prettySimpleType snake_inner t
                                Nothing -> ""
                              <> semi
                          | attrname <- getCTkeys children
                          -- [TODO] finish out the attribute definition -- particularly tricky if it's a DECIDE
                          ] )
         <> Prettyprinter.line <> rbrace <> Prettyprinter.line
       | className <- reverse $ topsortedClasses ct
       , (Just (_ctype, children)) <- [Map.lookup className ch]
       ]

-- classes and variables need to be topologically sorted because typescript is picky about that.

jsInstances :: Interpreted -> Doc ann
jsInstances l4i =
  let sctabs = scopetable l4i
  in
  vsep [ "//" <+> scopenameStr scopename <+> "scope" <> Prettyprinter.line
         <> vsep [ "const" <+> snake_case mt <+> prettyMaybeType snake_inner (getSymType symtype) <+> equals <+> nest 2 value <> Prettyprinter.line
                 | (mt, (symtype, vals)) <- Map.toList symtab'
                 , value <- case vals of
                              -- what we should do is gather all the paramtexts and join them in a single dictionary,
                              -- rather than assume that all the HC2 are paramtexts.
                              HC2 { hHead = RPParamText {} } : _ -> [lbrace <> Prettyprinter.line <> asValuePT l4i vals <> Prettyprinter.line <>
                                                                     rbrace <> Prettyprinter.line]
                              _                                  -> asValue l4i <$> vals
                 ]
         <> Prettyprinter.line
         <> "const GLOBALS" <+> equals <+> list [ snake_case mt | mt <- Map.keys symtab' ] <> semi
         <> Prettyprinter.line
       | (scopename , symtab') <- Map.toList sctabs
       ]
  where
    scopenameStr [] = "globals"
    scopenameStr x  = snake_case x

asValue :: Interpreted -> HornClause2 -> Doc ann
asValue _l4i hc2@(HC2 { hHead = RPMT        _ })               = "value" <+> colon <+> dquotes (pretty (hHead hc2))
asValue _l4i hc2@(HC2 { hHead = RPConstraint  mt1 rprel mt2 }) = snake_case mt1 <+> colon <+> dquotes (snake_case mt2)
asValue _l4i hc2@(HC2 { hHead = RPBoolStructR mt1 rprel bsr }) = snake_case mt1 <+> colon <+> "(some => lambda)"
asValue  l4i hc2@(HC2 { hHead = RPParamText pt })              = trace ("asValue branch 4: " ++ show pt) $
                                                                     pretty (PT4 pt l4i)

asValuePT :: Interpreted -> [HornClause2] -> Doc ann
asValuePT l4i hc2s = trace ("asValuePT: " <> show hc2s) $
                     vsep [ pretty (PT4 pt l4i) <> comma
                          | HC2 { hHead = RPParamText pt } <- hc2s ]
                 
