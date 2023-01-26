{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

{-| transpiler to Typescript -}

module LS.XPile.Typescript where

-- the job of this module is to output valid typescript class definitions, decision functions, and variable instances.

-- consider: https://hackage.haskell.org/package/aeson-typescript
-- not suitable because L4 allows users to define their own types which are not known to Haskell's type system.

-- import Debug.Trace
import Prettyprinter

-- import AnyAll
import LS.PrettyPrinter 
-- import L4.Syntax as CoreL4

import LS.Types as SFL4
import LS.Rule as SFL4R
-- import L4.Annotation
import LS.Interpreter

-- import Data.Functor ( (<&>) )

import qualified Data.Map as Map
-- import qualified Data.Text as T
-- import Data.Maybe (catMaybes, fromMaybe, isJust)
-- import qualified Data.List.NonEmpty as NE
import Data.List (nub)


asTypescript :: [SFL4R.Rule] -> Doc ann
asTypescript rs =
  let l4i = l4interpret defaultInterpreterOptions rs
  in
    vvsep [ tsClasses l4i
         , jsInstances l4i
         ]

tsClasses :: Interpreted -> Doc ann
tsClasses l4i =
  let ct@(CT ch) = classtable l4i
  in
  vvsep [ "class" <+> snake_case [className] <>
          case clsParent ct className of
           Nothing       -> mempty
           (Just parent) -> " extends" <+> pretty parent
          -- attributes of the class are shown as decls
          <+> lbrace
          <//> indent 2 ( vsep [ snake_case [attrname] <>
                                 case attrType children attrname of
                                   Just t@(SimpleType TOptional _) -> " ?:" <+> prettySimpleType "ts" snake_inner t
                                   Just t@(SimpleType TOne      _) -> " :"  <+> prettySimpleType "ts" snake_inner t
                                   Just t                          -> " : " <+> prettySimpleType "ts" snake_inner t
                                   Nothing -> ""
                                 <> semi
                               | attrname <- getCTkeys children
                               -- [TODO] finish out the attribute definition -- particularly tricky if it's a DECIDE
                               ] )
          <//> rbrace
        | className <- reverse $ topsortedClasses ct
        , (Just (_ctype, children)) <- [Map.lookup className ch]
        ]

-- classes and variables need to be topologically sorted because typescript is picky about that.

jsInstances :: Interpreted -> Doc ann
jsInstances l4i =
  let sctabs = scopetable l4i
  in
  vvsep [ "//" <+> scopenameStr scopename <+> "scope" <//>
          vvsep [ "const" <+> snake_case mt <+> prettyMaybeType "ts" snake_inner (getSymType symtype) <+> equals <+> nest 2 value
                | (mt, (symtype, vals)) <- Map.toList symtab'
                , value <- case vals of
                              -- what we should do is gather all the paramtexts and join them in a single dictionary,
                              -- rather than assume that all the HC2 are paramtexts.
                              HC { hHead = RPParamText {} } : _ -> [lbrace </> asValuePT l4i vals </>
                                                                     rbrace <> Prettyprinter.line]
                              _                                  -> asValue l4i <$> vals
                 ]
        | (scopename , symtab') <- Map.toList sctabs
        ] </>
  "const GLOBALS" <+> equals <+> hang 0 (
    list ( snake_case <$> nub [ mt
                              | (_scopename , symtab') <- Map.toList sctabs
                              , mt <- Map.keys symtab' ] ) <> semi )
  where
    scopenameStr [] = "globals"
    scopenameStr x  = snake_case x

-- [TODO] convert the GIVEN ... logic into functions that do the right thing.

asValue :: Interpreted -> HornClause2 -> Doc ann
asValue _l4i  hc2@HC { hHead = RPMT        _ }                 = "value" <+> colon <+> dquotes (pretty (hHead hc2))
asValue _l4i _hc2@HC { hHead = RPConstraint  mt1 _rprel mt2 }  = snake_case mt1 <+> colon <+> dquotes (snake_case mt2)
asValue _l4i _hc2@HC { hHead = RPBoolStructR mt1 _rprel _bsr } = snake_case mt1 <+> colon <+> "(some => lambda)"
asValue  l4i _hc2@HC { hHead = RPParamText pt }                = pretty (PT4 pt l4i)
asValue  l4i  hc2@HC { hHead = RPnary      _rprel rp }         = asValue l4i hc2 {hHead = rp}

asValuePT :: Interpreted -> [HornClause2] -> Doc ann
asValuePT l4i hc2s = -- trace ("asValuePT: " <> show hc2s) $
                     vsep [ pretty (PT4 pt l4i) <> comma
                          | HC { hHead = RPParamText pt } <- hc2s ]
                 
