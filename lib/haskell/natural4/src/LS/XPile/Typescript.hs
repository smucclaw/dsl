{-# LANGUAGE OverloadedStrings #-}

{-| transpiler to Typescript -}

module LS.XPile.Typescript where

-- the job of this module is to output valid typescript class definitions, decision functions, and variable instances.

-- consider: https://hackage.haskell.org/package/aeson-typescript
-- not suitable because L4 allows users to define their own types which are not known to Haskell's type system.

-- import Debug.Trace

-- import AnyAll

-- import L4.Syntax as CoreL4

-- import L4.Annotation

-- import Data.Functor ( (<&>) )

import Data.HashMap.Strict qualified as Map
-- import qualified Data.Text as T
-- import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.List.NonEmpty as NE
import Data.List (intercalate, nub)
import Data.Maybe (isJust, isNothing)
import Data.Tree qualified as DT
import LS.Interpreter
  ( attrType,
    getCTkeys,
    globalFacts,
    l4interpret,
    topsortedClasses,
    extractEnums,
  )
import LS.PrettyPrinter
  ( ParamText4 (PT4, PT5),
    prettyMaybeType,
    prettySimpleType,
    snake_case,
    snake_inner,
    vvsep,
    (<//>),
    (</>),
  )
import LS.Rule as SFL4R
  ( Interpreted (classtable, scopetable),
    Rule(..),
    ruleLabelName
  )
import LS.Types as SFL4
  ( ClsTab (CT),
    HornClause (HC, hHead),
    HornClause2,
    MTExpr (MTT),
    ParamText,
    ParamType (TOne, TOptional),
    RelationalPredicate
      ( RPBoolStructR,
        RPConstraint,
        RPMT,
        RPParamText,
        RPnary
      ),
    TypeSig (InlineEnum, SimpleType),
    clsParent,
    defaultInterpreterOptions,
    getSymType,
    mt2text,
  )
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    colon,
    comma,
    dquotes,
    emptyDoc,
    encloseSep,
    equals,
    hang,
    indent,
    lbrace,
    line,
    list,
    nest,
    rbrace,
    semi,
    space,
    viaShow,
    vsep,
    (<+>),
  )
import LS.XPile.Logging

-- JsonLogic
import JsonLogic.Json
import JsonLogic.Pure.Type
import JsonLogic.Pure.Operation

asTypescript :: SFL4R.Interpreted -> XPileLog (Doc ann)
asTypescript l4i = do
  vvsep <$> sequence [ tsPrelude l4i
                     , tsEnums l4i
                     , tsClasses l4i
                     , globalDefinitions l4i
                     , toJsonLogic l4i
                     ]

-- | a convention for preserving the original strings and data structures for downstream use
tsPrelude :: Interpreted -> XPileLog (Doc ann)
tsPrelude l4i = return $
  vsep [ "// tsPrelude"
       , "export const L4Orig = { enums: { }, classes: { } }"
       ]

-- | output class declarations
tsClasses :: Interpreted -> XPileLog (Doc ann)
tsClasses l4i = return $
  let ct@(CT ch) = classtable l4i
  in
  vvsep [ "class" <+> snake_case [MTT className] <>
          case clsParent ct className of
           Nothing       -> mempty
           (Just parent) -> " extends" <+> pretty parent
          -- attributes of the class are shown as decls
          <+> lbrace
          -- <//> "  //" <+> viaShow csuper
          <//> indent 2 ( vsep [ snake_case [MTT attrname] <>
                                 case attrType children attrname of
                                   Just t@(SimpleType TOptional _) -> " ?:" <+> prettySimpleType "ts" (snake_inner . MTT) t
                                   Just t@(SimpleType TOne      _) -> " :"  <+> prettySimpleType "ts" (snake_inner . MTT) t
                                   Just t@(InlineEnum TOne      _) -> " :"  <+> snake_case [MTT attrname] <> "Enum"
                                   Just t                          -> " : " <+> prettySimpleType "ts" (snake_inner . MTT) t
                                   Nothing -> ""
                                 <> semi
                               | attrname <- getCTkeys children
                               -- [TODO] finish out the attribute definition -- particularly tricky if it's a DECIDE
                               ] )
          <//> rbrace
        | className <- reverse $ topsortedClasses ct
        , (Just (csuper, children)) <- [Map.lookup className ch]
        , case csuper of
            (Just (SimpleType _ _), _) -> True
            (Just (InlineEnum _ _), _) -> False -- we deal with enums separately below
            _                          -> True
        ]

-- | output enum declarations
tsEnums :: Interpreted -> XPileLog (Doc ann)
tsEnums l4i = return $
  vvsep ( showEnum <$> extractEnums l4i )
  where
    showEnum r@TypeDecl{super=Just (InlineEnum TOne enumNEList)} =
      let className = ruleLabelName r
      in 
        "enum" <+> snake_case className <+> lbrace
        <//> indent 2 ( vsep [ snake_case [enumStr] <> comma
                             | (enumMultiTerm, _) <- NE.toList enumNEList
                             , enumStr <- NE.toList enumMultiTerm
                             ] )
        <//> rbrace

          -- but we also want to preserve the original strings for downstream use
          -- so we use the L4Orig convention
        <//> "L4Orig.enums['" <> snake_case className <> "']" <+> equals <+> lbrace
        <//> indent 2 ( vsep [ snake_case [enumStr] <> colon <+> dquotes (pretty (mt2text [enumStr])) <> comma
                             | (enumMultiTerm, _) <- NE.toList enumNEList
                             , enumStr <- NE.toList enumMultiTerm
                             ] )
        <//> rbrace

  


-- | all the symbols we know about, including those that show up in GIVEN, DEFINE, and DECIDE.
-- This is probably not the best way to do it because we're commingling local and global variables here.
-- Happy to rethink all of this.
-- Also, not yet done, classes and variables need to be topologically sorted because typescript is picky about that.
jsInstances :: Interpreted -> XPileLog (Doc ann)
jsInstances l4i = return $
  let sctabs = scopetable l4i
  in
  vvsep [ "//" <+> "scope" <+> scopenameStr scopename <//>
          "//" <+> viaShow symtab' <//>
          vvsep [ "const" <+> snake_case mt <+> prettyMaybeType "ts" (snake_inner . MTT) (getSymType symtype) <+> equals <+> nest 2 value
                | (mt, (symtype, vals)) <- Map.toList symtab'
                , value <- case vals of
                              -- what we should do is gather all the paramtexts and join them in a single dictionary,
                              -- rather than assume that all the HC2 are paramtexts.
                              HC { hHead = RPParamText {} } : _ -> [ encloseSep lbrace (line <> rbrace) comma ((line <>) <$> asValuePT l4i vals) ]
                              _                                 -> hc2ts l4i <$> vals
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

-- | top-level DEFINEs, going rule by rule
-- just the facts.
globalDefinitions :: Interpreted -> XPileLog (Doc ann)
globalDefinitions l4i = return $
  vvsep [ "const" <+> dumpNestedClass l4i f
        | f <- globalFacts l4i
        ]

-- | What's going on with nested classes?
-- See the discussion in localvars.org


dumpNestedClass :: Interpreted -> DT.Tree ParamText -> Doc ann
dumpNestedClass l4i (DT.Node pt children)
  | not $ null children =
    -- "// pt:"       <+> viaShow pt <//>
    -- "// children:" <+> viaShow children <//>
    pretty (PT5 pt l4i) <> nest 2 (encloseSep (line <> lbrace <> space) (line <> rbrace) (comma <> space) ( dumpNestedClass l4i <$> children ))
  | otherwise =
    -- "// pt:"       <+> viaShow pt <//>
    pretty (PT4 pt l4i)


-- | convert the decision logic into functions that do the right thing.
--
-- There are several sources of decision logic. In the L4 source, we have a ruleset of multiple rules, some of which are DECIDE rules.
--
-- The Interpreter contains a bunch of gnarly code that tries to do expansion of one boolstruct into another.
--
-- Going beyond the propositional domain, however, it becomes difficult to force everything to be inlined. This is why named variables and let bindings were invented!
--
-- Now, let us assume we have a mathlang-like DSL of some sort that is implemented in something like Typescript or Purescript, which allows traced evaluation.
--
-- Our task now is to organize the rulesets into a graph of computations and data flow; then we compile that graph down to the evaluation DSL.
--
-- We have a couple candidates for the mathlang DSL:
-- - https://jsonlogic.com/operations.html
-- - our own mathlang
-- - raw Typescript
--
-- Let's try the JsonLogic approach first.
--
-- We'll bypass the expansion provided by the Interpreter, and work with the raw rules themselves.
--
-- We might rely on the Interpreter to provide some of the data flow graphing, so we can trace variable expansion.
--
-- In this initial experiment, we do not implement scoped variables. All variables live in global scope.
--

hc2ts :: Interpreted -> HornClause2 -> Doc ann
hc2ts _l4i  hc2@HC { hHead = RPMT        _ }                 = "value" <+> colon <+> dquotes (pretty (hHead hc2))
hc2ts _l4i _hc2@HC { hHead = RPConstraint  mt1 _rprel mt2 }  = snake_case mt1 <+> colon <+> dquotes (snake_case mt2)
hc2ts _l4i _hc2@HC { hHead = RPBoolStructR mt1 _rprel _bsr } = snake_case mt1 <+> colon <+> "(some => lambda)"
hc2ts  l4i _hc2@HC { hHead = RPParamText pt }                = pretty (PT4 pt l4i)
hc2ts  l4i  hc2@HC { hHead = RPnary      _rprel [] }         = error "TypeScript: headless RPnary encountered"
hc2ts  l4i  hc2@HC { hHead = RPnary      _rprel rps }        = hc2ts l4i hc2 {hHead = head rps}


toJsonLogic :: Interpreted -> XPileLog (Doc ann)
toJsonLogic l4i = do
  mutterd 1 "toJsonLogic"
  return emptyDoc

-- dump only the paramtexts
asValuePT :: Interpreted -> [HornClause2] -> [Doc ann]
asValuePT l4i hc2s = -- trace ("asValuePT: " <> show hc2s) $
  [ pretty (PT4 pt l4i)
  | HC { hHead = RPParamText pt } <- hc2s ]
                 
