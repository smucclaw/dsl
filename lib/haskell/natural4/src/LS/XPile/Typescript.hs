{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

{-| transpiler to Typescript -}

module LS.XPile.Typescript where

-- the job of this module is to output valid typescript class definitions, decision functions, and variable instances.

-- consider: https://hackage.haskell.org/package/aeson-typescript
-- not suitable because L4 allows users to define their own types which are not known to Haskell's type system.

-- import Debug.Trace

import AnyAll

-- import L4.Syntax as CoreL4

-- import L4.Annotation

-- import Data.Functor ( (<&>) )

import Data.HashMap.Strict qualified as Map
-- import qualified Data.Text as T
import qualified Data.Text.Lazy as DTL
import qualified Data.List.NonEmpty as NE
import Data.List (intercalate, nub)
import Data.List.Extra (groupSort)
import Data.Maybe (isJust, isNothing)
import Data.Tree qualified as DT
import LS.Interpreter
  ( attrType,
    getCTkeys,
    globalFacts,
    l4interpret,
    topsortedClasses,
    extractEnums,
    attrsAsMethods,
  )
import LS.PrettyPrinter
  ( ParamText4 (PT4, PT5),
    commentWith,
    prettyMaybeType,
    prettySimpleType,
    snake_case,
    snake_inner,
    vvsep,
    (<//>),
    (</>),
  )
import LS.Rule as SFL4R
  ( Interpreted (classtable, scopetable, origrules, valuePreds),
    Rule(..),
    ruleLabelName,
  )
import LS.Types as SFL4
  ( ClsTab (CT),
    HornClause (HC, hHead),
    HornClause2,
    BoolStructR(..),
    MultiTerm,
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
    ValuePredicate(..),
    clsParent,
    defaultInterpreterOptions,
    getSymType,
    mt2text,
    rp2text,
    RPRel(..),
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
    braces,
    line,
    list,
    nest,
    rbrace,
    semi,
    parens,
    space,
    viaShow,
    vsep,
    (<+>),
  )
import Text.Pretty.Simple (pShowNoColor)
import LS.XPile.Logging

-- JsonLogic
import Text.JSON

asTypescript :: SFL4R.Interpreted -> XPileLog (Doc ann)
asTypescript l4i = do
  vvsep <$> sequence [ tsPrelude l4i
                     , pure "// tsEnums",           tsEnums l4i
                     , pure "// tsClasses",         tsClasses l4i
                     , pure "// globalDefinitions [commented out]" --, globalDefinitions l4i
                     , pure "// jsInstances",       jsInstances l4i
                     , pure "// toJsonLogi",        toJsonLogic l4i
                     , pure "// toPlainTS",         toPlainTS l4i
                     ]

-- | a convention for preserving the original strings and data structures for downstream use
tsPrelude :: Interpreted -> XPileLog (Doc ann)
tsPrelude l4i = return $
  vsep [ "// tsPrelude"
       , "export const L4Orig = { enums: { }, classes: { } }"
       ]

-- | output class declarations.
--
-- [TODO] -- add class methods here
--
-- We may need to upgrade all plain attributes to be methods -- because if bob's MyValue is 2 on tuesdays and 14 on wednesdays,
-- the appropriate way to model that is as a method; but then MyValue has type `() => number`. And alice, whose MyValue is always 40,
-- ends up with a @const 40@. That's why attributes may need to be upgraded to methods.

tsClasses :: Interpreted -> XPileLog (Doc ann)
tsClasses l4i = return $
  let ct@(CT ch) = classtable l4i
  in
  vvsep [ "class" <+> snake_case [MTT className] <> classExtension
          -- attributes of the class are shown as decls
          <+> lbrace
      --    <//> "  //" <+> viaShow csuper
          <//> "  // using prettySimpleType (old code path)"
          <//> indent 2 ( vsep [ snake_case [MTT attrname] <>
                                 case attrType children attrname of
                                   Just t@(SimpleType TOptional _) -> " () : null | " <+> prettySimpleType "ts" (snake_inner . MTT) t <+> braces (defaultMethod t)
                                   Just t@(SimpleType TOne      _) -> " () : "        <+> prettySimpleType "ts" (snake_inner . MTT) t <+> braces (defaultMethod t)
                                   Just t@(InlineEnum TOne      _) -> " () : "        <+> snake_case [MTT attrname] <> "Enum"
                                   Just t                          -> " () : "        <+> prettySimpleType "ts" (snake_inner . MTT) t <+> braces (defaultMethod t)
                                   Nothing -> "// tsClasses nothing case"
                                 <> semi
                               | attrname <- getCTkeys children
                               ]
                        )
          <//> "  //" <+> "using `methods` (new code path)"
          <//> indent 2 ( encloseSep (lbrace <> line) (line <> rbrace) (comma <> space)
                          $ concat [ methods l4i [MTT attrname]
                                   | attrname <- getCTkeys children
                                                 -- [TODO] finish out the attribute definition -- particularly tricky if it's a DECIDE, but we'll use the methods approach for that.
                                   ]
                        )
          <//> rbrace
        | className <- reverse $ topsortedClasses ct
        , (Just (csuper, children)) <- [Map.lookup className ch]
        , case csuper of
            (Just (SimpleType _ _), _) -> True
            (Just (InlineEnum _ _), _) -> False -- we deal with enums separately below
            _                          -> True
        , let classExtension =
                case clsParent ct className of
                  Nothing       -> mempty
                  (Just parent) -> " extends" <+> pretty parent
        ]

defaultMethod :: TypeSig -> Doc ann
defaultMethod (SimpleType TOne "string") = " return \"\" "
defaultMethod (SimpleType TOne "number") = " return 0 "
defaultMethod _                          = " return undefined "
  

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

  


-- | all the instance symbols we know about, including those that show up in GIVEN, DEFINE, and DECIDE.
-- This is probably not the best way to do it because we're commingling local and global variables here.
-- Happy to rethink all of this.
-- Also, not yet done, classes and variables need to be topologically sorted because typescript is picky about that.
jsInstances :: Interpreted -> XPileLog (Doc ann)
jsInstances l4i = return $
  let sctabs = scopetable l4i
  in
  vvsep [ "//" <+> "scope" <+> scopenameStr scopename <//>
          "// symtab' = " <+> commentWith "// " [DTL.toStrict $ pShowNoColor symtab'] <//>
          -- the above DTL.toStrict is needed otherwise the pShowNoColor get typed as Data.Text.Lazy.Internal.Text which is a little too deep into the internals for me to be comfortable with.

          -- [TODO] there is a mysterious dup here for alice in micromvp3
          vvsep [ "const" <+> snake_case mt <+> prettyMaybeType "ts" (snake_inner . MTT) (getSymType symtype) <+> equals <+> nest 2 value
--                  </> "// symtype = " <+> viaShow symtype
--                  </> "// val = " <+> viaShow val
                | (mt, (symtype, vals)) <- Map.toList symtab'
                , symtype /= (Nothing,[])
                , val <- nub vals
                , value <- case val of
                              -- what we should do is gather all the paramtexts and join them in a single dictionary,
                              -- rather than assume that all the HC2 are paramtexts.
                             -- We could do with an additional test against the keyword, should be a Define.

                             -- also, we should preorganize all the constContents into a canonical data structure hierarchy, rather than try to fit things into the sctabs
                              HC { hHead = RPParamText {} } ->
                                let constContents = asValuePT l4i vals ++ methods l4i mt
                                in [ encloseSep (lbrace <> line) (line <> rbrace) (comma <> space) constContents ]
                              _ -> 
                                [hc2ts l4i val]
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

-- | This was originallyh written as instanceMethods and used in jsInstances. Next we are experimenting with expanding it to work for tsClasses as well.
--
-- If the list of valpreds ever gets very big, we can optimize the quadratic algo here using a map.
methods :: Interpreted -> SFL4.MultiTerm -> [Doc ann]
methods l4i mt =
  let marshalled = groupSort [ (attrName, vp)
                             | vp@ValPred{..} <- valuePreds l4i
                             , attrRel == Just RPis
                             , objPath == [mt2text mt]
                             ]
  in
  [ line <> "// methods go here"
    </> pretty attrName' <+> equals <+> parens emptyDoc <+> "=>" <+> braces ( indent 1 $
      vsep [ vpTS <> semi
           | vp@ValPred{..} <- vps
           , let vpTS = vpToTS l4i vp
           ] <> space
      )
  | (attrName', vps) <- marshalled
  ]

-- | convert the @attrVal :- attrCond@ part of a ValuePredicate to typescript syntax
vpToTS :: Interpreted -> ValuePredicate -> Doc ann
vpToTS l4i ValPred{..}
  | attrCond == Just (Leaf (RPMT [MTT "OTHERWISE"]))
    || isNothing attrCond                = "return" <+> pretty attrVal
  | isJust    attrCond && isJust attrVal = "if" <+> parens (rp2ts attrCond) <+> braces ("return" <+> pretty attrVal)
  | otherwise                            = "return null // [TODO]"
  where
    rp2ts :: Maybe BoolStructR -> Doc ann
    rp2ts Nothing = "true"
    rp2ts (Just bsr) = bsr2ts bsr

    bsr2ts :: BoolStructR -> Doc ann
    bsr2ts (Leaf (RPConstraint mt1 rprel mt2) ) = pretty (mt2text mt1) <+> renderRPrel rprel <+> pretty (mt2text mt2)
    bsr2ts (Leaf (RPnary RPis [rp1, rp2])) = pretty (rp2text rp1) <+> renderRPrel RPis <+> pretty (rp2text rp2)
    bsr2ts (Leaf (RPnary RPsum rps)) = "sum" <> list (pretty . rp2text <$> rps)
    bsr2ts (Any pp rps) = "any" <> parens (list (bsr2ts <$> rps))
    bsr2ts (All pp rps) = "all" <> parens (list (bsr2ts <$> rps))
    bsr2ts (Not rp) = "not" <> parens (bsr2ts rp)
    bsr2ts _ = error "bsr2ts needs more cases"

    renderRPrel RPis = "=="
    renderRPrel RPgt = ">"
    renderRPrel RPlt = "<"
    renderRPrel _    = error "add a renderRPrel in Typescript.hs"

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
-- Let's try the raw Typescript approach first, so we can be sure we know what we're doing.
--
-- We'll bypass most of what is provided by the Interpreter, and work with the raw rules themselves.
--
-- We might rely on the Interpreter to provide some of the data flow graphing, so we can trace variable expansion.
--
-- In this initial experiment, we do not implement scoped variables. All variables live in global scope.
--
-- Without further ado: [TODO] this is a work in progress
hc2ts :: Interpreted -> HornClause2 -> Doc ann
hc2ts _l4i  hc2@HC { hHead = RPMT        _ }                 = "value" <+> colon <+> dquotes (pretty (hHead hc2))
hc2ts _l4i _hc2@HC { hHead = RPConstraint  mt1 _rprel mt2 }  = snake_case mt1 <+> colon <+> dquotes (snake_case mt2) <+> "// hc2ts RPConstraint"
hc2ts _l4i _hc2@HC { hHead = RPBoolStructR mt1 _rprel _bsr } = snake_case mt1 <+> colon <+> "(some => lambda)" 
hc2ts  l4i _hc2@HC { hHead = RPParamText pt }                = pretty (PT4 pt l4i) <+> "// hc2ts RPParamText"
hc2ts  l4i  hc2@HC { hHead = RPnary      _rprel [] }         = error "TypeScript: headless RPnary encountered"
hc2ts  l4i  hc2@HC { hHead = RPnary      _rprel rps }        = hc2ts l4i hc2 {hHead = head rps} <+> "// hc2ts RPnary"

-- | for debugging at the moment only.
toPlainTS :: Interpreted -> XPileLog (Doc ann)
toPlainTS l4i = do
  return $ vvsep [ "//" <+> viaShow valpred
                 | valpred <- valuePreds l4i
                 ]
  
toJsonLogic :: Interpreted -> XPileLog (Doc ann)
toJsonLogic l4i = do
  mutterd 1 "toJsonLogic"
  -- 2 + 2
  varData <- case decode "{ \"foo\" : \"bar\" }" of
               Ok jsVarData -> return jsVarData
               Error err    -> mutterd 2 "error while decoding" >> mutter err
  
  return $ "const varData" <+> equals <+> pretty (encode varData)

-- dump only the paramtexts
asValuePT :: Interpreted -> [HornClause2] -> [Doc ann]
asValuePT l4i hc2s = -- trace ("asValuePT: " <> show hc2s) $
  [ pretty (PT4 pt l4i)
  | HC { hHead = RPParamText pt } <- hc2s ]
                 
