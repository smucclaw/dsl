{-# LANGUAGE OverloadedStrings #-}

{-| Org-mode transpiler
-}

module LS.XPile.Org (toOrg) where

import LS.Interpreter
    ( qaHornsT,
      ruleLocals,
      getMarkings,
      isRuleAlias,
      qaHornsR,
      groupedByAOTree,
      expandBSR,
      getAndOrTree,
      exposedRoots,
      globalFacts,
      ruleDecisionGraph,
      expandRule,
      classGraph,
      extractEnums,
      defaultToSuperClass, defaultToSuperType,
      attrsAsMethods,
      entryPoints,
      )

import LS.RelationalPredicates ( partitionExistentials, getBSR )
import LS.Rule
    ( Interpreted(..),
      Rule(..),
      hasGiven,
      hasClauses,
      ruleLabelName,
      Rule(clauses, given) )
import LS.Types ( unCT
                , TypeSig (InlineEnum, SimpleType)
                , ParamType (TOne, TOptional)
                , ClassHierarchyMap
                )
import LS.PrettyPrinter
    ( myrender, vvsep, (</>), tildes, (<//>), snake_case, srchs, orgexample )

import Prettyprinter
    ( vsep, viaShow, hsep, emptyDoc, (<+>), Pretty(pretty), Doc, indent, line )
import Data.HashMap.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor (first)
import Data.Graph.Inductive (prettify)
import Data.Text qualified as Text
import LS.XPile.Logging

-- | org-mode output
toOrg :: Interpreted -> [Rule] -> String
toOrg l4i rs = Text.unpack (myrender (musings l4i rs))

-- | Talk a little bit about what we've interpreted.
-- The output of this function gets saved to the workdir's @org/@ directory
-- and can be viewed inside the @LATEST.org@ output file.
-- If you are working on the Interpreter and want to see what it is thinking,
-- this is a good place to add "printf debugging".
--
-- When you view the @LATEST.org@ output file, org-mode is recommended.
-- This comes naturally in Emacs. In VS Code you will need to install plugins.

musings :: Interpreted -> [Rule] -> Doc ann
musings l4i rs =
  let cg = classGraph (classtable l4i) []
      expandedRules = nub $ concatMap (expandRule rs) rs
      decisionGraph = ruleGraph l4i
      (eRout, eRerr)         = xpLog (exposedRoots l4i)
  in vvsep [ "* musings"
           , "** Global Facts" </> srchs (globalFacts l4i)

           , "** Class Hierarchy"
           , vvsep [ vvsep [ "*** Class:" <+> pretty fullyQualifiedClassName <+>
                             "extends" <+> viaShow superclass <>
                             if not $ null (unCT attrs)
                             then " with new attributes"
                             </> let prettyClassAttrs :: ClassHierarchyMap -> Doc ann
                                     prettyClassAttrs chmap =
                                       vsep [ "-" <+> pretty attr <+> "::" <+> viaShow inferrableTypeSig
                                              <> if not (null (unCT attrchildren))
                                                 then line <> indent 2 (prettyClassAttrs (unCT attrchildren))
                                                 else emptyDoc
                                            | (attr, (inferrableTypeSig, attrchildren)) <- Map.toList chmap
                                            ]
                                 in prettyClassAttrs (unCT attrs)
                             else emptyDoc
                           ]
                   | (cname, (superclass, attrs)) <- cg
                   , let fullyQualifiedClassName = Text.intercalate "." (reverse cname) ]
           , "** The entire classgraph"
           , srchs cg
           
           , "** Enums"
           , vvsep [ "***" <+> snake_case className
                   <//> vsep [ "-" <+> snake_case [enumStr]
                             | (enumMultiTerm, _) <- NE.toList enumNEList
                             , enumStr <- NE.toList enumMultiTerm
                             ]
                     | r@TypeDecl{super=Just (InlineEnum TOne enumNEList)} <- extractEnums l4i
                     , let className = ruleLabelName r
                   ]

           , "** Symbol Table"
           , "we know about the following scopes"
           , vvsep [ "*** Rule:" <+> hsep (pretty <$> rn) </>
                     vvsep [ "**** symbol:" <+> tildes (pretty mt)
                             </> srchs hc
                             </> "**** typesig:" <+> tildes (viaShow its)

                           | (mt, (its, hc)) <- Map.toList st ]
                   | (rn, st) <- Map.toList $ scopetable l4i ]

           , "** attributes as methods"
           , "we dump expressions of the form DECIDE class's record's attribute IS someValue WHEN someCondition"
           , let aam = xpLog $ attrsAsMethods rs -- [TODO] this duplicates work done in the Interpreter -- find a way to coherently log common errors from the Interpreter itself, clean up l4i's valuePreds
             in srchs (fst aam) </> vsep (pretty <$> snd aam)
           , "** Dataflow modelling"
           , "*** entryPoints" </> let (ePout, ePerr) = xpLog (entryPoints l4i) in srchs ePout </> vsep (pretty <$> ePerr)
           , "** the Rule Decision Graph"
           , orgexample (pretty (prettify (first ruleLabelName decisionGraph)))
           , "*** logging output" </> vsep (pretty <$> ruleGraphErr l4i)

           , "** Decision Roots"
           , "rules which are not just RuleAlises, and which are not relied on by any other rule"
           , srchs (ruleLabelName <$> eRout)
           , "*** logging output from exposedRoots" </> vsep (pretty <$> eRerr)
           , "*** Nubbed, Exposed, Decision Roots"
           , "Each ruleset can be organized into multiple trees. Each tree contains rules."
           , "The leaves of the trees contain datapoints we need to collect from the user, typically by asking the user for that data in some interactive Q&A form style."
           , "The roots of the trees are the top-level answers that are computed from the rest of the tree: in the current Vue web UI, those roots show up on the LHS of the UI, and the user can choose between them."
           , "Here, we expose the decision roots, and the rules that (may) contribute to them, as a list of rules."
           , "Maybe some of the decision roots are identical and don't need to be repeated; so we nub them."
           , "Now, you may notice that the rule graph shown here does not necessarily line up with the actual data flow."
           , "That's because this works fine for the simpler cases of only propositional logic."
           , "But when the rules involve records and arithmetic calculations, graph construction needs to be more intelligent."
           , "That work is in progress. See https://app.asana.com/0/1181141497134329/1205219649158495"
           , vvsep [ "**** Decision Root" <+> viaShow n
                     </> "The rules in this decision tree are produced from ~groupedByAOTree~ run on ~exposedRoots~:"
                     </> vsep [ "-" <+> pretty (ruleLabelName r) | r <- uniqrs ]
                     </> "The first element in the list is the important one: it's the root of this rule tree."
                     </> "***** grpval" </> srchs grpval
                     </> "***** head uniqrs"
                     </> "let's look at the first element in more detail."
                     </> srchs (head uniqrs)
                     </> "***** and the rest of uniqrs in the tail"
                     </> srchs (tail uniqrs)
                     </> "***** getAndOrTree (head uniqrs)" </> srchs (getAndOrTree l4i 1 $ head uniqrs)
                     </> "This should contain the post-expansion set of questions we need to ask the user."
                     </> "***** getBSR [head uniqrs]" </> srchs (mapMaybe getBSR [head uniqrs])
                     </> "... in BoolStructR format"
                     </> "***** expandBSR" </> srchs (expandBSR l4i 1 <$> mapMaybe getBSR uniqrs)
                     </> vvsep [ "****** uniq rules" </> srchs r
                                 </> "******* givens" </> srchs (given r)
                                 </> vvsep [ "******* horn clause" </> srchs c
                                             </> "******** partitionExistentials"
                                             </> "We had the idea that if you want to distinguish existential variables from universal variables,"
                                             <//> "you can put the universal variables in GIVEN, and"
                                             <//> "the existential variables as individual terms with type declarations and nothing else, in the IF."
                                             </> srchs (partitionExistentials c)
                                           | c <- clauses r ]
                               | r <- uniqrs
                               , hasClauses r
                               , hasGiven r
                               ]
                   | ((grpval, uniqrs),n :: Int) <- Prelude.zip (groupedByAOTree l4i -- NUBBED
                                                                  eRout      -- EXPOSED
                                                                ) [1..]
                   , not $ null uniqrs
                   ]

           , "** qaHornsR" , vvsep [ "***" <+> viaShow (concat names) </> srchs boolstruct | (names, boolstruct) <- qaHornsR l4i ]
           , "** qaHornsT" , vvsep [ "***" <+> viaShow (concat names) </> srchs boolstruct | (names, boolstruct) <- qaHornsT l4i ]
           , "** expandedRules"
           , if expandedRules == nub rs
             then "(ahem, they're actually the same as unexpanded, not showing)"
             else vvsep [ "***" <+> hsep (pretty <$> ruleLabelName r) </> srchs r | r <- expandedRules ]
           , "** getAndOrTrees, direct"
           , vvsep [ "***" <+> hsep (pretty <$> ruleLabelName r) </> srchs (getAndOrTree l4i 1 r) | r <- rs ]
           , vvsep [ "** Things that are RuleAliases"
                   , vsep [ "-" <+> pretty rlname
                          | r <- rs -- this is AccidentallyQuadratic in a pathological case.
                          , let rlname = ruleLabelName r
                          , isRuleAlias l4i rlname ]
                   ]
           , vvsep [ "** default markings"
                   , "terms annotated with TYPICALLY so we tell XPile targets what their default values are"
                   , srchs (getMarkings l4i)
                   ]
           , "** symbol tables (~scopetable l4i~)"
           , vvsep [ "***" <+> pretty lhs </> srchs rhs | (lhs, rhs) <- Map.toList (scopetable l4i) ]

           , "** class tables (~classtable l4i~)" </> srchs (classtable l4i)
           , vvsep [ "***" <+> pretty lhs </> srchs rhs | (lhs, rhs) <- Map.toList (unCT $ classtable l4i) ]

           , "** The original rules (~origrules l4i~)"
           , vvsep [ "***" <+> pretty (ruleLabelName r) </> srchs r
                   </> "**** local variables" </> srchs (ruleLocals l4i r)
                   | r <- rs ]
           ]
