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
      classGraph )
import LS.PrettyPrinter ( tildes, (</>), vvsep, myrender )
import LS.RelationalPredicates ( partitionExistentials, getBSR )
import LS.Rule
    ( Interpreted(classtable, scopetable),
      hasGiven,
      hasClauses,
      ruleLabelName,
      Rule(clauses, given) )
import LS.Types ( unCT )
import LS.PrettyPrinter

import Prettyprinter
    ( vsep, viaShow, hsep, emptyDoc, (<+>), Pretty(pretty), Doc )
import Text.Pretty.Simple ( pShowNoColor )
import Data.HashMap.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.List (nub)
import Data.Bifunctor (first)
import Data.Graph.Inductive (prettify)
import Data.Text qualified as Text


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
      decisionGraph = ruleDecisionGraph l4i rs
  in vvsep [ "* musings"
           , "** Global Facts" </> srchs (globalFacts l4i)
           , "** Class Hierarchy"
           , vvsep [ vvsep [ "*** Class:" <+> pretty cname <>
                             if null (Prelude.tail cname) then emptyDoc
                             else hsep (" belongs to" : (pretty <$> Prelude.tail cname))
                           , if null cchild
                             then emptyDoc
                             else "**** extends" <+> maybe "" viaShow (fst . fst $ cchild) <+> "with new attributes"
                                  </> srchs (snd cchild)
                           , "**** deets" </> srchs cname
                           ]
                   | (cname, cchild) <- cg ]
           , "** The entire classgraph"
           , srchs cg
           , "** Symbol Table"
           , "we know about the following scopes"
           , vvsep [ "*** Rule:" <+> hsep (pretty <$> rn) </>
                     vvsep [ "**** symbol:" <+> tildes (pretty mt)
                             </> srchs hc
                             </> "**** typesig:" <+> tildes (viaShow its)

                           | (mt, (its, hc)) <- Map.toList st ]
                   | (rn, st) <- Map.toList $ scopetable l4i ]

           , "** the Rule Decision Graph"
           , example (pretty (prettify (first ruleLabelName decisionGraph)))

           , "** Decision Roots"
           , "rules which are not just RuleAlises, and which are not relied on by any other rule"
           , srchs (ruleLabelName <$> exposedRoots l4i)

           , "*** Nubbed, Exposed, Decision Roots"
           , "maybe some of the decision roots are identical and don't need to be repeated; so we nub them"
           , vvsep [ "**** Decision Root" <+> viaShow (n :: Int)
                     </> vsep [ "-" <+> pretty (ruleLabelName r) | r <- uniqrs ]
                     </> "***** grpval" </> srchs grpval
                     </> "***** head uniqrs" </> srchs (head uniqrs)
                     </> "***** getAndOrTree (head uniqrs)" </> srchs (getAndOrTree l4i 1 $ head uniqrs)
                     </> "***** getBSR [head uniqrs]" </> srchs (mapMaybe getBSR [head uniqrs])
                     </> "***** expandBSR" </> srchs (expandBSR l4i 1 <$> mapMaybe getBSR uniqrs)
                     </> vvsep [ "****** uniq rules" </> srchs r
                                 </> "******* givens" </> srchs (given r)
                                 </> vvsep [ "******* horn clause" </> srchs c
                                             </> "******** partitionExistentials"
                                             </> srchs (partitionExistentials c)
                                           | c <- clauses r ]
                               | r <- uniqrs
                               , hasClauses r
                               , hasGiven r
                               ]
                   | ((grpval, uniqrs),n) <- Prelude.zip (groupedByAOTree l4i $ -- NUBBED
                                                          exposedRoots l4i      -- EXPOSED
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
  where
    srchs :: (Show a) => a -> Doc ann
    srchs = src "haskell" . pretty . pShowNoColor
    src lang x = vsep [ "#+begin_src" <+> lang, x, "#+end_src" ]
    example  x = vsep [ "#+begin_example", x, "#+end_example" ]

