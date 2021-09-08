module AnyAll.Relevance where

import AnyAll.Types



-- given a tree
-- and a marking indicating which values are known,
-- return a marking indicating which nodes in the tree are still of interest.
relevant :: Item a -> Marking a -> ShouldAsk a
relevant aa marking = (const Ask) <$> marking


-- basically logical shortcut
dispositive :: Marking a -> Item a -> Bool
dispositive marking (Leaf x) = case marking ! x of
                                 
  
