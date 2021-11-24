{-# LANGUAGE NamedFieldPuns #-}
module LS.XPile.Uppaal where


import L4.Syntax as CoreL4

import LS.Types as SFL4
import qualified Data.Set as Set
-- import L4.Annotation
import Data.Text.Lazy (unpack)
import qualified Data.Text.Lazy as TL
import qualified AnyAll as AA
import L4.PrintProg

type Ann = ()

taSysToString :: CoreL4.TASys () -> String
taSysToString = show . showL4 [PrintSystem UppaalStyle]

toL4TA :: [SFL4.Rule] -> CoreL4.TASys ()
toL4TA rules = foldr (addRule henceChannels) emptyTASys { channelsOfSys =  Set.toList henceChannels } rules
  where 
    henceChannels = Set.fromList $ concatMap getHence rules 

getHence :: SFL4.Rule -> [String]
getHence Regulative{ hence = Just (RuleAlias rname)} = [unpack rname]
getHence _ = []
-- TODO: Handle recursive Hence
  
emptyTASys :: TASys ()
emptyTASys
  = TASys
      {annotOfSys = (), nameOfTASys = "Unknown name", declsOfSys = [],
       channelsOfSys = [], automataOfSys = []}

addRule :: Set.Set ChannelName -> SFL4.Rule -> CoreL4.TASys () -> CoreL4.TASys ()
addRule hc r@Regulative{rlabel = Just (_,_,lb)} ts | unpack lb `Set.member` hc = ts {automataOfSys = ruleToTA r (Just lb) : automataOfSys ts}
                                                   | otherwise = ts {automataOfSys = ruleToTA r Nothing : automataOfSys ts}
addRule hc r ts = ts

-- TODO Nested
-- TODO Handle party

-- TODO: Make it recursive to handle missing fields gracefully
ruleToTA :: SFL4.Rule -> Maybe TL.Text -> TA ()
-- ruleToTA Regulative{rlabel, temporal, upon= [ AA.Leaf upn ]} Nothing = TA 
ruleToTA Regulative{rlabel, temporal, upon= [ AA.Leaf upn ]} _ = TA 
    { nameOfTA = rName
    , annotOfTA = ()
    , locsOfTA = [initialLoc, uponLoc]
    , clocksOfTA = [ruleTimer]
    , transitionsOfTA = [uponTransition ]
    , initialLocOfTA = initialLoc
    , invarsOfTA = []
    , labellingOfTA = []
    }
  where
    rName = maybe "TODO_generate_unique_name" (unpack . thrd) rlabel
    ruleTimer = Clock $ "time" ++ rName
    initialLoc = Loc "Initial"
    uponLoc = Loc $ "Upon_" ++ toValidName (unpack (pt2text upn))
    uponTransition = (simpleTransition initialLoc uponLoc) {
                                 actionOfTransition = TransitionAction [ruleTimer] (Skip ())
                                 }
ruleToTA r _ = error $ "Unexpected rule type: " ++ show r

simpleTransition :: Loc -> Loc -> Transition ()
simpleTransition src tgt = Transition { sourceOfTransition = src
                                       , targetOfTransition = tgt
                                       , guardOfTransition = TransitionGuard [] Nothing
                                       , syncOfTransition = Nothing
                                       , actionOfTransition = TransitionAction [] (Skip ())
                                       }

thrd :: RuleLabel -> TL.Text
thrd (_,_,t) = t

toValidName :: [Char] -> [Char]
toValidName = map underscorize
  where
    underscorize ' ' = '_'
    underscorize c = c



{-
- Identify automata
- Identify channels

- Look into automata
- Difficult: Figure out states

Q: Global clock?

"Within X time" means "Reset timer when entering this state, Exit condition when leaving state"

-}

-- data Rule = Regulative
--             { subj     :: BoolStructP               -- man AND woman AND child
--             , keyword  :: MyToken                   -- Every | Party | TokAll
--             , who      :: Maybe BoolStructP         -- who walks and (eats or drinks)
--             , cond     :: Maybe BoolStructP         -- if it is a saturday
--             , deontic  :: Deontic            -- must
--             , action   :: BoolStructP          -- fart loudly AND run away
--             , temporal :: Maybe (TemporalConstraint Text.Text) -- Before "midnight"
--             , hence    :: Maybe Rule
--             , lest     :: Maybe Rule
--             , rlabel   :: Maybe RuleLabel
--             , lsource  :: Maybe Text.Text
--             , srcref   :: Maybe SrcRef
--             , upon     :: [BoolStructP] -- UPON entering the club (event prereq trigger)
--             , given    :: Maybe ParamText
--             , having   :: Maybe ParamText  -- HAVING sung...
--             , orig     :: [(Preamble, BoolStructP)]
--             }
--           | Constitutive
--             { name     :: ConstitutiveName   -- the thing we are defining
--             , keyword  :: MyToken       -- Means, Includes, Is, Deem
--             , letbind  :: BoolStructP   -- might be just a bunch of words to be parsed downstream
--             , cond     :: Maybe BoolStructP -- a boolstruct set of conditions representing When/If/Unless
--             , given    :: Maybe ParamText
--             , rlabel   :: Maybe RuleLabel
--             , lsource  :: Maybe Text.Text
--             , srcref   :: Maybe SrcRef
--             , orig     :: [(Preamble, BoolStructP)]
--             }
--           | TypeDecl
--             { name     :: ConstitutiveName  --      DEFINE Sign
--             , super    :: Maybe TypeSig     --                  :: Thing
--             , has      :: Maybe [ParamText] -- HAS foo :: List Hand \n bar :: Optional Restaurant
--             , enums    :: Maybe ParamText   -- ONE OF rock, paper, scissors (basically, disjoint subtypes)
--             , rlabel   :: Maybe RuleLabel
--             , lsource  :: Maybe Text.Text
--             , srcref   :: Maybe SrcRef
--             }
--           | DefNameAlias -- inline alias, like     some thing AKA Thing
--             { name   :: ConstitutiveName   -- "Thing" -- the thing usually said as ("Thing")
--             , detail :: BoolStructP        -- "some thing"
--             , nlhint :: Maybe Text.Text  -- "lang=en number=singular"
--             , srcref :: Maybe SrcRef
--             }
--           | RuleAlias Text.Text -- internal softlink to a rule label (rlabel), e.g. HENCE NextStep
--           | RuleGroup { rlabel :: Maybe RuleLabel }  -- § NextStep
--           | RegFulfilled  -- trivial top
--           | RegBreach     -- trivial bottom