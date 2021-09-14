module AnyAll.Types where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Either
import Data.Maybe
import Data.String
import Data.Map      as Map

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

type Bool = Boolean

--
-- the "native" data type is a simple tree of Items
--

data Item a =
    Leaf a
  | All (Label a) (Array (Item a))
  | Any (Label a) (Array (Item a))

-- boilerplate for class derivations
derive instance  eqItem :: (Eq a) => Eq (Item a)
derive instance genericItem :: Generic (Item a) _
instance showItem :: (Show a) => Show (Item a) where show eta = genericShow eta

--
-- Item uses Label
--
data Label a = Pre a
             | PrePost a a
derive instance  eqLabel :: (Eq a) => Eq (Label a)
derive instance genericLabel :: Generic (Label a) _
instance showLabel :: (Show a) => Show (Label a) where show = genericShow

-- an Item tree represents the logic.
-- a Marking contains the current state of which elements have received user input;
-- if no user input was received, the Marking gives default values

newtype Marking a = Marking (Map.Map a (Default Bool))
derive instance  eqMarking :: (Eq a) => Eq (Marking a)
derive newtype instance showMarking :: (Show a) => Show (Marking a)

getMarking (Marking mapmap) = mapmap

newtype Default a = Default (Either (Maybe a) (Maybe a))
derive instance  eqDefault :: (Eq a) => Eq (Default a)
derive newtype instance showDefault :: (Show a) => Show (Default a)


-- together, an Item and a Marking get jazzed up into a tree of Q, which has more structure,
-- and represents the result of and/or shortcutting.
-- a Q tree informs the UI of what to display, what to hide, what to ask for.

newtype Q a = Q { shouldView :: ShouldView
                , andOr      :: AndOr a
                , prePost    :: Maybe (Label a)
                , mark       :: Default Bool
                , children   :: Array (Q a)
                }
derive instance  eqQ :: (Eq a) => Eq (Q a)
derive instance genericQ :: Generic (Q a) _
instance showQ :: (Show a) => Show (Q a) where show eta = genericShow eta

-- TODO: use record wildcards instead
mkQ sv ao pp m c = Q { shouldView: sv
                     , andOr:      ao
                     , prePost:    pp
                     , mark:       m
                     , children:   c }

newtype Blurt = Blurt { shouldView :: String
                      , andOr      :: { tag :: String
                                      , contents :: Maybe String }
                      , prePost    :: String
                      , mark       :: { source :: String
                                      , value :: String }
                      , children   :: Array Blurt
                      }
derive instance  eqBlurt :: Eq Blurt
derive instance genericBlurt :: Generic Blurt _
instance showBlurt :: Show Blurt where show eta = genericShow eta

qblurt :: Q String -> Blurt
qblurt (Q q@{ shouldView, andOr, prePost, mark, children }) =
  Blurt { shouldView : show shouldView
        , andOr      : case andOr of And -> { tag: "Any", contents: Nothing }
                                     Or  -> { tag: "All", contents: Nothing }
                                     (Simply x) -> { tag: "Leaf"
                                                   , contents: Just x }
        , prePost    : show prePost
        , mark       : dumpDefault mark
        , children   : qblurt <$> children
        }

dumpDefault (Default ( Left x))  = { source: "default", value: maybeBool2string x }
dumpDefault (Default (Right x))  = { source: "user",    value: maybeBool2string x }

maybeBool2string (Just true)  = "true"
maybeBool2string (Just false) = "false"
maybeBool2string Nothing      = "undefined"

data ShouldView = View | Hide | Ask
derive instance  eqShouldView ::        Eq (ShouldView)
derive instance genericShouldView :: Generic ShouldView _
instance showShouldView :: Show ShouldView where show eta = genericShow eta

data AndOr a = And      -- All
             | Or       -- And
             | Simply a -- Leaf
derive instance  eqAndOr :: (Eq a) => Eq (AndOr a)
derive instance genericAndOr :: Generic (AndOr a) _
instance showAndOr :: (Show a) => Show (AndOr a) where show = genericShow

-- a few other types for configuration of the user interface

data DisplayPref = DPTerse | DPNormal | DPVerbose
derive instance  eqDisplayPref :: Eq DisplayPref
derive instance genericDisplayPref :: Generic DisplayPref _
instance showDisplayPref :: Show DisplayPref where show = genericShow

data Hardness = Soft -- use Left defaults
              | Hard -- require Right input
derive instance  eqHardness :: Eq (Hardness)
derive instance genericHardness :: Generic Hardness _
instance showHardness :: Show Hardness where show = genericShow


data StdinSchema a = StdinSchema { marking :: Marking a
                                 , andOrTree :: Item a }
derive instance  eqStdinSchema :: (Eq a) => Eq (StdinSchema a)
derive instance genericStdinSchema :: Generic (StdinSchema a) _
instance showStdinSchema :: (Show a) => Show (StdinSchema a) where show = genericShow


{-
getSV :: ShouldView -> Q a -> Maybe (a, Default Bool)
getSV sv1 (Q sv2 (Simply x) pp m)
  | sv1 == sv2 = Just (x, m)
  | otherwise  = Nothing
getSV _ _ = Nothing

getAsks :: (ToJSONKey a, Ord a) => QTree a -> Map.Map a (Default Bool)
getAsks qt = Map.fromList $ catMaybes $ getSV Ask <$> flatten qt

getAsksJSON :: (ToJSONKey a, Ord a) => QTree a -> B.ByteString
getAsksJSON = encode . getAsks

getViews :: (ToJSONKey a, Ord a) => QTree a -> Map.Map a (Default Bool)
getViews qt = Map.fromList $ catMaybes $ getSV View <$> flatten qt

getViewsJSON :: (ToJSONKey a, Ord a) => QTree a -> B.ByteString
getViewsJSON = encode . getViews

getForUI :: (ToJSONKey a, Ord a) => QTree a -> B.ByteString
getForUI qt = encode (Map.fromList [("view" :: TL.Text, getViews qt)
                                   ,("ask" :: TL.Text, getAsks qt)])

-}
