{-# LANGUAGE TemplateHaskell #-}
-- -- {-# OPTIONS_GHC -ddump-splices -dsuppress-uniques #-}

module Classy where

-- import Control.Lens
--  -- we use a little lens magic
-- data Point = Point
--  { _x     :: Int
--  , _y     :: Int
--  , _label :: String
--  }
-- makeClassy ''Point

-- data MyNatural = MyNatural {
--   _myint :: Int
-- } deriving (Show, Eq, Ord)
-- makeClassy ''MyNatural

-- class Validation a where
--   isValid :: a -> Bool

-- instance Validation MyNatural where
--   isValid myn = myn^.myint >= 0

-- type Address = [String]

-- data Identifier = SG_UEN  String
--                 | SG_NRIC String
--                 | SG_FIN  String
--                 | US_TIN  String
--                 | Passport
--                   { nationality :: String
--                   , passportNo  :: String
--                   }
--   deriving (Show, Eq)
-- data Person = Person
--   { _officialName :: String
--   , _address :: Address
--   , _identifier :: Identifier
--   } deriving (Show, Eq)
-- makeClassy ''Person

-- data Corporation   = Corporation   { _cperson :: Person }
-- data NaturalPerson = NaturalPerson { _nperson :: Person }
-- makeClassy ''NaturalPerson
-- instance HasPerson NaturalPerson where
--   person = nperson

-- data Majority = Major | Minor
--   deriving (Eq)

-- data CapablePerson = CapablePerson
--   { _np               :: NaturalPerson
--   , _adulthood        :: Majority
--   , _parentOrGuardian :: Maybe CapablePerson
--   }
-- makeClassy ''CapablePerson
-- instance HasNaturalPerson CapablePerson where
--   naturalPerson = np

-- instance Validation CapablePerson where
--   isValid p =
--     p^.adulthood == Major
--     || maybe False isValid (p^.parentOrGuardian)
