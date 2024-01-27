{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds, KindSignatures, AllowAmbiguousTypes, ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, DataKinds, GADTs #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}


{- VERY WIP! more of a stub / outline rn -}
module LS.XPile.MathLang.Logging where
-- TODO: Add export list

import Data.Time qualified as Time
import Log (LogT, Logger, defaultLogLevel, object, (.=))
import Log qualified
import Log.Backend.StandardOutput qualified as Log

-- will uncomment when get to implementing this
-- import Effectful
-- import Effectful.Error.Dynamic 
-- import Effectful.Reader.Dynamic 

-- import Optics ((^.))
-- import Data.Generics.Product.Types (types)
-- import Data.String.Interpolate (i)

-- import Data.HashSet qualified as HS
-- import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- import Data.String (IsString)
-- import Prettyprinter (Pretty)



-- TODO: upstream logging stuff later
data LoggingDestination
  = -- | Logs are printed on the standard output
    StdOut
  | -- | Logs are printed on the standard output in JSON format
    Json
  | -- | Logs are sent to a file as JSON
    JSONFile
  deriving (Show, Generic)


data LogCfg = MkLogCfg 
  { logDest :: LoggingDestination
  }
  deriving stock (Show)

