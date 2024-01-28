{- | Parse L4 into a Generic MathLang lambda calculus (and thence to Meng's Math Lang AST)

If prototyping in GHCi / REPL, use these:

    :set -XTypeApplications
    :set -XDataKinds
    :set -XDeriveGeneric
    :set -XFlexibleContexts
    :set -XTypeFamilies
    import GHC.Generics
-}

{-# OPTIONS_GHC -W #-}
-- {-# OPTIONS_GHC -foptimal-applicative-do #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, OverloadedLabels #-}
{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, DataKinds, TypeFamilies #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module LS.XPile.MathLang.GenericMathLang.ToGenericMathLang where
-- TODO: Add export list

import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST
-- TODO: Add import list
import LS.XPile.MathLang.Logging (LogConfig)
import LS.XPile.MathLang.GenericMathLang.TranslateL4

-- import AnyAll qualified as AA
-- import LS.Types qualified as L4
import LS.Types as L4 (RelationalPredicate(..), RPRel(..), MTExpr(..), EntityType, HornClause (..))
-- import LS.Interpreter (qaHornsT)
import LS.Rule (Interpreted(..), extractMTExprs, getGivenWithSimpleType,
                defaultHorn)
import LS.Rule qualified as L4 (Rule(..))

import Effectful
import Effectful.Error.Dynamic
import Effectful.Reader.Static (runReader, Reader)
import Effectful.State.Static.Shared (State, runState)
-- experimenting with Effectful.Error rn; will switch over to Control.Monad.Validate later
-- import Control.Monad.Validate
--   ( MonadValidate (..)
--     , Validate
--     , refute
--     )
import Data.HashSet qualified as HS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Optics
import Data.Generics.Sum.Constructors
-- import Data.Generics.Product.Types (types)
-- import Prettyprinter (Pretty)
import Data.String.Interpolate (__i)
import Data.String (IsString)
import Data.Text qualified as T
-- import LS.Utils.TextUtils (int2Text, float2Text)
import Data.Foldable qualified as F (toList)

import LS.Types (TypeSig(..), TypedMulti)
import LS.XPile.MathLang.UtilsLCReplDev


{-------------------------------------------------------------------------------
   Orchestrating and pretty printing
-------------------------------------------------------------------------------}
type Analyzed = Interpreted

-- | placeholder so natural L4 won't crash while I prototype 
toMathLangGen :: Analyzed -> (String, [String])
toMathLangGen _ = ("not yet implemented", [])

{- | Entry point for transforming the original L4 rules into generic lamda calculus 
     Outputs either the LC repn or errors if there're errors.
Note:
* Not using qaHornsT 
    b/c it looks like it'll be a lot more work and result in convoluted code, 
    for very little benefit. Can always refactor down the road to use it if nec 
* TODO re filtering for Hornlikes: Will want to work with type decls / record decls etc in the future
-}
toMathLangGen' :: Analyzed -> (String, [String])
toMathLangGen' l4a =
  let l4Hornlikes = l4a.origrules ^.. folded % filteredBy (_Ctor @"Hornlike")
  in case l4ToLCProgram l4Hornlikes of
    Left errors -> makeErrorOut errors
    Right lamCalcProgram -> (printLC lamCalcProgram, [])

-- runAndValidate = undefined

makeErrorOut :: ToLCError -> (String, [String])
-- makeErrorOut :: [ToLCError] -> (String, [String])
makeErrorOut errors = ("not yet implemented", ["not yet implemented"])
  -- (makeErrorReport errors, map (T.unpack . stringifyToLCError) errors)
  --   where
  --     makeErrorReport errors =
  --       [__i|
  --           ERRORS FOUND:
  --           #{T.intercalate "\n" . map stringifyToLCError $ errors}
  --       |]
        -- Not 'DRY' b/c the error reporting is intended, and will be made, to be more sophisticated than just listing the errors


-- | Print LC program to some sort of interchange format for subsequent serialization
printLC :: LCProgram -> String
printLC program = "Not Yet Implemented"