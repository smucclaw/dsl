{-# OPTIONS_GHC -W #-}
-- {-# OPTIONS_GHC -foptimal-applicative-do #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures, AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}


{-
Parse L4 into a Generic MathLang lambda calculus (and thence to Meng's Math Lang AST)
-}
module LS.XPile.MathLang.L4ToGenericMathLang where
-- TODO: Add export list

import Data.Text qualified as T
import LS.Utils.TextUtils (int2Text, float2Text)

import LS.XPile.MathLang.GenericMathLangAST
-- TODO: Add import list

import LS.XPile.MathLang.Logging (LogCfg)

import Effectful
import Effectful.Error.Dynamic 
import Effectful.Reader.Dynamic 


-- experimenting with Effectful.Error rn; will switch over to Control.Monad.Validate later
-- import Control.Monad.Validate
--   ( MonadValidate (..)
--     , Validate
--     , refute
--     )
-- import Optics
-- import Data.Generics.Product.Types (types)
-- import Data.String.Interpolate (i)

-- import AnyAll qualified as AA
-- import LS.Types qualified as L4
-- import LS.Types (RelationalPredicate(..), RPRel(..), MTExpr(..))
import LS.Interpreter (qaHornsT)
import LS.Rule as L4 (Rule(..), Interpreted(..), extractMTExprs)

-- import Data.HashSet qualified as HS
-- import Data.Hashable (Hashable)
-- import GHC.Generics (Generic)

-- import Data.String (IsString)
-- import Prettyprinter (Pretty)


-- TODO: Will upstream this sort of xpiler config record type def in the future
data CompCfg = MkCompCfg 
  { logCfg :: LogCfg
--   , features :: CompFeatureConfig
  }


data LCEnv = MkEnv {
  
}

data ToLC a

instance Functor ToLC
instance Applicative ToLC
instance Monad ToLC



-------------------------

{-------------------------
the patterns in the head:
-------------------------
* RPMT MultiTerm

    * RPMT [ MTT "case 1 qualifies" ]
    * RPMT
        [ MTT "ind"
        , MTT "qualifies a la case 3"
        ]

* RPConstraint  MultiTerm RPRel MultiTerm 
  
    [ MTT "incomeTaxRate" ] RPis [ MTF 1.0e-2 ]

* RPBoolStructR MultiTerm RPRel BoolStructR

    Hornlike
        { name =
            [ MTT "meets the property eligibility criteria for GSTV-Cash" ]
        , super = Nothing
        , keyword = Means
        , given = Nothing
        , giveth = Nothing
        , upon = Nothing
        , clauses =
            [ HC
                { hHead = RPBoolStructR
                    [ MTT "meets the property eligibility criteria for GSTV-Cash" ] RPis
                    ( Any Nothing
                        [ Not
                            ( Leaf
                                ( RPMT
                                    [ MTT "owns more than one property" ]
                                )
                            )
                        , Leaf
                            ( RPMT
                                [ MTT "owns 2 or more HDB flats and no other property" ]
                            )
                        ]
                    )
                , hBody = Nothing
                }
            ]
* RPnary RPRel [RelationalPredicate] --- potentially recursive 

      RPnary RPis
        [ RPMT
            [ MTT "income tax component" ]
        , RPnary RPproduct
            [ RPMT
                [ MTT "annualIncome" ]
            , RPMT
                [ MTT "incomeTaxRate" ]
            ]
        ]

-----------------------------------
Another way of carving up the space
-----------------------------------


---- Set Var 

-------- Set Var to True If ...
    A hc with no body, and where RPis in head

    , HC
        { hHead = RPnary RPis
            [ RPMT
                [ MTT "taxesPayableAlive" ]
            , RPnary RPsum
                [ RPMT
                    [ MTT "income tax component" ]
                , RPMT
                    [ MTT "asset tax component" ]
                ]
            ]
        , hBody = Nothing
        }



---------------------

-}


{- for later 
newtype ToGenMathLang a = ToGenMathLang (Eff '[Reader (undefined), Error undefined ] a)
  deriving newtype (Functor, Applicative, Monad)

-}

type Analyzed = Interpreted
toMathLangGen :: Analyzed -> (String, [String])
toMathLangGen l4i = ("NotYetImplemented", []) 
