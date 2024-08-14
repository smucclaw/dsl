{- | Parse L4 into a Generic MathLang lambda calculus (and thence to Meng's Math Lang AST)

If prototyping in GHCi / REPL, use these:

    :set -XTypeApplications
    :set -XDataKinds
    :set -XDeriveGeneric
    :set -XFlexibleContexts
    :set -XTypeFamilies
    import GHC.Generics

Yes, these are more high-powered than what I 'really' need in this file
(the only thing that requires them rn is ` (_Ctor @"Hornlike")`);
I had used them b/c I was prototyping and wasn't sure from the outset
how much would be needed to in effect parse the notoriously complicated L4 data structures.
-}

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, OverloadedLabels #-}
{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, DataKinds, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}

module LS.XPile.MathLang.GenericMathLang.ToGenericMathLang
  (toMathLangGen, getHornlikes, insertTypeDecls, expandHornlikes)
where

import AnyAll (BoolStructF (..))
import Data.Functor.Foldable (Recursive (..))
import Flow ((|>))
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST
-- TODO: Add import list
import LS.XPile.MathLang.GenericMathLang.TranslateL4
  ( ToLCError, runToLC, l4ToLCProgram )

import LS.Interpreter (Interpreted(..), expandClauses)
import LS.Rule (Rule(..), _Hornlike, _TypeDecl
                -- defaultHorn
                -- defaultHorn is useful for prototyping in the REPL
                )
import LS.Types (ParamText, MultiTerm, TypedMulti, MTExpr(MTT), BoolStructR, RelationalPredicate(RPMT))
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)

-- import Effectful
-- experimenting with Effectful.Error rn
-- see Mattermost slack for discussion of error handling and MonadValidate
import Optics (cosmosOf, toListOf, gplate, folded, (%), filteredBy, (^..))
-- import Data.Generics.Product.Types (types)
-- import Prettyprinter (Pretty)
-- import Data.String.Interpolate (__i)
import Text.Pretty.Simple (pShowNoColor)
import Data.String.Interpolate (__i)
-- import LS.Utils.TextUtils (int2Text, float2Text)
-- import Data.Foldable qualified as F (toList)

-- import LS.XPile.MathLang.UtilsLCReplDev


{-------------------------------------------------------------------------------
   Orchestrating and pretty printing
-------------------------------------------------------------------------------}
{- | Entry point for transforming the original L4 rules into generic lamda calculus
     Outputs either the LC repn or errors if there're errors.
Note:
* Not using qaHornsT
    b/c it looks like it'll be a lot more work and result in convoluted code,
    for very little benefit. Can always refactor down the road to use it if nec
* TODO re filtering for Hornlikes: Will want to work with type decls / record decls etc in the future
-}
toMathLangGen :: Interpreted -> (String, [String])
toMathLangGen l4i =
  let l4Hornlikes = getHornlikes l4i
--                      |> expandHornlikes l4i
                      |> insertTypeDecls l4i
  in case runToLC $ l4ToLCProgram l4Hornlikes of
    Left errors -> makeErrorOut errors
    Right lamCalcProgram -> (renderLC lamCalcProgram, [])

-- Utility functions for expanding rules and inserting TypeDecls into GIVENs
-- (Introduced in 2024-06, I hope we deal with global vs. local variables better later.)

-- | Extract all 'Hornlike' rules.
getHornlikes :: Interpreted -> [Rule]
getHornlikes l4i = l4i.origrules ^.. folded % cosmosOf (gplate @Rule) % filteredBy _Hornlike

-- | Extract all 'TypeDecl' rules.
getTypeDecls :: Interpreted -> [Rule]
getTypeDecls l4i = l4i.origrules ^.. folded % cosmosOf (gplate @Rule) % filteredBy _TypeDecl

-- | Insert all the type declarations as additional "givens" into
-- every horn-like rule.
--
-- Expects the passed rules to be horn-likes.
--
insertTypeDecls :: Interpreted -> [Rule] -> [Rule]
insertTypeDecls l4i = fmap (insertIntoRule allTypeDecls)
  where
    tdRules :: [Rule]
    tdRules = getTypeDecls l4i

    insertIntoRule :: Maybe ParamText -> Rule -> Rule
    insertIntoRule tds rl = rl {given = tds <> rl.given}

    rule2TypeDecls :: Rule -> [TypedMulti]
    rule2TypeDecls td = case (td.has, td.name) of
      ([], x:xs) -> [(x :| xs                    , td.super)]
      ([],   []) -> [(MTT "UnnamedTypeDecl" :| [], td.super)]
      (ts,    _) -> concatMap rule2TypeDecls ts

    allTypeDecls :: Maybe ParamText
    allTypeDecls = tdRules |> foldMap rule2TypeDecls |> nonEmpty

-- | This function expands the rules, i.e. inserting child rules into parent rules.
-- To do that, it calls 'expandClauses' from 'Interpreter'. Direct output of
-- 'expandClauses' leaves the child rules intact, which results in redundancy.
--
-- So the line @r.name `notElem` leaves@ removes the child rules that have
-- already been inserted into the parent.
--
-- TODO: Yongming has worries about `expandClauses` being lossy.
--
expandHornlikes :: Interpreted -> [Rule] -> [Rule]
expandHornlikes l4i hls =
  [ r {clauses = expandClauses l4i 1 (clauses r)}
  | r <- hls
  , r.name `notElem` leaves ]
  where
    leaves = foldMap getMTs allBS
    allBS = toListOf (gplate @BoolStructR) hls

    getMTs :: BoolStructR -> [MultiTerm]
    getMTs = cata \case
      LeafF (RPMT x@[MTT _]) -> [x]
      NotF x -> x
      AnyF _ xs -> mconcat xs
      AllF _ xs -> mconcat xs
      _ -> []

-- | Makes report for errors; can try using `diagnose` package / lib for this
makeErrorOut :: ToLCError -> (String, [String])
makeErrorOut _errors = ("not yet implemented", ["not yet implemented"])
  -- (makeErrorReport errors, map (T.unpack . stringifyToLCError) errors)
  --   where
  --     makeErrorReport errors =
  --       [__i|
  --           ERRORS FOUND:
  --           #{T.intercalate "\n" . map stringifyToLCError $ errors}
  --       |]
        -- Not 'DRY' b/c the error reporting is intended, and will be made, to be more sophisticated than just listing the errors


-- | 'Print' LC program to some sort of interchange format for subsequent serialization
renderLC :: LCProgram -> String
renderLC program =
  [__i|
    lcProgram = #{lcProgram}
    globalVars = #{globalVars}
    giveths = #{giveths}
    userFuns = #{userFuns}
  |]
  where
    lcProgram = pShowNoColor program.lcProgram
    userFuns = pShowNoColor program.userFuns
    giveths = pShowNoColor program.giveths
    globalVars = pShowNoColor program.globalVars
