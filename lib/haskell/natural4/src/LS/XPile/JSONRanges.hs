{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-| transpiler to JSON instances representing a sampling across the
space of possible classes, similar to QuickCheck. The JSON produced by
this transpiler is meant to be composed with runtime scenarios from
elsewhere in our codebase. -}

module LS.XPile.JSONRanges (asJSONRanges) where

import Data.Foldable (for_)
import Data.HashMap.Strict qualified as Map
import Data.List (groupBy, nub)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, maybeToList)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import LS.Rule (Interpreted (classtable))
import LS.Types
  ( EntityType,
    MTExpr (MTB, MTI, MTT),
    MultiTerm,
    ParamType (TOne),
    TypeSig (..),
    TypedClass,
    extendedAttributes,
    unCT,
  )
import LS.XPile.Logging
  ( XPileLog,
    XPileLogE,
    mutter,
    mutterd,
    mutterdhsf,
    pShowNoColorS,
    xpError,
    xpReturn,
  )
import Prettyprinter
  ( Doc,
    colon,
    comma,
    encloseSep,
    viaShow,
    (<+>),
  )
import Prettyprinter.Interpolate (di)
import Text.Pretty.Simple (pShowNoColor)
import Text.Regex.PCRE.Heavy qualified as PCRE
import Data.Traversable (for)

data Dimension lbl vals = Dimension lbl [vals]
  deriving (Show, Eq)
type Space     lbl vals = [Dimension lbl vals]

-- | similar to quickcheck, compute concrete variants of an input class,
-- building instances which differ along their Arbitrary attributes.
-- That is to say, if a class had an attribute which was a `colorEnum` (Red, Green, Blue), and another attribute which was a boolean,
-- this function would return six JSON instances representing every possible combination.
asJSONRanges :: Interpreted -> XPileLog (Doc ann)
asJSONRanges l4i = do
  mutterd 1 "asJSONRanges"
  let ct = Map.toList $ unCT $ classtable l4i

  classDimensions :: [(EntityType, Space EntityType MultiTerm)] <-
    for ct \(thisclass, (itypesig, ct)) -> do
      let ea1 = extendedAttributes (classtable l4i) thisclass
          ea2 = maybe [] (Map.toList . unCT) ea1
          ea3 = filter isArbitrary ea2

      mutterdhsf 2 ("class " <> show thisclass) pShowNoColorS (itypesig, ct)

      dims <- for ea3 \(attrName, ((explicitTS, _inferredTS), children)) -> do
        mutterdhsf 3 ("attribute: " <> T.unpack attrName) pShowNoColorS explicitTS
        pure $ Dimension attrName (variants explicitTS)
        -- [TODO] we do not recurse into attribute children; when we need to have proper support for nested records, add.

      mutterdhsf 3 "dimensions" pShowNoColorS dims
      pure (thisclass, dims)

  mutterd 1 "classDimensions cardinality"
  for_ classDimensions \(et, space) -> do
    mutterd 2 [i|class #{et} expecting #{product [ length values | Dimension lbl values <- space ]} points in configuration space, i.e. JSON instances|]
    for_ space \(Dimension lbl values) -> do
      mutterd 3 [i|#{length values} #{lbl}|]

  mutterdhsf 1 "classDimensions" pShowNoColorS classDimensions

  curlyList <$> for classDimensions \(className, dims) -> do
    mutterdhsf 2 [i|dims for #{className}|] pShowNoColorS dims

    let points = take 1000 $ extend dims []
    mutterdhsf 2 [i|points for #{className}|] pShowNoColorS points

    pure $ [di|#{className}: |] <+>
      curlyList
        [ [di|#{show i}: |] <+> curlyList [[di|#{k}: #{quickPretty v}|] | (k, v) <- point]
        | (point, i) <- zip points [1..]
        ]
  where
    isArbitrary :: (EntityType, TypedClass) -> Bool
    isArbitrary (_, ((Just (InlineEnum _    _ ), _), _)) = True
    isArbitrary (_, ((Just (SimpleType TOne st), _), _)) = isBool st
    isArbitrary _                                        = False

    isBool = (PCRE.≈ [PCRE.re|^(b|B)ool(ean)?$|]) 

    curlyList = encloseSep "{" "}" comma

    variants :: Maybe TypeSig -> [MultiTerm]
    variants Nothing = []
    variants (Just (SimpleType TOne et))
      | isBool et = [[MTB True], [MTB False]]
      | otherwise = []
    variants (Just (InlineEnum _pt  enums)) = [ pure ptmt'
                                              | (ptmt, pttype) <- NE.toList enums
                                              , ptmt' <- NE.toList ptmt
                                              ]
    variants (Just (SimpleType _pt  et)) = [] -- in future when we are cleverer we will know how to offer concrete variants of list and maybe types

    -- any decent Guild Navigator knows how to fold space.
    -- it's easy. Given a concrete extensional space of points, and an abstract Space of dimensions, grab the first abstract dimension off the rank and cross-product it with all remaining dimensions, until there are no dimensions left and all you have is points.

    -- consider if we aren't reinventing Data.Row row-types here
    extend :: Space EntityType a -> Points EntityType a -> Points EntityType a
    extend [] pts = pts
    extend ((Dimension dimKey dimVals):dims) [] = extend dims [ [(dimKey, dimVal)] | dimVal <- dimVals ]
    extend ((Dimension dimKey dimVals):dims) pts = extend dims
                                                   [ (dimKey, dimVal) : pt
                                                   | pt <- pts
                                                   , dimVal <- dimVals
                                                   ]

quickPretty :: MultiTerm -> Doc ann
quickPretty [MTB True] = "true"
quickPretty [MTB False] = "false"
quickPretty [MTI myint] = viaShow myint
quickPretty [MTT str]   = viaShow str
quickPretty x           = error [i|quickPretty error in pattern match: unhandled case: #{x}|]

type Point  lbl val = [(lbl, val)]
type Points lbl val = [Point lbl val]
