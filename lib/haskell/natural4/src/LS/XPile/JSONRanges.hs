{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-| transpiler to JSON instances representing a sampling across the
space of possible classes, similar to QuickCheck. The JSON produced by
this transpiler is meant to be composed with runtime scenarios from
elsewhere in our codebase. -}

module LS.XPile.JSONRanges where

import Control.Monad (forM_)
import Data.HashMap.Strict qualified as Map
import Data.List (groupBy, nub)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Maybe (maybeToList, fromMaybe)
import LS
import LS.XPile.Logging
  ( XPileLog,
    XPileLogE,
    mutter, mutterd, mutterdhsf,
    xpError,
    xpReturn,
    pShowNoColorS,
  )
import Prettyprinter
import Text.Pretty.Simple (pShowNoColor)

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
  let ct = unCT $ classtable l4i

  classDimensions :: [(EntityType, Space EntityType MultiTerm)] <-
    sequence
    [ do
        mutterdhsf 2 ("class " <> show thisclass) pShowNoColorS (itypesig, ct)
        dims <- sequence
          [ do
              mutterdhsf 3 ("attribute: " <> T.unpack attrName) pShowNoColorS explicitTS
              return $ Dimension attrName (variants explicitTS)
              -- [TODO] we do not recurse into attribute children; when we need to have proper support for nested records, add.
          | (attrName, ((explicitTS, _inferredTS), children)) <- ea3
          ]
        mutterdhsf 3 "dimensions" pShowNoColorS dims
        return (thisclass, dims)
    | (thisclass, (itypesig, ct)) <- Map.toList ct
    , let ea1 = extendedAttributes (classtable l4i) thisclass
          ea2 = fromMaybe [] $ Map.toList . unCT <$> ea1
          ea3 = filter isArbitrary ea2
    ]

  mutterd 1 "classDimensions cardinality"
  forM_ classDimensions $ \(et, space) -> do
    mutterd 2 ("class " <> show et <> " expecting " <>
               show (product [ length v | Dimension lbl values <- space, v <- values ]) )
    forM_ space $ \(Dimension lbl values) -> do
      mutterd 3 (show lbl <> " " <> show (length values))
      
  mutterdhsf 1 "classDimensions" pShowNoColorS classDimensions

  curlyList <$> sequence
    [ do
        mutterdhsf 2 ("dims for " <> show className) pShowNoColorS dims

        let points = take 100 $ extend dims []
        mutterdhsf 2 ("points for " <> show className) pShowNoColorS points

        return $ (viaShow className) <> colon <+>
          (curlyList 
            [ viaShow i <> colon <+> curlyList [ viaShow k <> colon <+> viaShow v | (k, v) <- point ]
            | (point, i) <- zip points [1..]
            ]
          )
                                      
    | (className, dims) <- classDimensions
    ]
  where
    isArbitrary :: (EntityType, TypedClass) -> Bool
    isArbitrary (_, ((Just (InlineEnum _    _ ), _), _)) = True
    isArbitrary (_, ((Just (SimpleType TOne st), _), _)) = isBool st
    isArbitrary _                                        = False

    isBool "Boolean" = True
    isBool "Bool"    = True
    isBool "boolean" = True
    isBool "bool"    = True
    isBool _         = False

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
                  

type Point  lbl val = [(lbl, val)]
type Points lbl val = [Point lbl val]
