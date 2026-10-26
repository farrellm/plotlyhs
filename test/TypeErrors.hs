{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- | Misuses of subplot grids that must not compile. This module is compiled
-- with deferred type errors, so each misuse compiles to code that throws
-- its type error; each test forces one and checks the message.
module TypeErrors (typeErrorTests) where

import Control.Exception (TypeError (..), evaluate, try)
import Data.Aeson (Value, encode, toJSON)
import Data.ByteString.Lazy qualified as BL
import Data.List (isInfixOf)
import Graphics.Plotly
import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit

typeErrorTests :: [TestTree]
typeErrorTests =
  [ rejects "a trace on the wrong kind of subplot" "a pie trace needs a domain subplot, but cell (1, 1) is xy" misuse1,
    rejects "a cell outside the grid" "cell (1, 3) is outside the 2 by 2 grid" misuse2,
    rejects "row 0" "cell (0, 1) is outside the 1 by 1 grid" misuse3,
    rejects "a blank cell" "cell (1, 2) of the grid is Blank" misuse4,
    rejects "a secondary y axis that is not there" "cell (1, 1) has no secondary y axis" misuse5,
    rejects "a secondary y axis for the wrong kind of trace" "a pie trace needs a domain subplot, but cell (1, 1) is xy" misuse6,
    rejects "a figure with no grid" "NoGrid" misuse7,
    rejects "no rows" "a grid needs at least one row" misuse8,
    rejects "no columns" "a grid needs at least one column" misuse9,
    rejects "ragged rows" "row 2 of the grid has 1 cells, but row 1 has 2" misuse10,
    rejects "a trace with no subplot in a cell" "a splom trace makes its own subplots" misuse11,
    rejects "a span past the edge" "a span reaches (1, 2), outside the 1 by 1 grid" misuse12,
    rejects "overlapping spans" "two cells of the grid cover (2, 2)" misuse13,
    rejects "a span over a subplot" "two cells of the grid cover (1, 2)" misuse14,
    rejects "an empty span" "a span must cover at least one row" misuse15,
    rejects "a span of nothing" "a span must hold a subplot" misuse16,
    rejects "selecting a row outside the grid" "row 3 is outside the 2 by 2 grid" misuse17,
    rejects "selecting a blank cell" "cell (1, 2) of the grid is Blank" misuse18,
    rejects "a runtime cell of the wrong kind" "a pie trace needs a domain subplot, but the cell is xy" misuse19
  ]

-- Each misuse is a binding of its own: a deferred type error is thrown
-- where its binding is evaluated.

misuse1 :: Value
misuse1 = toJSON $ makeSubplots @(Uniform 1 1 XY) subplotOptions & addTraceAt @1 @1 pie

misuse2 :: Value
misuse2 = toJSON $ makeSubplots @(Uniform 2 2 XY) subplotOptions & addTraceAt @1 @3 scatter

misuse3 :: Value
misuse3 = toJSON $ makeSubplots @(Uniform 1 1 XY) subplotOptions & addTraceAt @0 @1 scatter

misuse4 :: Value
misuse4 = toJSON $ makeSubplots @'[ '[ 'Span 1 2 XY, 'Blank]] subplotOptions & addTraceAt @1 @2 scatter

misuse5 :: Value
misuse5 = toJSON $ makeSubplots @(Uniform 1 1 XY) subplotOptions & addTraceSecondaryY @1 @1 scatter

misuse6 :: Value
misuse6 = toJSON $ makeSubplots @'[ '[PlotY2]] subplotOptions & addTraceSecondaryY @1 @1 pie

misuse7 :: Value
misuse7 = toJSON $ figure [] & addTraceAt @1 @1 @Scatter @(Uniform 1 1 XY) scatter

misuse8 :: Value
misuse8 = toJSON $ makeSubplots @'[] subplotOptions

misuse9 :: Value
misuse9 = toJSON $ makeSubplots @'[ '[]] subplotOptions

misuse10 :: Value
misuse10 = toJSON $ makeSubplots @'[ '[XY, XY], '[XY]] subplotOptions

misuse11 :: Value
misuse11 = toJSON $ makeSubplots @(Uniform 1 1 XY) subplotOptions & addTraceAt @1 @1 splom

misuse12 :: Value
misuse12 = toJSON $ makeSubplots @'[ '[ 'Span 1 2 XY]] subplotOptions

misuse13 :: Value
misuse13 = toJSON $ makeSubplots @'[ '[XY, 'Span 2 1 XY], '[ 'Span 1 2 XY, 'Blank]] subplotOptions

misuse14 :: Value
misuse14 = toJSON $ makeSubplots @'[ '[ 'Span 1 2 XY, XY]] subplotOptions

misuse15 :: Value
misuse15 = toJSON $ makeSubplots @'[ '[ 'Span 0 1 XY]] subplotOptions

misuse16 :: Value
misuse16 = toJSON $ makeSubplots @'[ '[ 'Span 1 1 'Blank]] subplotOptions

misuse17 :: Value
misuse17 = toJSON $ makeSubplots @(Uniform 2 2 XY) subplotOptions & updateXAxes (inRow @3) (#visible ?~ True)

misuse18 :: Value
misuse18 = toJSON $ makeSubplots @'[ '[ 'Span 1 2 XY, 'Blank]] subplotOptions & updateXAxes (atCell @1 @2) (#visible ?~ True)

misuse19 :: Value
misuse19 = toJSON $ withUniformSubplots 1 1 subplotOptions (\f cells -> forgetGrid (foldr (`addTraceIn` pie) f (concat cells)))

-- | The figure, whose JSON this is, does not compile: forcing it throws a
-- type error that says this.
rejects :: String -> String -> Value -> TestTree
rejects name expected v = testCase name $ do
  r <- try (evaluate (BL.length (encode v)))
  case r of
    Left (TypeError msg)
      | expected `isInfixOf` msg -> pure ()
      | otherwise -> assertFailure ("a different type error:\n" <> msg)
    Right _ -> assertFailure "it compiled"
