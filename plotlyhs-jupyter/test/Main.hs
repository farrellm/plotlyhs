{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text qualified as T
import Graphics.Plotly
import Graphics.Plotly.Jupyter
import IHaskell.Display (Display (..), DisplayData (..), IHaskellDisplay (..))
import IHaskell.IPython.Types (MimeType (..))
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain . testGroup "plotlyhs-jupyter" $
    [ testCase "a figure is shown as HTML, with a plain-text fallback" $ do
        Display ds <- display fig
        map mime ds @?= [PlainText, MimeHtml],
      testCase "the HTML draws the figure" $ do
        let h = notebookHtml "plot" fig
        assertBool "no div" ("<div id=\"plot\"" `T.isInfixOf` h)
        assertBool "no newPlot" ("Plotly.newPlot(\"plot\"" `T.isInfixOf` h)
        assertBool "no loader" ("document.createElement(\"script\")" `T.isInfixOf` h),
      testCase "showing a figure twice gives two ids" $ do
        a <- htmlOf <$> display fig
        b <- htmlOf <$> display fig
        assertBool "same HTML" (a /= b),
      testCase "a trace is shown as a figure of its own" $ do
        Display ds <- display (scatter :: Trace Scatter)
        map mime ds @?= [PlainText, MimeHtml]
    ]
  where
    fig = figure [someTrace scatter]
    mime (DisplayData m _) = m
    htmlOf (Display ds) = [t | DisplayData MimeHtml t <- ds]
    htmlOf _ = []
