-- |
--
-- Simple histograms
module Graphics.Plotly.Histogram where

import Data.Aeson (toJSON)
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Graphics.Plotly.Base hiding (sort)
import Lens.Micro

-- | build a histogram with a given binsize
histogram ::
  -- | number of bins
  Int ->
  -- | the individual observations
  [Double] ->
  Trace
histogram nbins pts =
  let (lo, hi) = (minimum pts, maximum pts)
      binSize = (hi - lo) / realToFrac nbins
      binToX :: Int -> Double
      binToX binN = realToFrac binN * binSize + lo
      binMap :: [(Int, Int)]
      binMap = getBinMap lo binSize pts
   in bars & x ?~ map (toJSON . binToX . fst) binMap & y ?~ map (toJSON . snd) binMap

histMany :: Int -> [(Text, [Double])] -> [Trace]
histMany nbins hdata =
  let allPts = concatMap snd hdata
      (lo, hi) = (minimum allPts, maximum allPts)
      binSize = (hi - lo) / realToFrac nbins
      binToX :: Int -> Double
      binToX binN = realToFrac binN * binSize + lo
      getTrace (nm, pts) =
        let binMap = getBinMap lo binSize pts
         in bars
              & x ?~ map (toJSON . binToX . fst) binMap
              & y ?~ map (toJSON . snd) binMap
              & name ?~ nm
   in map getTrace hdata

goFill :: [(Int, Int)] -> [(Int, Int)]
goFill (car@(bin1, _) : cdr@((bin2, _) : _))
  | bin2 == bin1 + 1 = car : goFill cdr
  | otherwise = car : goFill ((bin1 + 1, 0) : cdr)
goFill l = l

getBinMap :: Double -> Double -> [Double] -> [(Int, Int)]
getBinMap lo binSize pts =
  let binf :: Double -> Int
      binf xv = floor $ (xv - lo) / binSize
      bins = NE.group $ sort $ map binf pts
      binMap :: [(Int, Int)]
      binMap = goFill $ map (\is -> (NE.head is, length is)) bins
   in binMap
