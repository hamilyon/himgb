-- quantile.hs

module Quantile
where
-- import Data.List
data Sections = Sections {
    byPercent :: [Double]
}

quantilesBp :: Sections -> [Double] -> [Double]
quantilesBp s input = belowPercentileInSortedList input s

belowPercentileInSortedList :: [Double] -> Sections -> [Double]

belowPercentileInSortedList list percentile = map (list !!) (percentileToIndex list percentile)

toIndex :: Double -> Double -> Int
toIndex len percentile = if percentile == 1 then (floor :: Double -> Int) (len - 1) else (floor :: Double -> Int) (len * percentile)

percentileToIndex :: [Double] -> Sections -> [Int]
percentileToIndex []    sections   = []
percentileToIndex from  (Sections [])    = []
percentileToIndex from sections = 
    let len = ((fromIntegral . length $ from) :: Double ) in
    map (toIndex len) (byPercent sections)

quantiles :: [Double] -> [Double]
quantiles =  quantilesBp (Sections[0, 0.25, 0.5, 0.75, 1])



