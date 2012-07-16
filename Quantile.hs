{-# LANGUAGE NoMonomorphismRestriction #-}
-- quantile.hs

module Quantile
where

import Data.List(sort)
data Sections = Sections {
    byPercent :: [Double]
}

quantilesBp :: Sections -> [a] -> [a]
quantilesBp s input = belowPercentileInSortedList input s

belowPercentileInSortedList :: [a] -> Sections -> [a]

belowPercentileInSortedList list percentile = map (list !!) (percentileToIndex list percentile)

toIndex :: Double -> Double -> Int
toIndex len percentile = if percentile == 1 then (floor :: Double -> Int) (len - 1) else (floor :: Double -> Int) (len * percentile)

percentileToIndex :: [a] -> Sections -> [Int]
percentileToIndex []    sections   = []
percentileToIndex from  (Sections [])    = []
percentileToIndex from sections = 
    let len = ((fromIntegral . length $ from) :: Double ) in
    map (toIndex len) (byPercent sections)

quantiles :: Ord a => [a] -> [a]
quantiles =  (quantilesBp (Sections[0, 0.25, 0.5, 0.75, 1])) . sort

mean xs = sum (map fromIntegral xs) / fromIntegral (length xs)

-- helper
printStats words = do
    putStrLn $ "quantiles = " ++ (show . quantiles) (map length words)
    putStrLn $ "mean = " ++ (show . mean) (map length words)
    putStrLn $ "length = " ++ (show . length) words


