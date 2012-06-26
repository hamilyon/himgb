-- quantile.hs

module Quantile
where
-- import Data.List
data Sections = Sections {
    byPercent :: [Double]
}

quantilesBp :: Sections -> [Double] -> [Double]
quantilesBp s input = reverse (quantilesBp' s [] input)

quantilesBp' :: Sections -> [Double] -> [Double] -> [Double]
quantilesBp' (Sections []) soFar input = soFar
quantilesBp' (Sections (x:xs)) soFar input = quantilesBp' (Sections xs ) (belowPercentileInSortedList input x : soFar) input

belowPercentileInSortedList :: [Double] -> Double -> Double
belowPercentileInSortedList [] percentile = 0
belowPercentileInSortedList list percentile = list !! (indexPercentileInSortedList list percentile)

indexPercentileInSortedList list percentile = if percentile == 1 then (head . reverse) list else floor (((fromIntegral . length $ list) :: Double ) * percentile)

quantiles :: [Double] -> [Double]
quantiles [] = [0,0,0,0,0]
-- quantiles list = quantilesBp 


