{-# LANGUAGE NoMonomorphismRestriction #-}
-- Testquantile.hs

module Testquantile

where 
import Quantile
import Test.HUnit
import Test.QuickCheck

import System.Random (Random(..), RandomGen)
import Test.QuickCheck

main =
    do
        putStrLn "quickcheck start"
--         quickCheck propSummHolds
        quickCheckWith myStdArgs propIndexPositive
        quickCheckWith myStdArgs ( propInBounds :: [Double] -> Property )
        putStrLn "quickcheck end"
        counts <- runTestTT $ TestList [testEmpty, 
                                        testBelowPercentileInSortedList,
                                        testBelowHigh,
                                        testBelowLowPercentile,
                                        testStandart]
        putStrLn . show $ counts


testEmpty = TestCase $ assertEqual
    "zeros from zero input"  []  (quantiles [] :: [Double])

testStandart = TestCase $ assertEqual
    "standart case"  [91.0,120.0,136.0,140.0,180.0]
        (quantiles [91, 100, 110, 120, 120, 121, 130, 135, 136, 136, 136, 137, 137, 138, 140, 151, 159, 170, 180])

propIndexPositive :: [Double] -> Property
propIndexPositive x =  forAll ratio $ \y -> all (>= 0) (percentileToIndex x $ singlePercentile y)

singlePercentile = Sections . (:[])

propInBounds :: [Double] -> Property
propInBounds x = forAll ratio $ \y -> all (<= (length x - 1)) (percentileToIndex x $ singlePercentile y)

testBelowPercentileInSortedList = TestCase $ 
                [7]      @=? (belowPercentileInSortedList [1..7] $ singlePercentile 0.9)

testBelowHigh = TestCase $ 
                [9000]    @=? (belowPercentileInSortedList [1,1,1,1,1, 1 , 7 ,9000] $ singlePercentile 1.0)


testBelowLowPercentile = TestCase $ 
                [1]       @=? (belowPercentileInSortedList [1,1,1,1,1, 1 , 7 ,9000] $ singlePercentile 0.1)

testComplex = TestCase $
    [136, 159, 180, 180, 180] @=? 
    (quantilesBp (Sections[0.5, 0.85, 0.95, 0.99, 1]) [91, 100, 110, 120, 120, 121, 130, 135, 136, 136, 136, 137, 137, 138, 140, 151, 159, 170, 180])

ratio :: Gen Double
ratio = choose (0, 1)

myStdArgs = Args Nothing 1000 5000 1000 True
