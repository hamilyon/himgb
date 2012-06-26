-- Testquantile.hs

module Testquantile

where 
import Quantile
import Test.HUnit
import Test.QuickCheck

main =
    do
        putStrLn "quickcheck start"
--         quickCheck propSummHolds
        quickCheck propIndexPositive
        quickCheck ( propInBounds :: [Double] -> Property )
        putStrLn "quickcheck end"
        counts <- runTestTT $ TestList [testEmpty, 
                                        testBelowPercentileInSortedList,
                                        testBelowHigh,
                                        testBelowLowPercentile]
        putStrLn . show $ counts


testEmpty = TestCase $ assertEqual
    "zeros from zero input"  [0,0,0,0,0]  (quantiles [])

-- propSummHolds :: [Double] -> Bool
-- propSummHolds x =  (head $ quantilesBp (Sections [1]) x) == sum x

propIndexPositive :: [Double] -> Double -> Bool
propIndexPositive x y = if y<0 then True else (indexPercentileInSortedList x y) >= 0

propInBounds x = forAll ratio $ \y -> (indexPercentileInSortedList x y) <= (length x - 1)

testBelowPercentileInSortedList = TestCase $ 
                7      @=? (belowPercentileInSortedList [1..7] 0.9)

testBelowHigh = TestCase $ 
                9000    @=? (belowPercentileInSortedList [1,1,1,1,1, 1 , 7 ,9000] 1.0)


testBelowLowPercentile = TestCase $ 
                1       @=? (belowPercentileInSortedList [1,1,1,1,1, 1 , 7 ,9000] 0.1)

testComplex = TestCase $
    [136, 159, 180, 180, 180] @=? 
    (quantilesBp (Sections[0.5, 0.85, 0.95, 0.99, 1]) [91, 100, 110, 120, 120, 121, 130, 135, 136, 136, 136, 137, 137, 138, 140, 151, 159, 170, 180])

ratio :: Gen Double
ratio = choose (0, 1)





