import Data.List.Split
import System.Random
import Data.Map (adjust, unionWith, fromListWith, Map, lookup, keys, fromList, singleton, toList)

import Data.Set (union, Set, intersection)
import qualified Data.Set as S (fromList, size)
import Data.Maybe (catMaybes)
import Criterion.Main (defaultMain, bench, whnf, bgroup)

import System.IO (readFile)
{-import System.IO.Strict (readFile)-}

u = undefined
{-(defaultMain, bench, whnf, bgroup) = (u,u,u,u)-}

rev = u

data RwGraph = RwGraph {
    transposed :: Map Int (Map Int Double)
    {-transposed :: Map Int (Set Int)-}
} deriving Show
-- import qualified Data.Vector.Random.Mersenne as G

bmain = defaultMain [
        bgroup "randomWalk" [
              bench "100/100/1"      $ whnf rev [1..3000]
            , bench "140000/50/3"    $ whnf rev [1..300000]
            , bench "14000000/500/3" $ whnf rev [1..30000]
            ]
        ]
rmain = do
    let g = randomGraph 100 (mkStdGen 359353)

    print $ show $ g
    print $ show $ singleRandomWalk g (mkStdGen 100) 1 2 (\_ -> True)

main :: IO ()
main =  do
    results <- validate
    print $ show $ results


randomWalksBench :: Int -> Int -> Int -> [Int]
randomWalksBench 0 _ _ = error "trying to simulate 0-sized graph"
randomWalksBench gSize nWalkers nSteps = randomWalks g (mkStdGen 100) (((foldl1 min) . keys . transposed) g) nSteps (\_ -> True) nWalkers
                                            where g = randomGraph gSize (mkStdGen 100)

randomWalks :: RwGraph -> StdGen -> Int -> Int -> (Int -> Bool) -> Int -> [Int]
randomWalks g gen start steps isFinish count = snd (walks g gen start steps isFinish count [])
    where walks g gen start steps isFinish 0     soFar = (gen, soFar)
          walks g gen start steps isFinish count soFar =
           walks g newGen start steps isFinish (count - 1) (soFar ++ [newRes])
            where (newGen, newRes) = singleRandomWalk g gen start steps isFinish

singleRandomWalk :: RwGraph -> StdGen -> Int -> Int -> (Int -> Bool) -> (StdGen, Int)
singleRandomWalk graph stdGen            start  0        isFinish = (stdGen, start)
singleRandomWalk graph stdGen            start  minSteps isFinish =
    singleRandomWalk graph newGen newStart (minSteps - 1) isFinish
    where
        newStart = case Data.Map.lookup start (transposed graph) of
                    Nothing -> start
                    Just newPos -> list !! index
                        where
                        (index, _) = randomR (0, length list) stdGen
                        list = (keys newPos)
        (_, newGen) = random stdGen :: (Int, StdGen)

-- TODO: generate clustered graph
randomGraph :: Int -> StdGen -> RwGraph
randomGraph size stdGen =
    RwGraph (fromListOf2Lists randomInput)
    where
        randomInput = chunksOf 2 $ take (2*size) (randomRs (0,approxNumberOfNodes) stdGen :: [Int])
        approxNumberOfNodes  = (ceiling . (** 0.5) . (* 7)) (fromIntegral size) :: Int
        fromListOf2Lists = (fromListWith (unionWith (+))) . (Prelude.map toTupleSet)

fromListWithSum = fromListWith (unionWith (+))

prepareTrainData  =  prepareData "./hand_execution.txt" 3
prepareTestData   =  return [2,5,6]

toStachastic :: [Int] -> [(Int, Double)]
toStachastic l = map (\x -> (x, m)) l
                where m = (1.0 / (fromIntegral $ length l) :: Double)

-- TODO: generate clustered graph
toGraph :: [(Int,Int,Double)] -> RwGraph
toGraph rawData =
    RwGraph (fromListOfTuples rawData)
    where
        fromListOfTuples = (fromListWith (unionWith (+))) . (Prelude.map (\(p1, p2, weight) -> (p1, singleton p2 weight)))

prepareData :: String -> Int -> IO([(Int,Int,Double)])
prepareData fileName chunk = do
    file <- readFile fileName

    let split = Data.List.Split.wordsBy (\c -> not $ elem c "1234567890%") ((splitOn "---" file ) !! chunk)
    let by3s = chunksOf 3 split
    let typed = map ((\(x,y,z) -> (r y,r x,readDouble z)) . toTuple) by3s
    return typed
    where
        r = read :: String -> Int
        toTuple [a,b,c] = (a,b,c)
        readDouble = toDouble . readRational
        readRational = read :: String -> Rational
        toDouble x = fromRational x :: Double

generateGuess :: RwGraph -> [Int]  -> IO [Int]
generateGuess g knownVector = return result
    where result = iterativeProduceWith
                    (dotProduce)
                    0.85
                    (transposed g)
                    kV
                    (toStachastic [1..dimensions])
                    20
                    (1 / fromIntegral dimensions)
                    where 
                        kV = (toStachastic knownVector)
                        dimensions = foldl max 0 (keys (transposed g)) :: Int

iterativeProduceWith :: ( Map Int (Map Int Double) -> [(Int, Double)] -> [(Int, Double)]) ->
                        Double ->
                        Map Int (Map Int Double) ->
                        [(Int, Double)] ->
                        [(Int, Double)] ->
                        Int ->
                        Double ->
                        [Int]

iterativeProduceWith dotProduce alpha g knownVector currentVector 0 threshhold = 
                    ((map fst) . (filter (\(x,d) -> d >= threshhold))) currentVector
iterativeProduceWith dotProduce alpha g knownVector currentVector nIterations threshhold = 
                    iterativeProduceWith dotProduce alpha g knownVector newVector (nIterations - 1) threshhold
                    where
                        newVector = scalarProduce alpha (dotProduce g currentVector) `plusVector` scalarProduce (1 - alpha) knownVector

dotProduce :: Map Int (Map Int Double) -> [(Int, Double)] -> [(Int, Double)]
dotProduce g v = foldl (plusVector) [] $ map (columnProduce g) v

columnProduce :: Map Int (Map Int Double) -> (Int, Double) -> [(Int, Double)]
columnProduce g (x, d) = case (look g x `mapf` (scalarProduce d . toList)) of
                            Nothing -> []
                            Just v -> v

mapf = flip fmap

plusVector :: [(Int, Double)] -> [(Int, Double)] -> [(Int, Double)]
plusVector a b = toList (fromListWith (+) (concat [a, b]))

scalarProduce :: Double -> [(Int, Double)]-> [(Int, Double)]
scalarProduce d = map (\(x, e) -> (x, e * d))

validate :: IO (Double,Double)
validate = do
    trainData <- prepareTrainData
    testData <- prepareTestData
    guess <- generateGuess (toGraph trainData) [5]
    print $ show $ guess
    etalon <- generateEtalon testData
    result <- computeQualityMetric etalon guess
    return result
        where
              generateEtalon = return . id

computeQualityMetric :: [Int] -> [Int] -> IO (Double, Double)
computeQualityMetric etalon guess = return (correctness, completeness)
    where
        correctness = fromIntegral correctResults / fromIntegral (length guess)
        completeness = fromIntegral correctResults / fromIntegral (length etalon)
        correctResults = S.size $ intersection (S.fromList etalon) (S.fromList guess)

toTupleSet [a,b] = (a, fromList [(b,1)] )
toTupleSet _ = error "only lists of length 2 supported"

look :: Map Int (Map Int Double) -> Int -> Maybe (Map Int Double)
look = flip Data.Map.lookup
