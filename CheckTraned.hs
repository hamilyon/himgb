module CheckTraned
    where

import ReadString
import Spam
import Prob1
import System.Directory 
import Control.Applicative
import Lorien
import Stemmer
import Cutter

data Stats = Stats {
    overallSpam:: Integer,
    overallHam :: Integer,
    detectedSpamOk::Integer,
    detectedHamOk::Integer,
    detectedFalseSpam::Integer,
    detecteFalseHam::Integer
} deriving Show

plus :: Stats -> Stats -> Stats
plus o1 o2 = Stats
                (overallSpam o1 + overallSpam o2)
                (overallHam o1 + overallHam o2)
                (detectedSpamOk o1 + detectedSpamOk o2)
                (detectedHamOk o1 + detectedHamOk o2)
                (detectedFalseSpam o1 + detectedFalseSpam o2)
                (detecteFalseHam o1 + detecteFalseHam o2)

detectedSpamOkUnit      = Stats 0 0 1 0 0 0
detectedHamOkUnit       = Stats 0 0 0 1 0 0
detectedFalseSpamUnit   = Stats 0 0 0 0 1 0
detecteFalseHamUnit     = Stats 0 0 0 0 0 1

accumulateStats ::  (String -> Bool) ->
                    (Integer -> Stats) -> 
                    (Integer -> Stats) -> 
                    [String] -> 
                    Stats

--accumulateStats     clasifier
--                    detectedHamOkUnit
--                    detecteFalseHamUnit  = foldr (`plus` (getstats clasifier detectedHamOkUnit detecteFalseHamUnit))

accumulateStats = undefined

getstats :: (String -> Bool) -> (Integer -> Stats) -> (Integer -> Stats) -> String -> Stats
getstats = undefined

batch_size = 10

checkTrained = do
    spamTrainBatch <- concat <$> readLorienBatch "test_data/spam/train/"
    hamTrainBatch  <- concat <$> readBatch "test_data/ham/train/"
    spamTestBatch  <- concat <$> readLorienBatch "test_data/spam/test/"
    hamTestBatch   <- concat <$> readBatch "test_data/ham/test/"

    let trainedClasifier = train spamTrainBatch hamTrainBatch 1
    printTestQuality trainedClasifier spamTestBatch hamTestBatch

readLorienBatch :: FilePath -> IO [String]
readLorienBatch path = do
    -- list dir contents
    names <- getDirectoryContents path
    let processed = take batch_size names
    mapM ((fmap lorienToPlain) . readData) names

readBatch :: FilePath -> IO [String]
readBatch path = do
    names <- getDirectoryContents path
    let processed = take batch_size names
    mapM readData names

printTestQuality :: SpamClassificationData -> String -> String -> IO()
printTestQuality trainedClasifier spamTestBatch hamTestBatch = do
    (putStrLn . show) $ (accumulateStats  
                            (isSpamWithTolerance spamProb tolerance trainedClasifier)
                            detectedHamOkUnit 
                            detecteFalseHamUnit 
                            hamTestBatch)


                        `plus`
                        (accumulateStats                             
                            (isSpamWithTolerance spamProb tolerance trainedClasifier)
                            detectedFalseSpamUnit 
                            trainedClasifier 
                            spamTestBatch)
                      
