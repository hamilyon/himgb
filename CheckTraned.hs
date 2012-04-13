module CheckTraned
    where

import ReadString
import Spam
import Prob1
import System.Directory 
import Control.Applicative
import Lorien

data Stats {
    overallSpam:: Integer,
    overallHam :: Integer,
    detectedSpamOk::Integer,
    detectedHamOk::Integer,
    detectedFalseSpam::Integer,
    detecteFalseHam::Integer
}

accumulateStats :: SpamClassificationData -> (SpamClassificationData -> String -> Double) -> [String] -> Stats



batch_size = 10

checkTrained = do
    spamTrainBatch <- concat <$> readLorienBatch "test_data/spam/train/"
    hamTrainBatch  <- readBatch "test_data/ham/train/"
    spamTestBatch  <- concat <$> readLorienBatch "test_data/spam/test/"
    hamTestBatch   <- readBatch "test_data/ham/test/"

    let trainedClasirier = train spamTrainBatch hamTrainBatch 1
    printTestQuality trainedClasirier spamTestBatch hamTestBatch

readLorienBatch :: FilePath -> IO [String]
readLorienBatch path = do
    -- list dir contents
    names <- getDirectoryContents path
    let processed = take batch_size names
    mapM ((fmap lorienToPlain) . readData) names

readBatch :: FilePath -> IO String
readBatch = do
    names <- getDirectoryContents path
    let processed = take batch_size names
    mapM readData names

printTestQuality :: SpamClassificationData -> String -> String -> IO()
printTestQuality = do
    accumulateStats
