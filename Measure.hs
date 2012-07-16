-- [load] test/measure for spam filter learning
-- import Criterion.Main (defaultMain, bench, whnf, bgroup)
module Measure where

import ReadString
import Spam
import Prob3
import Lorien
import Stemmer
import Cutter
import Control.Monad
import Control.Applicative
import System.Directory
import System.FilePath ((</>))
import Control.DeepSeq
import System.IO
import Quantile (quantiles, mean, printStats)
import Data.Map (keys)
import Config
import Data.List.Split
import Train
import CheckTrained
import GuessEnglishEnc

smoother = 1


train__  tokens smoother dir ntokens = do
    spamTrainBatch <- concat <$> readLorienBatch (dir </> "spam/train/") 1000
    hamTrainBatch  <- readPlain       (dir </> "ham/train.txt")

    
    putStrLn $ "initial spam train lengths " ++ ((show . length) $ tokens spamTrainBatch)
    putStrLn $ "initial ham  train lengths " ++ ((show . length) $ tokens hamTrainBatch)
    
    let trainedClasifier = train_ (take ntokens (tokens spamTrainBatch))
                                  (take ntokens (tokens hamTrainBatch)) (fromIntegral smoother)

    putStrLn $ "spamDict : "
    printStats ((keys . spamDict) trainedClasifier)

    putStrLn $ "hamDict : "
    printStats ((keys . hamDict) trainedClasifier)

    h <- openFile "trained.txt" WriteMode
    hPutStrLn h (show trainedClasifier)
    hClose h

-- main = train__ defaultTokenize 1 dir 50000

-- main = findCulprits (dir </> "spam/test/") 1 30

testCulprits = do
    trainedClasifier <- loadTrained
    findCulpritsInBatch trainedClasifier ["../corpi/test_data/spam/test/1287432791.3971_1133.lorien","../corpi/test_data/spam/test/1287960117.8677_737.lorien","../corpi/test_data/spam/test/1234551696.31348_1020.lorien"]

findCulprits :: FilePath -> Integer -> Integer -> IO()
findCulprits path to_find max = do
    files <- (getFiles path) :: IO [String]
    let batches = Data.List.Split.splitEvery (fromIntegral to_find) files
    trainedClasifier <- loadTrained
    foldr1 (>>) $ map (findCulpritsInBatch trainedClasifier) batches

findCulpritsInBatch :: SpamClassificationDict -> [FilePath] -> IO()
findCulpritsInBatch trainedClasifier batch = do
    spamTestBatch  <- readLorienList batch
    quality <- testQuality trainedClasifier spamTestBatch [] defaultTokenize
    if ( detectedFalseHam quality ) == (fromIntegral $ length batch) then printBatchInfo spamTestBatch else return ()
    where 
        printBatchInfo spamTestBatch = do
            putStrLn "\n\n"
            putStrLn $ show $ batch
            foldr1 (>>) $ map printStats (map defaultTokenize spamTestBatch)


-- m = defaultMain [
--         bgroup "learn" [
--         bench "learn 100000" $ (train__ defaultTokenize 1 dir 100000)
--         ]
--         ]

--- for test

