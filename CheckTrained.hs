module CheckTrained
    where

import ReadString
import Spam
import Prob3
import System.Directory
import Control.Applicative
import Lorien
import Stemmer
import Tolerance
import Control.Monad
import System.FilePath ((</>))
import Control.DeepSeq
import Train
import Data.List.Split
import Config
import Quantile
import GuessEnglishEnc
import GuessEnglishEnc

data Stats = Stats {
    overallSpam:: Integer,
    overallHam :: Integer,
    detectedSpamOk::Integer,
    detectedHamOk::Integer,
    detectedFalseSpam::Integer,
    detectedFalseHam::Integer
} deriving Show

plus :: Stats -> Stats -> Stats
plus o1 o2 = Stats
                (overallSpam o1 + overallSpam o2)
                (overallHam o1 + overallHam o2)
                (detectedSpamOk o1 + detectedSpamOk o2)
                (detectedHamOk o1 + detectedHamOk o2)
                (detectedFalseSpam o1 + detectedFalseSpam o2)
                (detectedFalseHam o1 + detectedFalseHam o2)

detectedSpamOkUnit      = Stats 1 0 1 0 0 0
detectedHamOkUnit       = Stats 0 1 0 1 0 0
detectedFalseSpamUnit   = Stats 0 1 0 0 1 0
detectedFalseHamUnit    = Stats 1 0 0 0 0 1

nullStats               = Stats 0 0 0 0 0 0

accumulateStats ::  ([String] -> Bool) ->
                    (Stats) ->
                    (Stats) ->
                    [[String]] ->
                    Stats

accumulateStats isSpam okUnit falseUnit checkMessages = foldr plus nullStats
            (map
                ((\x -> if x then okUnit else falseUnit) . isSpam)
                checkMessages)


-- getstats :: (String -> Bool) -> (Integer -> Stats) -> (Integer -> Stats) -> String -> Stats
-- getstats = undefined

checkTrained = do
    spamTestBatch  <- readLorienBatch            (dir </> "spam/test/") 1000
    hamTestBatch   <- readPlain                  (dir </> "ham/test.txt")

    trainedClasifier <- loadTrained
    let hamTestBatchGrouped = glueBy 2 (lines hamTestBatch)

    printTestQuality trainedClasifier (filter (fmap not isGarbage) spamTestBatch) hamTestBatchGrouped defaultTokenize

glueBy nLines lines = map (foldr  ((++) . ((++) " ")) "") (Data.List.Split.splitEvery nLines lines)

printTestQuality :: SpamClassificationDict -> [String] -> [String] -> (String -> [String]) -> IO()
printTestQuality trainedClasifier spamTestBatch hamTestBatch tokenizer = do 
    quality <- testQuality trainedClasifier spamTestBatch hamTestBatch tokenizer 
    (putStrLn . show) $ quality

testQuality :: SpamClassificationDict -> [String] -> [String] -> (String -> [String]) -> IO(Stats)
testQuality trainedClasifier spamTestBatch hamTestBatch tokenizer = do
    let tokenizedHam = map tokenizer hamTestBatch
    let tokenizedSpam = map tokenizer spamTestBatch
    -- foldr1 (>>) $ map printStats tokenizedHam
    -- foldr1 (>>) $ map printStats tokenizedSpam
    -- putStrLn $ show $ tokenizedSpam
    return ((accumulateStats
                            (not . (tolerance . (spamProb trainedClasifier)))
                            detectedHamOkUnit
                            detectedFalseSpamUnit $
                            tokenizedHam)
                        `plus`
                        (accumulateStats
                            (tolerance . (spamProb trainedClasifier))
                            detectedSpamOkUnit
                            detectedFalseHamUnit $
                            tokenizedSpam))

checkSingle s = do
    trainedClasifier <- loadTrained
    let seeProb = spamProb trainedClasifier . defaultTokenize 
    putStrLn . show $ seeProb s
