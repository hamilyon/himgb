module CheckTrained
    where

import ReadString
import Spam
import Prob3
import System.Directory
import Control.Applicative
import Lorien
import Stemmer
import Cutter
import Control.Monad
import System.FilePath ((</>))
import Control.DeepSeq
import Train

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

detectedSpamOkUnit      = Stats 1 0 1 0 0 0
detectedHamOkUnit       = Stats 0 1 0 1 0 0
detectedFalseSpamUnit   = Stats 1 0 0 0 1 0
detectedFalseHamUnit    = Stats 0 1 0 0 0 1

nullStats               = Stats 0 0 0 0 0 0

accumulateStats ::  (String -> Bool) ->
                    (Stats) ->
                    (Stats) ->
                    [String] ->
                    Stats

accumulateStats isSpam okUnit falseUnit checkMessages = foldr plus nullStats
            (map
                ((\x -> if x then okUnit else falseUnit) . isSpam)
                checkMessages)


-- getstats :: (String -> Bool) -> (Integer -> Stats) -> (Integer -> Stats) -> String -> Stats
-- getstats = undefined
dir = "../corpi/test_data/" -- ham/train.txt

checkTrained = do
    spamTestBatch  <- readLorienBatch            (dir </> "spam/test/") 10000
    hamTestBatch   <- readPlain                  (dir </> "ham/test.txt")

    trainedClasifier <- loadTrained
    printTestQuality trainedClasifier spamTestBatch (lines hamTestBatch)



printTestQuality :: SpamClassificationDict -> [String] -> [String] -> IO()
printTestQuality trainedClasifier spamTestBatch hamTestBatch = do
    (putStrLn . show) $ (accumulateStats
                            (not . (tolerance . (spamProb trainedClasifier)))
                            detectedHamOkUnit
                            detectedFalseHamUnit
                            hamTestBatch)
                        `plus`
                        (accumulateStats
                            (tolerance . (spamProb trainedClasifier))
                            detectedSpamOkUnit
                            detectedFalseSpamUnit
                            spamTestBatch)
