module CheckTrained
    where

import ReadString
import Spam
import Prob1
import System.Directory
import Control.Applicative
import Lorien
import Stemmer
import Cutter
import Control.Monad
import System.FilePath ((</>))
import Control.DeepSeq

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
detectedFalseHamUnit    = Stats 0 0 0 0 0 1

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

batch_size :: Int
batch_size = 2 :: Int

dir = "../corpi/test_data/"

checkTrained = do
    spamTrainBatch <- concat <$> readLorienBatch (dir </> "spam/train/")
    hamTrainBatch  <- readPlain       (dir </> "ham/train.txt")
    spamTestBatch  <- readLorienBatch            (dir </> "spam/test/")
    hamTestBatch   <- readPlain                  (dir </> "ham/test.txt")

    let trainedClasifier = train spamTrainBatch hamTrainBatch 1
    printTestQuality trainedClasifier spamTestBatch (lines hamTestBatch)

readLorienBatch path = readBatchofFilesSkipErrors path batch_size lorienToPlain

readPlain :: FilePath -> IO String
readPlain = readDataSkipErrorsStrict

getFiles :: FilePath -> IO [String]
getFiles path = (map (path </>)) <$> (filter (not . isHidden)) <$> getDirectoryContents path

readBatchofFilesSkipErrors :: FilePath -> Int -> (String -> String) -> IO [String]
readBatchofFilesSkipErrors path batch_size processor = do
    files <- (getFiles path) :: IO [String]
    processed_files <- (fmap (map processor)) (readFilesMax files batch_size)
    return (processed_files)

isHidden ('.':_) = True
isHidden _       = False

printTestQuality :: SpamClassificationData -> [String] -> [String] -> IO()
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
