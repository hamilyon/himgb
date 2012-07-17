-- ProduceGuessEstimates.hs

module Main
where 

import GuessEnglishEnc
import Config
import ReadString
import System.FilePath ((</>))
import Lorien
import Control.Monad
import qualified Data.Map as Map

-- main = filesWithEnglishCorrelation (dir </> "spam/test/") 3000 0.3 typicalSpam
main = checkEnglishCorrelation (dir </> "spam/test/") 3000 typicalSpam

-- main = testCulprits

checkEnglishCorrelation path max etalon = do
    files <- (getFiles path) :: IO [String]
    spamTestBatch  <- readLorienListZip (take max files) :: IO [(FilePath, String)]
    
    corr <- (mapM (englishFequenciesCorrelation etalon)) spamTestBatch
    foldr1 (>>) (map (putStrLn . show) corr)
    --return ()

filesWithEnglishCorrelation path max threshhold etalon = do
    filenames <- fmap (map fst) (filterByEnglishCorrelation path max threshhold etalon)
    mapM_ putStrLn filenames

filterByEnglishCorrelation :: FilePath -> Int -> Double -> Map.Map Char Double -> IO [(FilePath, Double)]
filterByEnglishCorrelation path max threshhold etalon = do
    files <- (getFiles path) :: IO [String]
    spamTestBatch  <- readLorienListZip (take max files) :: IO [(FilePath, String)]
    freqs <- (mapM (englishFequenciesCorrelation etalon)) spamTestBatch
    return (filter ((>=threshhold) . snd) freqs)
    

-- showEnglishFequenciesCorrelation :: (FilePath, String) -> IO()
-- showEnglishFequenciesCorrelation inputString = do 
--    putStrLn $ show $ englishFequenciesCorrelation inputString

englishFequenciesCorrelation :: Map.Map Char Double -> (FilePath, String) -> IO (FilePath, Double)
englishFequenciesCorrelation etalon inputString = do 
    let result = correlation etalon ((freq . snd) inputString)
    return (fst inputString, result)

