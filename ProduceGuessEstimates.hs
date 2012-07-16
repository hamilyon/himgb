-- ProduceGuessEstimates.hs

module Main
where 

import GuessEnglishEnc
import Config
import ReadString
import System.FilePath ((</>))
import Lorien
import Control.Monad

main = filesWithEnglishCorrelation (dir </> "spam/test/") 3000 0.3

-- main = testCulprits

checkEnglishCorrelation path max = do
    files <- (getFiles path) :: IO [String]
    spamTestBatch  <- readLorienListZip (take max files) :: IO [(FilePath, String)]
    
    corr <- (mapM englishFequenciesCorrelation) spamTestBatch
    -- foldr1 (>>) (map (putStrLn . show) corr)
    return ()

filesWithEnglishCorrelation path max threshhold = do
    filenames <- fmap (map fst) (filterByEnglishCorrelation path max threshhold)
    mapM_ putStrLn filenames

filterByEnglishCorrelation :: FilePath -> Int -> Double -> IO [(FilePath, Double)]
filterByEnglishCorrelation path max threshhold  = do
    files <- (getFiles path) :: IO [String]
    spamTestBatch  <- readLorienListZip (take max files) :: IO [(FilePath, String)]
    freqs <- (mapM englishFequenciesCorrelation) spamTestBatch
    return (filter ((>=threshhold) . snd) freqs)
    

-- showEnglishFequenciesCorrelation :: (FilePath, String) -> IO()
-- showEnglishFequenciesCorrelation inputString = do 
--    putStrLn $ show $ englishFequenciesCorrelation inputString

englishFequenciesCorrelation :: (FilePath, String) -> IO (FilePath, Double)
englishFequenciesCorrelation inputString = do 
    let result = correlation typicalEnglish ((freq . snd) inputString)
    return (fst inputString, result)

