-- load test/measure for spam filter learning
import Criterion.Main (defaultMain, bench, whnf, bgroup)

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
import Quantile (quantiles, mean)
import Data.Map (keys)
import Config

smoother = 1

printStats words = do
    putStrLn $ "quantiles = " ++ (show . quantiles) (map length words)
    putStrLn $ "mean = " ++ (show . mean) (map length words)
    putStrLn $ "length = " ++ (show . length) words


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

main = train__ defaultTokenize 1 dir 50000

m = defaultMain [
        bgroup "learn" [
        bench "learn 100000" $ (train__ defaultTokenize 1 dir 100000)
        ]
        ]

--- for test

