{-# LANGUAGE NoMonomorphismRestriction #-}
-- Train.hs

module Train
    where

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

saveTrained tokens smoother dir = do
    spamTrainBatch <- concat <$> readLorienBatch (dir </> "spam/train/") batch_size
    hamTrainBatch  <- readPlain       (dir </> "ham/train.txt")

    let trainedClasifier = train_ (tokens spamTrainBatch) (tokens hamTrainBatch) smoother

    h <- openFile "trained.txt" WriteMode
    hPutStrLn h (show trainedClasifier)
    hClose h

loadTrained :: IO (SpamClassificationDict)
loadTrained = do
    h <- openFile "trained.txt" ReadMode
    trainedClasifier <- read <$> (hGetLine h) 
    hClose h
    return trainedClasifier

--- config

batch_size :: Int
batch_size = 15 :: Int

--- for tests

smoother = 1
