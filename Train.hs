-- Train.hs

module Train
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
import System.IO
import CheckTrained

saveTrained = do
    spamTrainBatch <- concat <$> readLorienBatch (dir </> "spam/train/")
    hamTrainBatch  <- readPlain       (dir </> "ham/train.txt")

    let trainedClasifier = train_ (tokens spamTrainBatch) (tokens hamTrainBatch) 1

    h <- openFile "trained.txt" WriteMode
    hSeek h AbsoluteSeek 0
    hPutStrLn h (show trainedClasifier)
    hClose h

loadTrained :: IO (SpamClassificationDict)
loadTrained = do
    h <- openFile "trained.txt" ReadMode
    trainedClasifier <- read <$> (hGetLine h) 
    hClose h
    return trainedClasifier
