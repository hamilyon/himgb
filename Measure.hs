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
import CheckTrained

smoother = 1

train__  tokens smoother dir ntokens = do
    spamTrainBatch <- concat <$> readLorienBatch (dir </> "spam/train/") 1000
    hamTrainBatch  <- readPlain       (dir </> "ham/train.txt")
    
    h <- openFile "trained.txt" WriteMode
    hPutStrLn h (show $ train_  (take ntokens (tokens spamTrainBatch))    
                                (take ntokens (tokens hamTrainBatch)) (fromIntegral smoother)
                )
    hClose h

main = train__ defaultTokenize 1 dir 50000

m = defaultMain [
        bgroup "learn" [
        bench "learn 100000" $ (train__ defaultTokenize 1 dir 100000)
        ]
        ]
