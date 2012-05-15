module Spam where
import Test.QuickCheck
import Data.List
-- import Prob1
import qualified Data.Map as M
import qualified Data.Set as S

data SpamExample = SpamExample {
    corpus :: SpamClassificationData,
    message :: String
}

data SpamClassificationData = SpamClassificationData {
    spam :: String,
    ham :: String,
    smooth_k :: Double
}


crp spamClassificationData = spam spamClassificationData ++ " " ++ ham spamClassificationData

relevant = ham
not_relevent = spam

train = SpamClassificationData

data SpamClassificationDict = SpamClassificationDict {
    spamDict :: M.Map String Double,
    hamDict :: M.Map String Double,
    smooth_k_ :: Double
}
