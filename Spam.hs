module Spam where
import Test.QuickCheck
import Data.List
-- import Prob1

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

-- showSpam :: SpamExample -> String
-- showSpam spamExample = spam $ spamClassificationData spamExample

-- prop_eq xs = spamProb xs == pmessagespam xs

