module Spam where
import Test.QuickCheck
import Data.List
-- import Prob1

data SpamExample = SpamExample {
    spamClassificationData :: SpamClassificationData,
    message :: String
}

data SpamClassificationData = SpamClassificationData {
    spam :: String,
    ham :: String
}

crp spamClassificationData = spam spamClassificationData ++ " " ++ ham spamClassificationData


-- prop_eq xs = spamProb xs == pmessagespam xs

