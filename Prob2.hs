module Prob2 where
import qualified Data.Map as DM
import Text.Printf

import Spam

fullcorp spamClassificationData = crp spamClassificationData

-- Return a Map representing the "Bag of Words" count of all the words in the
-- corpus.
wordFreq :: String -> DM.Map String Double
wordFreq corpus = DM.fromListWith (+) $ [(w, 1.0) | w <- (words corpus)]

-- Count the occurrences of a word in a string.
countWord :: String -> String -> Double
countWord s m = fromIntegral $ length $ filter (==s) $ words m

-- How many unique words are there in a string?
uniqueWords :: String -> Double
uniqueWords = fromIntegral . DM.size . wordFreq

-- Parameter for Laplace smoothing.
smoother :: Double
smoother = 5.0

-- What is the likelihood that  is in a , given a superset
-- of that corpus called , which is all the possible words.
pword :: String -> String -> Double -> String -> Double
pword corpus fullCorpus k word = top/bottom
  where top = k + (countWord word corpus)
        bottom = (fromIntegral $ length . words $ corpus) + k*(uniqueWords fullCorpus)

-- Use this for calculating the p(spam) and p(ham) likelihoods;
-- i.e. a measure of how likely something is to be spam without taking
-- into consideration the contents.
pthing :: Double -> Double -> Double -> Double
pthing count total k = (count + k)/(total + k*2) -- 2 = number of classes (spam, ham)

-- What is the probability that a word is spam?
pspam :: SpamClassificationData -> Double
pspam spamClassificationData = 
    pthing (fromIntegral . length . words $ (spam spamClassificationData)) (fromIntegral . length . words $ (crp spamClassificationData)) (smooth_k spamClassificationData)

-- What is the probability that a word is ham?
pham :: SpamClassificationData -> Double
pham spamClassificationData = 
    pthing (fromIntegral . length . words $ (ham spamClassificationData)) (fromIntegral . length . words $ (crp spamClassificationData)) (smooth_k spamClassificationData)

-- What is the probability that a message is spam?
pmessagespam :: SpamClassificationData -> String -> Double
pmessagespam spamClassificationData message = top/bottom
  where top = (pspam spamClassificationData) * product pspamwords
        pspamwords = map 
            (pword 
                (spam spamClassificationData) 
                (crp spamClassificationData) 
                (smooth_k spamClassificationData))
             $ words message
        bottom = top + (pham spamClassificationData) * product phamwords
        phamwords = map 
            (pword 
                (ham spamClassificationData) 
                (crp spamClassificationData) 
                (smooth_k spamClassificationData)) 
            $ words message

