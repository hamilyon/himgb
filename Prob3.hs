{-# LANGUAGE NoMonomorphismRestriction #-}

module Prob3
 where

import Spam
import Data.List

spam_corpus1 = "i want to convey my passion for your generosity supporting folks that require assistance with the topic your very own"
ham_corpus1 = "based on your artwork from elementary school i would guess you drew panels 1 and 4 and the camera on wayne coyne microphone you look like a pirate"

train_ :: [String] -> [String] -> Integer -> SpamClassificationDict
train_ spam ham smoother = SpamClassificationDict spamDict hamDict smoother
    where
        spamDict = undefined
        hamDict = undefined
        smoother = undefined

spamClassificationData1 = SpamClassificationData spam_corpus1 ham_corpus1 1

show_off = do
    putStr . show $ spamProb spamClassificationData1 "offer is very secret"

--spamProb :: String -> Double
--spamProb = undefined
spamProb :: SpamClassificationData -> String -> Double
spamProb spamClassificationData message = en * k :: Double where
    en = spamProb' spamClassificationData message
    k = 1 / ((spamProb' spamClassificationData message) + (hamProb' spamClassificationData message)) :: Double

spamProb' :: SpamClassificationData -> String -> Double
spamProb' spamClassificationData s = (spamProbTotal spamClassificationData) * (product $ map
    (singleWord
        spamClassificationData spam)
    (words s))

hamProb'  spamClassificationData s = (hamProbTotal spamClassificationData) * (product $ map
    (singleWord
        spamClassificationData ham)
    (words s))




count :: Fractional a =>  String -> [String] -> a
count word corpus = countall $ filter (==word) corpus

countall :: Fractional a => [String] -> a
countall = fromIntegral . length 

-- THIS will be 0.5
spamProbTotal :: SpamClassificationData -> Double
spamProbTotal spamClassificationData = 0.5

hamProbTotal spamClassificationData = 0.5

-- THIS WILL BE MAP LOOKUP
singleWord :: SpamClassificationData -> (SpamClassificationData -> String) -> String -> Double
singleWord spamClassificationData field word = nom/den
  where nom = smooth_k spamClassificationData + (count word ((words . field) spamClassificationData))
        den = ((countall . words . field) spamClassificationData) + (smooth_k spamClassificationData) *  
            ((fromIntegral . length . nub . words) $ crp spamClassificationData)


classify :: SpamClassificationData -> [String] -> Double
classify classificator message = undefined
-- total = num +
-- / (total + laplaceSmoother * classes)

-- main = show_off
