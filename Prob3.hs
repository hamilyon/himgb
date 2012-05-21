{-# LANGUAGE NoMonomorphismRestriction #-}

module Prob3
 where

import Spam
import Data.List
import qualified Data.Map as Map


spam_corpus1 = "i want to convey my passion for your generosity supporting folks that require assistance with the topic your very own"
ham_corpus1 = "based on your artwork from elementary school i would guess you drew panels 1 and 4 and the camera on wayne coyne microphone you look like a pirate"

train_ :: [String] -> [String] -> Double -> SpamClassificationDict
train_ spam ham smoother = SpamClassificationDict spamDict hamDict smoother (countall corpus)
    where
        spamDict = Map.fromList $ zip
                                    corpus $
                                    map (getWordSpamminess spam corpus smoother) corpus 
        hamDict = Map.fromList $ zip
                                    corpus $
                                    map (getWordSpamminess ham corpus smoother) corpus 
        corpus = spam ++ ham

spamClassificationData1 = train_ (words spam_corpus1) (words ham_corpus1) 1

show_off = do
    putStr . show $ spamProb spamClassificationData1 "offer is very secret"

--spamProb :: String -> Double
--spamProb = undefined
spamProb :: SpamClassificationDict -> String -> Double
spamProb spamClassificationDict message = en * k :: Double where
    en = spamProb' spamClassificationDict message
    k = 1 / ((spamProb' spamClassificationDict message) + (hamProb' spamClassificationDict message)) :: Double

spamProb' :: SpamClassificationDict -> String -> Double
spamProb' spamClassificationDict s = (spamProbTotal spamClassificationDict) * (product $ map
    (singleWord
        spamClassificationDict spamDict)
    (words s))

hamProb'  spamClassificationDict s = (hamProbTotal spamClassificationDict) * (product $ map
    (singleWord
        spamClassificationDict hamDict)
    (words s))

count :: Fractional a =>  String -> [String] -> a
count word corpus = countall $ filter (==word) corpus

countall :: Fractional a => [String] -> a
countall = fromIntegral . length 

spamProbTotal :: SpamClassificationDict -> Double
spamProbTotal spamClassificationDict = 0.5

hamProbTotal spamClassificationDict = 0.5

singleWord :: SpamClassificationDict -> (SpamClassificationDict -> (Map.Map String Double)) -> String -> Double
singleWord spamClassificationDict field word = case Map.lookup word (field spamClassificationDict)
    of {
        Just v -> v;
        Nothing -> defaultSpamminess spamClassificationDict
    }

getWordSpamminess :: [String] -> [String] -> Double -> String -> Double
getWordSpamminess spam corpus smooth_k word = nom/den
  where nom = smooth_k + (count word ((spam)))
        den = ((countall spam)) + (smooth_k) *  
            ((fromIntegral . length . nub) corpus)

getDefaultSpamminess corpusNubCount = 1.0 / corpusNubCount



-- main = show_off




