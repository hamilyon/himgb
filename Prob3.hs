{-# LANGUAGE BangPatterns #-}

module Prob3
 where

import Spam
import Data.List
import Data.Set (fromList, toList)
import qualified Data.Map as Map


spam_corpus1 = "i want to convey my passion for your generosity supporting folks that require assistance with the topic your very own"
ham_corpus1 = "based on your artwork from elementary school i would guess you drew panels 1 and 4 and the camera on wayne coyne microphone you look like a pirate"

train_ :: [String] -> [String] -> Double -> SpamClassificationDict
train_ spam ham smoother = SpamClassificationDict spamDict hamDict smoother (countall corpus)
    where
        spamCount = (getWordSpamminess spam Map.empty)
        spamDict = Map.map
            (\summ -> (summ + smoother) / (spamDen)) spamCount
        hamCount = (getWordSpamminess ham Map.empty)
        hamDict = Map.map
            (\summ -> (summ + smoother) / (hamDen))
            hamCount

        spamKeys = Map.keys spamCount
        hamKeys = Map.keys hamCount

        nubCount = (fromIntegral . length . group . sort) (spamKeys ++ hamKeys)

        spamDen = getWordSpamminessDen spam nubCount smoother
        hamDen =  getWordSpamminessDen ham nubCount smoother
        corpus = spam ++ ham

spamClassificationData1 = train_ (words spam_corpus1) (words ham_corpus1) 1

show_off = do
    putStr . show $ spamProb spamClassificationData1 $ words "offer is very secret"

--spamProb :: String -> Double
--spamProb = undefined
spamProb :: SpamClassificationDict -> [String] -> Double
spamProb spamClassificationDict message = en / ik :: Double where
    en = spamProb' spamClassificationDict message
    ik = ((spamProb' spamClassificationDict message) + (hamProb' spamClassificationDict message)) :: Double

spamProb' :: SpamClassificationDict -> [String] -> Double
spamProb' spamClassificationDict s = (spamProbTotal spamClassificationDict) * (product $ map
    (singleWord
        spamClassificationDict spamDict)
    (s))

hamProb'  spamClassificationDict s = (hamProbTotal spamClassificationDict) * (product $ map
    (singleWord
        spamClassificationDict hamDict)
    (s))

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
        Nothing -> getDefaultSpamminess $ nubCount spamClassificationDict
    }

getWordSpamminess :: [String] -> Map.Map String Double -> Map.Map String Double
getWordSpamminess   []        mapSoFar = mapSoFar
getWordSpamminess   ((x):(xs))   !mapSoFar = 
        getWordSpamminess xs (Map.insertWith (+) x 1 mapSoFar)

getWordSpamminessDen spam nubCount smooth_k = ((countall spam)) + (smooth_k) * nubCount

getDefaultSpamminess corpusNubCount = 1.0 / corpusNubCount



-- main = show_off














