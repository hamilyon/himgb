{-# LANGUAGE NoMonomorphismRestriction #-}
-- See <http://www.randomhacks.net/articles/2007/02/22/bayes-rule-and-drug-tests>.

module Prob1
 where

import Spam
import Data.List

data HeroinStatus = User | Clean
  deriving (Show, Eq)

data Test = Pos | Neg
  deriving (Show, Eq)

data Perhaps = Perhaps { heroin :: HeroinStatus, test :: Test, p1 :: Integer}

percent::forall t t1. Num t1 => t1 -> t -> t -> [(t, t1)]
percent p x1 x2 =
  [(x1, p), (x2, 100-p)]

percentUser :: Num t1 => t1 -> [(HeroinStatus, t1)]
percentUser p = percent p User Clean
percentPos p = percent p Pos Neg

heroinStatus :: Fractional t1 => [(HeroinStatus, t1)]
heroinStatus = percentUser 0.1
-- testResult1 ::  -> t -> t -> [(t, t1)]
testResult1 :: Fractional t1 => [(Test, t1)]
testResult1 = percentPos 99
testResult2 :: Fractional t1 => [(Test, t1)]
testResult2 = percentPos 1

inference :: Fractional t => [(HeroinStatus, [(Test, t)])]
inference = [(User, testResult1), (Clean, testResult2)]

-- exact :: [(HeroinStatus, Rational)] [(HeroinStatus, )]

exact :: Show t1 => [(HeroinStatus, t1)] -> [(HeroinStatus, [(Test, t)])] -> t1
exact prior inference = undefined

--main = do
--    putStr . show $ exact heroinStatus inference

--data KindaBayesNet = KindaBayesNet {
    --bayes_a_postrpiori :: (Evidence evidence, Fractional probability) => [Hipothesis] -> evidence -> probability,
    --allHipothesis :: Hipothesis a1 => [a1],
    --allEvidence :: Eq a => [a],
    --bayes_a_priori :: (Fractional probability, Hipothesis h) => h probability
--}

--class Hipothesis h where
    --a_priori :: Fractional h => h
    --a_priori _ = 0.5

--instance Hipothesis HeroinStatus where
    --a_priori Clean = 0.99
    --a_priori User = 0.01

--class Evidence a where
    --a_posteriori :: (Hipothesis b, Fractional c) => a -> b -> c


spam_corpus1 = "i want to convey my passion for your generosity supporting folks that require assistance with the topic your very own"
ham_corpus1 = "based on your artwork from elementary school i would guess you drew panels 1 and 4 and the camera on wayne coyne microphone you look like a pirate"

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

-- hamProb'  = product $ map (smoothedLikelihood (words spam_corpus) ham_corpus  )

count :: Fractional a =>  String -> [String] -> a
count word corpus = countall $ filter (==word) corpus

countall :: Fractional a => [String] -> a
countall = fromIntegral . length 
-- prior _ = 0.5

psmoothed :: String -> [String] -> Double -> Double
psmoothed word in_words smoother = psmoothed' (count word in_words) (countall in_words) smoother

psmoothed' :: Double -> Double -> Double -> Double
psmoothed' word in_words smoother = (c + smoother) / (d + 2*smoother) where
    c = word
    d = in_words

smoothedLikelihood4map :: [String] -> Double -> String -> Double
smoothedLikelihood4map in_words smoother word = smoothedLikelihood word in_words smoother

smoothedLikelihood :: String -> [String] -> Double -> Double
smoothedLikelihood = psmoothed
    --(num + smoother) / countall in_words where
    --num = count word in_words 

spamProbTotal :: SpamClassificationData -> Double
spamProbTotal spamClassificationData = 
    psmoothed' (countall (words $ spam spamClassificationData)) 
        (countall (words $ crp spamClassificationData)) 
        (smooth_k spamClassificationData)

hamProbTotal spamClassificationData = 
    psmoothed' (countall (words $ ham spamClassificationData)) 
        (countall (words $ crp spamClassificationData)) 
        (smooth_k spamClassificationData)

singleWord :: SpamClassificationData -> (SpamClassificationData -> String) -> String -> Double
singleWord spamClassificationData field word = nom/den
  where nom = smooth_k spamClassificationData + (count word ((words . field) spamClassificationData))
        den = ((countall . words . field) spamClassificationData) + (smooth_k spamClassificationData) *  
            ((fromIntegral . length . nub . words) $ crp spamClassificationData)


-- total = num +
-- / (total + laplaceSmoother * classes)

-- main = show_off
