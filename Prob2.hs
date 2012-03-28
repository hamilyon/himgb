module Prob2 where
import qualified Data.Map as DM
import Text.Printf

import Spam

-- Some sample text.
--spam_corpus = "i want to convey my passion for your generosity supporting folks that require assistance with the topic your very own"
--ham_corpus = "based on your artwork from elementary school i would guess you drew panels 1 and 4 and the camera on wayne coyne microphone you look like a pirate"

-- spamClassificationData = SpamClassificationData spam_corpus ham_corpus

-- fullcorp = spam ++ " " ++ ham
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

{-main = do
  -- Print out some probabilities to test.
  putStrLn $ printf "p(spam) = %.9f" $ pspam
  putStrLn $ printf "p(ham) = %.9f" $ pham
  putStrLn $ printf "p(\"my\" | spam) = %.9f" $ pword spam fullcorp smoother "my"
  putStrLn $ printf "p(\"my\" | ham) = %.9f" $ pword ham fullcorp smoother "my"

  putStrLn $ printf "p(\"you\" | spam) = %.9f" $ pword spam fullcorp smoother "you"
  putStrLn $ printf "p(\"you\" | ham) = %.9f" $ pword ham fullcorp smoother "you"
  putStrLn $ printf "vocab %.2f" $ uniqueWords fullcorp

  -- Test a message that is super spammy. Yeah, I get lots of spam that looks like this.
  let message = "your generosity many thanks i want to convey your artwork"

  putStrLn $ printf ("Message: \"" ++ message ++ "\"")
  putStrLn $ printf "Spam probability: %.9f" $ pmessagespam message

  -- Test something that's kind of nonsensical but is probably not spam.
  let message = "i would guess wayne coyne look like a pirate"
  putStrLn $ printf ("Message: \"" ++ message ++ "\"") 
  putStrLn $ printf "Spam probability: %.9f" $ pmessagespam message -}
  

-- $ runhaskell spamfilter.hs
-- p(spam) = 0.431034480
-- p(ham) = 0.568965500
-- p("my" | spam) = 0.026666667
-- p("my" | ham) = 0.021459227
-- p("you" | spam) = 0.022222223
-- p("you" | ham) = 0.030042918
-- vocab 41.00
-- Message: "your generosity many thanks i want to convey your artwork"
-- Spam probability: 0.716469300
-- Message: "i would guess wayne coyne look like a pirate"
-- Spam probability: 0.194385620
