import qualified Data.Map as DM
import Text.Printf

-- Some sample text.
spam = "i want to convey my passion for your generosity supporting folks that require assistance with the topic your very own"
ham = "based on your artwork from elementary school i would guess you drew panels 1 and 4 and the camera on wayne coyne microphone you look like a pirate"

fullcorp = spam ++ " " ++ ham

-- Return a Map representing the "Bag of Words" count of all the words in the
-- corpus.
wordFreq :: String -> DM.Map String Float
wordFreq corpus = DM.fromListWith (+) $ [(w, 1.0) | w <- (words corpus)]

-- Count the occurrences of a word in a string.
countWord :: String -> String -> Float
countWord s m = fromIntegral $ length $ filter (==s) $ words m

-- How many unique words are there in a string?
uniqueWords :: String -> Float
uniqueWords = fromIntegral . DM.size . wordFreq

-- Parameter for Laplace smoothing.
smoother :: Float
smoother = 5.0

-- What is the likelihood that  is in a , given a superset
-- of that corpus called , which is all the possible words.
pword :: String -> String -> Float -> String -> Float
pword corpus fullCorpus k word = top/bottom
  where top = k + (countWord word corpus)
        bottom = (fromIntegral $ length . words $ corpus) + k*(uniqueWords fullCorpus)

-- Use this for calculating the p(spam) and p(ham) likelihoods;
-- i.e. a measure of how likely something is to be spam without taking
-- into consideration the contents.
pthing :: Float -> Float -> Float -> Float
pthing count total k = (count + k)/(total + k*2) -- 2 = number of classes (spam, ham)

-- What is the probability that a word is spam?
pspam :: Float
pspam = pthing (fromIntegral . length . words $ spam) (fromIntegral . length . words $ fullcorp) smoother

-- What is the probability that a word is ham?
pham :: Float
pham = pthing (fromIntegral . length . words $ ham) (fromIntegral . length . words $ fullcorp) smoother

-- What is the probability that a message is spam?
pmessagespam :: String -> Float
pmessagespam message = top/bottom
  where top = pspam * product pspamwords
        pspamwords = map (pword spam fullcorp smoother) $ words message
        bottom = top + pham * product phamwords
        phamwords = map (pword ham fullcorp smoother) $ words message

main = do
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
  putStrLn $ printf "Spam probability: %.9f" $ pmessagespam message

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
