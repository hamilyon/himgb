import Test.QuickCheck
import Data.List
import Prob1
import Prob2
import Test.QuickCheck
import Spam
import Text.Printf

spam_corpus = "i want to convey my passion for your generosity supporting folks that require assistance with the topic your very own"
ham_corpus = "based on your artwork from elementary school i would guess you drew panels 1 and 4 and the camera on wayne coyne microphone you look like a pirate"
c = spam_corpus ++ " " ++ ham_corpus
spamClassificationData = SpamClassificationData spam_corpus ham_corpus 1
spamClassificationData0 = SpamClassificationData spam_corpus ham_corpus 0

testMessage = "offer is very secret"
-- pmessagespam is reference
prop_eq = spamProb spamClassificationData testMessage == pmessagespam spamClassificationData testMessage


{- main = quickCheck
        prop_eq
--     Test.QuickCheck.printTestCase
-}

main = do
    putStrLn $ show $ spamProb spamClassificationData testMessage
    putStrLn $ show $ pmessagespam spamClassificationData testMessage
    putStrLn $ "your generosity many thanks i want to convey your artwork " ++ (show $ spamProb spamClassificationData "your generosity many thanks i want to convey your artwork")
    putStrLn $ "your generosity many thanks i want to convey your artwork " ++ (show $ pmessagespam spamClassificationData "your generosity many thanks i want to convey your artwork")
    putStrLn $ "0 your generosity many thanks i want to convey your artwork " ++ (show $ spamProb spamClassificationData0 "your generosity many thanks i want to convey your artwork")
    putStrLn $ "0 your generosity many thanks i want to convey your artwork " ++ (show $ pmessagespam spamClassificationData0 "your generosity many thanks i want to convey your artwork")
    
    
    putStrLn $ printf "p(spam) = %.9f" $ pspam spamClassificationData
    putStrLn $ printf "p(ham) = %.9f" $ pham spamClassificationData

    putStrLn $ printf "p(spam) = %.9f" $ spamProbTotal spamClassificationData
    putStrLn $ printf "p(ham) = %.9f" $ hamProbTotal spamClassificationData

    putStrLn $ printf "pword(\"my\" | spam) = %.9f" $ pword (spam spamClassificationData)
                                                      (crp spamClassificationData)
                                                      (smooth_k spamClassificationData)
                                                      "my"
    putStrLn $ printf "pword(\"my\" | ham) = %.9f"  $ pword (ham spamClassificationData)
                                                      (crp spamClassificationData)
                                                      (smooth_k spamClassificationData)
                                                      "my"

    putStrLn $ printf "psmoothed(\"my\" | spam) = %.9f" $ singleWord spamClassificationData spam
                                                      "my"
    putStrLn $ printf "psmoothed(\"my\" | ham) = %.9f" $ singleWord spamClassificationData ham
                                                      "my"

{-
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


