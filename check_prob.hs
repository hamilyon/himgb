import Test.QuickCheck
import Data.List
import Prob1
import Prob2
import Test.QuickCheck.Batch
import Spam

spam_corpus = "i want to convey my passion for your generosity supporting folks that require assistance with the topic your very own"
ham_corpus = "based on your artwork from elementary school i would guess you drew panels 1 and 4 and the camera on wayne coyne microphone you look like a pirate"
spamClassificationData = SpamClassificationData spam_corpus ham_corpus 1

testMessage = "offer is very secret"
-- pmessagespam is reference
prop_eq xs = spamProb spamClassificationData testMessage 1 == pmessagespam spamClassificationData testMessage 1

main = do
    runTests "spam reference" options
        [ run prop_eq
        ]
