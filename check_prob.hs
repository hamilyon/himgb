import Test.QuickCheck
import Data.List
import Prob1
import Test.QuickCheck.Batch
import Spam

prop_idempotent xs = qsort (qsort xs) == qsort xs

main = do
    runTests "simple" options
        [ run prop_empty_id
        , run prop_char
        , run prop_text
        , run prop_line
        , run prop_double
        ]
