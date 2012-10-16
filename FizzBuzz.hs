{-# LANGUAGE MonadComprehensions #-}
module Main where
import Data.Monoid (mappend)
import Data.Maybe (fromMaybe, listToMaybe, maybe)
import System.Environment (getArgs)


fizzbuzz i = fromMaybe (show i) $ mappend ["fizz" | i `rem` 3 == 0] $
                                  mappend ["buzz" | i `rem` 5 == 0]
                                          ["bazz" | i `rem` 7 == 0]

-- mapM_ is our iterator, putStrLn writes to console.
main = mapM_ putStrLn [ fizzbuzz i | i <- [1..100] ]


