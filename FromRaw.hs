{-# LANGUAGE OverloadedStrings #-}
-- input strings are like "{1,2,3,1900438}"
import qualified Data.ByteString.Char8 as T
import  qualified Data.HashTable.IO as HT

import Data.Maybe
import Control.Applicative

main = do
  ls <- T.lines `fmap` T.getContents
  let ws = map (fst . parseInts . \x -> ([],x)) ls
  print ws
  h <- HT.new :: IO (HT.BasicHashTable (Int, Int) Int)
  flip mapM_ ws $ \w -> do
    let clique = allToAll w
    flip mapM_ clique $ \c -> do
        r <- HT.lookup h c
        case r of
            Nothing -> HT.insert h c (1::Int)
            Just n  -> HT.insert h c (n+1)
  print  =<< HT.toList h


parseInts :: ([Int], T.ByteString) -> ([Int], T.ByteString)
parseInts (soFar, restString) = if T.empty == restString then (soFar, "") else
                    let parsed = T.readInt restString in
                        case parsed of
                        Nothing -> parseInts (soFar, T.tail restString)
                        Just (result, remaining) -> parseInts (result:soFar, remaining)
allToAll :: [Int] -> [(Int, Int)]
allToAll xs = catMaybes [if x>=y then Nothing else Just (x,y) | x <- xs, y <- xs]

{-readInteger-}

