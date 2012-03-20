{- -}

import qualified Data.Map as Map
import Data.List (sortBy)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import System.IO
import System.Environment

-- words -> (spams, hams)
-- total # spam msgs
-- total # ham msgs

data FreqPair = FreqPair {
      spams :: Int,
      hams  :: Int
    } deriving (Show, Read, Eq)
data Filter = Filter {
      wordsMap :: Map.Map String FreqPair,
      numSpams :: Int,
      numHams  :: Int
    } deriving (Show, Read)



main = do
  args <- getArgs
  progn $ head args

progn fname = do
  withFile fname ReadMode $ \handle -> do
                nspams'    <- hGetLine handle
                let nspams = read nspams' :: Int   
                spams      <- nlines handle nspams
                nhams'     <- hGetLine handle
                let nhams  = read nhams' :: Int   
                hams       <- nlines handle nhams
                _          <- hGetLine handle -- skip a line!
                msgs'      <- hGetContents handle
                let msgs   = lines msgs'
{-
                putStrLn $ "nspams: " ++ show nspams
                putStrLn $ "spams: " ++ show spams
                putStrLn $ "nhams: " ++ show nhams
                putStrLn $ "hams: " ++ show hams
                putStrLn $ "msgs: " ++ show msgs
                putStrLn ""
-}
                let (probsLst,finState) = runState (do
                                                     initState spams hams
                                                     fs <- get
                                                     mapM bayes msgs)
                                                    (Filter (Map.fromList []) 0 0)
                mapM_ print probsLst
                --print finState
  

nlines h 0 = return []
nlines h n = do line <- hGetLine h
                lines <- nlines h (n - 1)
                return $ line : lines

--------------------------

addSpam :: String -> State Filter ()
addSpam word = do fs <- get
                  let m = wordsMap fs
                  let cur = maybe (FreqPair 0 0) id (Map.lookup word m)
                  put $ fs { wordsMap = (Map.insert word (cur { spams = (spams cur) + 1}) m) }

addHam :: String -> State Filter ()
addHam word = do fs <- get
                 let m = wordsMap fs
                 let cur = maybe (FreqPair 0 0) id (Map.lookup word m)
                 put $ fs { wordsMap = (Map.insert word (cur { hams = (hams cur) + 1}) m) }

tokenize :: String -> [String]
tokenize = words -- TODO: better tokenization

initState :: [String] -> [String] -> State Filter ()
initState spams hams = do
  put $ Filter Map.empty (length spams) (length hams)
  mapM_ addSpam (concatMap tokenize spams)
  mapM_ addHam (concatMap tokenize hams)
  return ()


spaminess :: String -> State Filter Float
spaminess word = do
  fs <- get
  let m = wordsMap fs
  let pair = maybe (FreqPair 0 0) id (Map.lookup word m)
  let pWS = (fromIntegral (spams pair))/(fromIntegral (numSpams fs))
  let pWH = (fromIntegral (hams pair))/(fromIntegral (numHams fs))
  let p = if pWS+pWH == 0 then 0.4 else (pWS/(pWS+pWH))
  return $ min 0.99 $ max 0.01 p 


bayes :: String -> State Filter Float
bayes msg = do
  probs <- mapM spaminess $ tokenize msg
  let ps = take 15 $ sortBy (\a b -> compare (abs (b-0.5)) (abs (a-0.5))) probs
      np = product $ map (\x -> 1-x) ps
      pp = product ps
  return $ pp/(np+pp)



