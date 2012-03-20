-- file: ch11/QC-basics.hs
import Test.QuickCheck
import Data.List
import System.Environment (getArgs)

main :: IO()
main = do
    args <- getArgs
    putStrLn $ show args
