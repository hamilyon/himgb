module ReadString
where 

import System.IO
import System.IO.Strict
import Control.DeepSeq
import Control.Applicative
import System.Directory
import System.FilePath ((</>))

-- readData :: FilePath -> IO (String)
-- readData fname = System.IO.Strict.hGetContents =<< (openFile fname ReadMode)
readData = System.IO.Strict.readFile

readDataSkipErrorsStrict fname = catch (System.IO.Strict.readFile fname) (\_ -> return "")

readDataSkipErrors fname =
    catch
        (do
            x <- file
            return x)
        (\_ -> return "")

    where file = System.IO.Strict.readFile fname

readFilesMax :: [String] -> Int -> IO [String]
readFilesMax [] _ = return []
readFilesMax _ 0 = return []
readFilesMax fnames max_to_read = do
    file <- readDataSkipErrors $ (head fnames)
    let rest_of_files = ( readFilesMax (tail fnames) (max_to_read-1) )
    if null file then (readFilesMax (tail fnames) max_to_read) else (fmap (++ ((:[]) file) )) rest_of_files
 -- (fmap ((:[] file) ++)) (


readPlain :: FilePath -> IO String
readPlain = readDataSkipErrorsStrict


--- utils 

getFiles :: FilePath -> IO [String]
getFiles path = (map (path </>)) <$> (filter (not . isHidden)) <$> getDirectoryContents path

readBatchofFilesSkipErrors :: FilePath -> Int -> (String -> String) -> IO [String]
readBatchofFilesSkipErrors path batch_size processor = do
    files <- (getFiles path) :: IO [String]
    processed_files <- (fmap (map processor)) (readFilesMax files batch_size)
    return (processed_files)

isHidden ('.':_) = True
isHidden _       = False

