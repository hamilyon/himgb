module ReadString
where 

import System.IO
import System.IO.Strict
import Control.DeepSeq

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
