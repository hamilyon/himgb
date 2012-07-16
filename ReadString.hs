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
readFilesMax fnames max_to_read = fmap (map snd) (readFilesMaxZip fnames max_to_read)

readFilesMaxZip :: [String] -> Int -> IO [(FilePath, String)]
readFilesMaxZip [] _ = return []
readFilesMaxZip _ 0 = return []
readFilesMaxZip fnames max_to_read = do
    contents <- readDataSkipErrors $ (head fnames)
    let file = ((head fnames),contents)
    let rest_of_files = ( readFilesMaxZip (tail fnames) (max_to_read-1) )
    if null . snd $ file then (readFilesMaxZip (tail fnames) max_to_read) else (fmap (++ ((:[]) file) )) rest_of_files

readPlain :: FilePath -> IO String
readPlain = readDataSkipErrorsStrict


--- utils 

getFiles :: FilePath -> IO [String]
getFiles path = (map (path </>)) <$> (filter (not . isHidden)) <$> getDirectoryContents path

readBatchofFilesSkipErrors :: FilePath -> Int -> (String -> String) -> IO [String]
readBatchofFilesSkipErrors path batch_size processor = do
    files <- (getFiles path) :: IO [String]
    readListFilesSkipErrors files processor

isHidden ('.':_) = True
isHidden _       = False

readListFilesSkipErrors :: [FilePath] -> (String -> String) -> IO [String]
readListFilesSkipErrors files processor = do
    processed_files <- (fmap (map processor)) (readFilesMax files (length files))
    return (processed_files)

readListFilesSkipErrorsZip :: [FilePath] -> (String -> String) -> IO [(FilePath, String)]
readListFilesSkipErrorsZip files processor = (fmap (map (wrap processor))) (readFilesMaxZip files (length files))

wrap = \f -> \(a,b) -> (a,f b)
