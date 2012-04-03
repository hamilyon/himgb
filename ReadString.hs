module ReadString
where 

import System.IO

readData :: FilePath -> IO (String)
readData fname = hGetContents =<< (openFile fname ReadMode)
