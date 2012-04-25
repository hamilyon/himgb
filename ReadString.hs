module ReadString
where 

import System.IO
import System.IO.Strict

-- readData :: FilePath -> IO (String)
-- readData fname = System.IO.Strict.hGetContents =<< (openFile fname ReadMode)
readData = System.IO.Strict.readFile

readDataSkipErrorsStrict fname = catch (System.IO.Strict.readFile fname) (\_ -> return "")

readDataSkipErrors fname = catch (System.IO.readFile fname) (\_ -> return "")
