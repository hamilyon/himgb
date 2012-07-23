--Launcher.hs
import CheckTrained
import Options
import Measure
import System.FilePath ((</>))
import Config
import GuessEnglishEnc
import Stemmer
import System.IO


-- main = checkTrained
main = do 
    print probCmdArgsMode
    h <- openFile "selfCheck.txt" WriteMode
    selfCheck 
        (dir </> "spam/test/") 5
        (dir </> "ham/test.txt")
        1
        defaultTokenize
        (fmap not isGarbage)
        2
        h
    hClose h


