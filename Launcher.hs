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
    mode <- probCmdArgsMode
    h <- openFile ("report.html") WriteMode
    selfCheck 
        (dir </> "spam/test/") 1000
        (dir </> "ham/test.txt")
        1
        defaultTokenize
        (fmap not isGarbage)
        2
        h
    hClose h

