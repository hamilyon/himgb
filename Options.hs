{-# LANGUAGE DeriveDataTypeable #-}
-- Options.hs
module Options

where
import System.Console.CmdArgs

data Options = Options
    {report :: [FilePath]}
    deriving (Data,Typeable,Show,Eq)
    
options = Options
    {report = def &= opt "report.html" &= typFile &= help "Generate a report in HTML"}
    &=
    verbosity &=
    help "Spam filtering utility" &=
    summary "Prob 0.0.1" &=
    details ["Prob can be trained on spam examples and then quickly " ++
             "filter messages as being spam or not spam  using baesian filter",""
            ,"Run self-check:","  prob src --report"]


probCmdArgsMode = cmdArgsMode options

