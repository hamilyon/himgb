-- Tabulate.hs

module Tabulate
where
import System.IO
hTabulate :: Show a => [[a]] -> Handle -> IO()
hTabulate matrix h  = do
    hPutStr h (tabulate matrix)
    
tabulate :: Show a => [[a]] -> String
tabulate matrix = unlines (map (\line ->"<tr>"++(unlines line)++"</tr>") tds)
    where
    tds = map (map (\a -> "<td>"++(show a)++"</td>")) matrix

