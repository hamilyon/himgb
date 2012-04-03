module Stemmer
where

import HtmlSpam

stem :: Stem -> String -> String
stem _ = id

tokens :: String -> [String]
tokens = words

tokenize :: Stem -> String -> [String]
tokenize x = (map (stem x)) . tokens
