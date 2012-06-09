module Stemmer
where

-- import HtmlSpam
import Data.List.Split
import Data.Char (toLower)

stem :: String -> String
stem = map toLower

tokens :: String -> [String]
tokens = (Data.List.Split.wordsBy $ not . ((flip elem) $ ['a'..'z']++['A'..'Z']))

tokenize :: String -> [String]
tokenize = (map stem) . tokens

defaultTokenize :: String -> [String]
defaultTokenize = (map stem) . tokens
