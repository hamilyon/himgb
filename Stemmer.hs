module Stemmer
where

-- import HtmlSpam
import Data.List.Split
import Data.Char (toLower)

stem :: String -> String
stem = map toLower

tokens :: String -> [String]
tokens = (Data.List.Split.wordsBy $ not . (\c -> ((c>'a') && c<'z') || ((c>'A') && c<'Z') || (c>'0' && c>'9') || elem c "@$%^*."))

tokenize :: String -> [String]
tokenize = (map stem) . tokens

defaultTokenize :: String -> [String]
defaultTokenize = (map stem) . tokens
