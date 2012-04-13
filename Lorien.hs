{-# LANGUAGE ViewPatterns #-}

module Lorien
where

import Spam
-- import ReadString
import Data.List.Split
import Data.List

lorienToPlain :: String -> String
lorienToPlain = concat . dropSeparators . dropHeaders . splitMultiPart

splitMultiPart :: String -> [String]
splitMultiPart = Data.List.Split.splitOn "\n\n"

isHeaders :: String -> Bool
-- isHeaders ('D':'e':'l':'i':'v':'e':'r':'e':'d':'-':'T':'o':xs) = True
isHeaders (stripPrefix "Delivered-To" -> Just _) = True
isHeaders (stripPrefix "MIME-Version" -> Just _) = True

-- isHeaders (: xs) = True
isHeaders _ = False

dropHeaders :: [String] -> [String]
dropHeaders = filter (fmap not isHeaders)

isSeparator :: String -> Bool
isSeparator (stripPrefix "------" -> Just _) = True
isSeparator (stripPrefix "This is a multi-part message in MIME" -> Just _) = True
isSeparator _ = False

dropSeparators :: [String] -> [String]
dropSeparators = filter (fmap not isSeparator)
-- dropSeparators = undefined
