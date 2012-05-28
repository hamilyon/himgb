{-# LANGUAGE TupleSections #-}

module Cutter
where

import HtmlSpam
import Spam
import TreeTransform
import Data.Maybe

isSpamWithTolerance :: (SpamClassificationData -> [String] -> Double) -> (Double -> Bool) -> SpamClassificationData -> [String] -> Bool
isSpamWithTolerance classificator tolerance trainingData message  = (tolerance . (classificator trainingData)) message

tolerance :: Double -> Bool
tolerance x = if x > 0.1 then True else False

filterSpam :: (Stem -> String -> [String]) -> (SpamClassificationData -> [String] -> Double) -> SpamClassificationData -> HiperText -> Bool
filterSpam tokener classificator trainingData = (nothingToFalse . cutSpam tokener classificator trainingData)

nothingToFalse :: Maybe a -> Bool
nothingToFalse Nothing = False
nothingToFalse _ = True

cutSpam :: (Stem -> String -> [String]) -> (SpamClassificationData -> [String] -> Double) -> SpamClassificationData -> HiperText -> Maybe Action
cutSpam tokener classificator trainingData hiperText | (null . childs) hiperText            = Nothing
                                                     | (length . visibleText) hiperText < 5 = Nothing
                                                     | isSpam hiperText                     = Just (Cut hiperText)
                                                     | otherwise                            = 
                                                            Just (Cut $ (fromChildren . spamToCut)
                                                            (toChildren hiperText))
        where
        visibleText = (tokener (Stem True) . plainify) :: HiperText -> [String]
        isSpam = (isSpamWithTolerance classificator tolerance trainingData) . visibleText
        cutSpam' = cutSpam
        spamToCut = filter (filterSpam tokener classificator trainingData)

fromChildren :: [HiperText] -> HiperText
fromChildren children = HiperText (map ((,"")) children) Nothing

toChildren :: HiperText -> [HiperText]
toChildren hiperText = map fst (childs hiperText)

fromAction :: Action -> HiperText
fromAction (Cut hiperText) = hiperText

toMaybes = map Just

