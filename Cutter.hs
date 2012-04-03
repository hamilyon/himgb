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
tolerance x = if x > 0.9 then True else False

filterSpam :: HiperText -> (Stem -> String -> [String]) -> (SpamClassificationData -> [String] -> Double) -> SpamClassificationData -> Bool
filterSpam = undefined

cutSpam :: HiperText -> (Stem -> String -> [String]) -> (SpamClassificationData -> [String] -> Double) -> SpamClassificationData -> Maybe Action
cutSpam hiperText tokener classificator trainingData | (null . childs) hiperText            = Nothing
                                                     | (length . visibleText) hiperText < 5 = Nothing
                                                     | isSpam hiperText                     = Just (Cut hiperText)
                                                     | otherwise                            = 
                                                            Just (Cut $ (fromChildren . catMaybes) {- $ map 
                                                            (cutSpam' :: HiperText -> Maybe Action) -}
                                                            (toMaybes
                                                                (toChildren hiperText)))
        where
        visibleText = (tokener (Stem True) . plainify) :: HiperText -> [String]
        isSpam = (isSpamWithTolerance classificator tolerance trainingData) . visibleText
        cutSpam' ht = cutSpam ht tokener classificator trainingData :: Maybe Action

fromChildren :: [HiperText] -> HiperText
fromChildren children = HiperText (map ((,"")) children) Nothing

toChildren :: HiperText -> [HiperText]
toChildren hiperText = map fst (childs hiperText)

toMaybes = map Just

