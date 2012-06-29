-- Tolerance.hs

module Tolerance
where

tolerance :: Double -> Bool
tolerance x = if x > 0.01 then True else False


