{-# LANGUAGE OverloadedStrings #-}

-- Question source: https://adventofcode.com/2021/day/6

import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.Map as M
import qualified Data.Text as T
import           Utilities

-- | Simulate for d days.
simulate :: Int -> [Integer] -> Integer
simulate d fs = sum $ flip execState (foldr (`addN` 1) M.empty fs) $
  forM_ [0..(d - 1)] $ \_ -> modify (foldr pass1Day M.empty . M.toList)
  where
    pass1Day (0, n) counts = addN 8 n $ addN 6 n counts
    pass1Day (c, n) counts = addN (c - 1) n counts
    addN k n               = M.insertWith (const (+ n)) k n

day6Part1 :: [Integer] -> Integer
day6Part1 = simulate 80

day6Part2 :: [Integer] -> Integer
day6Part2 = simulate 256

main :: IO ()
main = do
  input <- fmap readInt . T.splitOn "," <$> readInput "day6"
  print $ day6Part1 input
  print $ day6Part2 input
