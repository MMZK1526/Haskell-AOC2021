{-# LANGUAGE OverloadedStrings #-}

import           Data.List
import qualified Data.Text as T
import           Utilities

-- Question source: https://adventofcode.com/2021/day/7

day7Part1 :: [Integer] -> Integer
day7Part1 xs = minimum [sum $ abs . (x -) <$> ss | x <- (ss !!) <$> [i - 1, i]]
  where
    (ss, i) = (sort xs, length xs `div` 2)

day7Part2 :: [Integer] -> Integer
day7Part2 xs = minimum [sum $ step x <$> xs | x <- [avg - 1, avg]]
  where
    step x y = let diff = abs (x - y) in diff * (diff + 1) `div` 2
    avg      = round $ realToFrac (sum xs) / genericLength xs

main :: IO ()
main = do
  input <- fmap readInt . T.splitOn "," <$> readInput "day7"
  print $ day7Part1 input
  print $ day7Part2 input
