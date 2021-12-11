{-# LANGUAGE OverloadedStrings #-}

module Day7 where

  -- Question source: https://adventofcode.com/2021/day/7

import           Data.List
import qualified Data.Text as T
import           Utilities

day7Part1 :: [Integer] -> Integer
day7Part1 xs = sum $ abs . (sort xs !! (length xs `div` 2) -) <$> xs

day7Part2 :: [Integer] -> Integer
day7Part2 xs = minimum [sum $ step x <$> xs | x <- [avg - 1, avg]] `div` 2
  where
    step x y = let diff = abs (x - y) in diff * (diff + 1)
    avg      = round $ realToFrac (sum xs) / genericLength xs

main :: IO ()
main = do
  input <- map readInt . T.splitOn "," <$> readInput "day7"
  print $ day7Part1 input
  print $ day7Part2 input
