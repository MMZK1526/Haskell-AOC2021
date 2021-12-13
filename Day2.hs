{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day2 where

-- Question source: https://adventofcode.com/2021/day/2

import           Data.Bifunctor
import           Data.Text (Text)
import qualified Data.Text as T
import           Utilities

day2Part1 :: [(Text, Integer)] -> Integer
day2Part1 = uncurry (*) . foldl go (0, 0)
  where
    go (x, y) ("forward", c) = (x + c, y)
    go (x, y) ("down",    c) = (x, y + c)
    go (x, y) ("up",      c) = (x, y - c)

day2Part2 :: [(Text, Integer)] -> Integer
day2Part2 = uncurry (*) . fst . foldl go ((0, 0), 0)
  where
    go ((x, y), a) ("down",    c) = ((x, y)            , a + c)
    go ((x, y), a) ("up",      c) = ((x, y)            , a - c)
    go ((x, y), a) ("forward", c) = ((x + c, y + a * c), a)

main :: IO ()
main = do
  input <- map (second readInt . splitPair " ") . T.lines <$> readInput "day2"
  print $ day2Part1 input
  print $ day2Part2 input
