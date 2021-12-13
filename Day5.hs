{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day5 where

-- Question source: https://adventofcode.com/2021/day/5

import           Data.Bifunctor
import qualified Data.Foldable as F
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import           Utilities

type Point = ((Integer, Integer), (Integer, Integer))

-- Common logic for both parts. "False" for Part 1 and "True" for Part 2.
mapLines :: Bool -> [Point] -> Int
mapLines b = length . filter (> 1) . F.toList . foldr addLine M.empty
  where
    addLine ((x, y), (x', y')) grid
      | x == x'        = foldr (countUp . (x ,)) grid ys
      | y == y'        = foldr (countUp . (, y)) grid xs
      | b && xD == yD  = foldr countUp grid $ zip xs ys
      | b && xD == -yD = foldr countUp grid $ zip xs $ reverse ys
      | otherwise      = grid
      where
        xMin : xMax : _  = sort [x, x']
        yMin : yMax : _  = sort [y, y']
        (xD, yD, xs, ys) = (x' - x, y' - y, [xMin..xMax], [yMin..yMax])
        countUp          = flip (M.insertWith (const succ)) 1

day5Part1 :: [Point] -> Int
day5Part1 = mapLines False

day5Part2 :: [Point] -> Int
day5Part2 = mapLines True

main :: IO ()
main = do
  input <- map lineParser . T.lines <$> readInput "day5"
  print $ day5Part1 input
  print $ day5Part2 input
  where
    lineParser = bimap (sp readInt ",") (sp readInt ",") . sp id " -> "
    sp f       = (bimap f f .) . splitPair
