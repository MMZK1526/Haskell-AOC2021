{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day20 where

-- Question source: https://adventofcode.com/2021/day/20

import           Data.Array (Array)
import qualified Data.Array as A
import           Data.Bifunctor
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Gadgets.Array as A
import           Utilities

-- Does 2 transformations at once.
trans2 :: Array Int Int -> Map (Int, Int) Int -> Map (Int, Int) Int
trans2 algo image = transform 2 $ transform 3 image
  where
    (rMin, cMin)      = fst $ fst $ fromJust $ M.minViewWithKey image
    (rMax, cMax)      = fst $ fst $ fromJust $ M.maxViewWithKey image
    transform i image = foldl go M.empty imgRange
      where
        imgRange   = A.range ((rMin - i, cMin - i), (rMax + i, cMax + i))
        go img pos = M.insert pos (algo A.! uncurry readAt pos) img
        readAt r c = foo $ map (fromMaybe 0 . (image M.!?)) 
                   $ [(r + i, c + j) | i <- [-1..1], j <- [-1..1]]
        foo = foldl ((. fromIntegral) . (+) . (2 *)) 0

day20Part1 :: Array Int Int -> Map (Int, Int) Int -> Int
day20Part1 = (sum .) . trans2

day20Part2 :: Array Int Int -> Map (Int, Int) Int -> Int
day20Part2 = (sum .) . flip flip 25 . ((!!) .) . iterate . trans2

main :: IO ()
main = do
  [algo', image'] <- fmap T.unpack . T.splitOn "\n\n" <$> readInput "day20"
  let bin c = if c == '#' then 1 else 0
  let algo  = A.fromList $ bin <$> algo'
  let image = M.fromAscList $ map (second bin) 
            $ A.assocs $ A.from2DListR $ lines image'
  print $ day20Part1 algo image
  print $ day20Part2 algo image
