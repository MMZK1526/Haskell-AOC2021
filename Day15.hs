-- module Day15 where

-- Question source: https://adventofcode.com/2021/day/15

import           Control.Monad
import qualified Data.Array as A
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Gadgets.Array as A
import           Utilities

-- | Uses a series of right/down DP and left/up with the previous cache.
day15Part1 :: [[Int]] -> Int
day15Part1 rz = fst (optimise initTable A.! (0, 0)) - rzArr A.! (0, 0)
  where
    initTable         = A.tabulate ((0, 0), rMax) initialiser
    rzArr             = A.from2DListC rz
    rMax@(xMax, yMax) = snd $ A.bounds rzArr
    initialiser (x, y)
      | x == xMax && y == yMax = (rzArr A.! rMax, True)
      | otherwise              = (maxBound, False)
    optimise hArr
      | snd $ table A.! (0, 0) = table
      | otherwise              = optimise table
      where
        table = A.tabulate ((0, 0), rMax) go
        go (x, y)
          | b         = v
          | newV == h = (newV, True)
          | otherwise = (newV, False)
          where
            newV     = rzArr A.! (x, y) + fst (minimum $ catMaybes steps)
            steps    = map (table A.!?) [(x + 1, y), (x, y + 1)]
                    ++ map (hArr  A.!?) [(x - 1, y), (x, y - 1)]
            v@(h, b) = hArr A.! (x, y)

day15Part2 :: [[Int]] -> Int
day15Part2 = day15Part1 . map (liftM2 add [0..4]) . liftM2 (map . add) [0..4]
  where
    add i x = let m = (i + x) `mod` 9 in if m == 0 then 9 else m

main :: IO ()
main = do
  input <- map ((read . pure <$>) . T.unpack) . T.lines <$> readInput "day15"
  print $ day15Part1 input
  print $ day15Part2 input
