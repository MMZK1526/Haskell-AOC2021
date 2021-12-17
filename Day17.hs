{-# LANGUAGE OverloadedStrings #-}

module Day17 where

-- Question source: https://adventofcode.com/2021/day/17

import           Data.Bifunctor
import           Data.Maybe
import qualified Data.Text as T
import           Gadgets.Maybe
import           Utilities

day17Part1 :: (Int, Int) -> (Int, Int) -> Int
day17Part1 _ (y, _) = -y * (-y - 1) `div` 2

day17Part2 :: (Int, Int) -> (Int, Int) -> Int
day17Part2 (x, x') (y, y') = length valid0s
  where
    valid0s    = [(x0, y0) | x0 <- x0s, y0 <- y0s, match (simX x0) (simY y0)]
    x0s        = [ceiling ((sqrt (1 + 8 * fromIntegral x) - 1) / 2)..x']
    y0s        = [y..(-y - 1)]
    simX x0    = let stay = maybe (maxBound :: Int) floor
                 in  (ceiling $ fromJust $ solveX x0 x, stay $ solveX x0 x')
    simY y0    = (ceiling $ solveY y0 y', floor $ solveY y0 y)
    solveX v c = let b     = 2 * fromIntegral v + 1
                     delta = b ** 2 - 8 * fromIntegral c
                 in  toMaybe (const $ delta > 0) $ (b - sqrt delta) / 2
    solveY v c = let b = 2 * fromIntegral v + 1
                 in  (b + sqrt (b ** 2 - 8 * fromIntegral c)) / 2
    match a b  = fst a <= snd b && fst b <= snd a 
              && uncurry (<=) a && uncurry (<=) b

main :: IO ()
main = do
  [_, input] <- T.splitOn "target area: " <$> readInput "day17"
  let (xIn, yIn) = bimap parseR parseR $ splitPair ", " input
  print $ day17Part1 xIn yIn
  print $ day17Part2 xIn yIn
  where
    parseR = bimap readInt readInt . splitPair ".." . T.drop 2
