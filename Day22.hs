{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day22 where

-- Question source: https://adventofcode.com/2021/day/22

import qualified Data.Array as A
import           Data.Bifunctor
import           Data.Text (Text)
import qualified Data.Text as T
import           Utilities

day22Part1 :: [(Text, [(Int, Int)])] -> Integer
day22Part1 = day22Part2 . filter (all (uncurry (<=)) . snd) 
           . map (second (map (bimap (max (-50)) (min 50))))

-- | Note that the worst case is exponential when most of the cubes intersect.
-- Works for the given input.
day22Part2 :: [(Text, [(Int, Int)])] -> Integer
day22Part2 ps = go ps
  where
    sumCube     = product . map (\(a, a') -> fromIntegral $ max 0 $ a' - a + 1)
    sumCubes [] = 0
    sumCubes (p : ps)
      | s == 0    = sumCubes ps
      | otherwise = s - sumCubes (intersect p <$> ps) + sumCubes ps
      where
        s = sumCube p
    intersect   = zipWith (\(a, a') (b, b') -> (max a b, min a' b'))
    go []       = 0
    go ((op, p) : ps) = case op of
      "on"  -> go ps + sumCube p - sumCubes (intersect p . snd <$> ps)
      "off" -> go ps

main :: IO ()
main = do
  input <- fmap lineParser . T.lines <$> readInput "day22"
  print $ day22Part1 input
  print $ day22Part2 input
  where
    lineParser str = let (op, ps) = splitPair " " str
                         rawPs    = last . T.splitOn "=" <$> T.splitOn "," ps
                     in  (op, bimap readInt readInt . splitPair ".." <$> rawPs)
