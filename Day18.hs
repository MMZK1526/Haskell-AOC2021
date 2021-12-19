module Day18 where

-- Question source: https://adventofcode.com/2021/day/18

import           Control.Monad
import           Data.Bifunctor
import qualified Data.Text as T
import           Utilities

add :: [(Integer, Int)] -> [(Integer, Int)] -> [(Integer, Int)]
add = (eval .) . (. map (second (1 +))) . (++) . map (second (1 +))
  where
    eval xs
      | isReduced = eval xs'
      | otherwise = xs
      where
        (xs', isReduced) = eval1 xs
    eval1 xs
      | exploded  = first tail e
      | otherwise = first tail $ split (0, 0) xs
      where
        explode (oI, oN) [(i, n), (i', _)]
          | n > 4 = ([(oI + i, oN), (0, n - 1)], True)
        explode (oI, oN) ((i, n) : (i', _) : (nI, nN) : xs)
          | n > 4 = ((oI + i, oN) : (0, n - 1) : (i' + nI, nN) : xs, True)
        explode (oI, oN) ((i, n) : xs) = first ((oI, oN) :) $ explode (i, n) xs
        explode (oI, oN) []            = ([(oI, oN)], False)
        e@(afterExploding, exploded)   = explode (0, 0) xs
        split (oI, oN) []              = ([(oI, oN)], False)
        split (oI, oN) ((i, n) : xs)
          | i > 9     = let (q, r) = quotRem i 2
                        in  ((oI, oN) : (q, n + 1) : (q + r, n + 1) : xs, True)
          | otherwise = first ((oI, oN) :) $ split (i, n) xs

magnitude :: [(Integer, Int)] -> Integer
magnitude xs
  | [(i, 0)] <- xs' = i
  | otherwise       = magnitude xs'
  where
    magnitude1 ((i, n) : (i', n') : xs)
      | n == n' = (3 * i + 2 * i', n - 1) : xs
      | n < n'  = (i, n) : magnitude1 ((i', n') : xs)
    magnitude1 xs = xs
    xs'           = magnitude1 xs

day18Part1 :: [[(Integer, Int)]] -> Integer
day18Part1 = magnitude . foldl1 add

day18Part2 :: [[(Integer, Int)]] -> Integer
day18Part2 = maximum . join (liftM2 ((magnitude .) . add))

main :: IO ()
main = do
  input <- map (lineParser 0 . T.unpack) . T.lines <$> readInput "day18"
  print $ day18Part1 input
  print $ day18Part2 input
  where
    -- Parse into list of pairs where the second element represents the depth.
    lineParser _ ""         = []
    lineParser n ('[' : xs) = lineParser (n + 1) xs
    lineParser n (']' : xs) = lineParser (n - 1) xs
    lineParser n (',' : xs) = lineParser n xs
    lineParser n (i   : xs) = (read [i], n) : lineParser n xs
