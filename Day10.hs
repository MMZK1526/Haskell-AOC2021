module Day10 where

-- Question source: https://adventofcode.com/2021/day/10

import           Data.Either
import           Data.List
import           Data.Maybe
import qualified Data.Text as T
import           Utilities

-- | Valid bracket pairs accompanied with the questions-associated values.
pairs :: [(Char, (Char, (Integer, Integer)))]
pairs = [ (')', ('(', (3, 1)))
        , (']', ('[', (57, 2)))
        , ('}', ('{', (1197, 3)))
        , ('>', ('<', (25137, 4))) ]

-- | If the bracket sequence is invalid, returns the score. Otherwise returns
-- the remaining unmatched brackets.
checker :: String -> Either Integer String
checker = go []
  where
    go ls ""       = Right ls
    go ls (r : rs) = case lookup r pairs of
      Just (c, (v, _)) -> if head ls == c then go (tail ls) rs else Left v
      Nothing          -> go (r : ls) rs

day10Part1 :: [String] -> Integer
day10Part1 = sum . lefts . map checker

day10Part2 :: [String] -> Integer
day10Part2 xs = results !! div (length results) 2
  where
    results  = sort $ map (foldl mark 0) (rights $ map checker xs)
    mark n c = 5 * n + snd (fromJust $ lookup c (snd <$> pairs))

main :: IO ()
main = do
  input <- map T.unpack . T.lines <$> readInput "day10"
  print $ day10Part1 input
  print $ day10Part2 input
