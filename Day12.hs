{-# LANGUAGE OverloadedStrings #-}

module Day12 where

-- Question source: https://adventofcode.com/2021/day/12

import           Control.Monad
import           Data.Char
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Tuple
import           Utilities

day12Part1 :: [(String, String)] -> Integer
day12Part1 = go "start" . filter (("start" /=) . snd)
  where
    go "end" _     = 1
    go start paths = sum $ map (\next -> go next (filterPath next)) nexts
      where
        nexts = map snd $ filter ((== start) . fst) paths
        filterPath next
          | isUpper (head next) = paths
          | otherwise           = filter ((next /=) . snd) paths

-- | Set to record visited caves; bool to denote allowing repetitions.
day12Part2 :: [(String, String)] -> Integer
day12Part2 = go True S.empty "start" . filter (("start" /=) . snd)
  where
    go _ _ "end" _ = 1
    go canGoBack set start paths
      | isUpper (head start) = sum $ map (flip (go canGoBack set) paths) nexts
      | start `notElem` set  = sum $ map (flip (go canGoBack set') paths) nexts
      | canGoBack            = sum $ map (flip (go False set') paths) nexts
      | otherwise            = 0
      where
        set'  = S.insert start set
        nexts = map snd $ filter ((== start) . fst) paths

main :: IO ()
main = do
  input <- ap (++) (map swap) . map lineParser . T.lines <$> readInput "day12"
  print $ day12Part1 input
  print $ day12Part2 input
  where
    lineParser str = let [a, b] = T.unpack <$> T.splitOn "-" str in (a, b)
