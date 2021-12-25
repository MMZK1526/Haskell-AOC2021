{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day24 where

-- Question source: https://adventofcode.com/2021/day/24

import           Data.Array (Array)
import qualified Data.Array as A
import           Data.Array.ST (STArray)
import qualified Data.Array.ST as STA
import           Data.Bifunctor
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Gadgets.Array as A
import           Utilities

-- | The number of inputs processed and the temp result.
type Cache = (Int, Map Char Int)

runALU :: Array Int [[String]] -> Cache -> [Int] -> Cache
runALU cArr (cI, cV) = go cI cV (cArr A.! cI)
  where
    go cI state []                     _   = (cI, state)
    go cI state (("inp" : _   ) : _  ) []  = (cI, state)
    go cI state ((op    : args) : ops) ins = case op of
      "inp" -> go (cI + 1) (M.insert (head $ head args) i state) ops is
      "add" -> to (M.adjust (+ value a2) (head a1) state) ops ins
      "mul" -> to (M.adjust (* value a2) (head a1) state) ops ins
      "div" -> to (M.adjust (`div` value a2) (head a1) state) ops ins
      "mod" -> to (M.adjust (`mod` value a2) (head a1) state) ops ins
      "eql" -> to (M.adjust (fromEnum . (value a2 ==)) (head a1) state) ops ins
      where
        (to, [a1, a2]) = (go cI, args)
        i : is         = ins
        value str      = fromMaybe (read str) $ M.lookup (head str) state

searchRoot :: Bool -> Array Int [[String]] -> [Bool] -> Integer
searchRoot isInc cArr = toInt . reverse . snd . head 
                      . filter ((== 0) . (M.! 'z') . snd . fst) 
                      . foldr ((=<<) . uncurry . try) 
                              [((0, M.fromList $ zip "wxyz" $ repeat 0), [])] 
  where
    bases        = (if isInc then id else reverse) [1..9]
    try b cache@(cI, cV) xs 
      | not b                  = [(runALU cArr cache [i], i : xs) | i <- bases]
      | index < 1 || index > 9 = []
      | otherwise              = [(runALU cArr cache [index], index : xs)]
      where
        index = cV M.! 'z' `mod` 26 + read (last ((cArr A.! cI) !! 5))
    toInt xs     = read $ concat $ show <$> xs

day24Part1 :: Array Int [[String]] -> [Bool] -> Integer
day24Part1 = searchRoot False

day24Part2 :: Array Int [[String]] -> [Bool] -> Integer
day24Part2 = searchRoot True

main :: IO ()
main = do
  input <- fmap (map T.unpack . T.splitOn " ") . T.lines <$> readInput "day24"
  let (alu, rs) = (toALU input, reducers input)
  print $ day24Part1 alu rs
  print $ day24Part2 alu rs
  where
    toALU           = A.fromList . scanr (++) [] . unfoldr go
    reducers        = reverse . map ((== "26") . last) 
                    . filter ((["div", "z"] ==) . take 2)
    go []           = Nothing 
    go (str : strs) = Just $ first (str :) $ break ((== "inp") . head) strs
