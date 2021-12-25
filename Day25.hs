module Day25 where

-- Question source: https://adventofcode.com/2021/day/25

import qualified Data.Array as A
import           Data.Bifunctor
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Gadgets.Array as A
import           Utilities

day25Part1 :: (Int, Int) -> Map (Int, Int) Char -> Integer
day25Part1 dims@(width, height) cs = go cs 1
  where
    down  = first (\y -> (y + 1) `mod` height)
    right = second (\y -> (y + 1) `mod` width)
    go cs i
      | not b' = i
      | otherwise = go cs'' $ i + 1
      where
        rsR        = M.keys $ M.filter (== '>') cs
        rsD        = M.keys $ M.filter (== 'v') cs
        (cs'', b') = first (uncurry M.union) 
                   $ foldr goDown ((cs', M.empty), b) rsD
        (cs', b)   = first (uncurry M.union) 
                   $ foldr goRight ((cs, M.empty), False) rsR
        goRight p m@((mcs, mns), _)
          | M.member (right p) cs = m
          | otherwise             = (( M.delete p mcs
                                     , M.insert (right p) '>' mns ), True)
        goDown p m@((mcs, mns), _)
          | M.member (down p) cs' = m
          | otherwise             = (( M.delete p mcs
                                     , M.insert (down p) 'v' mns ), True)

day25Part2 :: Integer
day25Part2 = 42

main :: IO ()
main = do
  input <- fmap T.unpack . T.lines <$> readInput "day25"
  let dims = (length $ head input, length input)
  let cs   = M.filter (/= '.') $ M.fromAscList $ A.assocs $ A.from2DListR input
  print $ day25Part1 dims cs
  print day25Part2
