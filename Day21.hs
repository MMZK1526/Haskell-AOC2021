module Day21 where

-- Question source: https://adventofcode.com/2021/day/21

import           Control.Monad
import qualified Data.Array as A
import           Data.Bifunctor
import           Data.Maybe
import qualified Data.Text as T
import           Data.Tuple
import qualified Gadgets.Array as A
import           Utilities

mod', move :: Int -> Int -> Int
mod' p q = let r = p `mod` q in if r == 0 then q else r
move     = flip flip 10 . (mod' .) . (+)

day21Part1 :: Int -> Int -> Int
day21Part1 a b = go 0 1 a b 0 0
  where
    go i d pA pB sA sB
      | sA >= 1000 = i * sB
      | otherwise  = go (i + 3) ((d + 3) `mod'` 100) pB pA' sB (sA + pA')
      where
        pA' = move (3 * d + 3) pA

day21Part2 :: Int -> Int -> Integer
day21Part2 a b = uncurry max $ table A.! (a, b, 21, 21)
  where
    coes  = A.array (3, 9) $ zip [3..9] [1, 3, 6, 7, 6, 3, 1]
    table = A.tabulate ((0, 0, 1, 1), (10, 10, 21, 21)) go
    go (pA, pB, rA, rB)
      = foldl1 (ap (bimap . (+) . fst) ((+) . snd)) $ roll1 <$> [3..9]
      where
        roll1 n = swap $ join bimap (* coes A.! n) $ fromMaybe (1, 0)
                       $ table A.!? (pB, move n pA, rB, rA - move n pA)

main :: IO ()
main = do
  [a, b] <- fmap (read . pure . T.last) . T.lines <$> readInput "day21"
  print $ day21Part1 a b
  print $ day21Part2 a b
