{-# LANGUAGE OverloadedStrings #-}

module Day21 where

-- Question source: https://adventofcode.com/2021/day/21

import           Control.Monad
import qualified Data.Array as A
import           Data.Bifunctor
import           Data.Maybe
import qualified Data.Text as T
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
      | sB >= 1000 = i * sA
      | even i     = go (i + 3) d' pA' pB (sA + pA') sB
      | otherwise  = go (i + 3) d' pA pB' sA (sB + pB')
      where
        d'         = (d + 3) `mod'` 100
        (pA', pB') = bimap (move (3 * d + 3)) (move (3 * d + 3)) (pA, pB)

day21Part2 :: Int -> Int -> Integer
day21Part2 a b = uncurry max $ table A.! (a, b, 21, 21, False)
  where
    coes  = A.array (3, 9) $ zip [3..9] [1, 3, 6, 7, 6, 3, 1]
    table = A.tabulate ((0, 0, 1, 1, False), (10, 10, 21, 21, True)) go
    go (pA, pB, rA, rB, b)
      = foldl1 (ap (bimap . (+) . fst) ((+) . snd)) $ roll1 b <$> [3..9]
      where
        roll1 False n = join bimap (* coes A.! n) $ fromMaybe (1, 0)
                      $ table A.!? (move n pA, pB, rA - move n pA, rB, True)
        roll1 True  n = join bimap (* coes A.! n) $ fromMaybe (0, 1)
                      $ table A.!? (pA, move n pB, rA, rB - move n pB, False)

main :: IO ()
main = do
  [a, b] <- fmap (read . pure . T.last) . T.lines <$> readInput "day21"
  print $ day21Part1 a b
  print $ day21Part2 a b
