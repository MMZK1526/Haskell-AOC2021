{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day14 where

-- Question source: https://adventofcode.com/2021/day/14

import           Control.Monad
import           Data.Bifunctor
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Tuple
import qualified Gadgets.Counter as C
import           Utilities

-- | simulates "String" with "Rules" by "Int" times.
simulate :: Int -> Map String Char -> String -> Integer
simulate n rules str = fst (maximum result) - fst (minimum result)
  where
    keys            = C.fromList $ ap (zipWith (\a b -> [a, b])) tail str
    result          = map swap $ C.toPairList $ snd
                    $ iterate work (keys, C.fromList str) !! n
    work (ks, cs)   = fst $ foldl go ((C.empty, cs), ks) $ M.keys ks
    go (st, keys) k = (, keys) $ case rules M.!? k of
      Nothing -> st
      Just v  -> bimap (C.addN [v, c'] n . C.addN [c, v] n) (C.addN v n) st
      where
        (n, [c, c']) = (keys M.! k, k)

day14Part1 :: Map String Char -> String -> Integer
day14Part1 = simulate 10

day14Part2 :: Map String Char -> String -> Integer
day14Part2 = simulate 40

main :: IO ()
main = do
  str' : _ : rules' <- T.lines <$> readInput "day14"
  let (str, rules) = (T.unpack str', ruleParser rules')
  print $ day14Part1 rules str
  print $ day14Part2 rules str
  where
    ruleParser = M.fromList . map (bimap T.unpack T.head . splitPair " -> ")
