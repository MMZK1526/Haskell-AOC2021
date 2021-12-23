{-# LANGUAGE OverloadedStrings #-}

-- Question source: https://adventofcode.com/2021/day/8

module Day8 where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Tuple
import qualified Gadgets.Map as M
import           Utilities

-- | Decode wirings into "Char" of digits.
decode :: [String] -> Map String Char
decode ds = M.swapkv $
            fst $ mapGets (M.empty, sort <$> ds) [get1748, get36, get9052]
  where
    -- Apply each "get" functions in order.
    mapGets = foldl (\(ws, ds) f -> foldr f (ws, []) ds)
    get1748 d (w, ds) = case length d of
      2 -> (M.insert '1' d w, ds)
      3 -> (M.insert '7' d w, ds)
      4 -> (M.insert '4' d w, ds)
      7 -> (M.insert '8' d w, ds)
      _ -> (w, d : ds)
    get36 d (w, ds)
      | length d == 5 && has1     = (M.insert '3' d w, ds)
      | length d == 6 && not has1 = (M.insert '6' d w, ds)
      | otherwise = (w, d : ds)
      where
        has1 = null (w M.! '1' \\ d)
    get9052 d (w, ds)
      | length d == 6 && has3     = (M.insert '9' d w, ds)
      | length d == 6 && not has3 = (M.insert '0' d w, ds)
      | in6                       = (M.insert '5' d w, ds)
      | length d == 5 && not in6  = (M.insert '2' d w, ds)
      | otherwise                 = (w, d : ds)
      where
        (has3, in6) = (null (w M.! '3' \\ d), null (d \\ w M.! '6'))

day8Part1 :: [([String], [String])] -> Int
day8Part1 = sum . map (length . filter ((`elem` [2, 3, 4, 7]) . length) . snd)

day8Part2 :: [([String], [String])] -> Int
day8Part2 input
  = sum $ [read ((decode ins M.!) . sort <$> outs) | (ins, outs) <- input]

main :: IO ()
main = do
  input <- map parseLine . T.lines <$> readInput "day8"
  print $ day8Part1 input
  print $ day8Part2 input
  where
    parseLine s = let [as, bs] = T.splitOn " "  <$> T.splitOn " | " s
                  in  (T.unpack <$> as, T.unpack <$> bs)
