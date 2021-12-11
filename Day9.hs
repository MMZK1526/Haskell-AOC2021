{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import           Data.Array (Array)
import qualified Data.Array as A
import qualified Data.Foldable as F
import           Data.List
import           Data.Maybe
import qualified Data.Sequence as L
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Gadgets.Array as A
import           Utilities

-- Question source: https://adventofcode.com/2021/day/9

mapWithIndex2D :: (Int -> Int -> a -> b) -> [[a]] -> [[b]]
mapWithIndex2D f = zipWith (flip zipWith [0..] . flip f) [0..]

day9Part1 :: [[Char]] -> Int
day9Part1 hz = sum $ concat $ mapWithIndex2D bar hz
  where
    hzArr     = A.from2DListC hz
    bar x y h = if h < minimum (neighbours x y) then read [h] + 1 else 0
    neighbours x y
      = mapMaybe (hzArr A.!?) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- | Find all non-9 coordinates, go through them horizontally and vertically,
-- merging the adjacent points that are not yet in the same set.
day9Part2 :: [[Char]] -> Int
day9Part2 hz = product $ take 3 $ sortOn negate $ F.toList (length <$> basins)
  where
    is9 x y h             = if h == '9' then Nothing else Just (x, y)
    non9s                 = catMaybes $ concat $ mapWithIndex2D is9 hz
    non9Set               = L.fromList $ map S.singleton non9s
    pos c bs              = length $ L.takeWhileL not (S.member c <$> bs)
    (non9sH, non9sV)      = (sortOn snd non9s, sort non9s)
    adjH (x, y) (x', y')  = x + 1 == x' && y == y'
    adjV (x, y) (x', y')  = y + 1 == y' && x == x'
    basins                = mergeBy adjV non9sV (mergeBy adjH non9sH non9Set)
    mergeBy f (c : cs) bs = snd $ foldl (mergeOne f) (c, bs) cs
    mergeOne f (c, bs) c'
      | not (f c c') || i == j = (c', bs)
      | otherwise              = (c', mergeEntries i j)
      where
        (i, j)           = (pos c bs, pos c' bs)
        mergeEntries i j = let i' : j' : _ = sort [i, j] 
                           in  L.deleteAt i' $ L.deleteAt j' bs
                          L.|> S.union (bs `L.index` i) (bs `L.index` j)

main :: IO ()
main = do
  input <- map T.unpack . T.lines <$> readInput "day9"
  print $ day9Part1 input
  print $ day9Part2 input
