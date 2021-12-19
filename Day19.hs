{-# LANGUAGE OverloadedStrings #-}

module Day19 where

-- Question source: https://adventofcode.com/2021/day/19

import           Control.Monad
import           Data.Array (Array)
import qualified Data.Array as A
import           Data.Bifunctor
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Tuple
import qualified Gadgets.Counter as C
import qualified Gadgets.Set as S
import           Utilities

type Point    = (Int, Int, Int)
type Rotation = Array Int Int
type Rotation_ = Point -> Point

aggr, diff :: Point -> Point -> Point
aggr (x, y, z) (x', y', z') = (x + x', y + y', z + z')
diff (x, y, z) (x', y', z') = (x - x', y - y', z - z')

-- comp :: Rotation -> Rotation -> Rotation
-- comp r r' = id <$> r

-- Finds the transformation and offset of each scanner.
scannerPs :: [[Point]] -> Map Int (Rotation_, Point)
scannerPs pz = findScannerPos (S.singleton 0) (M.singleton 0 (id, (0, 0, 0)))
  where
    cmp ps ps'
      = maybeToList $ listToMaybe $ dropWhile ((< 12) . fst . snd)
      $ fmap ( second (maximum . fmap swap . C.group . liftM2 diff ps) 
             . ap (,) (`map` ps')
             ) rotations
    scannerRPos
      = do ((i, ps), (i', ps')) <- join (liftM2 (,)) $ zip [0..] pz
           guard $ i /= i'
           commons              <- cmp ps ps'
           return ((i, i'), second snd commons)
    findScannerPos S.Empty  known = known
    findScannerPos frontier known = go fs known scannerRPos
      where
        (f, fs)    = fromJust $ S.minView frontier
        go fs k [] = findScannerPos fs k
        go fs k (((i, j), (t, pos)) : ps)
          | f == i && M.notMember j k = go (S.insert j fs) k' ps
          | otherwise                 = go fs k ps
          where
            k'         = M.insert j (tI . t, aggr posI (tI pos)) k
            (tI, posI) = k M.! i
    rotations = [\(x, y, z) -> (-x, y, -z), \(x, y, z) -> (-x, -y, z),
                 \(x, y, z) -> (-x, z, y), \(x, y, z) -> (-x, -z, -y),
                 \(x, y, z) -> (-y, x, z), \(x, y, z) -> (-y, -x, -z),
                 \(x, y, z) -> (-y, z, -x), \(x, y, z) -> (-y, -z, x),
                 \(x, y, z) -> (-z, y, x), \(x, y, z) -> (-z, -y, -x),
                 \(x, y, z) -> (-z, x, -y), \(x, y, z) -> (-z, -x, y),
                 \(x, y, z) -> (x, y, z), \(x, y, z) -> (x, -y, -z),
                 \(x, y, z) -> (x, z, -y), \(x, y, z) -> (x, -z, y),
                 \(x, y, z) -> (y, x, -z), \(x, y, z) -> (y, -x, z),
                 \(x, y, z) -> (y, z, x), \(x, y, z) -> (y, -z, -x),
                 \(x, y, z) -> (z, y, -x), \(x, y, z) -> (z, -y, x),
                 \(x, y, z) -> (z, x, y), \(x, y, z) -> (z, -x, -y)]

day19Part1 :: Map Int (Rotation_, Point) ->[[Point]] -> Int
day19Part1 scanners = length . S.fromList . concat . zipWith merge [0..]
  where
    merge i = let (t, oft) = scanners M.! i in map ((`aggr` oft) . t)

day19Part2 ::  Map Int (Rotation_, Point) -> Int
day19Part2 = maximum . join (liftM2 ((addPt .) . diff)) . M.elems . M.map snd
  where
    addPt (x, y, z) = x + y + z

main :: IO ()
main = do
  input <- fmap lineParser . T.splitOn "\n\n" <$> readInput "day19"
  let scanners = scannerPs input
  print $ day19Part1 scanners input
  print $ day19Part2 scanners
  where
    lineParser = map ((\[x, y, z] -> (x, y, z)) . map readInt . T.splitOn ",")
               . tail . T.lines
