{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day19 where

-- Question source: https://adventofcode.com/2021/day/19

import           Control.Monad
import           Data.Bifunctor
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Tuple
import qualified Gadgets.Counter as C
import qualified Gadgets.Set as S
import           Utilities

type Rotation = [(Int, Int)] -- The second Int represents the sign

comp :: Rotation -> Rotation -> Rotation
comp r r' = map (\i -> let (i', s) = r !! i in second (s *) $ r' !! i') [0..2]

rot :: Rotation -> [Int] -> [Int]
rot r p = map (\i -> let (i'', s) = r !! i in s * p !! i'') [0..2]

rid :: Rotation
rid = [(0, 1), (1, 1), (2, 1)]

rotations :: [Rotation]
rotations
  = nub $ join (liftM2 comp)
  $ concatMap (take 4 . flip iterate rid . comp) [ [(0, 1), (2, 1), (1, -1)]
                                                 , [(2, -1), (1, 1), (0, 1)]
                                                 , [(1, 1), (0, -1), (2, 1)] ]

-- Finds the transformation and offset of each scanner.
scannerPs :: [[[Int]]] -> Map Int (Rotation, [Int])
scannerPs pz = findScannerPos (S.singleton 0) (M.singleton 0 (rid, [0, 0, 0]))
  where
    cmp ps ps'
      = maybeToList $ listToMaybe $ dropWhile ((< 12) . fst . snd)
      $ map (second (maximum . map swap . C.group . liftM2 (zipWith (-)) ps))
      $ ap (,) ((<$> ps') . rot) <$> rotations
    scannerRPos
      = do ((i, ps), (i', ps')) <- join (liftM2 (,)) $ zip [0..] pz
           guard $ i /= i'
           commons              <- cmp ps ps'
           return ((i, i'), second snd commons)
    findScannerPos S.Empty      known = known
    findScannerPos (f S.:<| fs) known = go fs known scannerRPos
      where
        go fs k [] = findScannerPos fs k
        go fs k (((i, j), (t, pos)) : ps)
          | f == i && M.notMember j k = go (S.insert j fs) k' ps
          | otherwise                 = go fs k ps
          where
            k' = let (tI, posI) = k M.! i
                 in  M.insert j (comp tI t, zipWith (+) posI (rot tI pos)) k

day19Part1 :: Map Int (Rotation, [Int]) -> [[[Int]]] -> Int
day19Part1 scs = length . S.fromList . concat . zipWith merge [0..]
  where
    merge i = let (r, oft) = scs M.! i in map (flip (zipWith (+)) oft . rot r)

day19Part2 :: Map Int (Rotation, [Int]) -> Int
day19Part2 = maximum . join (liftM2 ((sum .) . zipWith ((abs .) . (-)))) 
           . M.elems . M.map snd

main :: IO ()
main = do
  input <- fmap lineParser . T.splitOn "\n\n" <$> readInput "day19"
  let scanners = scannerPs input
  print $ day19Part1 scanners input
  print $ day19Part2 scanners
  where
    lineParser = map (map readInt . T.splitOn ",") . tail . T.lines
