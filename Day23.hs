{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- module Day23 where

-- Question source: https://adventofcode.com/2021/day/23

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Array (Array)
import qualified Data.Array as A
import           Data.Array.ST (STArray)
import qualified Data.Array.ST as STA
import           Data.Bifunctor
import           Data.Char
import           Data.Either
import qualified Data.Foldable as F
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as L
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Tuple
import qualified Gadgets.Array as A
import qualified Gadgets.Array.Mutable as MA
import qualified Gadgets.Array.ST as MA
import qualified Gadgets.Counter as C
import           Gadgets.Maybe
import qualified Gadgets.Set as S
import qualified Numeric as N
import           Text.Parsec
import           Text.Parsec.String
import           Utilities

type Game =
  (Map String (Int, Int), Map String Int, Map (Int, Int) String, Array Int Int)

day23 :: Game
day23 = (M.fromList [("A1", (2,1)),("A2", (8,2)),("B1", (2,2)),("B2", (6,1)),("C1", (4,2)),("C2", (8,1)),("D1", (6,2)),("D2", (4,1))], M.empty, M.fromList [((2,1), "A1"),((2,2), "B1"),((4,1), "D2"),((4,2), "C1"),((6,1), "B2"),((6,2), "D1"),((8,1), "C2"),((8,2), "A2")], A.fromList $ replicate 4 $ -1)

day23' :: Game
day23' = (M.fromList [("A1", (2,1)),("A2", (8,4)),("B1", (2,4)),("B2", (6,1)),("C1", (4,4)),("C2", (8,1)),("D1", (6,4)),("D2", (4,1)),("D3", (2,2)),("C3", (4,2)),("B3", (6,2)),("A3", (8,2)),("D4", (2,3)),("B4", (4,3)),("A4", (6,3)),("C4", (8,3))], M.empty, M.fromList [((2,1), "A1"),((2,4), "B1"),((4,1), "D2"),((4,4), "C1"),((6,1), "B2"),((6,4), "D1"),((8,1), "C2"),((8,4), "A2"),((2,2), "D3"),((4,2), "C3"),((6,2), "B3"),((8,2), "A3"),((2,3), "D4"),((4,3), "B4"),((6,3), "A4"),((8,3), "C4")], A.fromList $ replicate 4 $ -1)

config :: Map Char (Int, Int)
config = M.fromAscList
  [('A', (1, 2)), ('B', (10, 4)), ('C', (100, 6)), ('D', (1000, 8))]

homeX, cost :: String -> Int
homeX = snd . fromJust . flip M.lookup config . head
cost  = fst . fromJust . flip M.lookup config . head

toIx :: Int -> Int
toIx = subtract 1 . (`div` 2)

doors :: [Int]
doors = [2, 4, 6, 8]

doorS :: Set Int
doorS = S.fromList doors

moves :: Int -> (Int, Game) -> [(Int, Game)]
moves depth m@(c, (ps, ts, bs, arr))
  | not $ null t2h = [head t2h]
  | not $ null h2h = [head h2h]
  | length ts == 7 = []
  | otherwise      = h2t
  where
    t2h       = concatMap (\(str, x) -> goHome c str x (x, 0)) (M.assocs ts)
    h2h       = concatMap (\(str, p@(x, y)) ->
                           goHome (c + y * cost str) str x p) hs
    h2t       = concatMap (uncurry go) hs
    hs        = catMaybes [findCol x | x <- doors, arr A.! toIx x < 0]
    findCol x = msum $ map (\y -> fmap (, (x, y)) $ bs M.!? (x, y)) [1..depth]
    go str p@(x, y)
      = concat [goHori (c + y * cost str) str x p i | i <- [-1, 1]]
    goHome c str x p@(x', y)
      | hy < 0     = []
      | cantGoHome = []
      | y == depth = [(c', (ps', ts', bs', arr A.// [h', (toIx x', depth)]))]
      | otherwise  = [(c', (ps', ts', bs', arr A.// [h']))]
        where
          h@(hx, hy) = (homeX str, arr A.! toIx hx)
          cantGoHome = any ((`M.member` bs) . (, 0))
                            [min hx (x + 1)..max hx (x - 1)]
          c'         = c + cost str * (hy + abs (x - hx))
          (ps', ts') = (M.insert str h ps, M.delete str ts)
          (bs', h')  = (M.insert h str $ M.delete p bs, (toIx hx, hy - 1))
    goHori c str x p@(x', y) dir
      | dir * (x - 5) > 5  = []
      | M.member (x, 0) bs = []
      | S.member x doorS   = next
      | y == depth         = (c, (ps', ts', bs', arr A.// [h'])) : next
      | otherwise          = (c, (ps', ts', bs', arr)) : next
      where
        (c', next) = (c + cost str, goHori c' str (x + dir) p dir)
        (ps', ts') = (M.delete str ps, M.insert str x ts)
        (bs', h')  = (M.insert (x, 0) str $ M.delete p bs, (toIx x', depth))

isSorted :: Int -> Game -> Bool
isSorted depth (_, _, bs, _) = and [headEq (2, i) 'A' | i <- [1..depth]]
                            && and [headEq (4, i) 'B' | i <- [1..depth]]
                            && and [headEq (6, i) 'C' | i <- [1..depth]]
                            && and [headEq (8, i) 'D' | i <- [1..depth]]
  where
    headEq p c = fmap head (bs M.!? p) == Just c

runGame :: Int -> Game -> Int
runGame d game = go 0 (0, game)
  where
    go i m@(c, game)
      | isSorted d game = c
      | otherwise       = minimum $ maxBound : map (go (i + 1)) (moves d m)

day23Part1 :: Game -> Int
day23Part1 = runGame 2

day23Part2 :: Game -> Int
day23Part2 = runGame 4

main :: IO ()
main = do
  input <- T.lines <$> readInput "day23"
  print $ day23Part1 day23
  print $ day23Part2 day23'
