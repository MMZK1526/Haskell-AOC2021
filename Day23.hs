{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Day23 where

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

type Game = (Map String (Int, Int), Map (Int, Int) String)

day23 :: Game
day23 = (M.fromList [("A1", (2,1)),("A2", (8,2)),("B1", (2,2)),("B2", (6,1)),("C1", (4,2)),("C2", (8,1)),("D1", (6,2)),("D2", (4,1))], M.fromList [((2,1), "A1"),((2,2), "B1"),((4,1), "D2"),((4,2), "C1"),((6,1), "B2"),((6,2), "D1"),((8,1), "C2"),((8,2), "A2")])

test :: Game
test = (M.fromList [("A1", (2,2)),("A2", (8,2)),("B1", (2,1)),("B2", (6,1)),("C1", (4,1)),("C2", (6,2)),("D1", (4,2)),("D2", (8,1))], M.fromList [((2,1), "B1"),((2,2), "A1"),((4,1), "C1"),((4,2), "D1"),((6,1), "B2"),((6,2), "C2"),((8,1), "D2"),((8,2), "A2")])

config :: Map Char (Int, Int)
config = M.fromAscList
  [('A', (1, 2)), ('B', (10, 4)), ('C', (100, 6)), ('D', (1000, 8))]

headEq bs p c = fmap head (bs M.!? p) == Just c

homeX :: String -> Int
homeX = snd . fromJust . flip M.lookup config . head

cost :: String -> Int
cost = fst . fromJust . flip M.lookup config . head

moves :: (Int, Game) -> [(Int, Game)]
moves m@(c, (ps, bs))
  | not $ null topMoves = [head topMoves]
  | otherwise           = btmMoves
  where
    assocs   = M.assocs ps
    topMoves = concatMap (\(str, p@(x, y)) -> goHome c str x p) $ filter ((== 0) . snd . snd) assocs
    btmMoves = concatMap (uncurry go) $ catMaybes [findCol x | x <- foo]
    bar = catMaybes [findCol x | x <- foo]
    -- filter ((< 2) . snd . snd) assocs
    foo      = [2, 4, 6, 8]
    findCol x = msum $ map (\y -> fmap (, (x, y)) $ bs M.!? (x, y)) [1, 2]
    go str p@(x, y)
      | x == homeX str && and [headEq bs (x, i) (head str) | i <- [(y + 1)..2]] = []
      | not $ null homeRoute = homeRoute
      | otherwise = goLeft (c + y * cost str) str x p ++ goRight (c + y * cost str) str x p
      where
        homeRoute = goHome (c + y * cost str) str x p
    goHome c str x p
      | M.member (hx, 1) bs = []
      | any (\p -> p /= (x, 0) && p `M.member` bs) $ zip [min hx x..max hx x] (repeat 0) = []
      | M.notMember (hx, 2) bs = [(c + cost str * (2 + abs (x - hx)), (M.insert str (hx, 2) ps, M.insert (hx, 2) str $ M.delete p bs))]
      | head (bs M.! (hx, 2)) /= head str = []
      | otherwise = [(c + cost str * (1 + abs (x - hx)), (M.insert str (hx, 1) ps, M.insert (hx, 1) str $ M.delete p bs))]
      where
        hx   = homeX str
    goLeft c str x p
      | x < 0 = []
      | M.member (x, 0) bs = []
      | x `elem` [2, 4, 6, 8] = goLeft (c + cost str) str (x - 1) p
      | otherwise = (c, (M.insert str (x, 0) ps, M.insert (x, 0) str $ M.delete p bs)) : goLeft (c + cost str) str (x - 1) p
    goRight c str x p
      | x > 10 = []
      | M.member (x, 0) bs = []
      | x `elem` [2, 4, 6, 8] = goRight (c + cost str) str (x + 1) p
      | otherwise = (c, (M.insert str (x, 0) ps, M.insert (x, 0) str $ M.delete p bs)) : goRight (c + cost str) str (x + 1) p

isSorted :: Game -> Bool
isSorted (_, bs) = headEq bs (2, 1) 'A' && headEq bs (2, 2) 'A'
                && headEq bs (4, 1) 'B' && headEq bs (4, 2) 'B'
                && headEq bs (6, 1) 'C' && headEq bs (6, 2) 'C'
                && headEq bs (8, 1) 'D' && headEq bs (8, 2) 'D'

day23Part1 :: Game -> Int
day23Part1 game = go 0 (0, game)
  where
    go i (c, game)
      | isSorted game = c
      | otherwise     = minimum $ maxBound : [go (i + 1) x | x <- moves (c, game)]

day23Part2 = undefined

main :: IO ()
main = do
  input <- fmap id . T.lines <$> readInput "day23"
  -- print $ moves (0, test4)
  print $ day23Part1 day23
--   print $ day23Part1 input
--   print $ day23Part2 input
