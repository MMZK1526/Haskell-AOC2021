{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import           Control.Monad
import           Data.Array (Array)
import qualified Data.Array as A
import qualified Data.Foldable as F
import           Data.Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as L
import qualified Data.Text as T
import           Utilities

type Board = Seq (Integer, Bool)

initBoard :: [[Integer]] -> Board
initBoard = L.fromList . fmap (, False) . concat

updateBoard :: Integer -> Board -> Board
updateBoard i board = work 0
  where
    work 25 = board
    work n
      | v == (i, True)   = board
      | v == (i, False)  = L.update n (i, True) board
      | otherwise        = work $ n + 1
      where
        v = board `L.index` n

-- | Returns Nothing if the "Board" does not win, otherwise "Just" itself.
checkBoard :: Board -> Maybe Board
checkBoard board
  | or [checkRow i || checkCol i| i <- [0..4]] = Just board
  | otherwise                                  = Nothing
  where
    checkRow i = and $ snd . (board `L.index`) . (5 * i +) <$> [0..4]
    checkCol i = and $ snd . (board `L.index`) . (i +) . (5 *) <$> [0..4]

day4Part1 :: [Integer] -> [Board] -> Integer
day4Part1 (n : ns) boards = case msum winConfig of
  Nothing -> day4Part1 ns boards'
  Just b  -> n * sum (map fst $ filter (not . snd) $ F.toList b)
  where
    winConfig = checkBoard <$> boards'
    boards'   = updateBoard n <$> boards

day4Part2 :: [Integer] -> [Board] -> Integer
day4Part2 (n : ns) boards = case loseBoards of
  []  -> n * sum (map fst $ filter (not . snd) $ F.toList $ head boards')
  _   -> day4Part2 ns loseBoards
  where
    loseBoards = filter (isNothing . checkBoard) boards'
    boards'    = updateBoard n <$> boards

main :: IO ()
main = do
  nums' : _ : boards' <- T.lines <$> readInput "day4"
  let nums   = numParser nums'
  let boards = initBoard <$> boardParser boards'
  print $ day4Part1 nums boards
  print $ day4Part2 nums boards
  where
    numParser str  = readInt <$> T.splitOn "," str
    boardParser [] = [[]]
    boardParser (b : bs)
      | T.null b  = [] : bz
      | otherwise
        = ((readInt <$> filter (not . T.null) (T.splitOn " " b)) : b') : bs'
      where
        bz@(b' : bs') = boardParser bs
