{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import           Control.Monad
import           Data.Array (Array)
import qualified Data.Array as A
import qualified Data.Foldable as F
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Gadgets.Array as A
import           Utilities

-- | "Array" for the board and "Map" indicating whether a number is crossed.
type Board = (Array Int Integer, Map Integer Bool)

initBoard :: [[Integer]] -> Board
initBoard xs = let cxs = concat xs
               in (A.fromList cxs, M.fromList $ zip cxs $ repeat False)

updateBoard :: Integer -> Board -> Board
updateBoard i (vs, bs) = (vs, M.insert i True bs)

-- | Returns Nothing if the "Board" does not win, otherwise "Just" itself.
checkBoard :: Board -> Maybe Board
checkBoard board@(vs, bs)
  | or [checkRow i || checkCol i | i <- [0..4]] = Just board
  | otherwise                                   = Nothing
  where
    checkRow i = and $ (bs M.!) . (vs A.!) . (5 * i +) <$> [0..4]
    checkCol i = and $ (bs M.!) . (vs A.!) . (i +) . (5 *) <$> [0..4]

day4Part1 :: [Integer] -> [Board] -> Integer
day4Part1 (n : ns) boards = case msum (checkBoard <$> boards') of
  Nothing       -> day4Part1 ns boards'
  Just (vs, bs) -> n * sum (filter (not . (bs M.!)) $ F.toList vs)
  where
    boards' = updateBoard n <$> boards

day4Part2 :: [Integer] -> [Board] -> Integer
day4Part2 (n : ns) boards = case loseBoards of
  [] -> n * sum (filter (not . (bs M.!)) $ F.toList vs)
  _  -> day4Part2 ns loseBoards
  where
    loseBoards             = filter (isNothing . checkBoard) boards'
    boards'@((vs, bs) : _) = updateBoard n <$> boards

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
