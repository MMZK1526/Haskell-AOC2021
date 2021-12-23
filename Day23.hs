{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import qualified Gadgets.Map as M
import           Gadgets.Maybe
import qualified Gadgets.Set as S
import qualified Numeric as N
import           Text.Parsec
import           Text.Parsec.String
import           Utilities

type Game =
  (Map String (Int, Int), Map String Int, Map (Int, Int) String, Array Int Int)

config :: Map Char (Int, Int)
config = M.fromAscList
  [('A', (1, 2)), ('B', (10, 4)), ('C', (100, 6)), ('D', (1000, 8))]

isSorted :: Int -> Game -> Bool
isSorted depth (_, _, bs, _) = and [headEq (2, i) 'A' | i <- [1..depth]]
                            && and [headEq (4, i) 'B' | i <- [1..depth]]
                            && and [headEq (6, i) 'C' | i <- [1..depth]]
                            && and [headEq (8, i) 'D' | i <- [1..depth]]
  where
    headEq p c = fmap head (bs M.!? p) == Just c

-- Basically brute-forcing through possible moves (with certain optimisations).
runGame :: Int -> Game -> Int
runGame d = run 0 . (0 ,)
  where
    run i m@(c, game)
      | isSorted d game = c
      | otherwise       = minimum $ maxBound : map (run (i + 1)) (moves m)
    roomX          = snd . fromJust . flip M.lookup config . head
    cost           = fst . fromJust . flip M.lookup config . head
    toIx           = subtract 1 . (`div` 2)
    (doors, doorS) = ([2, 4, 6, 8], S.fromList doors)
    moves m@(c, (rs, hs, bs, arr))
      | not $ null h2r = [head h2r]
      | not $ null r2r = [head r2r]
      | length hs == 7 = []
      | otherwise      = r2h
      where
        h2r      = concatMap (\(str, x) -> goRoom c str x (x, 0)) (M.assocs hs)
        r2r      = concatMap (\(str, p@(x, y)) ->
                              goRoom (c + y * cost str) str x p) topRs
        r2h      = concatMap (uncurry go) topRs
        topRs    = catMaybes [getRow x | x <- doors, arr A.! toIx x < 0]
        getRow x = msum $ map (\y -> fmap (, (x, y)) $ bs M.!? (x, y)) [1..d]
        go str p@(x, y)
          = concat [goHori (c + y * cost str) str x p i | i <- [-1, 1]]
        goRoom c str x p@(x', y') -- From hallway to room
          | ry < 0     = []
          | cantGoRoom = []
          | y' == d    = [(c', (rs', hs', bs', arr A.// [r', (toIx x', d)]))]
          | otherwise  = [(c', (rs', hs', bs', arr A.// [r']))]
            where
              r@(rx, ry) = (roomX str, arr A.! toIx rx)
              cantGoRoom = any ((`M.member` bs) . (, 0))
                                [min rx (x + 1)..max rx (x - 1)]
              c'         = c + cost str * (ry + abs (x - rx))
              (rs', hs') = (M.insert str r rs, M.delete str hs)
              (bs', r')  = (M.insert r str $ M.delete p bs, (toIx rx, ry - 1))
        goHori c str x p@(x', y') dir -- From room to hallway
          | dir * (x - 5) > 5  = []
          | M.member (x, 0) bs = []
          | S.member x doorS   = next
          | y' == d            = (c, (rs', hs', bs', arr A.// [r'])) : next
          | otherwise          = (c, (rs', hs', bs', arr)) : next
          where
            (c', next) = (c + cost str, goHori c' str (x + dir) p dir)
            (rs', hs') = (M.delete str rs, M.insert str x hs)
            (bs', r')  = (M.insert (x, 0) str $ M.delete p bs, (toIx x', d))

day23Part1 :: Game -> Int
day23Part1 = runGame 2

day23Part2 :: Game -> Int
day23Part2 = runGame 4

main :: IO ()
main = do
  _ : _ : rawLines <- fmap T.unpack . T.lines <$> readInput "day23"
  let rawGame = parseRaw rawLines
  print $ day23Part1 $ toGame rawGame
  print $ day23Part2 $ toGame $ addMd rawGame
  where
    addMd raw  = M.union middle 
               $ M.map (\p@(x, y) -> if y == 1 then p else (x, 4)) raw
    middle     = M.fromList [ ("D3", (2, 2)), ("C3", (4, 2)), ("B3", (6, 2)) 
                            , ("A3", (8, 2)), ("D4", (2, 3)), ("B4", (4, 3))
                            , ("A4", (6, 3)), ("C4", (8, 3)) ]
    toGame raw = (raw, M.empty, M.swapkv raw, A.fromList $ replicate 4 $ -1)
    parseRaw   = fst . foldr (uncurry (flip . flip goLine)) 
                             (M.empty, M.fromList $ zip "ABCD" $ repeat 1) 
                     . zip [1..]
    goLine game i = foldr (go i) game . zip [-1..]
    go i (j, ch) game@(rs, counts)
      | ch `elem` "# " = game
      | otherwise      = ( M.insert (ch : show (counts M.! ch)) (j, i) rs
                         , M.adjust (+ 1) ch counts )
