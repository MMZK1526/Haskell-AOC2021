{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

import           Control.Monad
import           Control.Monad.ST
import           Data.Array (Array)
import           Data.Array.ST (STArray)
import qualified Data.Array.ST as STA
import           Data.Bifunctor
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Gadgets.Array as A
import qualified Gadgets.Array.Mutable as MA
import qualified Gadgets.Array.ST as MA
import           Gadgets.Maybe
import qualified Gadgets.Set as S
import           Utilities

-- Question source: https://adventofcode.com/2021/day/11

-- | Represents a single octapus. True if he is about to flash.
type Oct = (Int, Bool)

initOct :: [[Int]] -> [[Oct]]
initOct = map (map (, False))

-- | Simulates one round and returns the number of flashed octapuses.
simulate :: STArray s (Int, Int) Oct -> ST s Int
simulate arrST = do
  bds <- STA.range <$> STA.getBounds arrST
  let affect S.Empty = return 0
      affect flashes = do
        flashes' <- fmap concat $ forM (S.toList flashes) $ \e -> do
          mo <- arrST MA.!? e
          if   not $ flashing mo
          then return []
          else do
          arrST MA.=: e $ (0, False)
          fmap catMaybes $ forM (uncurry nbrs e) $ \e -> do
          mo <- MA.adjust' arrST updatePsv e
          return $ toMaybe ((flashing mo &&) . (`notElem` flashes)) e
        (length flashes +) <$> affect (S.fromList flashes')
  flashes <- fmap (S.fromAscList . catMaybes) $ forM bds $ \e -> do
    mo <- MA.adjust' arrST updateAct e
    return $ toMaybe (\_ -> flashing mo) e
  affect flashes
  where
  flashing mo    = mo == Just (0, True)
  updateAct o -- Used to increment one's self.
    | fst o == 9 = (0, True)
    | otherwise  = (fst o + 1, False)
  updatePsv o -- Used to increment neighbours.
    | fst o == 0 = o
    | otherwise  = updateAct o
  nbrs x y = [bimap f g (x, y) | (f, g) <- join (liftM2 (,)) [pred, id, succ]]

day11Part1 :: [[Int]] -> Int
day11Part1 xz = runST $ do
  arrST <- MA.thaw $ A.from2DListC $ initOct xz
  fmap sum $ forM [1..100] $ \_ -> simulate arrST

day11Part2 :: [[Int]] -> Integer
day11Part2 xz = runST $ do
  arrST <- MA.thaw $ A.from2DListC $ initOct xz
  size  <- length . STA.range <$> STA.getBounds arrST
  let findSync x = do
      count <- simulate arrST
      if count == size then return x else findSync (x + 1)
  findSync 1

main :: IO ()
main = do
  input <- map (map (read . (: [])) . T.unpack) . T.lines <$> readInput "day11"
  print $ day11Part1 input
  print $ day11Part2 input
