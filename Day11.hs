{-# LANGUAGE TupleSections #-}

import           Control.Monad
import           Control.Monad.ST
import           Data.Array (Array)
import qualified Data.Array as A
import           Data.Array.ST (STArray)
import qualified Data.Array.ST as STA
import           Data.Bifunctor
import           Data.List
import qualified Data.Text as T
import qualified Gadgets.Array as A
import qualified Gadgets.Array.Mutable as MA
import qualified Gadgets.Array.ST as MA
import           Utilities

-- Question source: https://adventofcode.com/2021/day/11

-- | Represents a single octapus. True if he is about to flash.
type Oct = (Int, Bool)

initOct :: [[Int]] -> [[Oct]]
initOct = map (map (, False))

-- | Simulates one round and returns the number of flashed octapuses.
simulate :: STArray s (Int, Int) Oct -> ST s Integer
simulate arrST = do
  bds <- STA.range <$> STA.getBounds arrST
  let affect = do
      counts <- fmap sum $ forM bds $ \e -> do
        oct <- arrST MA.!? e
        if   oct /= Just (0, True)
        then return 0
        else do
        arrST MA.=: e $ (0, False)
        forM_ (uncurry nbrs e) $ MA.adjust' arrST updatePsv
        return 1
      if counts == 0 then return 0 else (counts +) <$> affect
  forM_ bds $ fmap (== Just (0, True)) . MA.adjust' arrST updateAct
  affect
  where
  updateAct o -- Used to increment one's self.
    | fst o == 9 = (0, True)
    | otherwise  = (fst o + 1, False)
  updatePsv o -- Used to increment neighbours.
    | fst o == 0 = o
    | otherwise  = updateAct o
  nbrs x y = [bimap f g (x, y) | (f, g) <- join (liftM2 (,)) [pred, id, succ]]

day11Part1 :: [[Int]] -> Integer
day11Part1 xz = runST $ do
  arrST <- MA.thaw $ A.from2DListC $ initOct xz
  fmap sum $ forM [1..100] $ \_ -> simulate arrST

day11Part2 :: [[Int]] -> Integer
day11Part2 xz = runST $ do
  arrST <- MA.thaw $ A.from2DListC $ initOct xz
  size  <- genericLength . STA.range <$> STA.getBounds arrST
  let findSync x = do
      count <- simulate arrST
      if count == size then return x else findSync (x + 1)
  findSync 1

main :: IO ()
main = do
  input <- map (map (read . (: [])) . T.unpack) . T.lines <$> readInput "day11"
  print $ day11Part1 input
  print $ day11Part2 input
