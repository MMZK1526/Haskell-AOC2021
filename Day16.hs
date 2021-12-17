module Day16 where

-- Question source: https://adventofcode.com/2021/day/16

import qualified Data.Array as A
import           Data.Bifunctor
import           Data.Either
import qualified Data.Text as T
import           Text.Parsec
import           Utilities

data BITS = V Int Integer | O Int Int [BITS]

decodeBITS :: String -> BITS
decodeBITS = fst . fromRight undefined . parse parseBITS "" . concatMap toBin
  where
    toBin x    = let b = hexToBin [x] in replicate (4 - length b) '0' ++ b
    parseBITS  = do
      v <- binToDec <$> count 3 anyChar
      t <- binToDec <$> count 3 anyChar
      second (+ 6) <$> if t == 4 then parseV v else parseO v t
    parseV v   = first (V v . binToDec) <$> parseNums
    parseNums  = do
      h : n <- count 5 anyChar
      if h == '0' then return (n, 5) else bimap (n ++) (+ 5) <$> parseNums
    parseO v t = do
      i <- anyChar
      if   i == '0'
      then do
        l <- binToDec <$> count 15 anyChar
        bimap (O v t) (16 +) <$> parseLen l
      else do
        c <- binToDec <$> count 11 anyChar
        bimap (O v t) ((12 +) . sum) . unzip <$> count c parseBITS
    parseLen l
      | l <= 0    = return ([], 0)
      | otherwise = do
        (bit, n)   <- parseBITS
        (bits, n') <- parseLen (l - n)
        return (bit : bits, n + n')

day16Part1 :: String -> Int
day16Part1 = sumV . decodeBITS
  where
    sumV (V v _)      = v
    sumV (O v _ bits) = v + sum (sumV <$> bits)

day16Part2 :: String -> Integer
day16Part2 = evalBITS . decodeBITS
  where
    evalBITS (V _ v) = v
    evalBITS (O _ t bits)
      | t < 4     = (listOps A.! t) vs
      | otherwise = fromIntegral $ fromEnum $ (pairOps A.! t) $ listToPair vs
      where
        vs      = evalBITS <$> bits
        listOps = A.listArray (0, 3) [sum, product, minimum, maximum]
        pairOps = A.listArray (5, 7) $ uncurry <$> [(>), (<), (==)]

main :: IO ()
main = do
  input <- T.unpack <$> readInput "day16"
  print $ day16Part1 input
  print $ day16Part2 input
