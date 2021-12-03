{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import           Control.Monad
import           Data.Bifunctor
import           Data.List
import qualified Data.Text as T
import           Utilities

binToDec :: Integral i => i -> i
binToDec 0 = 0
binToDec i = 2 * binToDec (div i 10) + mod i 10

bitTest :: String -> Char
bitTest = go 0 0
  where
    go a b ('0' : xs) = go (a + 1) b xs
    go a b ('1' : xs) = go a (b + 1) xs
    go a b _          = if a > b then '0' else '1'

day2Part1 :: [String] -> Integer
day2Part1 ts = gamma * epsilon
  where
    gamma   = binToDec $ read $ bitTest <$> transpose ts
    epsilon = 2 ^ length (head ts) - 1 - gamma

day2Part2 :: [String] -> Integer
day2Part2 = uncurry (*)
          . bimap (binToDec . read) (binToDec . read)
          . join work . fmap (, [])
  where
    p zs                         = (bitTest (head . fst <$> zs) ==)
    go _ [(xf, xb)]              = [(xf, xb)]
    go f zs                      = next <$> filter (f . head . fst) zs
    work [(xf, xb)] [(xf', xb')] = (reverse xb ++ xf, reverse xb' ++ xf')
    work zs zs'                  = work (go (p zs) zs) (go (not . p zs') zs')
    next (x : xf, xb)            = (xf, x : xb)

main :: IO ()
main = do
  input <- fmap T.unpack . T.lines <$> readInput "day3"
  print $ day2Part1 input
  print $ day2Part2 input
