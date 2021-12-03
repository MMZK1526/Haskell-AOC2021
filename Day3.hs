{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- Question source: https://adventofcode.com/2021/day/3

import           Control.Monad
import           Data.Bifunctor
import           Data.List
import qualified Data.Text as T
import           Utilities

readBin :: String -> Integer
readBin = binToDec . read
  where
    binToDec 0 = 0
    binToDec i = 2 * binToDec (div i 10) + mod i 10

-- | Find the most common bit in the given bit-String.
bitTest :: String -> Char
bitTest = go 0 0
  where
    go a b ('0' : xs) = go (a + 1) b xs
    go a b ('1' : xs) = go a (b + 1) xs
    go a b _          = if a > b then '0' else '1'

day3Part1 :: [String] -> Integer
day3Part1 ts = gamma * epsilon
  where
    gamma   = readBin $ bitTest <$> transpose ts
    epsilon = 2 ^ length (head ts) - 1 - gamma

-- | OGR & CO2 calculated with @work@.
-- Using zippers to traverse through list without losing information.
day3Part2 :: [String] -> Integer
day3Part2 = uncurry (*) . bimap readBin readBin . join work . fmap (, [])
  where
    work [(xf, xb)] [(xf', xb')] = (reverse xb ++ xf, reverse xb' ++ xf')
    work zs zs'                  = work (go (p zs) zs) (go (not . p zs') zs')
    go _ [(xf, xb)]              = [(xf, xb)]
    go f zs                      = next <$> filter (f . head . fst) zs
    p zs                         = (bitTest (head . fst <$> zs) ==)
    next (x : xf, xb)            = (xf, x : xb)

main :: IO ()
main = do
  input <- fmap T.unpack . T.lines <$> readInput "day3"
  print $ day3Part1 input
  print $ day3Part2 input
