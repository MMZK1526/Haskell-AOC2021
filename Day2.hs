{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import           Control.Monad.Trans.State
import           Data.Text (Text)
import qualified Data.Text as T
import           Utilities

day2Part1 :: [(Text, Integer)] -> Integer
day2Part1 = uncurry (*) . foldr go (0, 0)
  where
    go ("forward", c) (x, y) = (x + c, y)
    go ("down",    c) (x, y) = (x, y + c)
    go ("up",      c) (x, y) = (x, y - c)

day2Part2 :: [(Text, Integer)] -> Integer
day2Part2 cs = uncurry (*) $ fst $ execState (mapM_ go cs) ((0, 0), 0)
  where
    go (op, c) = do
      ((x, y), a) <- get
      put $ case op of
        "down"    -> ((x, y), a + c)
        "up"      -> ((x, y), a - c)
        "forward" -> ((x + c, y + a * c), a)

main :: IO ()
main = do
  input <- fmap ((\[op, c] -> (op , readInt c)) . T.words) . T.lines 
    <$> readInput "day2"
  print $ day2Part1 input
  print $ day2Part2 input
