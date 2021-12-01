import qualified Data.Text as T
import           Utilities

day1Part1 :: [Integer] -> Integer
day1Part1 = go 0
  where
    go n (x : x' : xs) = go (if x < x' then n + 1 else n) $ x' : xs
    go n _             = n

day1Part2 :: [Integer] -> Integer
day1Part2 = day1Part1 . threeWindow

threeWindow :: [Integer] -> [Integer]
threeWindow (x : x' : x'' : xs) = (x + x' + x'') : threeWindow (x' : x'': xs)
threeWindow _                   = []

main :: IO ()
main = do
  input <- fmap readInt . T.lines <$> readInput "day1"
  print $ day1Part1 input
  print $ day1Part2 input
