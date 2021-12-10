import           Control.Monad
import qualified Data.Text as T
import           Utilities

-- Question source: https://adventofcode.com/2021/day/1

day1Part1 :: [Integer] -> Integer
day1Part1 = sum . map (fromIntegral . fromEnum) . ap (zipWith (<)) tail

day1Part2 :: [Integer] -> Integer
day1Part2 = sum . map (fromIntegral . fromEnum) . ap (zipWith (<)) (drop 3)

main :: IO ()
main = do
  input <- map readInt . T.lines <$> readInput "day1"
  print $ day1Part1 input
  print $ day1Part2 input
