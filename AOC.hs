import           Control.Monad
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24

days :: [IO ()]
days = [ Day1.main
       , Day2.main
       , Day3.main
       , Day4.main
       , Day5.main
       , Day6.main
       , Day7.main
       , Day8.main
       , Day9.main
       , Day10.main
       , Day11.main
       , Day12.main
       , Day13.main
       , Day14.main
       , Day15.main
       , Day16.main
       , Day17.main
       , Day18.main
       , Day19.main
       , Day20.main
       , Day21.main
       , Day22.main 
       , Day23.main
       , Day24.main ]

main :: IO ()
main = ((flip .) . flip) zipWithM_ [1..] days $ \i response -> do
  putStrLn $ "Day " ++ show i ++ ":"
  response
