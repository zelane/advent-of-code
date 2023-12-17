module Main where

import Day1 (solve)
import Day10 (solve)
import Day11 (solve)
import Day12 (solve)
import Day13 (solve)
import Day14 (solve)
import Day15 (solve)
import Day16 (solve)
import Day2 (solve)
import Day3 (solve)
import Day4 (solve)
import Day5 (solve)
import Day6 (solve)
import Day7 (solve)
import Day8 (solve)
import Day9 (solve)
import System.Environment (getArgs)

solveDay :: String -> IO ()
solveDay "1" = Day1.solve $ readFile "input/1.txt"
solveDay "2" = Day2.solve $ readFile "input/2.txt"
solveDay "3" = Day3.solve $ readFile "input/3.txt"
solveDay "4" = Day4.solve $ readFile "input/4.txt"
solveDay "5" = Day5.solve $ readFile "input/5.txt"
solveDay "6" = Day6.solve $ readFile "input/6.txt"
solveDay "7" = Day7.solve $ readFile "input/7.txt"
solveDay "8" = Day8.solve $ readFile "input/8.txt"
solveDay "9" = Day9.solve $ readFile "input/9.txt"
solveDay "10" = Day10.solve $ readFile "input/10.txt"
solveDay "11" = Day11.solve $ readFile "input/11.txt"
solveDay "12" = Day12.solve $ readFile "input/12.txt"
solveDay "13" = Day13.solve $ readFile "input/13.txt"
solveDay "14" = Day14.solve $ readFile "input/14.txt"
solveDay "15" = Day15.solve $ readFile "input/15.txt"
solveDay "16" = Day16.solve $ readFile "input/16.txt"

main :: IO ()
main = do
  args <- getArgs
  let day = head args
  solveDay day
