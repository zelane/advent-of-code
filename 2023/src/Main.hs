module Main where

import Day1 (solve)
import Day2 (solve)
import Day3 (solve)
import Day4 (solve)
import Day5 (solve)
import System.Environment (getArgs)

solveDay :: String -> IO ()
solveDay "1" = Day1.solve $ readFile "input/1.txt"
solveDay "2" = Day2.solve $ readFile "input/2.txt"
solveDay "3" = Day3.solve $ readFile "input/3.txt"
solveDay "4" = Day4.solve $ readFile "input/4.txt"
solveDay "5" = Day4.solve $ readFile "input/5.txt"

main :: IO ()
main = do
  args <- getArgs
  let day = head args
  solveDay day
