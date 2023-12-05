module Day2 where

import Data.List.Split (splitOn)
import Data.Map qualified as M
import Text.Regex.TDFA (getAllTextMatches, (=~))

data Game = Game
  { gid :: Int,
    dice :: [(String, Int)]
  }
  deriving (Show, Eq)

parse :: String -> Game
parse line = Game {gid = read $ drop 5 game, dice = dice}
  where
    dice = [(last p, read $ head p :: Int) | dice <- re, let p = splitOn " " dice]
    re = getAllTextMatches $ line =~ ("[0-9]+ [red|green|blue]+" :: String) :: [String]
    game = line =~ ("Game [0-9]+" :: String) :: String

possible :: Game -> Bool
possible game = all test $ dice game
  where
    test (colour, count) = case colour of
      "red" -> count <= 12
      "green" -> count <= 13
      "blue" -> count <= 14

minDice :: Game -> Int
minDice game = maxR * maxG * maxB
  where
    maxR = maximum [count | (colour, count) <- dice game, colour == "red"]
    maxG = maximum [count | (colour, count) <- dice game, colour == "green"]
    maxB = maximum [count | (colour, count) <- dice game, colour == "blue"]

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  let games = parse <$> lines
  print $ sum $ gid <$> filter possible games
  print $ sum $ minDice <$> games
