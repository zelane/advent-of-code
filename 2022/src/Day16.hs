{-# LANGUAGE TemplateHaskell #-}

module Day16 where

import Data.Either (fromRight)
import Data.Function.Memoize (deriveMemoizable, memoFix4, memoize4)
import Data.HashMap.Strict qualified as M
import Data.List (singleton)
import Data.Set qualified as S
import Text.Parsec

type Valve = (Int, [String])

deriveMemoizable ''S.Set

parseL :: String -> Either ParseError (String, Valve)
parseL = parse parser ""
  where
    parser = do
      id <- string "Valve " *> many1 alphaNum <* string " has flow rate="
      flow <- many1 digit
      subs <- try parseMulti <|> parseSingle
      return (id, (read flow, subs))
    parseMulti = string "; tunnels lead to valves " >> sepBy1 (many1 alphaNum) (string ", ")
    parseSingle = string "; tunnel leads to valve " >> (singleton <$> many1 alphaNum)

calc :: M.HashMap String Valve -> Bool -> Int -> Int
calc vs ele t = gom ele S.empty t "AA"
  where
    gom = memoize4 go
    go ele open time vid
      | time <= 0 && not ele = 0
      | time <= 0 = gom False open t "AA"
      | vid `notElem` open && press > 0 = max (released + opened) skip
      | otherwise = skip
      where
        released = press * (time - 1)
        skip = maximum $! gom ele open (time - 1) <$> neig
        opened = maximum $! gom ele (S.insert vid open) (time - 2) <$> neig
        (press, neig) = vs M.! vid

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let nov = ("", (0, []))
  let valves = M.fromList $ fromRight nov . parseL <$> input

  print $ calc valves False 30
  print $ calc valves True 26
