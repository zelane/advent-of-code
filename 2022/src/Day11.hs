module Day11 (solve) where

import Control.Lens (element, elements)
import Control.Lens.Operators ((%~))
import Data.Function ((&))
import Data.List (elemIndex, foldl1, sort)
import Data.List.Split (splitOn)
import Lib (parseInts)
import Text.Regex.TDFA ((=~))
import Text.Show.Functions ()

data Monkey = Monkey
  { index :: Int,
    items :: [Int],
    op :: Int -> Int,
    getTarget :: Int -> Int,
    dv :: Int
  }
  deriving (Show)

type State = ([[Int]], [Int])

parse :: [String] -> Monkey
parse [a, b, c, d, e, f] = Monkey i its opf test dv
  where
    [i, dv, mt, mf] = head . parseInts <$> [a, d, e, f] :: [Int]
    its = parseInts b
    [opc, opv] = splitOn " " $ c =~ ("(\\*|\\+) ([0-9]+|old)" :: String) :: [String]
    opo = if opc == "*" then (*) else (+)
    opf =
      if opv == "old"
        then (\x -> opo x x)
        else (\x -> opo x $ read opv)
    test worry = if mod worry dv == 0 then mt else mf

allbusiness :: (Int -> Int) -> [Monkey] -> State -> State
allbusiness _ [] state = state
allbusiness f (m : ms) state = allbusiness f ms newstate
  where
    monkeyItems = fst state !! index m
    newstate = foldl (business f m) state monkeyItems

business :: (Int -> Int) -> Monkey -> State -> Int -> State
business f m (items, counts) item = do
  let worry = f $ op m item
  let targetElem = element $ getTarget m worry
  let this = element $ index m
  let newItems =
        items
          & targetElem %~ (<> [worry])
          & this %~ tail
  let newCounts = counts & this %~ (+ 1)
  (newItems, newCounts)

answer :: [Monkey] -> (Int -> Int) -> Int -> Int
answer monkeys f i = product $ take 2 $ reverse $ sort counts
  where
    state = ([items m | m <- monkeys], replicate (length monkeys) 0)
    rounds = iterate (allbusiness f monkeys) state
    counts = snd (rounds !! i)

solve :: IO String -> IO ()
solve file = do
  input <- splitOn "\n\n" <$> file
  let monkeys = parse . lines <$> input
  let lcm = product $ dv <$> monkeys

  print $ answer monkeys (`quot` 3) 20
  print $ answer monkeys (`mod` lcm) 10000
