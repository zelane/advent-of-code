module Day7 where

import Data.List (elemIndex, group, sort, sortBy)
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)

replace :: (Eq b) => b -> b -> [b] -> [b]
replace a b = map (\x -> if a == x then b else x)

score :: Bool -> String -> Int
score jaw hand
  | not jaw || 'J' `notElem` hand = scoreH $ group_ hand
  | otherwise = maximum $ scoreH . group_ <$> jokers
  where
    jokers = uncurry (replace 'J') <$> zip "AKQT98765432" (repeat hand)
    group_ x = sortBy (comparing Down) $ length <$> group (sort x)
    scoreH = foldl (\a x -> a + x ^ 2) 0

compOrd :: Bool -> String -> String -> Ordering
compOrd jaw (a : as) (b : bs)
  | a == b = compOrd jaw as bs
  | otherwise = comparing scoreCard a b
  where
    scoreCard c = fromJust $ elemIndex c cards
    cards = if jaw then "J23456789TQKA" else "23456789TJQKA"

comp :: Bool -> (String, Int) -> (String, Int) -> Ordering
comp jokersAreWild (a, _) (b, _)
  | as == bs = compOrd jokersAreWild a b
  | otherwise = compare as bs
  where
    as = score jokersAreWild a
    bs = score jokersAreWild b

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let hands = [(hand, read stake :: Int) | line <- input, let [hand, stake] = words line]
  let sorted = snd <$> sortBy (comp False) hands
  print $ sum $ uncurry (*) <$> zip [1 ..] sorted

  let sorted2 = snd <$> sortBy (comp True) hands
  print $ sum $ uncurry (*) <$> zip [1 ..] sorted2
