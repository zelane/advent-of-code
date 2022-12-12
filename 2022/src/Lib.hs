module Lib where

import Text.Regex.TDFA (getAllTextMatches, (=~))

takeWhileInc :: (a -> Bool) -> [a] -> [a]
takeWhileInc _ [] = []
takeWhileInc f (x : xs) = if f x then x : takeWhileInc f xs else [x]

countUntil :: (a -> Bool) -> [a] -> Int
countUntil _ [] = 0
countUntil f (x : xs) = if f x then 1 + countUntil f xs else 1

zipX :: [[a]] -> [[a]]
zipX xs = if null $ head xs then [] else [head <$> xs] <> zipX (drop 1 <$> xs)

zipPair :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
zipPair f (a, b) (c, d) = (f a c, f b d)

parseInts :: String -> [Int]
parseInts s = if null matches then [] else read <$> matches
  where
    matches = getAllTextMatches (s =~ ("([0-9]+)" :: String))
