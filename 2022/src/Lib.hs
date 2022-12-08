module Lib where

takeWhileInc :: (a -> Bool) -> [a] -> [a]
takeWhileInc _ [] = []
takeWhileInc f (x : xs) = if f x then x : takeWhileInc f xs else [x]

countUntil :: (a -> Bool) -> [a] -> Int
countUntil _ [] = 0
countUntil f (x : xs) = if f x then 1 + countUntil f xs else 1
