-- import Numeric (readBin) no hsl for 9.2.3 :C

import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (filter, foldl', group, maximumBy, minimumBy, sort, transpose)

readBin :: String -> Int
readBin = foldl' (\acc x -> acc * 2 + digitToInt x) 0

mostCommon :: String -> Char
mostCommon = head . maximumBy (compare `on` length) . group . sort

leastCommon :: String -> Char
leastCommon = head . minimumBy (compare `on` length) . group . sort

answer1 :: (String -> Char) -> [String] -> Int
answer1 f lines = readBin $ map f $ transpose lines

answer2 :: (String -> Char) -> [String] -> Int -> Int
answer2 _ [line] _ = readBin line
answer2 f lines i = answer2 f filtered (i + 1)
  where
    criteria = f $ transpose lines !! i
    filtered = filter (\x -> x !! i == criteria) lines

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  print $ answer1 mostCommon lines * answer1 leastCommon lines
  print $ answer2 mostCommon lines 0 * answer2 leastCommon lines 0
