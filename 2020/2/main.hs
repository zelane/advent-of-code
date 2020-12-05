import Data.List
import Data.List.Split (splitOn)

splitLine :: String -> ((Int, Int), Char, String)
splitLine line = ((x, y), l, s)
    where
        parts =  splitOn " " line
        x = read $ head $ splitOn "-" (parts!!0) :: Int
        y = read $ last $ splitOn "-" (parts!!0) :: Int
        l = head (parts!!1)
        s = parts!!2

test :: ((Int, Int), Char, String) -> Bool
test ((min, max), l, s) =  min <= charCount && max >= charCount
    where
        charCount = length $ filter (==l) s

test2 :: ((Int, Int), Char, String) -> Bool
test2 ((a, b), l, s) = x /= y
    where
        x = (s !! (a - 1)) == l
        y = (s !! (b - 1)) == l

main = do
  lines <- fmap lines (readFile "input.txt")
  let splits = map splitLine lines
  print $ length $ filter test splits
  print $ length $ filter test2 splits
