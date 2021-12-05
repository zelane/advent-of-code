import Data.List (transpose)
import Data.List.Split (splitOn)

testTriangle :: [Int] -> Bool
testTriangle [a, b, c] = a + b > c && b + c > a && c + a > b
testTriangle _ = False

arrange :: [[Int]] -> [[Int]]
arrange [] = []
arrange sides = transpose (take 3 sides) ++ arrange (drop 3 sides)

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let sides = map read . filter (/= "") . splitOn " " <$> lines :: [[Int]]
  print $ length $ filter testTriangle sides
  print $ length $ filter testTriangle $ arrange sides
