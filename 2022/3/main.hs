import Data.List (elemIndex, intersect)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

alpha :: String
alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

part1 :: String -> Int
part1 x = fromJust (elemIndex inter alpha) + 1
  where
    (a, b) = splitAt (length x `div` 2) x

    inter = head $ a `intersect` b

part2 :: [String] -> Int
part2 [a, b, c] = fromJust (elemIndex x alpha) + 1
  where
    x = head $ intersect c $ intersect a b

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  print $ sum $ part1 <$> lines
  print $ sum $ part2 <$> chunksOf 3 lines
