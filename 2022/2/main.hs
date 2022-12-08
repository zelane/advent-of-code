import Data.List.Split (splitOn)

toPair :: String -> (Char, Char)
toPair s = (a, b)
  where
    [[a], [b]] = splitOn " " s

score :: (Char, Char) -> Int
score ('A', 'X') = 3 + 1
score ('A', 'Y') = 6 + 2
score ('A', 'Z') = 0 + 3
score ('B', 'X') = 0 + 1
score ('B', 'Y') = 3 + 2
score ('B', 'Z') = 6 + 3
score ('C', 'X') = 6 + 1
score ('C', 'Y') = 0 + 2
score ('C', 'Z') = 3 + 3

score2 :: (Char, Char) -> Int
score2 ('A', 'X') = 0 + 3
score2 ('A', 'Y') = 3 + 1
score2 ('A', 'Z') = 6 + 2
score2 ('B', 'X') = 0 + 1
score2 ('B', 'Y') = 3 + 2
score2 ('B', 'Z') = 6 + 3
score2 ('C', 'X') = 0 + 2
score2 ('C', 'Y') = 3 + 3
score2 ('C', 'Z') = 6 + 1

main :: IO ()
main = do
  pairs <- fmap toPair . lines <$> readFile "input.txt"
  print $ sum $ score <$> pairs
  print $ sum $ score2 <$> pairs
