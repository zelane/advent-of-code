import Data.List (sort, group)

basement :: [Char] -> Int -> Int
basement (x:xs) floor
        | floor == -1 = length xs + 1
        | x == '('  = basement xs (floor + 1) 
        | otherwise = basement xs (floor - 1) 

main :: IO ()
main = do
  file <- readFile "input.txt"
  let x = group $ sort file
  print $ length (head x) - length (last x)
  print $ length file - basement file 0
