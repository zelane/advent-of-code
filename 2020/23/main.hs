setDest :: [Int] -> [Int] -> Int -> Int
setDest cups x = y
  where

play :: [Int] -> [Int]
play cups = []
  where
    currentCup = head cups
    destL = currentCup - 1
    destC = if destL `elem` pickUp 
    pickUp = take 3 $ drop 1 cups
    newCups = cycle $ drop 4 cups

main :: IO ()
main = do
  let cups = [3, 8, 9, 1, 2, 5, 4, 6, 7]

  print cups
