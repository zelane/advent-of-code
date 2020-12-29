import Data.List (elemIndex, iterate')
import Data.Maybe (fromJust)

transform :: Int -> Int -> Int
transform subject loop = last $ take (loop + 1) $ iterate' (transform2 subject) 1

calcLoop :: Int -> Int
calcLoop public = fromJust $ elemIndex public $ iterate' (transform2 7) 1

transform2 :: Int -> Int -> Int
transform2 subject value = (value * subject) `mod` 20201227

main :: IO ()
main = do
  let card = 1526110
  let door = 20175123
  let doorLoop = calcLoop door

  print $ transform card doorLoop
