test :: [Int] -> [Int] -> Int
test pre (num : nums) = if valid then test newPre nums else num
  where
    sums = [x + y | x <- pre, y <- pre]
    valid = num `elem` sums
    newPre = drop 1 pre ++ [num]

find :: [Int] -> Int -> (Int, Int) -> Int
find nums target (left, right) =
  if result == target
    then minimum range + maximum range
    else find nums target (nLeft, nRight)
  where
    range = take right $ drop left nums
    result = sum range
    (nLeft, nRight)
      | result < target = (left, right + 1)
      | otherwise = (left + 1, right - 1)

main :: IO ()
main = do
  --   let pre = 5
  --   lines <- fmap lines (readFile "test.txt")
  let pre = 25
  lines <- fmap lines (readFile "input.txt")

  let nums = map read lines :: [Int]
  let answer1 = test (take pre nums) (drop pre nums)
  print answer1

  let answer2 = find nums answer1 (0, 1)
  print answer2
