
main = do
  lines <- fmap lines (readFile "input.txt")
  let expenses = map read lines :: [Int]
  let combinations = [(x + y, x * y) | x <- expenses, y <- expenses]
  print $ snd.head $ filter ((==2020) . fst) combinations

  let combinations2 = [(x + y + z, x * y * z) | x <- expenses, y <- expenses, z <- expenses]
  print $ snd.head $ filter ((==2020) . fst) combinations2
