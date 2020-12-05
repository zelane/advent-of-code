
enumerate x = zip [0..] x

isTree :: Int -> (Int, String) -> Bool
isTree r (0, _) = False
isTree r (y, row) = tree
    where
        x = (y * r)
        tree = (cycle row) !! x == '#'

main = do
  lines <- fmap lines (readFile "input.txt")
  let answer1 = length $ filter (isTree 3) (enumerate lines)
  print $ answer1
  let a = length $ filter (isTree 1) (enumerate lines)
  let b = length $ filter (isTree 5) (enumerate lines)
  let c = length $ filter (isTree 7) (enumerate lines)

  let oddLines = map snd (filter (even.fst) (enumerate lines))
  let d = length $ filter (isTree 1) (enumerate oddLines)
  
  print $ answer1 * a * b * c * d
