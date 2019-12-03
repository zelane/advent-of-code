
calcFuel :: (RealFrac a, Integral b) => a -> b
calcFuel input = (floor $ input / 3) - 2

calcAllFuel :: (RealFrac a, Integral b) => b -> a -> b
calcAllFuel acc input = if x < 1
                        then acc
                        else calcAllFuel (acc + x) (realToFrac x)
  where
    x = calcFuel input

main = do
  lines <- fmap lines (readFile "input.txt")
  let masses = map read lines :: [Float]
  print $ sum $ map calcFuel masses
  print $ sum $ map (calcAllFuel 0) masses
