import Data.List (sort)

mid :: Float -> Float -> Float
mid _min _max  = (_max + 1 - _min) / 2

search :: Float -> Float -> String -> Int
search _min _max [] = round _max
search _min _max ('F':xs) = search _min (_max - (mid _min _max)) xs
search _min _max ('B':xs) = search (_min + (mid _min _max)) _max xs
search _min _max ('L':xs) = search _min (_max - (mid _min _max)) xs
search _min _max ('R':xs) = search (_min + (mid _min _max)) _max xs

findMissing :: [(Int, Int)] -> Int
findMissing ((a, b):(c, d):xs) = if a == c && b + 1 /= d then (a * 8) + b + 1
                                 else findMissing xs

main = do
  lines <- fmap lines (readFile "input.txt")
  let rows = map (search 0 127) $ map (take 7) lines
  let cols = map (search 0 7) $ map (drop 7) lines
  print $ maximum $ zipWith (\ x y -> x * 8 + y) rows cols
  
  let seats = sort $ zip rows cols
  print $ findMissing seats
