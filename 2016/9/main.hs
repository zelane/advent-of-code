import Data.Function.Memoize (Memoizable, memoize)
import Data.List.Split (splitOn)

part1 :: String -> Int
part1 "" = 0
part1 s
  | head s == '(' = length repeated + part1 rest
  | otherwise = 1 + part1 (tail s)
  where
    [repeated, rest] = doo $ tail s

part2' :: String -> Int
part2' = memoize part2

part2 :: String -> Int
part2 "" = 0
part2 s
  | head s == '(' = part2' repeated + part2' rest
  | otherwise = 1 + part2' (tail s)
  where
    [repeated, rest] = doo $ tail s

doo :: String -> [String]
doo s = [repeated, drop a rest]
  where
    pat = takeWhile (/= ')') s
    [a, b] = read <$> splitOn "x" pat :: [Int]
    rest = drop (length pat + 1) s
    repeated = concat $ replicate b $ take a rest

main :: IO ()
main = do
  input <- readFile "input.txt"

  print $ part1 input
  print $ part2' input
