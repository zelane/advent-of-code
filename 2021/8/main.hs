import Data.Char (intToDigit)
import Data.List (elem, sort, sortOn, (\\))
import Data.List.Split (splitOn)

isUnique :: String -> Bool
isUnique s = length s `elem` [2, 3, 4, 7]

mapUnique :: [String] -> [(String, Int)]
mapUnique s = zip (sortOn length $ filter isUnique s) [1, 7, 4, 8]

diffSort :: String -> [String] -> [String]
diffSort base = sortOn (length . (base \\))

mapCodes :: [String] -> [(String, Int)]
mapCodes s = uniqueMap ++ [(two, 2), (three, 3), (five, 5), (nine, 9), (six, 6), (zero, 0)]
  where
    uniqueMap = mapUnique s
    four = fst $ uniqueMap !! 2
    fives = filter ((== 5) . length) s
    two = last $ diffSort four fives
    [three, five] = diffSort two $ filter (/= two) fives
    sixes = filter ((== 6) . length) s
    nine = head $ diffSort three sixes
    [six, zero] = diffSort five $ filter (/= nine) sixes

solve :: String -> String
solve line = map (intToDigit . snd . lookup) digits
  where
    [codes, digits] = map (splitOn " ") $ splitOn " | " line
    codeMap = mapCodes codes
    lookup x = head $ filter (\(c, v) -> sort c == sort x) codeMap

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let digits = concatMap (splitOn " " . last . splitOn " | ") lines

  print $ length $ filter isUnique digits
  print $ sum $ map (read . solve) lines
