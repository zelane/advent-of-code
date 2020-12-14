import qualified Data.Set as S
import Debug.Trace (traceShow)
import Text.Regex.Posix ((=~))

data Bag = Bag {name :: String, count :: Int}
  deriving (Show, Ord, Eq)

contains :: String -> [Bag] -> Maybe Bag
contains key bags = head $ filter (\x -> name x == key) bags

parseLine :: String -> (String, [Bag])
parseLine line = (bagName, subBags)
  where
    bagName = line =~ "[a-z]+ [a-z]+" :: String
    subMatches = line =~ "([0-9]+) ([a-z]+ [a-z]+)" :: [[String]]
    subBags = [Bag a (read b :: Int) | [a, b, _] <- subMatches]

solve1 :: [(String, [Bag])] -> String -> S.Set String -> S.Set String
solve1 [] _ accu = accu
solve1 (b : allBags) key accu = solve1 allBags key newSet
  where
    match = contains key (snd b)
    newSet = S.insert (name match) accu

main = do
  lines <- fmap lines (readFile "test.txt")
  let x = map parseLine lines
  print $ take 5 x
  print $ solve1 x "shiny gold" S.empty
