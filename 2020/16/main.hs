import Data.List (isInfixOf, sortOn, transpose, (\\))
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShow)
import Text.Regex.Posix ((=~))

data Rule = Rule {name :: String, a :: [Int], b :: [Int]} deriving (Show, Ord, Eq)

rule :: String -> Maybe Rule
rule line = if null re then Nothing else Just $ Rule name [a .. b] [c .. d]
  where
    re = line =~ "([a-z| ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)" :: [[String]]
    match = head re
    name = match !! 1
    [_, _, a, b, c, d] = map read match :: [Int]

ticket :: String -> Maybe [Int]
ticket line = if ',' `elem` line then Just ints else Nothing
  where
    ints = map read $ splitOn "," line :: [Int]

validate :: Int -> Rule -> Bool
validate x rule = x `elem` a rule || x `elem` b rule

invalidValues :: [Rule] -> [Int] -> [Int]
invalidValues rules vals = [x | x <- vals, not $ any (validate x) rules]

matchRules :: [Rule] -> [Int] -> [String]
matchRules rules vals = map name $ filter testRule rules
  where
    testRule r = all (`validate` r) vals

reduce :: [(Int, [String])] -> [(Int, String)] -> [(Int, String)]
reduce [] acc = acc
reduce ((c, rules) : xs) acc = reduce xs (acc ++ [(c, x)])
  where
    x = head $ rules \\ map snd acc

main :: IO ()
main = do
  lines <- fmap lines (readFile "input.txt")

  let rules = mapMaybe rule lines
  let (myTicket : nearTickets) = mapMaybe ticket lines

  print $ sum $ concatMap (invalidValues rules) nearTickets

  let validTickets = filter (null . invalidValues rules) nearTickets
  let matchingFields = sortOn (length . snd) $ zip [0 ..] $ map (matchRules rules) (transpose validTickets)

  let depatures = filter (isInfixOf "dep" . snd) $ reduce matchingFields []

  print $ product [myTicket !! x | (x, _) <- depatures]
