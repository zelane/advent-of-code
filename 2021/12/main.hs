import Data.Char (isLower)
import Data.List (delete, nub, union, (\\))
import Data.List.Split (splitOn)
import qualified Data.Map as M

type Nodes = M.Map String [String]

parse :: [String] -> Nodes
parse [] = M.empty
parse (s : sx) = M.unionWith union m $ parse sx
  where
    [a, b] = splitOn "-" s
    m = M.fromList [(a, [b]), (b, [a])]

paths :: Nodes -> Int -> Int
paths nodes maxDupe = run ["start"]
  where
    run path
      | cur == "end" = 1
      | length (visitedSmall \\ nub visitedSmall) > maxDupe = 0
      | otherwise = sum $ map run newpaths
      where
        cur = last path
        visitedSmall = filter (all isLower) path
        toVisit = delete "start" (nodes M.! cur)
        newpaths = map (\y -> path ++ [y]) toVisit

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let nodes = parse lines
  print $ paths nodes 0
  print $ paths nodes 1
