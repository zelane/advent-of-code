import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Tree as T
import Debug.Trace
import Text.Regex.Posix ((=~))

type Map = M.Map String [(String, Int)]

toMap :: Map -> [String] -> Map
toMap m [] = m
toMap m (x : xs) = traceShow (start, dest, dist) toMap nMap xs
  where
    [[_, start, dest, dist]] = x =~ "(.*) to (.*) = ([0-9]+)" :: [[String]]
    n = M.insertWith (++) start [(dest, read dist)] m

    nMap = M.insertWith (++) dest [(start, read dist)] n

paths :: Map -> String -> [String] -> [[String]] -> [[String]]
paths m start route routes = routes
  where
    l = last route
    dests = fromJust $ M.lookup l m
    newRoutes = [paths m start (route ++ [c]) routes | (c, a) <- dests]

buildTree :: Map -> String -> (String, [String])
buildTree m "Dublin" = ("Dublin", [])
buildTree m k = traceShow ("Asdasd", k, x, paths) (x, paths)
  where
    x = if k == "" then "Dublin" else k
    paths = map fst $ fromJust $ M.lookup x m

foldTree :: String -> [String] -> String
foldTree k nodes = k

main :: IO ()
main = do
  input <- lines <$> readFile "test.txt"
  print input
  let x = toMap M.empty input
  print x
  -- print $ solve1 x "Dublin" "" 0
  let tree = T.unfoldTree (buildTree x) ""

  -- let folded = T.foldTree
  -- putStr $ T.drawTree tree

  print $ take 20 $ T.flatten tree
