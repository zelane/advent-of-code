import Data.List (find, group, groupBy, iterate', sort, sortOn, union)
import Data.List.Extra (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)

type Poly = M.Map String Int
type Rules = M.Map String (String, String)

mapPairs :: [[String]] -> String -> (String, (String, String))
mapPairs rules [a, b] = (a : [b], (a : n, n ++ [b]))
  where
    [_, n] = fromMaybe ["", ""] (find (\x -> head x == a : [b]) rules)

step :: Rules -> Poly -> Poly
step rules poly = M.foldMapWithKey (k -> a -> m) poly
  where
    
    cur = map look 
    look s = fromJust $ M.lookup s rules

main :: IO ()
main = do
  lines <- lines <$> readFile "test.txt"
  let [temp, srules] = splitOn [""] lines
  let rules = map (splitOn " -> ") srules

  let it = iterate' (step rules) $ head temp
  let pairs = map (mapPairs rules . head) rules
  
  print pairs

