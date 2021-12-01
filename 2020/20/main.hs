import qualified Data.IntMap.Strict as M
import Data.List (intercalate, intersect, transpose)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Ma
import Data.Maybe (fromJust)

type Tile = [String]

type Coord = (Int, Int)

parseTile :: String -> (Int, Tile)
parseTile s = (read $ take 4 $ drop 5 title, body)
  where
    (title : body) = lines s

rotate :: Tile -> Tile
rotate = transpose

sides :: Tile -> Tile
sides tile = sides ++ map reverse sides
  where
    sides = [head tile, map last tile, last tile, map head tile]

matchAll :: Tile -> Tile -> Bool
matchAll a b = a /= b && not (null $ sides a `intersect` sides b)

-- matchWith :: (String -> String) ->

findAdj :: [(Int, Tile)] -> (Int, Tile) -> (Int, [(Int, Tile)])
findAdj tiles (id, tile) = (id, matches)
  where
    matches = [(idb, b) | (idb, b) <- tiles, matchAll tile b]

fitRight :: [(Int, Tile)] -> Tile -> (Int, Tile)
fitRight ((bid, b) : tiles) tile = if null matches then fitRight tiles tile else (bid, head matches)
  where
    matches = filter ((== right) . left) $ rotas ++ f_rotas
    right = map last tile
    left = map head
    rotas = take 4 $ iterate rotate b
    f_rotas = map reverse rotas

fit' :: Tile -> Tile -> (Tile, Coord)
fit' a b 
  | 
  where
    rotations = take 4 $ iterate rotate b
    frotations = map reverse rotations

fit :: [(Int, Tile)] -> (Int, Tile) -> Ma.Map Coord Tile -> Ma.Map Coord Tile
fit ((bid, b) : xs) a acc = acc

main :: IO ()
main = do
  file <- readFile "test.txt"
  let parts = splitOn "\n\n" file
  let tiles = map parseTile parts
  let tileMap = M.fromList tiles
  let matches = map (findAdj tiles) tiles
  let corners = filter ((== 2) . length . snd) matches
  print $ product $ map fst corners

  let (cid, cmatches) = head corners

  let first = fromJust $ M.lookup cid tileMap
  let (rid, right) = fitRight tiles first
  let (rid, right2) = fitRight tiles right

  mapM_ print first
  print ""
  mapM_ print right
  print ""
  mapM_ print right2
  where
    str = intercalate "\n"
