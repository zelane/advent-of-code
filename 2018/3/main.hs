import qualified Data.Set     as Set
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe      (fromJust)

plotCoords :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
plotCoords (offsetX, offsetY) (areaX, areaY) = [(x, y) | x <-rangeX, y <-rangeY]
    where
        rangeX = [offsetX..(offsetX+areaX-1)]
        rangeY = [offsetY..(offsetY+areaY-1)]

toIntPair :: [String] -> (Int, Int)
toIntPair [a, b] = (read a ::Int, read b ::Int)

plotClaim :: String -> [(Int, Int)]
plotClaim x = plotCoords offset area
    where
        offset = toIntPair (splitOn "," (head split))
        area   = toIntPair (splitOn "x" (last split))
        split  = splitOn ":" x

convertClaim :: String -> (String, [(Int, Int)])
convertClaim x = (head y, plotClaim $ last y)
    where
        y = splitOn "@" x

dupeSet :: [(Int, Int)] -> Set.Set (Int, Int)
dupeSet coords = Set.fromList $ concat $ filter ((>1) . length) (group $ sort $ coords)

checkFree :: Set.Set (Int, Int) -> (String, [(Int, Int)]) -> Bool
checkFree dupes (id, coords) = if Set.disjoint (Set.fromList coords) dupes then True else False

main = do
    file_lines <- fmap lines (readFile "input.txt")
    let claims = map convertClaim file_lines
    let dupes = dupeSet (concat (map snd claims))
    putStrLn $ show $ length $ dupes
    putStrLn $ show $ fst $ fromJust $ find (checkFree dupes) claims
