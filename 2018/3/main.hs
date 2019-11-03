import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, fromList)
import qualified Data.Set     as Set

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

lengthGt :: Int -> [a] -> Bool
lengthGt l x = length x > l

dupeSet :: [(Int, Int)] -> Set.Set (Int, Int)
dupeSet coords = Set.fromList $ concat $ filter (lengthGt 1) (group $ sort $ coords)

checkFree :: (String, [(Int, Int)]) -> Set.Set (Int, Int) -> Maybe String
checkFree (id, coords) dupes = if Set.disjoint (Set.fromList coords) dupes then Just id else Nothing

main = do
    file_lines <- fmap Text.lines (Text.readFile "input.txt")
    let lines = map Text.unpack file_lines
    let claims = map convertClaim lines
    let dupes = dupeSet (concat (map snd claims))
    putStrLn $ show $ length $ dupes

    putStrLn $ show $ checkFree (head claims) dupes
