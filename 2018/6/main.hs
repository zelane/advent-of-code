import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.List

data Coord = Coord {x:: Int, y:: Int} 
    deriving (Show, Ord, Eq)

toCoord :: String -> Coord
toCoord coordStr = Coord (coordLst !! 0) (coordLst !! 1)
    where 
        coordLst = map read (splitOn ", " coordStr)

calcBounds :: [Coord] -> [Int]
calcBounds coords = [minimum xs, maximum xs, minimum ys, maximum ys]
    where
        xs = map x coords
        ys = map y coords

closetCoord :: [Coord] -> Coord -> Maybe Coord
closetCoord coords square = if length closestCoords == 1 then Just $ fst $ head $ closestCoords else Nothing
    where
        closestCoords = filter ((==shortest) . snd) distances
        shortest      = minimum $ map snd distances
        distances     = [(coord, calcDistance square coord) | coord <- coords]

calcDistance :: Coord -> Coord -> Int
calcDistance a b = abs (x a - x b) + abs (y a - y b)

main = do
    input <- fmap lines $ readFile "input.txt"
    let coords = map toCoord input
    let bounds = calcBounds coords
    let inifi = catMaybes [closetCoord coords (Coord x y) | x <- [bounds!!0, bounds!!1], y <- [bounds!!2, bounds!!3]]
    let grid = [Coord x y | x <- [bounds!!0..bounds!!1], y <- [bounds!!2..bounds!!3]]

    let results = filter (\ x -> notElem x inifi) $ catMaybes $ map (closetCoord coords) grid
    let answer1 = last $ sort $ map length $ group $ sort $ results
    putStrLn $ show $ answer1

    let answer2 = length [square | square <- grid, let d = sum $ map (calcDistance square) coords, d < 10000]
    putStrLn $ show $ answer2
