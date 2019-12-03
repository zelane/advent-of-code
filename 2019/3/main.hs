import           Data.List.Split      (splitOn)
import qualified Data.Set as Set      (fromList, toList, intersection)
import           Data.List            (elemIndex, intersect)
import Data.Maybe (fromJust)

getTrans :: Char -> (Int, Int)
getTrans 'U' = (0, 1)
getTrans 'D' = (0, -1)
getTrans 'L' = (-1, 0)
getTrans 'R' = (1, 0)

traceWire :: [(Int, Int)] -> [String] -> [(Int, Int)]
traceWire route [] = route
traceWire route (step:steps) = traceWire (route ++ new_steps) steps
    where 
        prev = last route
        x = fst prev
        y = snd prev
        dir = head step
        dis = read (tail step) :: Int
        (tx, ty) = getTrans dir
        new_steps = [(x + (tx * i), y + (ty * i)) | i <- [1..dis]]

calcDist :: (Int, Int) -> (Int, Int) -> Int
calcDist _ (0, 0) = 99999
calcDist (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)

calcSteps :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> Int
calcSteps _ _ (0, 0) = 99999
calcSteps routeA routeB point = (fromJust $ elemIndex point routeA) + (fromJust $ elemIndex point routeB)

main = do
    lines <- fmap lines (readFile "input.txt")
    let wires = map (splitOn ",") lines
    let paths = map (traceWire [(0, 0)]) wires
    let crosses = Set.intersection (Set.fromList (paths!!0)) (Set.fromList (paths!!1))
    print $ minimum $ map (calcDist (0, 0)) (Set.toList crosses)
    print $ minimum $ map (calcSteps (paths!!0) (paths!!1)) (Set.toList crosses)
