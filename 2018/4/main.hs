import Data.List
import Data.List.Split (split, whenElt, keepDelimsL, splitOn, chunksOf)
import Data.Ord
import Data.Tuple.Extra

newGuard :: String -> Bool
newGuard line = isInfixOf "Guard" line

measureNap :: [String] -> [Int]
measureNap [sleep, wake] = [minute | minute <- napRange]
    where
        napRange    = [(parseMins sleep)..(parseMins wake)-1]
        parseMins x = read (take 2 $ last $ splitOn ":" x) :: Int

accumNaps :: [[String]] -> [Int]
accumNaps naps = concat (map measureNap naps)

mapSleep :: [String] -> (Int, [Int])
mapSleep events = (id, accumNaps naps)
    where 
        id   = read (tail $ last $ take 4 $ splitOn " " (head events)) :: Int
        naps = chunksOf 2 $ tail events

groupShifts :: (Int, [Int]) -> (Int, [Int]) -> Bool
groupShifts x y = (fst x) == (fst y)

foldShifts :: (Int, [Int]) -> (Int, [Int]) -> (Int, [Int])
foldShifts (idA, napsA) (idB, napsB) = (idA, (napsA ++ napsB))

collectStats :: (Int, [Int]) -> (Int, Int, [Int])
collectStats (id, [])   = (id, 0, [0])
collectStats (id, mins) = (id, minsSlept, mostSlept)
    where
        minsSlept  = sum mins
        mostSlept  = last $ sortedMins
        sortedMins = sortOn length $ group $ sort $ mins

main = do
    file_lines <- fmap lines (readFile "input.txt")
    let shifts = tail $ split (keepDelimsL $ whenElt newGuard) (sort file_lines)
    let sleepPatterns = map (foldr1 foldShifts) $ groupBy groupShifts $ sortOn fst $ map mapSleep shifts

    let stats = map collectStats sleepPatterns

    let laziestGuard = last $ sortOn snd3 stats
    putStrLn $ show $ (fst3 laziestGuard) * (head $ thd3 laziestGuard)

    let consistentGuard = last $ sortOn (length . thd3) stats
    putStrLn $ show $ (fst3 consistentGuard) * (head $ thd3 consistentGuard)
 