import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Set     as Set
import Data.List
import Data.List.Split (split, whenElt, keepDelimsL, splitOn, chunksOf)
import Data.Maybe      (fromJust)
import Debug.Trace
import Data.Time

newGuard :: String -> Bool
newGuard line = isInfixOf "Guard" line

measureNap :: [String] -> [(Int, Int)]
measureNap [sleep, wake] = [(minute, 1) | minute <- napRange]
    where
        napRange    = [startMin..endMin]
        startMin    = parseMins sleep
        endMin      = parseMins wake
        parseMins x = read (take 2 $ last $ splitOn ":" x) :: Int

accumNaps :: [[String]] -> [(Int, Int)]
accumNaps naps = concat (map measureNap naps)

mapSleep :: [String] -> (Int, [(Int, Int)])
mapSleep events = (id, accumNaps naps)
    where 
        id   = read (tail $ last $ take 4 $ splitOn " " (head events)) :: Int
        naps = chunksOf 2 $ tail events

foldShifts :: (Int, [(Int, Int)]) -> (String, [(Int, Int)]) -> (Int, [(Int, Int)])
foldShifts (idA, napsA) (idB, napsB) = if idA == idB then (idA, (concat napsA napsB)) else (idB, napsB)

main = do
    file_lines <- fmap Text.lines (Text.readFile "input.txt")
    let lines = sort $ map Text.unpack file_lines
    let shifts = tail $ split (keepDelimsL $ whenElt newGuard) lines
    let sleepPatterns = map mapSleep shifts
    let groupedSleepPatterns = foldr1 foldShifts (sortOn fst (take 4 sleepPatterns))
    putStrLn $ show $ groupedSleepPatterns
