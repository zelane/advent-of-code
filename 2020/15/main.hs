import qualified Data.IntMap.Strict as M
import Data.List (foldl')

answer :: (Int, M.IntMap Int) -> Int -> (Int, M.IntMap Int)
answer (lastVal, seen) i = (newVal, newSeen)
  where
    newSeen = M.insert lastVal i seen
    lastSeen = M.lookup lastVal seen
    newVal = maybe 0 (i -) lastSeen

main :: IO ()
main = do
  let input = [16, 11, 15, 0, 1, 7]
  let seen = M.fromList $ zip (init input) [0 ..]
  let l = length seen
  let a = last input

  print $ fst $ foldl' answer (a, seen) [l .. 2020 - 2]
  print $ fst $ foldl' answer (a, seen) [l .. 30000000 - 2]
