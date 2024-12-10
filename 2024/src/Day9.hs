module Day9 where

import Control.Monad.ST (RealWorld, runST)
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (Foldable (foldr'))
import Data.IORef
import Data.List (elemIndex, findIndex, group, isPrefixOf, partition, tails, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Debug.Trace (traceShow)

frag :: V.Vector String -> V.Vector String
frag v = runST $ do
  let left = fromJust $ V.findIndex (== ".") v
  let right = fromJust $ V.findIndexR (/= ".") v
  mv <- V.thaw v
  MV.swap mv left right
  V.freeze mv

defrag2 :: V.Vector String -> Int -> (Int, [String]) -> V.Vector String
defrag2 disk _ (r, x) = case space of
  Just l -> if l <= r then newDisk else disk
  _ -> disk
  where
    updates = [(rc, ".") | rc <- [r .. r + length x - 1]] ++ [(lc, head x) | lc <- [l .. l + length x - 1]]
    newDisk = (V.//) disk updates
    l = fromJust space
    space = findSpace (V.group disk) (length x)

findSpace :: [V.Vector String] -> Int -> Maybe Int
findSpace xx i = case ix of
  Just ix -> return $ length (V.concat $ take ix xx)
  _ -> Nothing
  where
    ix = findIndex (\x -> V.head x == "." && length x >= i) xx

defrag3 :: (Int, [(Int, Int)]) -> (Int, Int, Int) -> (Int, [(Int, Int)])
defrag3 (cs, spaces) (fid, fIdx, fSize) = case space of
  Just _ -> if sIdx <= fIdx then traceShow ("moving", fid, space, sIdx) (newCs, newSpaces) else (newCs, spaces)
  _ -> (newCs, spaces)
  where
    newSpaces = left ++ (if sSize > fSize then [(sIdx + fSize, sSize - fSize)] else []) ++ drop 1 right
    (left, right) = splitAt ssIdx spaces
    (sIdx, sSize) = spaces !! ssIdx
    ssIdx = fromJust space
    space = findIndex (\(idx, s) -> s >= fSize) spaces
    ixs = if isJust space && sIdx <= fIdx then [sIdx .. sIdx + fSize - 1] else [fIdx .. fIdx + fSize - 1] :: [Int]
    newCs = traceShow (fid, fSize, ixs, sIdx) cs + sum [fid * i | i <- ixs]

createIndex :: String -> Int -> [(Int, Int)]
createIndex [] _ = []
-- createIndex ('0' : sx) acc = createIndex sx (acc)
createIndex (s : sx) acc = (acc, c) : createIndex sx (acc + c)
  where
    c = digitToInt s -- what about the zeros?

solve :: IO String -> IO ()
solve file = do
  disk <- file

  let al = createIndex (init disk) 0
  let spaces = [(idx, s) | ((idx, s), fid) <- zip al [0 ..], odd fid]
  let files = [((fid `div` 2), idx, s) | ((idx, s), fid) <- zip al [0 ..], even fid]
  print $ fst $ foldr' (flip defrag3) (0, spaces) files

  --
  let expanded = V.fromList $ concat [replicate (digitToInt id) y | (id, n) <- zip (init disk) [0 ..], let y = if even n then show (n `div` 2) else "."]

  let files = group $ filter (/= ".") $ V.toList expanded
  let indexed = [(fromJust $ V.elemIndex (head x) expanded, x) | x <- files]
  let def = foldr' (\x a -> defrag2 a 0 x) expanded indexed

  -- print $ sum $ zipWith (*) [if x == "." then 0 else read x | x <- V.toList def] [0 ..]
  print $ [(x, i) | (x, i) <- zip (V.toList def) [0 ..], x /= ".", read x <= 15]
