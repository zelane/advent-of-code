module Day15 where

import Data.Char (isAlpha, ord)
import Data.Foldable (foldl')
import Data.IntMap qualified as M
import Data.List.Split (splitOn)
import Data.Sequence ((|>))
import Data.Sequence qualified as S

type Box = S.Seq (String, Int)

type Boxes = M.IntMap Box

hash :: String -> Int
hash = foldl' (\cv c -> ((cv + ord c) * 17) `mod` 256) 0

findReplace :: (Eq a) => (a, b) -> S.Seq (a, b) -> S.Seq (a, b)
findReplace a@(k, _) seq = case S.findIndexL ((== k) . fst) seq of
  Just i -> S.update i a seq
  _ -> seq |> a

remove :: (Eq a) => a -> S.Seq (a, b) -> S.Seq (a, b)
remove a seq = case S.findIndexL ((== a) . fst) seq of
  Just i -> S.deleteAt i seq
  _ -> seq

run :: Boxes -> String -> Boxes
run boxes s = M.insert boxNo newBox boxes
  where
    (label, op : fl) = span isAlpha s
    boxNo = hash label
    existing = M.findWithDefault [] boxNo boxes
    newBox
      | op == '=' = findReplace (label, read fl) existing
      | op == '-' = remove label existing

sumBox :: Int -> Box -> Int
sumBox box = S.foldlWithIndex (\a i (_, fl) -> a + ((box + 1) * (i + 1) * fl)) 0

solve :: IO String -> IO ()
solve file = do
  input <- file
  print $ foldl' (\a s -> a + hash s) 0 $ splitOn "," input

  let boxes = foldl' run M.empty $ splitOn "," input
  print $ M.foldrWithKey (\k v a -> a + sumBox k v) 0 boxes
