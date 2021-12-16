import Data.List (foldl', maximumBy, transpose, (!!))
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import qualified Data.Set as S

type Grid = [[Char]]

fold :: Grid -> (String, Int) -> Grid
fold grid (dir, v) = trans $ zipWith zipRow a $ reverse b
  where
    zipRow = zipWith (\a b -> if a == '#' || b == '#' then '#' else ' ')
    trans g = if dir == "y" then g else transpose g
    (a, b) = splitAt v $ trans grid

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let [spoints, sfolds] = splitOn [""] lines

  let points = S.fromList $ map ((\[x, y] -> (read x, read y)) . splitOn ",") spoints
  let (maxX, maxY) = (fst $ maximumBy (comparing fst) points, snd $ maximumBy (comparing snd) points)

  let folds = [(head s, read $ s !! 1 :: Int) | l <- sfolds, let s = splitOn "=" $ (!! 2) $ splitOn " " l]
  let grid = [[if (x, y) `S.member` points then '#' else ' ' | x <- [0 .. maxX]] | y <- [0 .. maxY]]

  let folded = fold grid $ head folds
  print $ sum $ fmap (length . filter (== '#')) folded

  let allFolds = foldl' fold grid folds
  mapM_ print allFolds

  print $ (,) <$> [1, 2, 3] <*> [1, 2, 3]