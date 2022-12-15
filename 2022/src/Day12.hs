module Day12 where

import Algorithm.Search (aStar)
import Data.Bifunctor (bimap)
import Data.Char (ord)
import Data.HashMap.Strict qualified as M
import Data.Maybe (fromJust, mapMaybe)

type Point = (Int, Int)

type Grid = M.HashMap Point Int

mapOptions :: Grid -> Point -> Int -> [Point]
mapOptions g (x, y) v = filter steppable options
  where
    options = bimap (x +) (y +) <$> [(0, 1), (1, 0), (0, -1), (-1, 0)]
    steppable p = M.member p g && v + 1 >= (g M.! p)

findP :: (Point -> [Point]) -> Point -> Point -> Maybe (Int, [Point])
findP next start end = aStar next dist (dist end) (== end) start
  where
    dist (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1)

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let points = [((x, y), ord v) | (y, l) <- zip [0 ..] input, (x, v) <- zip [0 ..] l]
  let sta = fst . head $ filter ((== ord 'S') . snd) points
  let end = fst . head $ filter ((== ord 'E') . snd) points
  let grid = M.insert sta (ord 'a') $ M.insert end (ord 'z') $ M.fromList points

  let options = M.mapWithKey (mapOptions grid) grid
  let next p = fromJust $ M.lookup p options
  print $ length $ snd $ fromJust $ findP next sta end

  let f x = findP next x end
  let as = fst <$> M.toList (M.filter (== ord 'a') grid)
  print $ minimum $ fst <$> mapMaybe f as
