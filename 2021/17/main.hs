import Data.Foldable (maximumBy)
import Data.List (find)
import Data.Ord (comparing)

type Target = (Int, Int, Int, Int)

type Probe = (Int, Int, Int, Int)

step :: Probe -> Probe
step (x, y, xv, yv) = (x + xv, y + yv, nxv, yv - 1)
  where
    nxv
      | xv > 0 = xv - 1
      | xv < 0 = xv + 1
      | otherwise = 0

launch :: Target -> Probe -> [(Int, Int)] -> (Bool, [(Int, Int)])
launch t probe path =
  if hit || miss
    then (hit, path ++ [(x, y)])
    else launch t (step probe) (path ++ [(x, y)])
  where
    (minx, maxx, miny, maxy) = t
    (x, y, _, _) = probe
    hit = minx <= x && x <= maxx && miny <= y && y <= maxy
    miss = x > maxx || y < miny

main :: IO ()
main = do
  let t = (235, 259, -118, -62) :: Target

  print $ maximumBy (comparing snd) $ [((vx, vy), snd $ maximumBy (comparing snd) path) | vx <- [0 .. 100], vy <- [0 .. 200], let (hit, path) = launch t (0, 0, vx, vy) [], hit]
  print $ length $ [(vx, vy) | vx <- [0 .. 500], vy <- [-500 .. 500], let (hit, path) = launch t (0, 0, vx, vy) [], hit]
