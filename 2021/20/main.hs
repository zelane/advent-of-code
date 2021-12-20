import Data.Char (digitToInt)
import qualified Data.HashMap.Strict as M
import Data.List (foldl')
import Data.Maybe (fromMaybe)

type Image = M.HashMap (Int, Int) Char

type Bounds = (Int, Int, Int, Int)

parse :: [String] -> Image
parse s = M.fromList $ concat [[((x, y), v) | (x, v) <- zip [0 ..] row] | (y, row) <- zip [0 ..] s]

readBin :: String -> Int
readBin = foldl' (\acc x -> acc * 2 + digitToInt x) 0

neighbors :: Int -> Int -> [(Int, Int)]
neighbors x y = [(x + xm, y + ym) | ym <- [-1 .. 1], xm <- [-1 .. 1]]

enhancePx :: String -> Char -> Image -> (Int, Int) -> Char
enhancePx algo d img pix = algo !! bin
  where
    neigh = uncurry neighbors pix
    bin = readBin [if fromMaybe d v == '#' then '1' else '0' | n <- neigh, let v = M.lookup n img]

bounds :: Image -> Bounds
bounds img = (minx - 1, miny - 1, maxx + 1, maxy + 1)
  where
    keys = M.keys img
    xs = map fst keys
    ys = map snd keys
    (minx, miny, maxx, maxy) = (minimum xs, minimum ys, maximum xs, maximum ys)

enhance :: String -> Int -> Image -> Image
enhance algo step img = M.fromList [((x, y), enhancePx algo d img (x, y)) | y <- [miny .. maxy], x <- [minx .. maxx]]
  where
    (minx, miny, maxx, maxy) = bounds img
    d = if head algo == '#' && even step then last algo else head algo

loop :: String -> Int -> Image -> [Image]
loop algo i img = newImg : loop algo (i + 1) newImg
  where
    newImg = enhance algo i img

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let algo = head lines
  let img = parse $ drop 2 lines

  let images = loop algo 0 img
  print $ length $ M.filter (== '#') $ images !! 1
  print $ length $ M.filter (== '#') $ images !! 49
