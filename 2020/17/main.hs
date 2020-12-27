import qualified Data.HashMap.Strict as M
import Data.List (iterate', permutations, replicate, transpose)
import Data.Maybe (catMaybes, fromJust)

transNd :: Int -> [[Int]]
transNd n = concat $ transpose $ replicate n (permutations [-1, 0, 1])

t = [-1, 0, 1]

trans3d = [[z, x, y] | z <- t, x <- t, y <- t, [z, x, y] /= [0, 0, 0]]

trans4d = [[w, z, x, y] | w <- t, z <- t, x <- t, y <- t, [w, z, x, y] /= [0, 0, 0, 0]]

r = [-8 .. 12]

adjMap = M.fromList [([z, x, y], adjs trans3d [z, x, y]) | z <- r, x <- r, y <- r]

adjMap4d = M.fromList [([w, z, x, y], adjs trans4d [w, z, x, y]) | w <- r, z <- r, x <- r, y <- r]

adjs ts coords = [zipWith (+) coords trans | trans <- ts]

activeAdj :: M.HashMap [Int] Char -> [Int] -> Int
activeAdj m coords = length $ filter (== '#') $ catMaybes adjV
  where
    adjV = map (`M.lookup` m) adj
    adj = fromJust $ M.lookup coords am
    am = if length coords == 3 then adjMap else adjMap4d

update :: M.HashMap [Int] Char -> [Int] -> Char -> Char
update m coord c
  | c == '#' && (adj == 2 || adj == 3) = '#'
  | c == '#' = '.'
  | c == '.' && adj == 3 = '#'
  | otherwise = '.'
  where
    adj = activeAdj m coord

parse :: [String] -> Int -> [[Int]]
parse lines d = concat [[pad ++ [x, y] | (x, c) <- zip [0 ..] line, c == '#'] | (y, line) <- zip [0 ..] lines]
  where
    pad = replicate (d - 2) 0

run :: M.HashMap [Int] Char -> M.HashMap [Int] Char
run m = M.mapWithKey (update m) m

main :: IO ()
main = do
  active <- fmap lines (readFile "input.txt")
  let parse3 = parse active 3
  let parse4 = parse active 4

  let cubes3d = M.mapWithKey (\k v -> if k `elem` parse3 then '#' else '.') empty3d
  let answer1 = M.toList $ last $ take 7 $ iterate' run cubes3d
  print $ length $ filter ((== '#') . snd) answer1

  let cubes4d = M.mapWithKey (\k v -> if k `elem` parse4 then '#' else '.') empty4d
  let answer2 = M.toList $ last $ take 7 $ iterate' run cubes4d
  print $ length $ filter ((== '#') . snd) answer2
  where
    empty3d = M.fromList [([z, x, y], '.') | z <- r, x <- r, y <- r]
    empty4d = M.fromList [([w, z, x, y], '.') | w <- r, z <- r, x <- r, y <- r]
