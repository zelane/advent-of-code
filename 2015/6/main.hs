import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import Text.Regex.Posix ((=~))

type Grid = M.Map (Int, Int) Int

type Cmd = (Int -> Int, [Int], [Int])

apply :: Grid -> Cmd -> Grid
apply grid (f, xs, ys) = foldl' app grid [(x, y) | x <- xs, y <- ys]
  where
    app g (x, y) = M.adjust f (x, y) g

parseCmd :: String -> Cmd
parseCmd x = (f, [read sx .. read ex], [read sy .. read ey])
  where
    [[_, cmd, sx, sy, ex, ey]] = x =~ "(turn on|turn off|toggle) ([0-9]+),([0-9]+) .* ([0-9]+),([0-9]+)" :: [[String]]
    f
      | cmd == "turn on" = const 1
      | cmd == "turn off" = const 0
      | cmd == "toggle" = \x -> if x == 1 then 0 else 1

parseCmd2 :: String -> Cmd
parseCmd2 x = (f, [read sx .. read ex], [read sy .. read ey])
  where
    [[_, cmd, sx, sy, ex, ey]] = x =~ "(turn on|turn off|toggle) ([0-9]+),([0-9]+) .* ([0-9]+),([0-9]+)" :: [[String]]
    f
      | cmd == "turn on" = (+ 1)
      | cmd == "turn off" = \x -> maximum [0, x - 1]
      | cmd == "toggle" = (+ 2)

main :: IO ()
main = do
  cmds <- lines <$> readFile "input.txt"

  let grid = M.fromList [((x, y), 0) | x <- [0 .. 999], y <- [0 .. 999]]

  let parsed = fmap parseCmd cmds
  let x = foldl' apply grid parsed
  print $ length $ filter (== 1) $ M.elems x

  let parsed = fmap parseCmd2 cmds
  let x = foldl' apply grid parsed
  print $ sum $ M.elems x
