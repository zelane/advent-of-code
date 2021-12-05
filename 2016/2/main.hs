import Data.List (elemIndex)
import Data.Maybe (fromJust)

type Keypad = [(Int, Int)]

squareKeypad = [(0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (0, 2), (1, 2), (2, 2)]

diamondKeypad = [(2, 0), (1, 1), (2, 1), (3, 1), (0, 2), (1, 2), (2, 2), (3, 2), (4, 2), (1, 3), (2, 3), (3, 3), (2, 4)]

chars = "123456789ABCD"

trans = [('U', (0, -1)), ('D', (0, 1)), ('L', (-1, 0)), ('R', (1, 0))]

applyT :: Keypad -> (Int, Int) -> (Int, Int) -> (Int, Int)
applyT kp (x, y) (tx, ty) = if t `elem` kp then t else (x, y)
  where
    t = (x + tx, y + ty)

applyIns :: Keypad -> Int -> String -> Int
applyIns kp key ins = fromJust $ elemIndex newLoc kp
  where
    newLoc = foldl (applyT kp) loc moves
    moves = map (fromJust . (`lookup` trans)) ins
    loc = kp !! (key - 1)

input :: Keypad -> [String] -> String
input kp ins = map (chars !!) $ drop 1 $ scanl (applyIns kp) 5 ins

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  print $ input squareKeypad lines
  print $ input diamondKeypad lines
