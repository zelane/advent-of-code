import Data.List (sort)
import Data.Maybe (fromJust, mapMaybe)

point :: Num a => Char -> a
point c = fromJust $ lookup c [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

match :: Char -> Char
match c = fromJust $ lookup c [(')', '('), (']', '['), ('}', '{'), ('>', '<')]

test :: String -> String -> (Maybe String, Maybe Char)
test ini [] = (return ini, Nothing)
test ini (c : cx)
  | c `elem` ['{', '[', '(', '<'] = test (ini ++ [c]) cx
  | match c == last ini = test (init ini) cx
  | otherwise = (Nothing, return c)

score :: Int -> Char -> Int
score s c = (s * 5) + points
  where
    points = fromJust $ lookup c [('(', 1), ('[', 2), ('{', 3), ('<', 4)]

main :: IO ()
main = do
  chunks <- lines <$> readFile "input.txt"
  let checked = map (test "") chunks
  print $ sum $ map point $ mapMaybe snd checked
  let scores = map (foldl score 0 . reverse) $ mapMaybe fst checked
  print $ sort scores !! (length scores `div` 2)
