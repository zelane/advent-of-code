import Data.Either (rights)
import Data.Foldable (toList)
import Data.List (transpose)
import Data.Sequence (Seq ((:<|)), fromList, index, update, (><))
import Text.Parsec
import Text.Regex.TDFA ((=~))

type Screen = Seq String

data Ins = Rect Int Int | Rot String Int Int deriving (Show)

parseIns :: Parsec String () Ins
parseIns =
  try (Rect <$> (string "rect " >> int) <*> (anyChar >> int))
    <|> Rot <$> (string "rotate " >> many1 letter) <*> (count 3 anyChar >> int) <*> (string " by " >> int)
  where
    int = read <$> many1 digit

trans :: Screen -> Screen
trans l = fromList $ transpose $ toList l

apply :: Screen -> Ins -> Screen
apply s i = case i of
  Rect x y -> rect s x y
  Rot "column" c x -> trans $ rot (trans s) c x
  Rot "row" r x -> rot s r x

rot :: Screen -> Int -> Int -> Screen
rot s r x = update r newRow s
  where
    newRow = rotate (length row - x) row
    row = s `index` r
    rotate = drop <> take

rect :: Screen -> Int -> Int -> Screen
rect s _ 0 = s
rect (s :<| sx) x y = (replicate x '#' ++ drop x s) :<| rect sx x (y - 1)

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let screen = fromList [['.' | x <- [0 .. 49]] | y <- [0 .. 5]]
  let ins = rights $ map (parse parseIns "") lines
  let result = foldl apply screen ins
  print $ sum $ fmap (length . filter (== '#')) result
  mapM_ print result
