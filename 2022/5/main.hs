import Control.Lens (element)
import Control.Lens.Setter ((.~))
import Data.Char (isSpace)
import Data.Function ((&))
import Data.List (foldl, transpose)
import Data.List.Split (splitOn)
import Text.Regex.Posix (getAllTextMatches, (=~))

parseIns :: String -> [Int]
parseIns s = [a, b - 1, c - 1]
  where
    [a, b, c] = read <$> getAllTextMatches (s =~ "([0-9]+)")

parseCrates :: [String] -> [String]
parseCrates s = dropWhile isSpace <$> filter match s
  where
    match x = x =~ "[A-Z]" :: Bool

run :: Bool -> [String] -> [Int] -> [String]
run rev crates [m, a, b] = do
  let newA = drop m (crates !! a)
  let newB = take m (crates !! a) ++ (crates !! b)
  crates
    & element a .~ newA
    & element b
      .~ ( if rev
             then reverse newB
             else newB
         )

main :: IO ()
main = do
  file <- readFile "input.txt"
  let [a, b] = lines <$> splitOn "\n\n" file
  let ins = parseIns <$> b
  let crates = parseCrates $ transpose $ init a
  print $ concatMap (take 1) $ foldl (run True) crates ins
  print $ concatMap (take 1) $ foldl (run False) crates ins
