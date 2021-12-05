import Data.List (find, group, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (Down (Down), compare, comparing)
import Text.Regex.Posix ((=~))

validate :: [String] -> (String, Int)
validate [_, n, id, cs] = if cs == check then (n, read id) else (n, 0)
  where
    name = concat $ splitOn "-" n
    sorted = sortBy (comparing (Down . length) <> compare) (group $ sort name)
    check = take 5 $ map head sorted
validate _ = ("", 0)

crack :: (String, Int) -> (Int, String)
crack (s, id) = (id, [shift c | c <- s, c /= '-'])
  where
    shift c = toEnum $ (((fromEnum c - 97) + id) `mod` 26) + 97

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let codes = concatMap (\s -> s =~ "([a-z|-]+)-([0-9]+)\\[(.*)\\]" :: [[String]]) lines
  let valid = filter ((> 0) . snd) $ map validate codes
  print $ sum $ map snd valid
  print $ find ((== "northpoleobjectstorage") . snd) $ map crack valid
