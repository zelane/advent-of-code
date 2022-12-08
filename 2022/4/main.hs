import Data.List (intersect, (\\))
import Text.Regex.Posix (getAllTextMatches, (=~))

type Assign = ([Int], [Int])

parse :: String -> Assign
parse s = ([a .. b], [c .. d])
  where
    [a, b, c, d] = read <$> getAllTextMatches (s =~ "([0-9]+)") :: [Int]

part1 :: Assign -> Bool
part1 (a, b) = null (a \\ b) || null (b \\ a)

part2 :: Assign -> Bool
part2 (a, b) = not $ null (a `intersect` b)

main :: IO ()
main = do
  lines <- fmap parse . lines <$> readFile "input.txt"
  print $ length $ filter part1 lines
  print $ length $ filter part2 lines
