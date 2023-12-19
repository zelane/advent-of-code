import qualified Data.Set as S
import Text.Regex.TDFA (getAllTextSubmatches, (=~))

data Area = On [Int] [Int] [Int] | Off [Int] [Int] [Int] deriving (Show)

type Core = S.Set (Int, Int, Int)

parse :: String -> Area
parse s = case s !! 1 of
  'n' -> On [a .. b] [c .. d] [e .. f]
  'f' -> Off [a .. b] [c .. d] [e .. f]
  where
    [_, a, b, c, d, e, f] = map read re
    re = getAllTextSubmatches $ s =~ "x=(-?[0-9]+)..(-?[0-9]+),y=(-?[0-9]+)..(-?[0-9]+),z=(-?[0-9]+)..(-?[0-9]+)" :: [String]

apply :: Core -> Area -> Core
apply c a = case a of
  On rx ry rz -> S.union c $ S.fromList [(x, y, z) | x <- rx, y <- ry, z <- rz]
  Off rx ry rz -> S.difference c $ S.fromList [(x, y, z) | x <- rx, y <- ry, z <- rz]

main :: IO ()
main = do
  lines <- lines <$> readFile "test.txt"
  let ins = map parse lines
  print $ length $ foldl apply S.empty ins

-- Add all areas
-- Subtract area of overlapping offs
-- Intersections are cuboids, treat intersections as OFF ins, re-add once only
-- what if off overlaps with on intersection?
