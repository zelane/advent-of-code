import Data.List.Split
import Data.List
import Data.Function


img :: Char -> Char
img c = if c == '1' then '#' else ' '
 
squash :: String -> Char
squash pixels = img $ head $ filter (/='2') pixels

paint :: [String] -> String
paint layers = unlines $ chunksOf 25 pixels
    where
        pixels  = map squash grouped
        grouped = [map (!!x) layers | x <- [0..(25 * 6)-1]]

main = do
    pixels <- readFile "input.txt"
    let layers = parmap concat $ chunksOf 6 $ chunksOf 25 pixels
    let answer1 = minimumBy (compare `on` (length.head)) $ map (group.sort) layers
    print $ product.tail $ map length answer1
    putStr $ paint layers
