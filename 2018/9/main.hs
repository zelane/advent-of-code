import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromJust)
import Data.List
import Text.Regex


main = do
    input <- readFile "input.txt"
    let deps = fromJust $ matchRegex (mkRegex "([0-9]+) .* ([0-9]+)") input
    putStrLn $ show $ deps

