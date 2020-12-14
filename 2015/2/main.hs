import Data.List.Split (splitOn)
import Data.List (sort)

toTuple :: [String] -> (Int, Int, Int)
toTuple [l, w, h] = (read l, read w, read h)

calcPaper :: (Int, Int, Int) -> Int
calcPaper (l, w, h) = 2*a + 2*b + 2*c + minimum [a, b, c]
        where a = l*w
              b = w*h
              c = h*l

calcRibbon :: (Int, Int, Int) -> Int
calcRibbon (l, w, h) = wrap + bow
        where shortest = take 2 $ sort [l, w, h]
              wrap = (shortest!!0 * 2) + (shortest!!1 * 2)
              bow = l * w * h

main :: IO ()
main = do
    lines <- fmap lines (readFile "input.txt")
    let dimensions = map (toTuple.splitOn "x") lines
    print $ sum $ map calcPaper dimensions
    print $ sum $ map calcRibbon dimensions
