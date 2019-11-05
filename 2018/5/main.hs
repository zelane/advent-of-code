import Data.List
import Data.Char
import Debug.Trace

react :: Char -> Char -> Bool
react a b = if (toLower a) /= (toLower b) || (isLower a) == (isLower b) then False else True

slide :: (Char -> Char -> Bool) -> [Char] -> [Char] -> [Char]
slide _ [] a       = a
slide _ [x] a      = a ++ [x]
slide f (x:y:xs) a = if react x y then slide f xs a else slide f (y:xs) (a ++ [x])

repeated :: ([Char] -> [Char] -> [Char]) -> [Char] -> [Char]
repeated f polymer = if (length polymer) /= traceShow (length new_polymer) (length new_polymer) then repeated f new_polymer else new_polymer
    where
        new_polymer = f polymer []

main = do
    polymer <- readFile "input.txt"
    -- let polymer = "dabAcCaCBAcCcaDA"
    -- putStrLn $ show $ map (take 2) (tails polymer)
    -- putStrLn $ show $ slide react polymer []
    putStrLn $ show $ repeated (slide react) polymer
 