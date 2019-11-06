import Data.List
import Data.Char
import Debug.Trace

swapCase :: Char -> Char
swapCase a = if isLower a then toUpper a else toLower a

react :: Char -> Char -> Bool
react a b = if a == swapCase b then True else False

chainReact :: [Char] -> [Char] -> [Char]
chainReact [] a    = a
chainReact (x:xs) [] = chainReact xs [x]
chainReact (x:xs) a = if react (last a) x then chainReact xs (init a) else chainReact xs (a ++ [x])

main = do
    polymer <- readFile "input.txt"
    putStrLn $ show $ length $ chainReact polymer []
 