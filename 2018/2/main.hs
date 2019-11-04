import Data.List
import Data.Maybe (catMaybes)


hasLetterCount :: Int -> String -> Bool
hasLetterCount l code = any (lengthOf l) (group $ sort $ code)

lengthOf :: Int -> String -> Bool
lengthOf l x = length x == l

matchCodes :: [String] -> Int -> Int
matchCodes codes l = length $ filter (hasLetterCount l) codes

commonLetters :: String -> String -> [Char] -> [Char]
commonLetters [] [] x = x
commonLetters (a:as) (b:bs) x = if a == b then commonLetters as bs (x ++ [a]) else commonLetters as bs x

findMatch :: [String] -> String -> Maybe String
findMatch [] x = Nothing
findMatch (l:ls) x = if length common == 25 then Just common else findMatch ls x
    where
        common = commonLetters x l []

main = do
    lines <- fmap lines (readFile "input.txt")

    let answer_1 = (matchCodes lines 2) * (matchCodes lines 3)
    putStrLn $ show $ answer_1

    let answer_2 = head $ catMaybes $ map (findMatch lines) lines
    putStrLn $ show $ answer_2
