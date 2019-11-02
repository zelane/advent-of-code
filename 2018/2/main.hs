import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List


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
    file_lines <- fmap Text.lines (Text.readFile "input.txt")
    let lines = map Text.unpack file_lines

    let answer_1 = (matchCodes lines 2) * (matchCodes lines 3)
    putStrLn $ show $ answer_1

    let answer_2 = map (findMatch lines) lines
    putStrLn $ show $ answer_2
