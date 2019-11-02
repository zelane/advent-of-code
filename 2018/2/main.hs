import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.IntSet  as IntSet
import Data.Tuple.Extra
import Data.List
-- import Data.Map


hasLetterCount :: Int -> String -> Bool
hasLetterCount l code = any (lengthOf l) (group $ sort $ code)

lengthOf :: Int -> String -> Bool
lengthOf l x = length x == l

matchCodes :: [String] -> Int -> Int
matchCodes codes l = length $ filter (hasLetterCount l) codes

main = do
    file_lines <- fmap Text.lines (Text.readFile "input.txt")
    let lines = map Text.unpack file_lines
    let answer_1 = (matchCodes lines 2) * (matchCodes lines 3)
    putStrLn $ show $ answer_1
