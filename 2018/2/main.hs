import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.IntSet  as IntSet
import Data.Tuple.Extra
import Data.List
-- import Data.Map


twosAndThrees :: [String] -> (Bool, Bool)
twosAndThrees [] = (False, False) 
twosAndThrees (x:xs) = smush (two, three) (twosAndThrees xs)
    where 
        two   = length x == 2
        three = length x == 3

smush :: (Bool, Bool) -> (Bool, Bool) -> (Bool, Bool)
smush (a, b) (c, d) = (a || c, b || d)

parseCode :: String -> (Int, Int)
parseCode x = both fromEnum (twosAndThrees $ group $ sort $ x)

parseCodes :: [String] -> Int
parseCodes x = combine (map parseCode x)

combine :: [(Int, Int)] -> Int
combine x = (sum $ map fst x) * (sum $ map snd x)

main = do
    file_lines <- fmap Text.lines (Text.readFile "input.txt")
    let lines = map Text.unpack file_lines
    putStrLn $ show $ parseCodes lines
