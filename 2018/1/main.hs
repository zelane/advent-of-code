import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List

convert :: Text.Text -> Integer
convert x = sign (fst (split x)) (snd (split x)) 

split :: Text.Text -> ([Char], [Char])
split x = splitAt 1 (Text.unpack x)

sign :: [Char] -> [Char] -> Integer
sign "+" x = read x :: Integer
sign "-" x = -1 * (read x :: Integer)

main = do
    lines <- fmap Text.lines (Text.readFile "src/input.txt")

    let total = sum (map convert lines)
    putStrLn (show (total))
