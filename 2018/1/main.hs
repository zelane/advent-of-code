import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List

convert :: Text.Text -> Integer
convert x = sign (Text.unpack x)

sign :: String -> Integer
sign ('+':xs) = read xs :: Integer
sign ('-':xs) = -1 * (read xs :: Integer)

sum_check :: Integer -> [Integer] -> [Integer] -> Integer
sum_check x prev_values vecs = if (elem x prev_values)
    then x
    else sum_check (x + (head vecs)) (prev_values ++ [x]) (rotateList vecs)

rotateList :: [a] -> [a]
rotateList x = tail x ++ [head x]

main = do
    lines <- fmap Text.lines (Text.readFile "src/input.txt")

    let converted = map convert lines
    putStrLn (show (sum converted))

    -- let total = sum_check 0 [] converted
    -- putStrLn (show total)
