import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.IntSet  as IntSet
import Data.List

convert :: Text.Text -> Int
convert x = sign (Text.unpack x)

sign :: String -> Int
sign ('+':xs) = read xs :: Int
sign ('-':xs) = -1 * (read xs :: Int)

sum_check :: Int -> IntSet.IntSet -> [Int] -> Int
sum_check x prev_values vecs = if (IntSet.member x prev_values)
    then x
    else sum_check (x + (head vecs)) (IntSet.insert x prev_values) (rotateList vecs)

rotateList :: [a] -> [a]
rotateList x = tail x ++ [head x]

main = do
    lines <- fmap Text.lines (Text.readFile "src/input.txt")

    let converted = map convert lines
    putStrLn (show (sum converted))

    let total = sum_check 0 IntSet.empty converted
    putStrLn (show total)
