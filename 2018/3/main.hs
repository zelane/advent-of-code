import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List

main = do
    file_lines <- fmap Text.lines (Text.readFile "input.txt")
    let lines = map Text.unpack file_lines

    putStrLn $ show $ 0
