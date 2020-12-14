import Data.Char (digitToInt, intToDigit)
import Data.List (foldl')
import qualified Data.Map as M
import Numeric (showIntAtBase)
import Text.Regex.Posix ((=~))

maskInt :: String -> Int -> Int
maskInt mask int = toDec masked
  where
    binString = showIntAtBase 2 intToDigit int ""
    zeros = replicate (36 - length binString) '0'
    padded = zeros ++ binString
    masked = zipWith maskBit mask padded

maskBit :: Char -> Char -> Char
maskBit m x = if m == 'X' then x else m

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

run :: [String] -> M.Map Int Int -> M.Map Int Int
run [] results = results
run (l : lines) results = run nextMask newResults
  where
    mask = drop 7 l
    newResults = M.union (M.fromList mems) results
    memRows = takeWhile (\x -> take 4 x /= "mask") lines
    mems = map (parseMem mask) memRows
    nextMask = drop (length memRows) lines

parseMem :: String -> String -> (Int, Int)
parseMem mask memString = (read address, maskedVal) :: (Int, Int)
  where
    [[_, address, value]] = memString =~ "\\[(.*)\\] = ([0-9]+)" :: [[String]]
    maskedVal = maskInt mask (read value)

main :: IO ()
main = do
  lines <- fmap lines (readFile "input.txt")
  let mem1 = run lines M.empty

  print $ sum mem1
