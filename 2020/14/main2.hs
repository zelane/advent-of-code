import Data.Char (digitToInt, intToDigit)
import Data.List (foldl')
import qualified Data.Map as M
import Numeric (showIntAtBase)
import Text.Regex.Posix ((=~))

data Mem = Mem {addr :: Int, val :: String}
  deriving (Show, Ord, Eq)

maskInt :: String -> Int -> [Int]
maskInt mask int = ints
  where
    binString = showIntAtBase 2 intToDigit int ""
    zeros = replicate (36 - length binString) '0'
    padded = zeros ++ binString
    masked = maskStr mask padded [""]
    ints = map toDec masked

maskStr :: String -> String -> [String] -> [String]
maskStr [] [] acc = acc
maskStr (m : ms) (s : ss) acc = maskStr ms ss results
  where
    options = maskBit m s
    results = [a : b | a <- options, b <- acc]

maskBit :: Char -> Char -> [Char]
maskBit m x
  | m == '0' = [x]
  | m == '1' = ['1']
  | m == 'X' = ['1', '0']

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

run :: [String] -> M.Map Int Int -> M.Map Int Int
run [] results = results
run (l : lines) results = run nextMask newResults
  where
    mask = drop 7 l
    newResults = M.union (M.fromList mems) results
    memRows = takeWhile (\x -> take 4 x /= "mask") lines
    mems = concatMap (parseMem mask) memRows
    nextMask = drop (length memRows) lines

parseMem :: String -> String -> [(Int, Int)]
parseMem mask memString = zip addresses (repeat (read value))
  where
    [[_, address, value]] = memString =~ "\\[(.*)\\] = ([0-9]+)" :: [[String]]
    addresses = maskInt mask (read address)

main :: IO ()
main = do
  lines <- fmap lines (readFile "input.txt")
  let mem1 = run lines M.empty

  print $ sum mem1
