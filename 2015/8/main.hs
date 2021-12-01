import Data.String.Utils (replace, startswith)
import Debug.Trace (traceShow)
import Text.Regex.Posix ((=~))

m :: String -> (Int, Int)
m x = (a, b)
  where
    re = x =~ "(\\\\x[a-f0-9][a-f0-9]|\\\\\\\"|\\\\\\\\|[a-z])" :: [[String]]
    y = concatMap (drop 1) re
    z = sum $ map extra y
    a = length x - length y
    b = (length x + z + 4) - length x
    extra x
      | startswith "\\x" x = 1
      | x == "\\\"" = 2
      | x == "\\\\" = 2
      | otherwise = 0

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print $ sum $ map (fst . m) input
  print $ sum $ map (snd . m) input
