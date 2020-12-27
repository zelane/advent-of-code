import Data.Char (digitToInt)
import Debug.Trace (traceShow)
import Text.Regex.Posix ((=~))

run :: Int -> Char -> String -> (String, Int)
run acc _ "" = ("", acc)
run acc op (x : cs)
  | c `elem` ints = run newAcc op cs
  | c == '*' = run acc '*' cs
  | c == '+' = run acc '+' cs
  | c == '(' = run _newAcc op subCs
  | c == ')' = (cs, acc)
  | c == ' ' = run acc op cs
  where
    c = x
    ints = ['1', '2', '3', '4', '5', '6', '7', '8', '9']
    newAcc = if op == '*' then acc * digitToInt c else acc + digitToInt c
    (subCs, subAcc) = run 0 '+' cs
    _newAcc = if op == '*' then acc * subAcc else acc + subAcc

parseBrackets :: String -> String -> (String, String)
parseBrackets acc [] = (parseMul $ parseAdd acc, [])
parseBrackets acc (c : cs)
  | c == '(' = parseBrackets (acc ++ subVal) subCs
  | c == ')' = (parseMul $ parseAdd acc, cs)
  | otherwise = parseBrackets (acc ++ [c]) cs
  where
    (subVal, subCs) = parseBrackets "" cs

parseOp :: String -> String -> (Int -> Int -> Int) -> String
parseOp s op f =
  if null match then s else parseOp rep op f
  where
    re = "([0-9]+)\\s?\\" ++ op ++ "\\s?([0-9]+)"
    match = s =~ re :: [[String]]
    (a, b) = s =~ re :: (Int, Int)
    val = f (read (m !! 1)) (read (m !! 2)) :: Int where m = head match
    rep = take a s ++ show val ++ drop (a + b) s :: String

parseAdd :: String -> String
parseAdd s = parseOp s "+" (+)

parseMul :: String -> String
parseMul s = parseOp s "*" (*)

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"

  print $ sum $ map (snd . run 0 '+') lines
  print $ sum $ map (read . fst . parseBrackets "") lines
