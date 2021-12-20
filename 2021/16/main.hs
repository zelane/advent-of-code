import Data.Char (digitToInt, intToDigit)
import Data.Either.Extra (fromRight')
import Data.List (foldl')
import Numeric (readHex, showIntAtBase)
import Text.Parsec

data Packet = Op Int Int [Packet] | Lit Int Int Int deriving (Show)

readBin :: String -> Int
readBin = foldl' (\acc x -> acc * 2 + digitToInt x) 0

showBin :: (Integral a, Show a) => a -> ShowS
showBin = showIntAtBase 2 intToDigit

parseA :: Parsec String () [Packet]
parseA = many (choice [try lit, try op1, try op2])

op1 :: Parsec String () Packet
op1 = do
  ver <- count 3 anyChar
  id <- count 3 anyChar
  l <- char '0'
  spLen <- readBin <$> count 15 anyChar
  sps <- count spLen anyChar
  let subPackets = fromRight' $ parse parseA "" sps
  return $ Op (readBin id) (readBin ver) subPackets

op2 :: Parsec String () Packet
op2 = do
  ver <- count 3 anyChar
  id <- count 3 anyChar
  l <- char '1'
  spLen <- readBin <$> count 11 anyChar
  subPackets <- count spLen $ choice [try lit, try op1, try op2]
  return $ Op (readBin id) (readBin ver) subPackets

lit :: Parsec String () Packet
lit = do
  ver <- count 3 anyChar
  id <- string "100"
  ones <- many $ char '1' >> count 4 anyChar
  last <- char '0' >> count 4 anyChar
  let val = concat $ ones ++ [last]
  return $ Lit (readBin id) (readBin ver) (readBin val)

calc :: Packet -> Int
calc i = case i of
  Lit _ _ v -> v
  Op 0 _ subs -> sum $ map calc subs
  Op 1 _ subs -> product $ map calc subs
  Op 2 _ subs -> minimum $ map calc subs
  Op 3 _ subs -> maximum $ map calc subs
  Op 5 _ [a, b] -> if calc a > calc b then 1 else 0
  Op 6 _ [a, b] -> if calc a < calc b then 1 else 0
  Op 7 _ [a, b] -> if calc a == calc b then 1 else 0

main :: IO ()
main = do
  input <- readFile "input.txt"
  let bins = "0" ++ showBin (fst . head $ readHex input) ""
  let parsed = parse parseA "" bins
  print $ head $ calc <$> fromRight' parsed

-- print $ sum $ map (!! 1)  $ fromRight' o
