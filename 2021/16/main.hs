{-# LANGUAGE DuplicateRecordFields #-}

import Data.Char (digitToInt)
import Data.Either (fromLeft)
import Data.Either.Extra (fromLeft', fromRight')
import Data.List (foldl')
import Debug.Trace
import Numeric (readHex, showIntAtBase)
import Text.Parsec

hexToBin :: Char -> String
hexToBin c = case c of
  '0' -> "0000"
  '1' -> "0001"
  '2' -> "0010"
  '3' -> "0011"
  '4' -> "0100"
  '5' -> "0101"
  '6' -> "0110"
  '7' -> "0111"
  '8' -> "1000"
  '9' -> "1001"
  'A' -> "1010"
  'B' -> "1011"
  'C' -> "1100"
  'D' -> "1101"
  'E' -> "1110"
  'F' -> "1111"

data Packet = OP Int Int [Packet] | Lit Int Int Int deriving (Show)

readBin :: String -> Int
readBin = foldl' (\acc x -> acc * 2 + digitToInt x) 0

parseA :: Parsec String () [Packet]
parseA = do
  re <- many (choice [try lit, try op1, try op2])
  many anyChar
  return re

op1 :: Parsec String () Packet
op1 = do
  -- parserTrace "op1"
  ver <- count 3 anyChar
  id <- count 3 anyChar
  l <- char '0'
  spLen <- readBin <$> count 15 anyChar
  sps <- count spLen anyChar
  let subPackets = fromRight' $ parse parseA "" sps
  return $ OP (readBin id) (readBin ver) subPackets

op2 :: Parsec String () Packet
op2 = do
  -- parserTrace "op2"
  ver <- count 3 anyChar
  id <- count 3 anyChar
  l <- char '1'
  spLen <- readBin <$> count 11 anyChar
  subPackets <- count spLen $ choice [try lit, try op1, try op2]
  return $ OP (readBin id) (readBin ver) subPackets

lit :: Parsec String () Packet
lit = do
  -- parserTrace "lit"
  ver <- count 3 anyChar
  id <- string "100"
  ones <- many $ char '1' >> count 4 anyChar
  last <- char '0' >> count 4 anyChar
  let v = concat $ ones ++ [last]
  return $ Lit (readBin id) (readBin ver) (readBin v)

-- calc :: [[Int]] -> Int
-- calc (i : is) = case i !! 1 of
--   3 -> max (take subpackets is)
--   where
--     subpackets = i !! 2

main :: IO ()
main = do
  input <- readFile "input.txt"
  let bins = concatMap hexToBin input
  let o = parse parseA "" bins
  print o

-- print $ sum $ map (!! 1)  $ fromRight' o
