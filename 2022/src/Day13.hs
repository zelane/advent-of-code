{-# LANGUAGE InstanceSigs #-}

module Day13 where

import Data.Either (fromRight)
import Data.List (elemIndex, sort)
import Data.List.Split (chunksOf)
import Text.Parsec
  ( ParseError,
    between,
    char,
    digit,
    many1,
    parse,
    sepBy,
    (<|>),
  )
import Text.Parsec.String (Parser)

data Val = Num Int | List [Val] deriving (Show, Eq)

instance Semigroup Val where
  (List xs) <> y = List (xs <> [y])

instance Monoid Val where
  mempty = List []

instance Ord Val where
  compare :: Val -> Val -> Ordering
  compare x y
    | x == y = EQ
    | test x y = LT
    | otherwise = GT

parseVal :: String -> Either ParseError Val
parseVal = parse valParser ""
  where
    valParser = numParser <|> listParser
    numParser = Num . read <$> many1 digit
    listParser = List <$> between (char '[') (char ']') (sepBy valParser (char ','))

test :: Val -> Val -> Bool
test (Num x) (Num y) = x < y
test (List xs) (Num y) = test (List xs) $ List [Num y]
test (Num y) (List xs) = test (List [Num y]) $ List xs
test (List []) (List []) = True
test (List xs) (List []) = False
test (List []) (List ys) = True
test (List (x : xs)) (List (y : ys)) = if y == x then test (List xs) (List ys) else test x y

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let vals = fromRight mempty . parseVal <$> filter (/= "") input
  print $ sum [i | (i, [a, b]) <- zip [1 ..] (chunksOf 2 vals), test a b]
  let a = List [List [Num 2]]
  let b = List [List [Num 6]]
  let part2 = sort $ vals <> [a, b]
  print $ do
    a <- elemIndex a part2
    b <- elemIndex b part2
    return $ (a + 1) * (b + 1)
