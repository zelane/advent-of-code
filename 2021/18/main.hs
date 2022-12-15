import Text.Parsec

data Val = Num Int | List [Val] deriving (Show, Eq)

parseVal :: String -> Either ParseError Val
parseVal = parse valParser ""
  where
    valParser = numParser <|> listParser
    numParser = Num . read <$> many1 digit
    listParser = List <$> between (char '[') (char ']') (sepBy valParser (char ','))

main :: IO ()
main = do
  -- input <- readFile "input.txt"
  print $ parseVal "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
