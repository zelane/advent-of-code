import Text.Parsec

data Pair = A Pair Pair | B Int Pair | C Pair Int | D Int Int deriving (Show)

parse' :: Parsec String () Pair
parse' = choice [try parseA, try parseB, try parseC, try parseD]

parseA :: Parsec String () Pair
parseA = A <$> (char '[' >> parse') <*> (char ',' >> parse' <* char ']')

parseB :: Parsec String () Pair
parseB = B <$> (char '[' >> int) <*> (char ',' >> parse' <* char ']')

parseC :: Parsec String () Pair
parseC = C <$> (char '[' >> parse') <*> (char ',' >> int <* char ']')

parseD :: Parsec String () Pair
parseD = D <$> (char '[' >> int) <*> (char ',' >> int <* char ']')

iter :: Pair -> Int -> (Int, Int) -> Pair
iter pair depth (addL, addR) = case pair of
  A left right -> A (iterP left) (iterP left)
  B left right -> B left right
  C left right -> C left right
  D left right -> D left right
  where
    iterP p = iter p (depth + 1) (addL, addR)

int = read <$> many1 digit

main :: IO ()
main = do
  -- input <- readFile "input.txt"
  print $ parse parse' "" "[1,2]"
  print $ parse parse' "" "[[1,2],2]"
  print $ parse parse' "" "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
