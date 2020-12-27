import qualified Data.HashMap.Lazy as M

play :: [Int] -> [Int] -> [Int]
play a [] = a
play [] b = b
play (a : as) (b : bs) = if a > b then play (as ++ [a, b]) bs else play as (bs ++ [b, a])

play2 :: M.HashMap ([Int], [Int]) Int -> [Int] -> [Int] -> (Char, [Int])
play2 _ a [] = ('a', a)
play2 _ [] b = ('b', b)
play2 cache a b
  | (a, b) `M.member` cache = ('a', a)
  | playSubgame && subGame == 'a' = awins
  | playSubgame && subGame == 'b' = bwins
  | a1 > b1 = awins
  | b1 > a1 = bwins
  where
    newCache = M.insert (a, b) 1 cache
    (a1 : as) = a
    (b1 : bs) = b
    playSubgame = a1 <= length as && b1 <= length bs
    subGame = fst $ play2 M.empty (take a1 as) (take b1 bs)
    awins = play2 newCache (as ++ [a1, b1]) bs
    bwins = play2 newCache as (bs ++ [b1, a1])

score :: [Int] -> Int
score x = sum $ zipWith (*) [1 .. length x] $ reverse x

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let ints = map read $ filter (\x -> x `notElem` ["Player 1:", "Player 2:", ""]) lines :: [Int]
  let (one, two) = splitAt (length ints `div` 2) ints

  print $ score $ play one two
  print $ score $ snd $ play2 M.empty one two
