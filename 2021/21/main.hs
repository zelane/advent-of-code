import Data.Function.Memoize (memoize2)
import Data.List (delete, foldl', foldl1', nub, permutations)
import Debug.Trace (traceShow)

turn :: Int -> Int -> Int
turn pos dice = ((newPos - 1) `mod` 10) + 1
  where
    moves = (dice * 3) + 3
    newPos = pos + moves

play :: Int -> Int -> (Int, Int) -> (Int, Int) -> Int
play dice rolls (s1, p1) (s2, p2)
  | np1 >= 1000 = p2 * (rolls + 3)
  | np2 >= 1000 = np1 * (rolls + 6)
  | otherwise = play (dice + 6) (rolls + 6) (ns1, np1) (ns2, np2)
  where
    ns1 = turn s1 dice
    ns2 = turn s2 (dice + 3)
    np1 = p1 + ns1
    np2 = p2 + ns2

turn2 :: Int -> Int -> Int
turn2 pos moves = ((newPos - 1) `mod` 10) + 1
  where
    newPos = pos + moves

play2' :: (Int, Int) -> (Int, Int) -> (Int, Int)
play2' = memoize2 play2

rolls :: [Int]
rolls = [a + b + c | a <- [1 .. 3], b <- [1 .. 3], c <- [1 .. 3]]

play2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
play2 (s1, p1) (s2, p2)
  | p1 >= 21 && p2 >= 21 = (0, 0)
  | p1 >= 21 = (1, 0)
  | p2 >= 21 = (0, 1)
  | otherwise = foldl1' tSum [play2' (ns1, ns1 + p1) (ns2, ns2 + p2) | ns1 <- ns1s, ns2 <- ns2s]
  where
    ns1s = map (turn2 s1) rolls
    ns2s = map (turn2 s2) rolls
    tSum (a, b) (c, d) = (a + c, b + d)

main :: IO ()
main = do
  let input = (4, 8)
  -- let input = (5, 10)

  print $ play 1 0 (fst input, 0) (snd input, 0)
  print ""
  print $ play2' (fst input, 0) (snd input, 0)

-- 444356092776315
-- 341960390180808

-- 11997614504960505
-- 341960390180808

-- 711480
-- 265845890886828

-- 7177839053944356
-- 265845890886828