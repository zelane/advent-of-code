import Data.IntMap.Strict as M (IntMap, delete, fromList, insert, lookup, member, null)
import Data.Maybe (fromJust)

dest :: M.IntMap Int -> [Int] -> Int -> Int
dest cups taken d
  | d `elem` taken = dest cups taken (d - 1)
  | d `M.member` cups = d
  | otherwise = dest cups taken (maximum cups)

play :: Int -> M.IntMap Int -> (Int, M.IntMap Int)
play cup cups = (oldC, moveABC)
  where
    a = fromJust $ M.lookup cup cups
    b = fromJust $ M.lookup a cups
    c = fromJust $ M.lookup b cups
    d = dest cups [a, b, c] (cup - 1)
    oldC = fromJust $ M.lookup c cups
    oldD = fromJust $ M.lookup d cups
    moveABC = M.insert cup oldC $ M.insert c oldD $ M.insert d a cups

run :: Int -> Int -> M.IntMap Int -> M.IntMap Int
run 0 _ cups = cups
run i cup cups = run (i - 1) newCup newCups
  where
    (newCup, newCups) = play cup cups

toList :: M.IntMap Int -> [Int]
toList = toList2 1 []

toList2 :: Int -> [Int] -> M.IntMap Int -> [Int]
toList2 k s m = if M.null m then s else s ++ [k] ++ toList2 n s (M.delete k m)
  where
    n = fromJust $ M.lookup k m

toMap :: [Int] -> M.IntMap Int
toMap l = M.fromList $ zip l (tail l ++ [head l])

main :: IO ()
main = do
  let input = [9, 5, 2, 3, 1, 6, 4, 8, 7]

  let answer1 = run 100 (head input) (toMap input)
  print $ concatMap show $ drop 1 $ toList answer1

  let input2 = input ++ [maximum input + 1 .. 1000000]
  let answer2 = run 10000000 (head input2) (toMap input2)
  print $ product $ take 2 $ drop 1 $ toList answer2
