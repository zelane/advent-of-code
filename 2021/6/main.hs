import Data.List (iterate')
import Data.List.Split (splitOn)
import Data.Sequence (Seq, adjust', drop, filter, fromList, index, replicate, (|>))
import Prelude hiding (drop, filter, replicate)

step :: Seq Int -> Seq Int
step f = adjust' (+ zeroFishes) 6 withBabies
  where
    zeroFishes = index f 0
    withBabies = drop 1 f |> zeroFishes

conv :: Seq Int -> Seq Int
conv = foldl (flip (adjust' (+ 1))) (replicate 9 0)

main :: IO ()
main = do
  line <- readFile "input.txt"
  let fish = conv $ fromList $ map read $ splitOn "," line

  print $ sum $ last $ take 81 $ iterate' step fish
  print $ sum $ last $ take 257 $ iterate' step fish
