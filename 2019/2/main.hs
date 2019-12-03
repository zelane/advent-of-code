import           Data.List.Split      (splitOn)
import           Data.Sequence hiding (take, drop, filter)
import qualified Data.Sequence as S   (take, drop)
import           Data.Maybe

at :: Int -> Seq Int -> Int
at i s = fromJust $ s !? i

run :: Int -> Seq Int -> Int
run i prog = if o == 99 then at 0 prog else run (i + 4) mem
  where
    (o:<|a:<|b:<|c:<|_) = S.take 4 (S.drop i prog)
    op = if o == 1 then (+) else (*)
    x = at a prog `op` at b prog
    mem = update c x prog

start :: Seq Int -> Int -> Int -> Int
start prog noun verb = run 0 mem
  where
    mem = update 2 verb $ update 1 noun prog

main = do
  file <- readFile "input.txt"
  let prog = fromList (map read (splitOn "," file) :: [Int])
  print $ start prog 12 2
  let all = [(start prog a b, a * 100 + b) | a <- [0..99], b <- [0..99]]
  print $ snd . head $ filter ((==19690720) . fst) all
