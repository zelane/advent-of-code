{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

type Instruction = (String, Int)

parseIns :: String -> Instruction
parseIns s = (i, read v) where [i, v] = words s

answer1 :: [Instruction] -> (Int, Int) -> Int
answer1 [] (hoz, dep) = hoz * dep
answer1 ((ins, val) : xs) (hoz, dep) = answer1 xs (hoz + h, dep + d)
  where
    (h, d) = case ins of
      "forward" -> (val, 0)
      "down" -> (0, val)
      "up" -> (0, - val)

answer2 :: [Instruction] -> (Int, Int, Int) -> Int
answer2 [] (_, hoz, dep) = hoz * dep
answer2 ((ins, val) : xs) (aim, hoz, dep) = answer2 xs (a, hoz + h, dep + d)
  where
    (a, h, d) = case ins of
      "forward" -> (aim, val, val * a)
      "down" -> (aim + val, 0, 0)
      "up" -> (aim - val, 0, 0)

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let instructions = parseIns <$> lines
  print $ answer1 instructions (0, 0)
  print $ answer2 instructions (0, 0, 0)
