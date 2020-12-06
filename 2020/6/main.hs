import Data.List.Split (splitOn)
import Data.List (group, sort)

fanswer2 :: ([String], [String]) -> Int
fanswer2 (group, answers) = length $ filter ((==l).length) answers
    where l = length group

main = do
  lines <- readFile "input.txt"
  let groups = map (splitOn "\n") $ splitOn "\n\n" lines

  let answers = map (group.sort.concat) groups
  let answer1 = sum (map length answers)
  print answer1
  
  let answer2 = sum $ map fanswer2 (zip groups answers)
  print answer2
