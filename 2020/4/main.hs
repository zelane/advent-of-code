import Data.List.Split (splitOn)
import Data.List (sort)
import Text.Regex (matchRegex, mkRegex)
import Data.Maybe (isJust)
import Data.Char (isDigit)

parseEntry :: Bool -> String -> Bool
parseEntry skipVals entry = validKeys && (skipVals || validVals)
    where 
        noBreaks = splitOn "\n" entry
        noSpaces = map (splitOn " ") noBreaks
        pairs = map (splitAt 4) $ concat noSpaces
        keys = filter (/="cid") $ sort $ map (head . splitOn ":") $ concat noSpaces
        validKeys = keys == ["byr","ecl","eyr","hcl","hgt","iyr","pid"]
        validVals = all validate pairs

validate :: (String, String) -> Bool
validate ("cid:", v) = True
validate ("byr:", v) = x >= 1920 && x <= 2002
    where x = (read v)::Int
validate ("ecl:", v) = elem v ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validate ("eyr:", v) = x >= 2020 && x <= 2030
    where x = (read v)::Int
validate ("hcl:", v) = head v == '#' && regexMatch
    where regexMatch = isJust $ matchRegex (mkRegex "[0-9a-fA-F]{6}") (tail v)
validate ("hgt:", v) = if unit == "in" then 59 <= value && value <= 76 
                       else if unit == "cm" then 150 <= value && value <= 193
                       else False
    where (x, unit) = splitAt ((length v) - 2) v
          value = read x::Int
validate ("iyr:", v) = x >= 2010 && x <= 2020
    where x = (read v)::Int
validate ("pid:", v) = (length v) == 9 && all isDigit v

main = do
  file <- readFile "input.txt"
  
  let entries = splitOn "\n\n" file
  let answer1 = length $ filter (parseEntry True) entries
  print answer1

  let answer2 = length $ filter (parseEntry False) entries
  print answer2
