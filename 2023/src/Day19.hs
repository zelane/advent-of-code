module Day19 where

import Data.Char (isAlpha)
import Data.List.Split (splitOn)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import GHC.Ix (Ix (inRange))
import Text.Regex.TDFA ((=~))

type Check = (String, String, Int, String) -- part, op, next

data Ins = Check String String Int String | End String | Jmp String deriving (Eq, Show)

type Part = M.Map String Int

type Workflows = [(String, [Ins])]

parseWorkflow :: String -> (String, [Ins])
parseWorkflow s = (id', ins <$> parts)
  where
    (id', rest) = span isAlpha s
    parts = splitOn "," $ tail $ init rest
    ins i
      | length i == 1 = End i
      | all isAlpha i = Jmp i
      | otherwise =
          let [_, a, b, c, d] = head (i =~ ("([a-z])(<|>)([0-9]+):([a-zA-Z]+)" :: String) :: [[String]])
           in Check a b (read c) d

parseParts :: String -> Part
parseParts s = M.fromList [(p !! 1, read $ last p) | p <- parts]
  where
    parts = s =~ ("([a-z])=([0-9]+)" :: String) :: [[String]]

foldW :: Workflows -> [Ins] -> [[(String, String, Int)]]
foldW wfs (i : ins) = case i of
  Check a ">" v n -> (((a, ">", v) :) <$> next n) <> (((a, "<=", v) :) <$> foldW wfs ins)
  Check a "<" v n -> (((a, "<", v) :) <$> next n) <> (((a, ">=", v) :) <$> foldW wfs ins)
  End a -> [[(a, "end", 0)]]
  Jmp a -> foldW wfs (look a)
  where
    look x = fromJust $ lookup x wfs
    next x
      | x == "A" = [[("A", "end", 0)]]
      | x == "R" = []
      | otherwise = foldW wfs (look x)

foldRanges :: M.Map String (Int, Int) -> (String, String, Int) -> M.Map String (Int, Int)
foldRanges ran (a, op, x) = M.adjust adjust a ran
  where
    adjust (min', max') = case op of
      ">" -> (max min' (x + 1), max')
      ">=" -> (max min' x, max')
      "<" -> (min', min max' (x - 1))
      "<=" -> (min', min max' x)

calc :: [(Int, Int)] -> Int
calc rs = product [ma - mi + 1 | (mi, ma) <- rs]

test :: [(Int, Int)] -> [Int] -> Bool
test ranges vs = and $ zipWith inRange ranges vs

testAll :: [[(Int, Int)]] -> [Int] -> Bool
testAll ranges vs = any (`test` vs) ranges

solve :: IO String -> IO ()
solve file = do
  input <- file
  let [a, b] = lines <$> splitOn "\n\n" input
  let works = parseWorkflow <$> a
  let parts = parseParts <$> b

  let ranges = foldW works (fromJust $ lookup "in" works)
  let valid = init <$> filter ((\(x, _, _) -> x == "A") . last) ranges
  let def = M.fromList $ [(c, (1, 4000)) | c <- ["x", "m", "a", "s"]]
  let ranges' = M.elems . foldl foldRanges def <$> valid

  print $ sum . concat $ filter (testAll ranges') (M.elems <$> parts)
  print $ sum $ calc <$> ranges'
