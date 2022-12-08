import Data.List (nub)

findMarker :: Int -> String -> Int
findMarker l s = loop l 0 s
  where
    loop l i s =
      if length (nub $ take l s) == l
        then i + l
        else loop l (i + 1) (drop 1 s)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ findMarker 4 input
  print $ findMarker 14 input
