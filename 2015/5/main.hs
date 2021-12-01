import Data.List (isInfixOf)
import Data.Foldable (toList)
import Text.Regex.Posix ((=~))

countIf :: Foldable f => (a -> Bool) -> f a -> Int
countIf cond = length . filter cond . toList

nice :: String -> Bool
nice s = countIf (`elem` "aeiou") s > 2 && s =~ "(.)\\1" && not (any (`isInfixOf` s) ["ab", "cd", "pq", "xy"])

nicev2 :: String -> Bool
nicev2 s = s =~ "(..).*\\1" && s =~ "(.).\\1"

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"

  print $ countIf nice input
  print $ countIf nicev2 input
