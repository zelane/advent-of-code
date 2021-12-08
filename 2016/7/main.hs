import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Text.Regex (mkRegex, subRegex)
import Text.Regex.Posix ((=~))

r :: String
r = "(\\[[a-z]+\\])"

type IP = ([String], [String])

parse :: String -> IP
parse s = (super, hyper)
  where
    hyper = map head (s =~ r :: [[String]])
    super = splitOn " " $ subRegex (mkRegex r) s " "

part1 :: IP -> Bool
part1 (super, hyper) = any checkTLS super && not (any checkTLS hyper)

checkTLS :: String -> Bool
checkTLS [a] = False
checkTLS (a : b : rest) = hasAbba || checkTLS (b : rest)
  where
    hasAbba = a /= b && [a, b] == reverse (take 2 rest)

part2 :: IP -> Bool
part2 (super, hyper) = or [b `isInfixOf` h | b <- babs, h <- hyper]
  where
    abas = filter (/= "") $ concatMap checkSSL super
    babs = map bab abas
    bab (a : b : _) = [b, a, b]

checkSSL :: String -> [String]
checkSSL [a, b] = []
checkSSL (a : b : c : rest) = aba : checkSSL (b : c : rest)
  where
    aba = if a == c && a /= b then [a, b, c] else []

main :: IO ()
main = do
  lines <- lines <$> readFile "input.txt"
  let ips = map parse lines
  print $ length $ filter part1 ips
  print $ length $ filter part2 ips
