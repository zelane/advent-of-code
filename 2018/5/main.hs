import Data.Char
import Data.Sequence hiding         (length)
import qualified Data.Sequence as S (length, filter)
import qualified Data.Set      as Set


swapCase :: Char -> Char
swapCase a = if isLower a then toUpper a else toLower a

react :: Char -> Char -> Bool
react a b = if a == swapCase b then True else False

chainReact :: Seq Char -> Seq Char -> Seq Char
chainReact Empty result         = result
chainReact (unit:<|poly) Empty  = chainReact poly (singleton unit)
chainReact (unit:<|poly) result = if react lastUnit unit
                                  then chainReact poly rs
                                  else chainReact poly (result |> unit)
    where
        (rs:|>lastUnit) = result

filterLetter :: Seq Char -> Char -> Seq Char
filterLetter polymer unit = S.filter f polymer
    where 
        f x = (toLower x) /= unit

findShortest :: Seq Char -> [Char] -> Int -> Int
findShortest polymer [] x     = x
findShortest polymer (c:cs) x = findShortest polymer cs longest
    where 
        fpolymer   = filterLetter polymer c
        polyLength = S.length $ chainReact fpolymer empty
        longest    = min polyLength x

main = do
    polymer <- readFile "input.txt"
    let poly = fromList polymer
    let polyLen = S.length $ chainReact poly empty
    putStrLn $ show $ polyLen
    
    let allLetters = Set.toList $ Set.fromList $ map toLower polymer
    putStrLn $ show $ findShortest poly allLetters polyLen
