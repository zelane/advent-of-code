import Data.Char
import Data.Sequence hiding (length)
import qualified Data.Sequence as S (length)


swapCase :: Char -> Char
swapCase a = if isLower a then toUpper a else toLower a

react :: Char -> Char -> Bool
react a b = if a == swapCase b then True else False

chainReact :: Seq Char -> Seq Char -> Seq Char
chainReact Empty  a         = a
chainReact (x :<| xs) Empty = chainReact xs (singleton x)
chainReact (x :<| xs) a     = if react (lastS a) x then chainReact xs (initS a) else chainReact xs (a |> x)

lastS :: Seq Char -> Char
lastS (xs :|> x) = x

initS :: Seq Char -> Seq Char
initS (xs :|> x) = xs

main = do
    polymer <- readFile "input.txt"
    let poly = fromList polymer
    putStrLn $ show $ S.length $ chainReact poly empty
 