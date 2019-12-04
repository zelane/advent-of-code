import Data.Digits (digits)
import Data.List (group, sort)

checkInc :: [Int] -> Bool
checkInc [] = True
checkInc [_] = True
checkInc (c:code) = c <= head code && checkInc code

checkRep :: (Int -> Bool) -> [Int] -> Bool
checkRep test code = any (test.length) $ group $ code

validate :: (Int -> Bool) -> Int -> Bool
validate test code = checkInc codeList && checkRep test codeList
    where codeList = digits 10 code

main :: IO()
main = do
    print $ length $ [code | code <- [246515..739105], validate (>=2) code]
    print $ length $ [code | code <- [246515..739105], validate (==2) code]
