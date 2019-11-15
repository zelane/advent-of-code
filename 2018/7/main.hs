import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.List
import Text.Regex

process :: [Char] -> [Char] -> String -> String
process [], _, done = done
process (p:parts), deps, done = 
    where
        to_do    = filter (notElem done) parts
        pre_reqs = concatMap snd $ filter (fst . elem) deps

main = do
    input <- fmap lines $ readFile "test.txt"
    let deps  = catMaybes $ map (matchRegex (mkRegex " ([A-Z]) .* ([A-Z]) ")) input
    let parts = nub $ concatMap concat deps
    

