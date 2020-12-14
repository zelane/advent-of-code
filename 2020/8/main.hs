import qualified Data.Set as S
import Debug.Trace (traceShow)
import Text.Regex.Posix ((=~))

parseCode :: String -> (String, Int)
parseCode ins = (code, val)
  where
    [_, code, _val] = head (ins =~ "([a-z]+) ([+|-][0-9]+)" :: [[String]])
    val
      | head _val == '-' = read _val :: Int
      | otherwise = read (drop 1 _val) :: Int

runCmd :: (String, Int) -> (Int, Int) -> (Int, Int)
runCmd (cmd, val) (index, acc) = case cmd of
  "nop" -> (index + 1, acc)
  "acc" -> (index + 1, acc + val)
  "jmp" -> (index + val, acc)

run :: [(String, Int)] -> (Int, Int) -> S.Set Int -> (Bool, Int)
run cmds (index, acc) visited
  | index `elem` visited = (False, acc)
  | index > length cmds - 1 = (True, acc)
  | otherwise = run cmds (runCmd cmd (index, acc)) _visited
  where
    cmd = cmds !! index
    _visited = S.insert index visited

fixProg :: [(String, Int)] -> [(String, Int)] -> Int
fixProg prog ((code, val) : cmds) = if exit then acc else fixProg prog cmds
  where
    newCode = case code of
      "nop" -> "jmp"
      "acc" -> "acc"
      "jmp" -> "nop"
    index = (length prog - length cmds) - 1
    fprog = take index prog ++ [(newCode, val)] ++ cmds
    (exit, acc) = run fprog (0, 0) S.empty

main = do
  lines <- fmap lines (readFile "input.txt")
  let prog = map parseCode lines
  print $ snd $ run prog (0, 0) S.empty

  print $ fixProg prog prog
