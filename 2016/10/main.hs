import Data.List (sort)
import Data.Map (Map, adjust, empty, fromList, insert, toList, union, (!))
import qualified Data.Map as M (filter)
import Debug.Trace (traceShow)
import Text.Regex.TDFA ((=~))

type BotMap = Map String Bot

data Bot = Bot
  { botId :: String,
    botVals :: [Int],
    botIns :: (String, String)
  }
  deriving (Show)

parseBots :: BotMap -> [String] -> BotMap
parseBots bots [] = bots
parseBots bots (s : sx) = parseBots newBots sx
  where
    [_, bid, low, high] = head (s =~ "(bot [0-9]+).*(output [0-9]+|bot [0-9]+).*(output [0-9]+|bot [0-9]+)" :: [[String]])
    newBot = Bot bid [] (low, high)
    newBots = insert bid newBot bots

applyVal :: BotMap -> [String] -> BotMap
applyVal bots [] = bots
applyVal bots (s : sx) = applyVal newBots sx
  where
    [_, val, bid] = head (s =~ "([0-9]+).*(bot [0-9]+)" :: [[String]])
    bot = bots ! bid
    newBot = bot {botVals = botVals bot ++ [read val]}
    newBots = insert bid newBot bots

give :: BotMap -> String -> Int -> BotMap
give bm bid x = adjust (\bot -> bot {botVals = botVals bot ++ [x]}) bid bm

run :: BotMap -> BotMap
run bm =
  if null active then bm else run $ step bm (bid, bot)
  where
    (bid, bot) = head active
    active = toList $ M.filter ((== 2) . length . botVals) bm

step :: BotMap -> (String, Bot) -> BotMap
step bm (abid, active) = do
  let (low, high) = botIns active
  let [lowV, highV] = sort $ botVals active
  let giveH = give bm low lowV
  let giveL = give giveH high highV
  let dropHL = insert abid (active {botVals = []}) giveL
  if lowV == 17 && highV == 61 then traceShow abid dropHL else dropHL

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let bots = parseBots empty $ filter ((== 'b') . head) input
  let valued = applyVal bots $ filter ((== 'v') . head) input
  let withOutputs = union valued $ fromList [(bid, Bot bid [] ("", "")) | x <- [0 .. 20], let bid = "output " ++ show x]
  let endState = run withOutputs
  print $ product $ head . botVals <$> M.filter ((`elem` ["output 0", "output 1", "output 2"]) . botId) endState
