module Day11 where

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  print ""
