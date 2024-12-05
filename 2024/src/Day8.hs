module Day8 where

solve :: IO String -> IO ()
solve file = do
  lines <- lines <$> file
  print ""
