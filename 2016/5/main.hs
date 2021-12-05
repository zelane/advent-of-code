{-# LANGUAGE OverloadedStrings #-}

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (pack, unpack)
import Data.Char (intToDigit)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T

hash :: String -> Text
hash = T.pack . unpack . encode . MD5.hash . pack

part2 :: [Text] -> [Char]
part2 hashes = map ((`T.index` 6) . fromJust . findP) ['0', '1', '2', '3', '4', '5', '6', '7']
  where
    findP p = find ((== p) . (`T.index` 5)) hashes

main :: IO ()
main = do
  let hashes = [h | x <- [1 ..], let h = hash ("ugkcyxxp" ++ show x), T.take 5 h == "00000"]
  let pass = map (`T.index` 5) $ take 8 hashes
  print pass
  print $ part2 hashes
