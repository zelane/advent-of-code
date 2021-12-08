{-# LANGUAGE OverloadedStrings #-}

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (ByteString, index, isPrefixOf, pack)
import Data.List (find)
import Data.Maybe (fromJust)

hash :: ByteString -> ByteString
hash = encode . MD5.hash

part2 :: [ByteString] -> [Char]
part2 hashes = map ((`index` 6) . fromJust . findP) ['0', '1', '2', '3', '4', '5', '6', '7']
  where
    findP p = find ((== p) . (`index` 5)) hashes

main :: IO ()
main = do
  let hashes = [h | x <- [1 ..], let h = hash $ pack ("ugkcyxxp" ++ show x), "00000" `isPrefixOf` h]
  print $ map (`index` 5) $ take 8 hashes
  print $ part2 hashes
