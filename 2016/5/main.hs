{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.MD5 (md5, md5DigestBytes)
import Data.Hash.MD5 (Str (Str), md5s)
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Text as T (Text, pack, take, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy.Encoding (encodeUtf8)

main :: IO ()
main = do
  -- let pass = take 8 [hash !! 5 | x <- [1 ..], let hash = show $ md5 $ encodeUtf8 $ "ugkcyxxp", T.take 5 hash == T.pack "00000"]
  let pass = take 8 [hash !! 5 | x <- [1 ..], let hash = md5s . Str $ "ugkcyxxp" ++ show x, take 5 hash == "00000"]
  print pass

-- print $ fromJust $ find (\x -> take 6 x == "000001") [hash | x <- [1 ..], let hash = show $ md5 $ fromString $ pre ++ show x]
