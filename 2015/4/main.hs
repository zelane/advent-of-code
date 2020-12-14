import Data.Digest.Pure.MD5 (md5)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (isPrefixOf)

find :: String -> String -> [Int] -> Int
find secret prefix (x:xs) | prefix `isPrefixOf` hash = x
                          | otherwise = find secret prefix xs
      where hash = show $ md5 $ pack $ secret ++ show x

main :: IO ()
main = do
      print $ find "yzbqklnj" "00000" [0..]
      print $ find "yzbqklnj" "000000" [0..]
