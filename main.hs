{-# LANGUAGE OverloadedLists #-}

module Codewars.G964.DblLinear where

import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Data.Sequence ((!?), (|>))
import qualified Data.Sequence as S
import Debug.Trace (traceShow)

-- import Data.IntSet (foldl')

dblLinear :: Int -> Integer
dblLinear n = traceShow (S.take 10 seq) toInteger $ 0
  where
    seq = foldl' calc [1] ([0 ..] :: S.Seq Int)

calc :: S.Seq Int -> Int -> S.Seq Int
calc s n = s |> (x * 2 + 1) |> (x * 3 + 1)
  where
    x = fromJust $ s !? n

main = do
  print $ dblLinear 10