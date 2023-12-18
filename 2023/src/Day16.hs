{-# LANGUAGE Strict #-}

module Day16 where

import Data.Bifunctor (bimap)
import Data.HashMap.Strict qualified as M
import Data.HashTable.IO qualified as H
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as S
import Data.Tuple (swap)

type Cache = H.BasicHashTable ((Int, Int), (Int, Int)) Bool

type Grid = M.HashMap (Int, Int) Char

rotateR :: (Int, Int) -> (Int, Int)
rotateR = swap . bimap (* (-1)) (* (-1))

trace :: Grid -> (Int, Int) -> (Int, Int) -> IO Int
trace g sPos sDir = do
  cache <- H.new :: IO Cache
  length <$> go cache sPos sDir
  where
    go cache pos@(x, y) dir = do
      ext <- H.lookup cache (dir, pos)
      if isJust ext
        then return []
        else do
          H.insert cache (dir, pos) True
          let char = fromMaybe '!' $ M.lookup pos g
          let next (mx, my) = S.insert pos <$> go cache (x + mx, y + my) (mx, my)
          case (char, dir) of
            ('!', _) -> return []
            ('/', _) -> next $ rotateR dir
            ('\\', _) -> next $ swap dir
            ('|', (_, 0)) -> next (0, -1) <> next (0, 1)
            ('-', (0, _)) -> next (-1, 0) <> next (1, 0)
            _ -> next dir

solve :: IO String -> IO ()
solve file = do
  input <- lines <$> file
  let grid = M.fromList $ concat [[((x, y), c) | (x, c) <- zip [0 ..] line] | (y, line) <- zip [0 ..] input]

  let maxX = length (head input) - 1
  let maxY = length input - 1

  let edges =
        [((x, 0), (0, 1)) | x <- [0 .. maxX]]
          <> [((x, maxY), (0, -1)) | x <- [0 .. maxX]]
          <> [((0, y), (1, 0)) | y <- [0 .. maxY]]
          <> [((maxX, y), (-1, 0)) | y <- [0 .. maxY]]

  print =<< trace grid (0, 0) (1, 0)
  print . maximum =<< mapM (uncurry (trace grid)) edges
