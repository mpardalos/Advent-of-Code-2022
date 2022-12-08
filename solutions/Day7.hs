{-# LANGUAGE OverloadedStrings #-}

module Day7 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import Data.Vector qualified as V

data Line
  = LineCD !ByteString
  | LineLS
  | LineFile !Int
  | LineDir !ByteString
  deriving (Show)

parseLine :: ByteString -> Line
parseLine line = case BS.head line of
  '$' -> case BS.index line 2 of
    'c' -> LineCD (BS.drop 5 line)
    'l' -> LineLS
    _ -> undefined
  'd' -> LineDir (BS.drop 4 line)
  _ -> LineFile $ fst $ fromJust $ BS.readInt line

type Path = Vector ByteString

data DirTree
  = Dir
      ByteString
      -- ^ Name
      Int
      -- ^ Size
      (Vector DirTree)
      -- ^ Children
  deriving (Show)

addToPath :: Path -> Int -> DirTree -> DirTree
addToPath path addSize (Dir name size children)
  | V.length path == 0 = error "Cannot add to empty path"
  | V.length path == 1 = Dir name (size + addSize) children
  | otherwise =
      Dir
        name
        (size + addSize)
        ( V.map
            ( \child@(Dir childName _ _) ->
                if childName == (path V.! 1)
                  then addToPath (V.tail path) addSize child
                  else child
            )
            children
        )

createPath :: Path -> DirTree -> DirTree
createPath path (Dir name size children)
  | V.length path == 0 = error "Cannot add to empty path"
  | V.length path == 1 = error "Cannot add a root path (only a single segment)"
  | V.length path == 2 = Dir name size (V.cons (Dir (path V.! 1) 0 V.empty) children)
  | otherwise =
      Dir
        name
        size
        ( V.map
            ( \child@(Dir childName _ _) ->
                if childName == (path V.! 1)
                  then createPath (V.tail path) child
                  else child
            )
            children
        )

dirTreeFromLines :: [Line] -> DirTree
dirTreeFromLines = snd . foldl' go (V.singleton "/", Dir "/" 0 V.empty)
  where
    go :: (Path, DirTree) -> Line -> (Path, DirTree)
    go (path, dirTree) (LineCD "..") = (V.init path, dirTree)
    go (path, dirTree) (LineCD dirname) = (V.snoc path dirname, dirTree)
    go (path, dirTree) LineLS = (path, dirTree)
    go (path, dirTree) (LineFile size) = (path, addToPath path size dirTree)
    go (path, dirTree) (LineDir name) = (path, createPath (V.snoc path name) dirTree)

dirSizes :: DirTree -> [Int]
dirSizes (Dir _ size children) = size : concatMap dirSizes children

part1 :: ByteString -> Int
part1 =
  sum
    . filter (<= 100000)
    . dirSizes
    . dirTreeFromLines
    . map parseLine
    . tail
    . BS.lines

maxSize :: Int
maxSize = 70000000

requiredSize :: Int
requiredSize = 30000000

part2 :: ByteString -> Int
part2 input = minimum $ filter (>= needToFree) $ dirSizes dirTree
  where
    needToFree = requiredSize - availableSize
    availableSize = maxSize - totalUsed
    totalUsed = let (Dir _ size _) = dirTree in size
    dirTree = dirTreeFromLines $ map parseLine $ tail $ BS.lines input
