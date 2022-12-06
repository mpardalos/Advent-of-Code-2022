module Day6 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (Foldable (foldl'))
import Data.List (sort)
import Safe (headDef, tailSafe)

solve :: Int -> ByteString -> Int
solve distinctCount str = go [] 0
  where
    go :: [Char] -> Int -> Int
    go acc idx
      | length acc == distinctCount = idx
      | char `notElem` acc = go (char : acc) (idx + 1)
      | otherwise = go (char : takeWhile (/= char) acc) (idx + 1)
      where
        char = BS.index str idx

part1 :: ByteString -> Int
part1 = solve 4

part2 :: ByteString -> Int
part2 = solve 14
