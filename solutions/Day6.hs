module Day6 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS

solve :: Int -> ByteString -> Int
solve requiredCount input = go BS.empty 0
  where
    go :: ByteString -> Int -> Int
    go currentRun idx
      | BS.length currentRun == requiredCount = idx
      | currentChar `BS.notElem` currentRun = go (BS.cons currentChar currentRun) (idx + 1)
      | otherwise = go (currentChar `BS.cons` BS.takeWhile (/= currentChar) currentRun) (idx + 1)
      where
        currentChar = BS.index input idx

part1 :: ByteString -> Int
part1 = solve 4

part2 :: ByteString -> Int
part2 = solve 14
