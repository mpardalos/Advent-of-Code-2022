module Day3(part1, part2) where

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString (ByteString)
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Char     (ord)
import           Data.List.Split

split2 :: ByteString -> (ByteString, ByteString)
split2 s = BS.splitAt (BS.length s `div` 2) s

priority :: Int -> Int
priority c
  | ord 'a' <= c && c <= ord 'z' = c - ord 'a' + 1
  | ord 'A' <= c && c <= ord 'Z' = c - ord 'A' + 27

intsetFromBytestring :: ByteString -> IntSet
intsetFromBytestring = BS.foldl' (\acc c -> IntSet.insert (ord c) acc) IntSet.empty

part1 :: ByteString -> Int
part1 = sum
  . map (IntSet.foldl' (\acc c -> acc + priority c) 0)
  . map (uncurry IntSet.intersection)
  . map (\(a, b) -> (intsetFromBytestring a, intsetFromBytestring b))
  . map split2
  . BS.lines

part2 :: ByteString -> Int
part2 = sum
  . map priority
  . map IntSet.findMin
  . map (foldr1 IntSet.intersection)
  . map (map intsetFromBytestring)
  . chunksOf 3
  . BS.lines
