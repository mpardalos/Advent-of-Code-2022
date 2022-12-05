module Day1(part1, part2) where

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString (ByteString)
import Data.Foldable
import Data.List
import Safe

-- >>> sum_just_runs [Just 1, Just 2, Just 3, Nothing, Just 1, Just 2]
-- [6, 3]
sum_just_runs :: [Maybe Int] -> [Int]
sum_just_runs = foldl' (\acc mNum -> case mNum of
                          Nothing  -> 0 : acc
                          Just num -> (num + headDef 0 acc) : tailSafe acc) []

part1 :: ByteString -> Int
part1 =
  maximum
  . sum_just_runs
  . map (fmap fst . BS.readInt)
  . BS.lines

part2 :: ByteString -> Int
part2 =
  sum
  . take 3
  . reverse
  . sort
  . sum_just_runs
  . map (fmap fst . BS.readInt)
  . BS.lines
