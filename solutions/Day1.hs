module Day1 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (Foldable (foldl'))
import Data.List (sort)
import Safe (headDef, tailSafe)

-- >>> sum_just_runs [Just 1, Just 2, Just 3, Nothing, Just 1, Just 2]
-- [6, 3]
sumJustRuns :: [Maybe Int] -> [Int]
sumJustRuns =
  foldl'
    ( \acc mNum -> case mNum of
        Nothing -> 0 : acc
        Just num -> (num + headDef 0 acc) : tailSafe acc
    )
    []

part1 :: ByteString -> Int
part1 =
  maximum
    . sumJustRuns
    . map (fmap fst . BS.readInt)
    . BS.lines

part2 :: ByteString -> Int
part2 =
  sum
    . take 3
    . reverse
    . sort
    . sumJustRuns
    . map (fmap fst . BS.readInt)
    . BS.lines
