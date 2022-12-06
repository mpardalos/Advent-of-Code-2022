{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day4 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS

readSectionAssignment :: ByteString -> ((Int, Int), (Int, Int))
readSectionAssignment s =
  let Just (a, s1) = BS.readInt s
      s2 = BS.drop 1 s1
      Just (b, s3) = BS.readInt s2
      s4 = BS.drop 1 s3
      Just (c, s5) = BS.readInt s4
      s6 = BS.drop 1 s5
      Just (d, _s7) = BS.readInt s6
   in ((a, b), (c, d))

part1 :: ByteString -> Int
part1 =
  length
    . filter (\((a, b), (c, d)) -> (a <= c && d <= b) || (c <= a && b <= d))
    . map readSectionAssignment
    . BS.lines

part2 :: ByteString -> Int
part2 =
  length
    . filter
      ( \((a, b), (c, d)) ->
          not $
            (a < c && b < c)
              || (a > d && b > d)
              || (c < a && d < a)
              || (c > b && d > b)
      )
    . map readSectionAssignment
    . BS.lines
