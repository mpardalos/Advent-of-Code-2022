module Day18 (part1, part2) where

import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Set (Set)
import Data.Set qualified as Set
import Util

type Coordinates = (Int, Int, Int)

parseLine :: ByteString -> Coordinates
parseLine = parseOrError $ do
  x <- P.decimal
  _ <- P.char ','
  y <- P.decimal
  _ <- P.char ','
  z <- P.decimal
  return (x, y, z)

neighbours :: Coordinates -> [Coordinates]
neighbours (x, y, z) =
  [ (x + 1, y, z)
  , (x - 1, y, z)
  , (x, y + 1, z)
  , (x, y - 1, z)
  , (x, y, z + 1)
  , (x, y, z - 1)
  ]

countExposedFaces :: Set Coordinates -> Int
countExposedFaces blocks = Set.foldl go 0 blocks
  where
    go acc coords = acc + (length $ filter (`Set.notMember` blocks) $ neighbours coords)

part1 :: ByteString -> Int
part1 input =
  BS.lines input
    & map parseLine
    & Set.fromList
    & countExposedFaces

part2 :: ByteString -> Int
part2 = const 0
