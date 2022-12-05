module Day2(part1, part2) where

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString (ByteString)
import Data.Foldable
import Data.List

data Move = Rock | Paper | Scissors
  deriving (Enum, Show)

fromMoveChar 'A' = Rock
fromMoveChar 'B' = Paper
fromMoveChar 'C' = Scissors

pointsFromGame
  :: Move -- ^ Their move
  -> Move -- ^ Your move
  -> Int -- ^ PointsFromGame to you
-- You lose
pointsFromGame Paper    Rock     = 1
pointsFromGame Scissors Paper    = 2
pointsFromGame Rock     Scissors = 3
-- You win
pointsFromGame Scissors Rock     = 7
pointsFromGame Rock     Paper    = 8
pointsFromGame Paper    Scissors = 9
-- Draw
pointsFromGame Rock     Rock     = 4
pointsFromGame Paper    Paper    = 5
pointsFromGame Scissors Scissors = 6

pointsFromLine_part1 :: ByteString -> Int
pointsFromLine_part1 (BS.unpack -> [opponentChar, ' ', yourChar]) = pointsFromGame opponentMove yourMove
  where
    opponentMove = fromMoveChar opponentChar
    yourMove = case yourChar of
      'X' -> Rock
      'Y' -> Paper
      'Z' -> Scissors

part1 :: ByteString -> Int
part1 = sum . map pointsFromLine_part1 . BS.lines

whatBeats :: Move -> Move
whatBeats Rock     = Paper
whatBeats Paper    = Scissors
whatBeats Scissors = Rock

whatLosesTo :: Move -> Move
whatLosesTo Paper    = Rock
whatLosesTo Scissors = Paper
whatLosesTo Rock     = Scissors

pointsFromLine_part2 :: ByteString -> Int
pointsFromLine_part2 (BS.unpack -> [opponentChar, ' ', resultChar]) = pointsFromGame opponentMove yourMove
  where
    opponentMove = fromMoveChar opponentChar
    yourMove = case resultChar of
      'X' -> whatLosesTo opponentMove
      'Y' -> opponentMove
      'Z' -> whatBeats opponentMove

part2 :: ByteString -> Int
part2 = sum . map pointsFromLine_part2 . BS.lines
