module Day2 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS

data Move = Rock | Paper | Scissors
  deriving (Enum, Show)

fromMoveChar :: Char -> Move
fromMoveChar 'A' = Rock
fromMoveChar 'B' = Paper
fromMoveChar 'C' = Scissors
fromMoveChar _ = undefined

pointsFromGame ::
  -- | Their move
  Move ->
  -- | Your move
  Move ->
  -- | PointsFromGame to you
  -- You lose
  Int
pointsFromGame Paper Rock = 1
pointsFromGame Scissors Paper = 2
pointsFromGame Rock Scissors = 3
-- You win
pointsFromGame Scissors Rock = 7
pointsFromGame Rock Paper = 8
pointsFromGame Paper Scissors = 9
-- Draw
pointsFromGame Rock Rock = 4
pointsFromGame Paper Paper = 5
pointsFromGame Scissors Scissors = 6

pointsFromLine_part1 :: ByteString -> Int
pointsFromLine_part1 (BS.unpack -> [opponentChar, ' ', yourChar]) = pointsFromGame opponentMove yourMove
  where
    opponentMove = fromMoveChar opponentChar
    yourMove = case yourChar of
      'X' -> Rock
      'Y' -> Paper
      'Z' -> Scissors
      _ -> undefined
pointsFromLine_part1 _ = undefined

part1 :: ByteString -> Int
part1 = sum . map pointsFromLine_part1 . BS.lines

whatBeats :: Move -> Move
whatBeats Rock = Paper
whatBeats Paper = Scissors
whatBeats Scissors = Rock

whatLosesTo :: Move -> Move
whatLosesTo Paper = Rock
whatLosesTo Scissors = Paper
whatLosesTo Rock = Scissors

pointsFromLine_part2 :: ByteString -> Int
pointsFromLine_part2 (BS.unpack -> [opponentChar, ' ', resultChar]) = pointsFromGame opponentMove yourMove
  where
    opponentMove = fromMoveChar opponentChar
    yourMove = case resultChar of
      'X' -> whatLosesTo opponentMove
      'Y' -> opponentMove
      'Z' -> whatBeats opponentMove
      _ -> undefined
pointsFromLine_part2 _ = undefined

part2 :: ByteString -> Int
part2 = sum . map pointsFromLine_part2 . BS.lines
