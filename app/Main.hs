{-# OPTIONS -fdefer-typed-holes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Main where
import           Data.Foldable (foldl')
import           Data.List     (sort)
import           Safe
import           Text.Printf   (printf)
import           Text.Read     (readMaybe)

--- Day 1 ----------------------------------------------------------------------

-- >>> sum_just_runs [Just 1, Just 2, Just 3, Nothing, Just 1, Just 2]
-- [6, 3]
sum_just_runs :: [Maybe Int] -> [Int]
sum_just_runs = foldl' (\acc mNum -> case mNum of
                          Nothing  -> 0 : acc
                          Just num -> (num + headDef 0 acc) : tailSafe acc) []

day1_part1 :: String -> Int
day1_part1 =
  maximum
  . sum_just_runs
  . map (readMaybe @Int)
  . lines

day1_part2 :: String -> Int
day1_part2 =
  sum
  . take 3
  . reverse
  . sort
  . sum_just_runs
  . map (readMaybe @Int)
  . lines

--- Day 2 ----------------------------------------------------------------------

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

pointsFromLine_part1 :: String -> Int
pointsFromLine_part1 [opponentChar, ' ', yourChar] = pointsFromGame opponentMove yourMove
  where
    opponentMove = fromMoveChar opponentChar
    yourMove = case yourChar of
      'X' -> Rock
      'Y' -> Paper
      'Z' -> Scissors

day2_part1 :: String -> Int
day2_part1 = sum . map pointsFromLine_part1 . lines

whatBeats :: Move -> Move
whatBeats Rock     = Paper
whatBeats Paper    = Scissors
whatBeats Scissors = Rock

whatLosesTo :: Move -> Move
whatLosesTo Paper    = Rock
whatLosesTo Scissors = Paper
whatLosesTo Rock     = Scissors

pointsFromLine_part2 :: String -> Int
pointsFromLine_part2 [opponentChar, ' ', resultChar] = pointsFromGame opponentMove yourMove
  where
    opponentMove = fromMoveChar opponentChar
    yourMove = case resultChar of
      'X' -> whatLosesTo opponentMove
      'Y' -> opponentMove
      'Z' -> whatBeats opponentMove

day2_part2 :: String -> Int
day2_part2 = sum . map pointsFromLine_part2 . lines

--- Infrastructure -------------------------------------------------------------

run :: Show a => String -> (String -> a) -> String -> IO ()
run name solution file = do
  input <- readFile ("data/" <> file)
  printf "%s: %s\n" name (show $ solution input)

main :: IO ()
main = do
  run "Day 1 test" day1_part1 "day1_test"
  run "Day 1 part 1" day1_part1 "day1"
  run "Day 1 part 2" day1_part2 "day1"

  run "Day 2 part 1" day2_part1 "day2"
  run "Day 2 part 2" day2_part2 "day2"
