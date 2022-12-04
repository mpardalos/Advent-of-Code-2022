{-# OPTIONS -fdefer-typed-holes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
module Solutions(Solution(..),solutions) where
import           Data.Foldable (foldl')
import           Data.List     (sort, intersect, nub)
import           Safe
import           Text.Read     (readMaybe)
import           Data.Char     (ord)
import           Data.List.Split
import           Debug.Trace
import           System.Environment
import           Control.Monad

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

--- Day 3 ----------------------------------------------------------------------

split2 :: String -> (String, String)
split2 s = splitAt (length s `div` 2) s

priority :: Char -> Int
priority c = if
  | 'a' <= c && c <= 'z' -> ord c - ord 'a' + 1
  | 'A' <= c && c <= 'Z' -> ord c - ord 'A' + 27

day3_part1 :: String -> Int
day3_part1 = sum
  . concatMap (map priority)
  . map nub
  . map (uncurry intersect)
  . map split2
  . lines

day3_part2 :: String -> Int
day3_part2 = sum
  . map priority
  . map (\[badge] -> badge)
  . map nub
  . map (\[a, b, c] -> intersect a (intersect b c))
  . chunksOf 3
  . lines

--- Day 4 ----------------------------------------------------------------------

readSectionAssignment :: String -> ((Int, Int), (Int, Int))
readSectionAssignment (splitOn "," ->
                       [ splitOn "-" -> [a,b]
                       , splitOn "-" -> [c,d]
                       ]) = ((read a, read b), (read c, read d))

day4_part1 :: String -> Int
day4_part1 = length
  . filter (\((a, b), (c, d)) -> (a <= c && d <= b) || (c <= a && b <= d))
  . map readSectionAssignment
  . lines

day4_part2 :: String -> Int
day4_part2 = length
  . filter (\((a, b), (c, d)) -> not
             $ (a < c && b < c)
             || (a > d && b > d)
             || (c < a && d < a)
             || (c > b && d > b))
  . map readSectionAssignment
  . lines

--- Infrastructure -------------------------------------------------------------

data Solution where
  MkSolution :: Show a => String -> (String -> a) -> String -> Solution

solutions :: [Solution]
solutions =
  [ MkSolution "Day 1 part 1" day1_part1 "day1"
  , MkSolution "Day 1 part 2" day1_part2 "day1"
  , MkSolution "Day 2 part 1" day2_part1 "day2"
  , MkSolution "Day 2 part 2" day2_part2 "day2"
  , MkSolution "Day 3 part 1" day3_part1 "day3"
  , MkSolution "Day 3 part 2" day3_part2 "day3"
  , MkSolution "Day 4 part 1" day4_part1 "day4"
  , MkSolution "Day 4 part 2" day4_part2 "day4"
  ]
