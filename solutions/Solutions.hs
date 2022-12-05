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
import           Control.DeepSeq
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString (ByteString)

--- Day 1 ----------------------------------------------------------------------

-- >>> sum_just_runs [Just 1, Just 2, Just 3, Nothing, Just 1, Just 2]
-- [6, 3]
sum_just_runs :: [Maybe Int] -> [Int]
sum_just_runs = foldl' (\acc mNum -> case mNum of
                          Nothing  -> 0 : acc
                          Just num -> (num + headDef 0 acc) : tailSafe acc) []

day1_part1 :: ByteString -> Int
day1_part1 =
  maximum
  . sum_just_runs
  . map (fmap fst . BS.readInt)
  . BS.lines

day1_part2 :: ByteString -> Int
day1_part2 =
  sum
  . take 3
  . reverse
  . sort
  . sum_just_runs
  . map (fmap fst . BS.readInt)
  . BS.lines

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

pointsFromLine_part1 :: ByteString -> Int
pointsFromLine_part1 (BS.unpack -> [opponentChar, ' ', yourChar]) = pointsFromGame opponentMove yourMove
  where
    opponentMove = fromMoveChar opponentChar
    yourMove = case yourChar of
      'X' -> Rock
      'Y' -> Paper
      'Z' -> Scissors

day2_part1 :: ByteString -> Int
day2_part1 = sum . map pointsFromLine_part1 . BS.lines

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

day2_part2 :: ByteString -> Int
day2_part2 = sum . map pointsFromLine_part2 . BS.lines

--- Day 3 ----------------------------------------------------------------------

split2 :: ByteString -> (ByteString, ByteString)
split2 s = BS.splitAt (BS.length s `div` 2) s

priority :: Char -> Int
priority c = if
  | 'a' <= c && c <= 'z' -> ord c - ord 'a' + 1
  | 'A' <= c && c <= 'Z' -> ord c - ord 'A' + 27

day3_part1 :: ByteString -> Int
day3_part1 = sum
  . concatMap (map priority)
  . map nub
  . map (uncurry intersect)
  . map (\(a, b) -> (BS.unpack a, BS.unpack b))
  . map split2
  . BS.lines

day3_part2 :: ByteString -> Int
day3_part2 = sum
  . map priority
  . map (\[badge] -> badge)
  . map nub
  . map (\[a, b, c] -> intersect a (intersect b c))
  . map (map BS.unpack)
  . chunksOf 3
  . BS.lines

--- Day 4 ----------------------------------------------------------------------

readSectionAssignment :: ByteString -> ((Int, Int), (Int, Int))
readSectionAssignment s =
  let Just (a, s1) = BS.readInt s
      s2 = BS.drop 1 s1
      Just (b, s3) = BS.readInt s2
      s4 = BS.drop 1 s3
      Just (c, s5) = BS.readInt s4
      s6 = BS.drop 1 s5
      Just (d, s7) = BS.readInt s6
   in ((a, b), (c, d))

day4_part1 :: ByteString -> Int
day4_part1 = length
  . filter (\((a, b), (c, d)) -> (a <= c && d <= b) || (c <= a && b <= d))
  . map readSectionAssignment
  . BS.lines

day4_part2 :: ByteString -> Int
day4_part2 = length
  . filter (\((a, b), (c, d)) -> not
             $ (a < c && b < c)
             || (a > d && b > d)
             || (c < a && d < a)
             || (c > b && d > b))
  . map readSectionAssignment
  . BS.lines

--- Infrastructure -------------------------------------------------------------

data Solution where
  MkSolution :: (NFData a, Show a) => String -> (ByteString -> a) -> FilePath -> Solution

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
