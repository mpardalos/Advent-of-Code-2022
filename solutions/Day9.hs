module Day9 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.List (scanl')
import Data.Maybe (fromJust)
import Data.Set qualified as Set

data Position = P {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Eq, Ord, Show)

readInput :: ByteString -> [Position -> Position]
readInput =
  concatMap
    ( \line ->
        let count = fst $ fromJust $ BS.readInt (BS.drop 2 line)
         in replicate count $ case BS.head line of
              'L' -> \(P x y) -> P (x - 1) y
              'U' -> \(P x y) -> P x (y + 1)
              'R' -> \(P x y) -> P (x + 1) y
              'D' -> \(P x y) -> P x (y - 1)
              _ -> error "Invalid input"
    )
    . BS.lines

tailFollow :: Position -> Position -> Position
tailFollow (P tailX tailY) (P headX headY) =
  let xdiff = headX - tailX
      ydiff = headY - tailY
   in if abs xdiff <= 1 && abs ydiff <= 1
        then P tailX tailY
        else P (tailX + sign xdiff) (tailY + sign ydiff)

part1 :: ByteString -> Int
part1 =
  Set.size
    . Set.fromList
    . scanl1 tailFollow
    . scanl' (flip ($)) (P 0 0)
    . readInput

part2 :: ByteString -> Int
part2 =
  Set.size
    . Set.fromList
    . (!! 9)
    . iterate (scanl1 tailFollow)
    . scanl' (flip ($)) (P 0 0)
    . readInput

sign :: Int -> Int
sign x
  | x < 0 = -1
  | x > 0 = 1
  | otherwise = 0
