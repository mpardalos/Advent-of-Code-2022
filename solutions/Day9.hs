module Day9 (part1, part2) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Int (Int32)
import Data.IntSet qualified as IntSet
import Data.List (scanl')
import Data.Maybe (fromJust)

data Position = P {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int32 deriving (Eq, Ord, Show)

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

countUniquePositions :: [Position] -> Int
countUniquePositions =
  IntSet.size
    . IntSet.fromList
    -- We combine a position into a single integer. This allows us to use IntSet
    -- for counting unique positions (which is much faster than Set).
    -- To do this, we assume that each component of the position only needs 16
    -- bits, and so we pack the two componenents into a single 32 bit int
    . map (\(P x y) -> fromIntegral $ shiftL x 16 .|. (y .&. 0xFFFF))

part1 :: ByteString -> Int
part1 =
  countUniquePositions
    . scanl1' tailFollow
    . scanl' (flip ($)) (P 0 0)
    . readInput

part2 :: ByteString -> Int
part2 =
  countUniquePositions
    . scanl1' tailFollow
    . scanl1' tailFollow
    . scanl1' tailFollow
    . scanl1' tailFollow
    . scanl1' tailFollow
    . scanl1' tailFollow
    . scanl1' tailFollow
    . scanl1' tailFollow
    . scanl1' tailFollow
    . scanl' (flip ($)) (P 0 0)
    . readInput

scanl1' :: (a -> a -> a) -> [a] -> [a]
scanl1' f (x : xs) = scanl' f x xs
scanl1' _ [] = []
{-# INLINE scanl1' #-}

sign :: (Ord a, Num a, Num b) => a -> b
sign x
  | x < 0 = -1
  | x > 0 = 1
  | otherwise = 0
{-# INLINE sign #-}
