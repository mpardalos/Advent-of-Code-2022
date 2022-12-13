{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day12 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (chr, ord)
import Data.Function ((&))
import Data.Vector qualified as V
import Optics (Field1 (..), Ixed (ix), (%), (.~))
import Safe (fromJustNote)
import Util.Grid (Grid)
import Util.Grid qualified as G

data Position
  = Start
  | End
  | Height Int
  deriving (Eq)

instance Show Position where
  show Start = "S"
  show End = "E"
  show (Height h) = [chr h]

height :: Position -> Int
height Start = ord 'a'
height End = ord 'z'
height (Height h) = h

positionFromChar :: Char -> Position
positionFromChar 'S' = Start
positionFromChar 'E' = End
positionFromChar c = Height (ord c)

readInput :: ByteString -> Grid Position
readInput =
  G.fromListOfLists
    . map (map positionFromChar)
    . map BS.unpack
    . BS.lines

findStart :: Grid Position -> G.Coordinates
findStart = fromJustNote "No start coordinates" . G.findCoordinates (== Start)

findEnd :: Grid Position -> G.Coordinates
findEnd = fromJustNote "No start coordinates" . G.findCoordinates (== End)

-- | Set the distances to every position on the grid from a starting position
findDistances :: (Int -> Int -> Bool) -> G.Coordinates -> Int -> Grid (Maybe Int, Position) -> Grid (Maybe Int, Position)
findDistances canReach c distanceCovered g
  | shouldUpdate =
      g
        & ix c % Optics._1 .~ Just distanceCovered
        & continue (G.west c)
        & continue (G.north c)
        & continue (G.east c)
        & continue (G.south c)
  | otherwise = g
  where
    (mBestDistance, currentPos) = g G.! c

    continue c'
      | shouldContinueTo c' = findDistances canReach c' (distanceCovered + 1)
      | otherwise = id

    shouldUpdate = case mBestDistance of
      Just bestDistance -> distanceCovered < bestDistance
      Nothing -> True

    shouldContinueTo c' = case g G.!? c' of
      Just (_, nextPos) -> canReach (height currentPos) (height nextPos)
      _ -> False

part1 :: ByteString -> Int
part1 input =
  inputGrid
    & fmap (Nothing,)
    & findDistances (\from to -> to <= from + 1) startCoords 0
    & (G.!? endCoords)
    & fromJustNote "End is out of bounds"
    & fst
    & fromJustNote "End not reached"
  where
    startCoords = findStart inputGrid
    endCoords = findEnd inputGrid
    inputGrid = readInput input

part2 :: ByteString -> Int
part2 input =
  inputGrid
    & fmap (Nothing,)
    & findDistances (\from to -> to >= from - 1) endCoords 0
    & G.contents
    & V.filter (\(_, pos) -> height pos == ord 'a')
    & V.mapMaybe fst
    & V.minimum
  where
    inputGrid = readInput input
    endCoords = findEnd inputGrid
