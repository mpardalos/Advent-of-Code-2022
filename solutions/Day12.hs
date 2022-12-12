{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day12 (part1, part2) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (chr, ord)
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (traceShow, traceShowId)
import Optics (Field1 (..), Ixed (ix), over, (%), (%~), (.~))
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
findDistances :: G.Coordinates -> Int -> Grid (Maybe Int, Position) -> Grid (Maybe Int, Position)
findDistances c distanceCovered g
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
      | shouldContinueTo c' = findDistances c' (distanceCovered + 1)
      | otherwise = id

    shouldUpdate = case mBestDistance of
      Just bestDistance -> distanceCovered < bestDistance
      Nothing -> True

    shouldContinueTo c' = case g G.!? c' of
      Just (_, nextPos) -> height nextPos <= height currentPos + 1
      _ -> False

part1 :: ByteString -> Int
part1 input =
  fromJustNote "End not reached" $
    fst $
      fromJustNote
        "End is out of bounds"
        (finalGrid G.!? endCoords)
  where
    startCoords = findStart inputGrid
    endCoords = findEnd inputGrid

    inputGrid = readInput input
    startGrid = fmap (Nothing,) inputGrid
    finalGrid = findDistances startCoords 0 startGrid

part2 :: ByteString -> Int
part2 = const 0
