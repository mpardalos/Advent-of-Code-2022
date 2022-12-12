{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day12 (part1, part2) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (chr, ord)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (traceShow, traceShowId)
import Optics (Ixed (ix), over)
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

-- | Set the distances for reaching a position in the grid from the start
findDistances :: G.Coordinates -> Int -> Grid (Maybe Int, Position) -> Grid (Maybe Int, Position)
findDistances c distanceCovered g = case g G.!? c of
  Just (Nothing, currentPos) -> proceed currentPos g
  Just (Just bestDistance, currentPos)
    | bestDistance <= distanceCovered -> g
    | otherwise -> proceed currentPos g
  where
    proceed currentPos =
      (if shouldProceed (G.west c) then findDistances (G.west c) (distanceCovered + 1) else id)
        . (if shouldProceed (G.north c) then findDistances (G.north c) (distanceCovered + 1) else id)
        . (if shouldProceed (G.east c) then findDistances (G.east c) (distanceCovered + 1) else id)
        . (if shouldProceed (G.south c) then findDistances (G.south c) (distanceCovered + 1) else id)
        -- Update this position
        . over
          (ix c)
          ( \(minDistance, p) ->
              (Just (maybe distanceCovered (min distanceCovered) minDistance), p)
          )
      where
        shouldProceed nextC = case g G.!? nextC of
          Just (_, nextPos) ->
            height nextPos <= height currentPos + 1
          _ -> False

part1 :: ByteString -> Int
part1 input =
  fromJustNote "End not reached" $
    fst $
      fromJustNote "End is out of bounds"
        (finalGrid G.!? endCoords)
  where
    startCoords = findStart inputGrid
    endCoords = findEnd inputGrid

    inputGrid = readInput input
    startGrid = fmap (Nothing,) inputGrid
    finalGrid = findDistances startCoords 0 startGrid

part2 :: ByteString -> Int
part2 = const 0
