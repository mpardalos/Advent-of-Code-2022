{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day12 (part1, part2) where

import Control.DeepSeq (NFData)
import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (chr, ord)
import Data.Function ((&))
import Data.Massiv.Array (Ix2 ((:.)))
import Data.Massiv.Array qualified as M
import GHC.Generics (Generic)
import Safe (fromJustNote)

type Grid a = M.Array M.BN M.Ix2 a

type MGrid s a = M.MArray s M.BN M.Ix2 a

type Coordinates = M.Ix2

data Tile
  = Start
  | End
  | Height Int
  deriving (Eq, Generic, NFData)

instance Show Tile where
  show Start = "S"
  show End = "E"
  show (Height h) = [chr h]

height :: Tile -> Int
height Start = ord 'a'
height End = ord 'z'
height (Height h) = h

positionFromChar :: Char -> Tile
positionFromChar 'S' = Start
positionFromChar 'E' = End
positionFromChar c = Height (ord c)

readInput :: ByteString -> Grid Tile
readInput =
  M.fromLists' M.Seq
    . map (map positionFromChar)
    . map BS.unpack
    . BS.lines

findStart :: Grid Tile -> Coordinates
findStart = fromJustNote "No start coordinates" . M.findIndex (== Start)

findEnd :: Grid Tile -> Coordinates
findEnd = fromJustNote "No start coordinates" . M.findIndex (== End)

-- | Set the distances to every position on the grid from a starting position
findDistancesST :: (Int -> Int -> Bool) -> Coordinates -> Int -> MGrid s (Maybe Int, Tile) -> ST s ()
findDistancesST canReach c distanceCovered g = do
  (mBestDistance, currentPos) <- M.readM g c

  let shouldUpdate = case mBestDistance of
        (Just bestDistance) -> distanceCovered < bestDistance
        Nothing -> True

  when shouldUpdate $ do
    M.modifyM_ g (\(_, p) -> pure (Just distanceCovered, p)) c
    forM_ [c + (0 :. -1), c + (-1 :. 0), c + (0 :. 1), c + (1 :. 0)] $ \c' -> do
      M.read g c' >>= \case
        Nothing -> pure ()
        Just (_, nextPos) ->
          when (canReach (height currentPos) (height nextPos)) $
            findDistancesST canReach c' (distanceCovered + 1) g

findDistances :: (Int -> Int -> Bool) -> Coordinates -> Int -> Grid (Maybe Int, Tile) -> Grid (Maybe Int, Tile)
findDistances canReach c distanceCovered g = M.withMArrayST_ g (findDistancesST canReach c distanceCovered)

part1 :: ByteString -> Int
part1 input =
  inputGrid
    & M.map (Nothing,)
    & M.compute
    & findDistances (\from to -> to <= from + 1) startCoords 0
    & (M.!? endCoords)
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
    & M.map (Nothing,)
    & M.compute
    & findDistances (\from to -> to >= from - 1) endCoords 0
    & M.flatten
    & M.sfilter (\(_, pos) -> height pos == ord 'a')
    & M.smapMaybe fst
    & M.sminimum'
  where
    inputGrid = readInput input
    endCoords = findEnd inputGrid
