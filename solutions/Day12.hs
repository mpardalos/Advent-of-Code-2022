{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day12 (part1, part2) where

import Control.DeepSeq (NFData)
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
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
  | Height {-# UNPACK #-} !Int
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
findDistancesST :: (Int -> Int -> Bool) -> Coordinates -> Int -> Grid Tile -> MGrid s Int -> ST s ()
findDistancesST canReach c !distanceCovered heights distances = do
  bestDistance <- M.readM distances c

  let shouldUpdate = bestDistance < 0 || distanceCovered < bestDistance
  when shouldUpdate $ do
    M.write_ distances c distanceCovered
    forM_ [c + (0 :. -1), c + (-1 :. 0), c + (0 :. 1), c + (1 :. 0)] $ \c' -> do
      case heights M.!? c' of
        Nothing -> pure ()
        Just !nextHeight -> do
          let currentHeight = heights M.! c
          if canReach (height currentHeight) (height nextHeight)
            then findDistancesST canReach c' (distanceCovered + 1) heights distances
            else pure ()

findDistances :: (Int -> Int -> Bool) -> Coordinates -> Int -> Grid Tile -> Grid Int
findDistances canReach c distanceCovered heights = runST $ do
  distances <- M.newMArray (M.size heights) (-1)
  findDistancesST canReach c distanceCovered heights distances
  M.freezeS distances

part1 :: ByteString -> Int
part1 input =
  inputGrid
    & M.compute
    & findDistances (\from to -> to <= from + 1) startCoords 0
    & (M.!? endCoords)
    & fromJustNote "End is out of bounds"
  where
    startCoords = findStart inputGrid
    endCoords = findEnd inputGrid
    inputGrid = readInput input

part2 :: ByteString -> Int
part2 input =
  inputGrid
    & M.compute
    & findDistances (\from to -> to >= from - 1) endCoords 0
    & M.zip inputGrid
    & M.flatten
    & M.sfilter (\(tile, _) -> height tile == ord 'a')
    & M.smapMaybe (\(_, distance) -> if distance >= 0 then Just distance else Nothing)
    & M.sminimum'
  where
    inputGrid = readInput input
    endCoords = findEnd inputGrid
