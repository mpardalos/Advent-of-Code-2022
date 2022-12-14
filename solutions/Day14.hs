{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day14 (part1, part2) where

import Control.Monad.ST
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (foldl', forM_)
import Data.Function ((&))
import Data.List.Split (divvy)
import Data.Massiv.Array (Ix2 ((:.)))
import Data.Massiv.Array qualified as M
import Data.Maybe (fromMaybe)
import Safe (fromJustNote)

type Grid a = M.Array M.U M.Ix2 a

type MGrid s a = M.MArray s M.U M.Ix2 a

type Coordinates = M.Ix2

readLine :: ByteString -> [Coordinates]
readLine line
  | BS.null line = []
  | otherwise =
      let (x, rest1) = fromJustNote "No x coordinate" $ BS.readInt line
          rest2 = BS.drop 1 rest1 -- Drop the comma
          (y, rest3) = fromJustNote "No y coordinate" $ BS.readInt rest2
          restFinal = BS.drop 4 rest3 -- Drop the arrow
          -- Flip because coordinates are in x,y but we need row,column
       in ((y :. x) : readLine restFinal)

makeLine :: [Coordinates] -> Grid Bool -> Grid Bool
makeLine !coords !initialGrid = M.withMArrayST_ initialGrid $ \grid -> do
  forM_ (divvy 2 1 coords) $ \[start, end] ->
    forM_ (indicesInLine start end) $ \idx ->
      M.write_ grid idx True

indicesInLine :: Coordinates -> Coordinates -> [Coordinates]
indicesInLine (row1 :. col1) (row2 :. col2)
  | row1 == row2 = [row1 :. col | col <- [min col1 col2 .. max col1 col2]]
  | col1 == col2 = [row :. col1 | row <- [min row1 row2 .. max row1 row2]]
  | otherwise = error "Diagonal line"

displayGrid :: Grid Bool -> String
displayGrid =
  M.ifoldSemi
    ( \(_ :. !col) !v ->
        if
            | col == 0 -> "\n" ++ bool "." "█" v
            | otherwise -> bool "." "█" v
    )
    ""

sandUntilSpill :: Int -> Grid Bool -> Int
sandUntilSpill entryColumn initialGrid = runST $ do
  mgrid <- M.thawS initialGrid
  go 0 mgrid
  where
    rollDownFrom :: forall s. MGrid s Bool -> Ix2 -> ST s Ix2
    rollDownFrom grid (row :. col)
      | not (M.isSafeIndex (M.sizeOfMArray grid) (row :. col)) =
          return (row :. col)
      | otherwise = do
          let down = (row + 1 :. col)
          let downLeft = (row + 1 :. col - 1)
          let downRight = (row + 1 :. col + 1)

          downOccupied <- fromMaybe False <$> M.read grid down
          downLeftOccupied <- fromMaybe False <$> M.read grid downLeft
          downRightOccupied <- fromMaybe False <$> M.read grid downRight

          if not downOccupied
            then rollDownFrom grid down
            else
              if not downLeftOccupied
                then rollDownFrom grid downLeft
                else
                  if not downRightOccupied
                    then rollDownFrom grid downRight
                    else return (row :. col)

    go :: forall s. Int -> MGrid s Bool -> ST s Int
    go counter grid = do
      -- landingIdx <- nextLanding grid
      stopIdx <- rollDownFrom grid (0 :. entryColumn)
      if M.isSafeIndex (M.sizeOfMArray grid) stopIdx
        then do
          M.write_ grid stopIdx True
          go (counter + 1) grid
        else return counter

part1 :: ByteString -> Int
part1 input =
  instructions
    & map (map (\(row :. col) -> (row :. col - minColumn)))
    & foldl' (flip makeLine) (M.replicate M.Par (M.Sz (rows :. columns)) False)
    & sandUntilSpill (500 - minColumn)
  where
    instructions = map readLine (BS.lines input)
    columns = maxColumn - minColumn + 1
    rows = maxRow + 1

    -- Add 1 column of margin so we can allow sand to fall off the edges
    minColumn = minimum [col | (_ :. col) <- concat instructions] - 1
    maxColumn = maximum [col | (_ :. col) <- concat instructions] + 1
    maxRow = maximum [row | (row :. _) <- concat instructions] + 1

part2 :: ByteString -> Int
part2 = const 0
