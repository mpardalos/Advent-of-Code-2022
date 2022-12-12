{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Util.Grid where

import Data.List (intercalate)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Optics (sliced)
import Optics (Index, IxValue, Ixed (..), Lens', atraversal, lens, over, set, view, views, (%))
import Optics.TH (makeFieldLabelsNoPrefix)
import Text.Printf (printf)

-- | A 2D grid, stored as a continuous, row-major vector
data Grid a = Grid
  { contents :: Vector a,
    columns :: Int
  }
  deriving (Functor)

instance Show a => Show (Grid a) where
  show g = go elements
    where
      shownItems = V.map show g.contents
      elementWidth = V.maximum $ V.map length shownItems
      elements = V.map (printf "%*s" elementWidth) shownItems

      go v =
        let (thisRow, rest) = V.splitAt g.columns v
         in intercalate " | " (V.toList thisRow) ++ if V.length rest > 0 then "\n" ++ go rest else ""

makeFieldLabelsNoPrefix ''Grid

-- | Coordinates in a grid. (Row, Column)
type Coordinates = (Int, Int)

east :: Coordinates -> Coordinates
east (r, c) = (r, c + 1)

west :: Coordinates -> Coordinates
west (r, c) = (r, c - 1)

south :: Coordinates -> Coordinates
south (r, c) = (r + 1, c)

north :: Coordinates -> Coordinates
north (r, c) = (r - 1, c)

-- | Neighbouring coordinates, not including diagonals
neighboursAxisAligned :: Coordinates -> [Coordinates]
neighboursAxisAligned c = [east c, north c, west c, south c]

empty :: Grid a
empty = Grid {contents = V.empty, columns = 0}

fromListOfLists :: [[a]] -> Grid a
fromListOfLists [] = empty
fromListOfLists (r : rs) =
  Grid
    { contents = V.fromList (concat (r : rs)),
      columns = length r
    }

-- | Convert an index into a grid's vector to its corresponding coordinates
indexToCoordinates :: Grid a -> Int -> Maybe Coordinates
indexToCoordinates g idx
  | idx < V.length g.contents = Just $ indexToCoordinatesUnchecked g idx
  | otherwise = Nothing

-- | Convert an index into a grid's vector to its corresponding coordinates
indexToCoordinatesUnchecked :: Grid a -> Int -> Coordinates
indexToCoordinatesUnchecked g idx = (idx `div` g.columns, idx `mod` g.columns)

-- | Convert coordinates in a grid to an index into its contents vector
coordinatesToIndex :: Grid a -> Coordinates -> Maybe Int
coordinatesToIndex g (r, c)
  | coordinatesInGrid (r, c) g = Just $ r * g.columns + c
  | otherwise = Nothing

coordinatesToIndexUnchecked :: Grid a -> Coordinates -> Int
coordinatesToIndexUnchecked g (r, c) = r * g.columns + c

coordinatesInGrid :: Coordinates -> Grid a -> Bool
coordinatesInGrid (r, c) g = 0 <= r && r < rows && 0 <= c && c < g.columns
  where
    rows = V.length g.contents `div` g.columns

(!?) :: Grid a -> Coordinates -> Maybe a
g !? (r, c) = (g.contents V.!?) =<< coordinatesToIndex g (r, c)

(!) :: Grid a -> Coordinates -> a
g ! (r, c) = g.contents V.! coordinatesToIndexUnchecked g (r, c)

findCoordinates :: (a -> Bool) -> Grid a -> Maybe Coordinates
findCoordinates p g = indexToCoordinatesUnchecked g <$> V.findIndex p g.contents

type instance Index (Grid a) = Coordinates

type instance IxValue (Grid a) = a

instance Ixed (Grid a) where
  ix cs =
    atraversal
      ( \g -> case g !? cs of
          Just v -> Right v
          Nothing -> Left g
      )
      ( \g v -> case coordinatesToIndex g cs of
          Just idx -> set (#contents % ix idx) v g
          Nothing -> g
      )

row :: Int -> Lens' (Grid a) (Vector a)
row n =
  lens
    (\grid@Grid {columns} -> view (#contents % sliced (columns * n) columns) grid)
    (\grid@Grid {columns} newRow -> set (#contents % sliced (columns * n) columns) newRow grid)

column :: Int -> Lens' (Grid a) (Vector a)
column n =
  lens
    ( \grid@Grid {columns} ->
        let rows = views #contents V.length grid `div` columns
            indices = V.enumFromStepN n columns rows
         in V.map (grid.contents V.!) indices
    )
    ( \grid@Grid {columns} newColumn ->
        let rows = views #contents V.length grid `div` columns
            indices = V.enumFromStepN n columns rows
         in over #contents (`V.update` V.zip indices newColumn) grid
    )
