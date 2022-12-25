{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -fprof-auto #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day23 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.List (iterate', unfoldr)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Safe (headMay)
import Text.Printf (printf)

type Coordinates = (Int, Int)

readInput :: ByteString -> Set Coordinates
readInput =
  snd
    . BS.foldl'
      ( \((row, col), grid) c -> case c of
          '.' -> ((row, col + 1), grid)
          '#' -> ((row, col + 1), Set.insert (row, col) grid)
          '\n' -> ((row + 1, 0), grid)
          _ -> error ("Invalid input char: " ++ [c])
      )
      ((0, 0), Set.empty)

rotate :: Int -> [a] -> [a]
rotate 0 [a, b, c, d] = [a, b, c, d]
rotate 1 [a, b, c, d] = [b, c, d, a]
rotate 2 [a, b, c, d] = [c, d, a, b]
rotate 3 [a, b, c, d] = [d, a, b, c]
rotate n [a, b, c, d] = rotate (n `mod` 4) [a, b, c, d]
rotate _ _ = undefined
{-# INLINE rotate #-}

-- | Moves that the elves will make for one step of the process. Given as a map of (to_position -> from_position)
getMoves :: Int -> Set Coordinates -> Map Coordinates Coordinates
getMoves n grid =
  foldr
    -- If the alter finds a Just in the Map, then it means there is another elf
    -- moving to the same position as this, and we can remove both.
    (\(from, to) !acc -> Map.alter (\case Nothing -> Just from; Just _ -> Nothing) to acc)
    Map.empty
    proposedMoves
  where
    neighbours (row, col) =
      [ (row + dr, col + dc)
        | dr <- [-1, 0, 1],
          dc <- [-1, 0, 1],
          not (dr == 0 && dc == 0)
      ]

    northNeighbours (row, col) = [(row - 1, col + dcol) | dcol <- [-1 .. 1]]
    southNeighbours (row, col) = [(row + 1, col + dcol) | dcol <- [-1 .. 1]]
    westNeighbours (row, col) = [(row + drow, col - 1) | drow <- [-1 .. 1]]
    eastNeighbours (row, col) = [(row + drow, col + 1) | drow <- [-1 .. 1]]

    -- The positions that the elves suggest moving to, not accounting for conflicts with other elves
    proposedMoves :: [(Coordinates, Coordinates)]
    proposedMoves =
      mapMaybe
        ( \(row, col) ->
            if not $ any (`Set.member` grid) (neighbours (row, col))
              then Nothing
              else
                fmap ((row, col),) . headMay . map snd . filter fst . rotate n $
                  [ (not . any (`Set.member` grid) $ northNeighbours (row, col), (row - 1, col)),
                    (not . any (`Set.member` grid) $ southNeighbours (row, col), (row + 1, col)),
                    (not . any (`Set.member` grid) $ westNeighbours (row, col), (row, col - 1)),
                    (not . any (`Set.member` grid) $ eastNeighbours (row, col), (row, col + 1))
                  ]
        )
        $ Set.toList grid

applyMoves :: Map Coordinates Coordinates -> Set Coordinates -> Set Coordinates
applyMoves changes grid = (grid Set.\\ Set.fromList from) `Set.union` Set.fromList to
  where
    to = Map.keys changes
    from = Map.elems changes

iterateIndexed :: (Int -> a -> a) -> a -> [a]
iterateIndexed f initX =
  map snd
    . iterate' (\(i, !x) -> (i + 1, f i x))
    $ (0, initX)

part1 :: ByteString -> Int
part1 = countEmpty . (!! 10) . iterateIndexed (\i grid -> applyMoves (getMoves i grid) grid) . readInput

part2 :: ByteString -> Int
part2 =
  (+ 1)
    . length
    . unfoldr
      ( \(i, !grid) ->
          let moves = getMoves i grid
           in if null moves
                then Nothing
                else Just ((), (i + 1, applyMoves moves grid))
      )
    . (0,)
    . readInput

showGrid :: Set Coordinates -> String
showGrid grid =
  unlines
    ( ("    | " ++ concat [show (abs col) | col <- [westMost .. eastMost]])
        : replicate (eastMost - westMost + 7) '-'
        : [ printf "%3d | " row ++ [displayChar (row, col) | col <- [westMost .. eastMost]]
            | row <- [northMost .. southMost]
          ]
    )
  where
    ((northMost, westMost), (southMost, eastMost)) = boundingBox grid
    displayChar cs
      | cs `Set.member` grid = '#'
      | otherwise = '.'

boundingBox :: Set Coordinates -> (Coordinates, Coordinates)
boundingBox grid = ((northMost, westMost), (southMost, eastMost))
  where
    northMost = minimum $ Set.map fst grid
    southMost = maximum $ Set.map fst grid
    westMost = minimum $ Set.map snd grid
    eastMost = maximum $ Set.map snd grid

countEmpty :: Set Coordinates -> Int
countEmpty elves = boundingBoxArea - Set.size elves
  where
    boundingBoxArea = (southMost - northMost + 1) * (eastMost - westMost + 1)
    ((northMost, westMost), (southMost, eastMost)) = boundingBox elves
