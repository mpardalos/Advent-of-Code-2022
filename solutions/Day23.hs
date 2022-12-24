{-# LANGUAGE TupleSections #-}

module Day23 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Set (Set)
import Data.Set qualified as Set
import Safe (headDef)

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
rotate 0 (x : xs) = (x : xs)
rotate n (x : xs) = rotate (n - 1) (xs ++ [x])
rotate _ [] = []

step :: Int -> Set Coordinates -> Set Coordinates
step n grid =
  Set.map
    ( \(from, to) ->
        if Set.size (Set.filter ((to ==) . snd) targets) == 1
          then to
          else from
    )
    targets
  where
    -- Add the target coordinates onto each elf
    targets :: Set (Coordinates, Coordinates)
    targets =
      Set.map
        ( \(row, col) ->
            ((row, col),)
              . headDef (row, col)
              . map snd
              . filter fst
              . rotate n
              $ [ ( not (Set.member (row - 1, col - 1) grid || Set.member (row - 1, col) grid || Set.member (row - 1, col + 1) grid),
                    (row - 1, col)
                  ),
                  ( not (Set.member (row + 1, col - 1) grid || Set.member (row + 1, col) grid || Set.member (row + 1, col + 1) grid),
                    (row + 1, col)
                  ),
                  ( not (Set.member (row - 1, col - 1) grid || Set.member (row, col - 1) grid || Set.member (row + 1, col - 1) grid),
                    (row, col - 1)
                  ),
                  ( not (Set.member (row - 1, col + 1) grid || Set.member (row, col + 1) grid || Set.member (row + 1, col + 1) grid),
                    (row, col + 1)
                  )
                ]
        )
        grid

iterateStep :: Int -> Set Coordinates -> Set Coordinates
iterateStep n = go 0
  where
    go i grid
      | i >= n = grid
      | otherwise = step (i `mod` 4) (go (i + 1) grid)

countEmpty :: Set Coordinates -> Int
countEmpty elves = boundingBoxArea - Set.size elves
  where
    boundingBoxArea = (southMost - northMost) * (eastMost - westMost)
    northMost = minimum $ Set.map fst elves
    southMost = maximum $ Set.map fst elves
    westMost = minimum $ Set.map snd elves
    eastMost = maximum $ Set.map snd elves

part1 :: ByteString -> Int
part1 = Set.size . iterateStep 10 . readInput

part2 :: ByteString -> Int
part2 = const 0
