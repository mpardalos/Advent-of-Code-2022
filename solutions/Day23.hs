{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Day23 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.List (findIndex, inits, iterate')
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace
import Safe (headDef)
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
rotate 0 (x : xs) = (x : xs)
rotate n (x : xs) = rotate (n - 1) (xs ++ [x])
rotate _ [] = []

step :: Int -> Set Coordinates -> Set Coordinates
-- step n grid | trace ("---\nn=" ++ show n ++ "\n" ++ showGrid grid) False = undefined
step n grid =
  Set.map
    ( \(from, to) ->
        if Set.size (Set.filter ((to ==) . snd) targets) <= 1
          then to
          else from
    )
    targets
  where
    neighbours (row, col) =
      [ (row + dr, col + dc)
        | dr <- [-1, 0, 1],
          dc <- [-1, 0, 1],
          not (dr == 0 && dc == 0)
      ]

    -- Add the target coordinates onto each elf
    targets :: Set (Coordinates, Coordinates)
    targets =
      Set.map
        ( \(row, col) ->
            if not $ any (`Set.member` grid) (neighbours (row, col))
              then ((row, col), (row, col))
              else
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

iterateIndexed :: (Int -> a -> a) -> a -> [a]
iterateIndexed f initX =
  map snd
    . iterate' (\(i, !x) -> (i + 1, f i x))
    $ (0, initX)

part1 :: ByteString -> Int
part1 = countEmpty . (!! 10) . iterateIndexed step . readInput

part2 :: ByteString -> Int
part2 = fromJust . findFirstUnchanged . iterateIndexed step . readInput

findFirstUnchanged :: [Set Coordinates] -> Maybe Int
findFirstUnchanged = fmap (+ 1) . findFirstTwoIdentical . zip [0 ..]
  where
    findFirstTwoIdentical ((_, x1) : (i, x2) : xs)
      | x1 == x2 = Just i
      | otherwise = trace ("recurse " ++ show i) $ findFirstTwoIdentical ((i, x2) : xs)
    findFirstTwoIdentical _ = Nothing

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
