{-# LANGUAGE LambdaCase #-}

module Day17 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Maybe (mapMaybe)

data JetDirection = PushLeft | PushRight
  deriving (Show)

parseInput :: ByteString -> [JetDirection]
parseInput input =
  BS.unpack input
    & mapMaybe
      ( \case
          '<' -> Just PushLeft
          '>' -> Just PushRight
          '\n' -> Nothing
          c -> error ("Invalid input: " <> show c)
      )

-- | The rock shapes are defined by their individual pieces, given relative to their bottom left.
-- Coordinates are (row, column), bottom-to-top, left-to-right
rockShapes :: [[(Int, Int)]]
rockShapes =
  [ -- ####
    [(0, 0), (0, 1), (0, 2), (0, 3)],
    -- .#.
    -- ###
    -- .#.
    [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)],
    -- ..#
    -- ..#
    -- ###
    [(0, 0), (0, 1), (0, 2), (1, 2), (2, 2)],
    -- #
    -- #
    -- #
    -- #
    [(0, 0), (1, 0), (2, 0), (3, 0)],
    -- ##
    -- ##
    [(0, 0), (0, 1), (1, 0), (1, 1)]
  ]

part1 :: ByteString -> Int
part1 = const 0

part2 :: ByteString -> Int
part2 = const 0
