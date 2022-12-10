module Day10 (part1, part2) where

import Control.DeepSeq (force)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.List (intercalate, scanl')
import Data.Maybe (fromJust)
import Debug.Trace
import Text.Printf (printf)

type Instruction = Int -> Int

runInstruction :: Int -> Instruction -> Int
runInstruction = flip ($)

readInputLine :: ByteString -> [Int -> Int]
readInputLine line = case BS.head line of
  'a' -> [id, (+ (fst $ fromJust $ BS.readInt $ BS.drop 5 line))]
  'n' -> [id]
  _ -> error ("Invalid input line: " <> BS.unpack line)

part1 :: ByteString -> Int
part1 input =
  sum $
    [ n * registerValues !! (n - 1)
      | n <- [20, 60, 100, 140, 180, 220]
    ]
  where
    registerValues =
      scanl' runInstruction 1 $
        concatMap readInputLine $
          BS.lines input

part2 :: ByteString -> String
part2 input = intercalate "\n" $ map (map charForPixel) pixelCycles
  where
    charForPixel :: (Int, Int) -> Char
    charForPixel (horizontalIndex, drawingCycle) =
      if abs ((spritePositions !! drawingCycle) - horizontalIndex) <= 1
        then '#'
        else ' '

    -- What cycle each pixel is drawn at
    pixelCycles :: [[(Int, Int)]]
    pixelCycles =
      map
        (zip [0 .. 39])
        [ [0 .. 39],
          [40 .. 79],
          [80 .. 119],
          [120 .. 159],
          [160 .. 199],
          [200 .. 239]
        ]

    spritePositions :: [Int]
    spritePositions =
      scanl' runInstruction 1 $
        concatMap readInputLine $
          BS.lines input
