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
    [ n * registerValues !! n
      | n <- [20, 60, 100, 140, 180, 220]
    ]
  where
    registerValues =
      (1 :) $
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
        [ [1 .. 40],
          [41 .. 80],
          [81 .. 120],
          [121 .. 160],
          [161 .. 200],
          [201 .. 240]
        ]

    spritePositions :: [Int]
    spritePositions =
      (1 :) $
        scanl' runInstruction 1 $
          concatMap readInputLine $
            BS.lines input
