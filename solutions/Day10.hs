module Day10 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.List (intercalate, scanl')
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

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
part2 input = intercalate "\n" $ map renderRow $ take 6 $ chunksOf 40 spritePositions
  where
    renderRow :: [Int] -> [Char]
    renderRow rowSpritePositions =
      [ if abs (spritePosition - rowPixelIndex) <= 1
          then '█'
          else ' '
        | (rowPixelIndex, spritePosition) <- zip [0 ..] rowSpritePositions
      ]

    spritePositions :: [Int]
    spritePositions =
      scanl' runInstruction 1 $
        concatMap readInputLine $
          BS.lines input
