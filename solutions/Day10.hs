module Day10 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.List (scanl')
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

registerValuesFromInput :: ByteString -> [Int]
registerValuesFromInput =
  scanl' runInstruction 1
    . concatMap readInputLine
    . BS.lines

part1 :: ByteString -> Int
part1 input =
  sum
    [ n * registerValuesFromInput input !! (n - 1)
      | n <- [20, 60, 100, 140, 180, 220]
    ]

renderRow :: [Int] -> [Char]
renderRow rowSpritePositions =
  [ if abs (spritePosition - pixelRowIndex) <= 1
      then '█'
      else ' '
    | (pixelRowIndex, spritePosition) <- zip [0 ..] rowSpritePositions
  ]

part2 :: ByteString -> String
part2 =
  unlines
    . map renderRow
    . take 6
    . chunksOf 40
    . registerValuesFromInput
