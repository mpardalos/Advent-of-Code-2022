{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day13 (part1, part2) where

import Data.Attoparsec.ByteString.Char8 (Parser, char, choice, decimal, parseOnly, sepBy)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.List (findIndex, intercalate, sort)
import Data.List.Split (chunksOf)
import Safe (fromJustNote)

data List = Item Int | List [List]
  deriving (Eq)

instance Show List where
  show (Item n) = show n
  show (List items) = "[" <> intercalate "," (map show items) <> "]"

instance Ord List where
  compare (List []) (List []) = EQ
  compare (List []) (List (_ : _)) = LT
  compare (List (_ : _)) (List []) = GT
  compare (List (x : xs)) (List (y : ys)) =
    case compare x y of
      LT -> LT
      EQ -> compare xs ys
      GT -> GT
  compare (Item x) (Item y) = compare x y
  compare (Item x) (List ys) = compare (List [Item x]) (List ys)
  compare (List xs) (Item y) = compare (List xs) (List [Item y])

readInput :: ByteString -> [List]
readInput = map parseLine . filter (not . BS.null) . BS.lines

parseLine :: ByteString -> List
parseLine = (\case Right l -> l; Left e -> error ("Cannot parse: " <> e)) . parseOnly listP
  where
    listP :: Parser List
    listP =
      choice
        [ Item <$> decimal,
          List <$> (char '[' *> listP `sepBy` char ',' <* char ']')
        ]

part1 :: ByteString -> Int
part1 input =
  readInput input
    & chunksOf 2
    & zip [1 ..]
    & filter (\(_, [l1, l2]) -> l1 <= l2)
    & map fst
    & sum

part2 :: ByteString -> Int
part2 input = decoderIndex1 * decoderIndex2
  where
    sortedPackets = sort $ [dividerPacket1, dividerPacket2] ++ readInput input
    dividerPacket1 = List [List [Item 2]]
    dividerPacket2 = List [List [Item 6]]

    decoderIndex1 = (fromJustNote "No [[2]] packet" $ findIndex (== dividerPacket1) sortedPackets) + 1
    decoderIndex2 = (fromJustNote "No [[6]] packet" $ findIndex (== dividerPacket2) sortedPackets) + 1
