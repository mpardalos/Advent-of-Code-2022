{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Day5 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (foldl')
import Data.Vector (Vector)
import Data.Vector qualified as V

type State = Vector ByteString

imapN :: Int -> (a -> a) -> Vector a -> Vector a
imapN n f = V.imap (\i val -> if i == n then f val else val)

readStateLine :: State -> ByteString -> State
readStateLine = go 0
  where
    go :: Int -> State -> ByteString -> State
    go stN st line
      | BS.null line = st
      | otherwise =
          let Just (c, s1) = BS.uncons (BS.drop 1 line)
              rest = BS.drop 2 s1
           in go
                (stN + 1)
                ( if c /= ' '
                    then imapN stN (`BS.snoc` c) st
                    else st
                )
                rest

readState :: ByteString -> (State, ByteString)
readState fullInput = go (V.replicate numColumns BS.empty) fullInput
  where
    numColumns = BS.length (BS.takeWhile (/= '\n') fullInput) `div` 4 + 1

    go :: State -> ByteString -> (State, ByteString)
    go state input
      | input `BS.index` 1 == '1' = (state, input)
      | otherwise =
          let (line, rest) = BS.span (/= '\n') input
           in go (readStateLine state line) (BS.drop 1 rest)

data Move = Move
  { count :: Int,
    from :: Int,
    to :: Int
  }
  deriving (Show)

readMove :: ByteString -> Move
readMove line =
  let s1 = BS.drop 5 line
      Just (count, s2) = BS.readInt s1
      s3 = BS.drop 6 s2
      Just (fromRead, s4) = BS.readInt s3
      s5 = BS.drop 4 s4
      Just (toRead, _) = BS.readInt s5
   in Move {count, from = fromRead - 1, to = toRead - 1}

applyMove :: Bool -> State -> Move -> State
applyMove shouldReverse state Move {..} =
  let crates = BS.take count $ state V.! from
   in V.imap
        ( \i stk ->
            if
                | i == from -> BS.drop count stk
                | i == to && shouldReverse -> BS.reverse crates <> stk
                | i == to -> crates <> stk
                | otherwise -> stk
        )
        state

solve :: Bool -> ByteString -> String
solve shouldReverse input =
  let (state, rest) = readState input
      moveLines = drop 2 (BS.lines rest)
      moves = map readMove moveLines
      finalState = foldl' (applyMove shouldReverse) state moves
   in V.toList $ V.map BS.head finalState

part1 :: ByteString -> String
part1 = solve True

part2 :: ByteString -> String
part2 = solve False
