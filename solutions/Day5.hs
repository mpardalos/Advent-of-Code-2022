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
readStateLine initialState line = foldl' addNumberedCrateToState initialState numberedCrates
  where
    crateCharPositions = [1, 5 .. BS.length line - 2]
    numberedCrates = zip [0 ..] (map (BS.index line) crateCharPositions)

    addNumberedCrateToState state (_, ' ') = state
    addNumberedCrateToState state (i, crate) = imapN i (`BS.snoc` crate) state

readState :: [ByteString] -> (State, [ByteString])
readState inputLines = (state, rest)
  where
    (stateLines, rest) = span (\l -> BS.index l 1 /= '1') inputLines
    numColumns = BS.length (head inputLines) `div` 4 + 1
    state = foldl' readStateLine (V.replicate numColumns BS.empty) stateLines

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
  let (state, restLines) = readState (BS.lines input)
      moves = map readMove (drop 2 restLines)
      finalState = foldl' (applyMove shouldReverse) state moves
   in V.toList $ V.map BS.head finalState

part1 :: ByteString -> String
part1 = solve True

part2 :: ByteString -> String
part2 = solve False
