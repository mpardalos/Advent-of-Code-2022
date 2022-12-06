{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Day5 (part1, part2) where

import Control.Monad.ST
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

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
    fromStack :: Int,
    toStack :: Int
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
   in Move {count, fromStack = fromRead - 1, toStack = toRead - 1}

applyMove :: Bool -> State -> Move -> State
applyMove shouldReverse state Move {..} = V.modify mutation state
  where
    mutation :: forall s. MV.MVector s ByteString -> ST s ()
    mutation mutState = do
      cratesTakenRef <- newSTRef Nothing
      MV.modifyM
        mutState
        ( \stack -> do
            let (cratesTaken, cratesLeft) = BS.splitAt count stack
            writeSTRef cratesTakenRef (Just cratesTaken)
            return cratesLeft
        )
        fromStack
      cratesTaken <- fromJust <$> readSTRef cratesTakenRef
      if shouldReverse
        then MV.modify mutState (BS.append (BS.reverse cratesTaken)) toStack
        else MV.modify mutState (BS.append cratesTaken) toStack

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
