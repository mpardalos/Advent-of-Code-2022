{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Day8 (part1, part2) where

import Control.Monad (forM_)
import Control.Monad.ST
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (ord)
import Data.Foldable (Foldable (..))
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as MV
import Debug.Trace
import GHC.Stack (HasCallStack)
import Text.Printf (printf)

readInput :: ByteString -> (Int, Vector Int)
readInput input = (length rows, V.fromList $ concat rows)
  where
    rows = map (map readDigit . BS.unpack) $ BS.lines input
    readDigit '0' = 0
    readDigit '1' = 1
    readDigit '2' = 2
    readDigit '3' = 3
    readDigit '4' = 4
    readDigit '5' = 5
    readDigit '6' = 6
    readDigit '7' = 7
    readDigit '8' = 8
    readDigit '9' = 9

part1 :: HasCallStack => ByteString -> Int
part1 input = length $ filter fst $ V.toList visibleForest
  where
    (size, initialForest) = readInput input
    visibleForest = V.modify markExternallyVisible $ V.map (False,) initialForest

    index row col = size * row + col

    markExternallyVisiblePass :: forall s. STRef s Int -> MVector s (Bool, Int) -> Int -> ST s ()
    markExternallyVisiblePass maxSeenRef forest idx =
      MV.modifyM
        forest
        ( \(alreadyVisible, height) -> do
            maxSeen <- readSTRef maxSeenRef
            if height > maxSeen
              then do
                writeSTRef maxSeenRef height
                return (True, height)
              else return (alreadyVisible, height)
        )
        idx

    markExternallyVisible :: forall s. MVector s (Bool, Int) -> ST s ()
    markExternallyVisible forest = do
      -- Rows, left-to-right
      forM_ [0 .. size - 1] $ \row -> do
        maxSeenRef <- newSTRef 0
        forM_ [0 .. size - 1] $ \col ->
          markExternallyVisiblePass maxSeenRef forest (index row col)

      -- Rows, right-to-left
      forM_ [0 .. size - 1] $ \row -> do
        maxSeenRef <- newSTRef 0
        forM_ (reverse [0 .. size - 1]) $ \col ->
          markExternallyVisiblePass maxSeenRef forest (index row col)

      -- Columns, top-to-bottom
      forM_ [0 .. size - 1] $ \col -> do
        maxSeenRef <- newSTRef 0
        forM_ [0 .. size - 1] $ \row ->
          markExternallyVisiblePass maxSeenRef forest (index row col)

      -- Columns, bottom-to-top
      forM_ [0 .. size - 1] $ \col -> do
        maxSeenRef <- newSTRef 0
        forM_ (reverse [0 .. size - 1]) $ \row ->
          markExternallyVisiblePass maxSeenRef forest (index row col)

data ScenicScores = ScenicScores
  { west :: Int,
    north :: Int,
    east :: Int,
    south :: Int
  }

instance Show ScenicScores where
  show (ScenicScores w n e s) = printf "%2d*%2d*%2d*%2d" w n e s

summarizeScore :: ScenicScores -> Int
summarizeScore (ScenicScores w n e s) = w * n * e * s

maxHeight :: Int
maxHeight = 10

showForest :: Show a => Int -> Vector (a, Int) -> String
showForest size forest =
  let (row, rest) = V.splitAt size forest
   in if V.length row > 0
        then "| " ++ show row ++ " |\n" ++ showForest size rest
        else ""

part2 :: HasCallStack => ByteString -> Int
part2 input = maximum $ map (summarizeScore . fst) $ V.toList scenicForest
  where
    (size, initialForest) = readInput input
    scenicForest = V.modify countVisible $ V.map (ScenicScores 0 0 0 0,) initialForest

    index row col = size * row + col

    -- Keep track of the index at which a height was last seen. Each tree can
    -- see back to the index of the closest tree equal to or taller than itself
    countVisible :: forall s. MVector s (ScenicScores, Int) -> ST s ()
    countVisible forest = do
      -- Rows, left-to-right
      forM_ [0 .. size - 1] $ \row -> do
        lastSeenVec <- MV.replicate maxHeight 0
        forM_ [0 .. size - 1] $ \col ->
          MV.modifyM
            forest
            ( \(scenicScores, height) -> do
                closestBlockingIndex <- maximum <$> V.freeze (MV.drop height lastSeenVec)
                let directionalScenicScore = col - closestBlockingIndex
                MV.write lastSeenVec height col
                return (scenicScores {west = directionalScenicScore}, height)
            )
            (index row col)

      -- Rows, right-to-left
      forM_ [0 .. size - 1] $ \row -> do
        lastSeenVec <- MV.replicate maxHeight (size - 1)
        forM_ (reverse [0 .. size - 1]) $ \col ->
          MV.modifyM
            forest
            ( \(scenicScores, height) -> do
                closestBlockingIndex <- minimum <$> V.freeze (MV.drop height lastSeenVec)
                let directionalScenicScore = closestBlockingIndex - col
                MV.write lastSeenVec height col
                return (scenicScores {east = directionalScenicScore}, height)
            )
            (index row col)

      -- Columns, top-to-bottom
      forM_ [0 .. size - 1] $ \col -> do
        lastSeenVec <- MV.replicate maxHeight 0
        forM_ [0 .. size - 1] $ \row ->
          MV.modifyM
            forest
            ( \(scenicScores, height) -> do
                closestBlockingIndex <- maximum <$> V.freeze (MV.drop height lastSeenVec)
                let directionalScenicScore = row - closestBlockingIndex
                MV.write lastSeenVec height row
                return (scenicScores {north = directionalScenicScore}, height)
            )
            (index row col)

      -- Columns, bottom-to-top
      forM_ [0 .. size - 1] $ \col -> do
        lastSeenVec <- MV.replicate 10 (size - 1)
        forM_ (reverse [0 .. size - 1]) $ \row ->
          MV.modifyM
            forest
            ( \(scenicScores, height) -> do
                closestBlockingIndex <- minimum <$> V.freeze (MV.drop height lastSeenVec)
                let directionalScenicScore = closestBlockingIndex - row
                MV.write lastSeenVec height row
                return (scenicScores {south = directionalScenicScore}, height)
            )
            (index row col)
