{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day8 (part1, part2) where

import Control.Monad (forM_)
import Control.Monad.ST
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (MVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import GHC.Stack (HasCallStack)

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
    readDigit c = error ("Not a digit: " ++ [c])

part1 :: HasCallStack => ByteString -> Int
part1 input = length $ filter fst $ V.toList visibleForest
  where
    (size, initialForest) = readInput input
    visibleForest = V.modify markExternallyVisible $ V.map (False,) initialForest

    index row col = size * row + col

    markExternallyVisiblePass :: forall s. STRef s Int -> MVector s (Bool, Int) -> Int -> Int -> ST s ()
    markExternallyVisiblePass maxSeenRef forest row col =
      MV.unsafeModifyM
        forest
        ( \(alreadyVisible, height) -> do
            maxSeen <- readSTRef maxSeenRef
            if height > maxSeen
              then do
                writeSTRef maxSeenRef height
                return (True, height)
              else return (alreadyVisible, height)
        )
        (index row col)

    markExternallyVisible :: forall s. MVector s (Bool, Int) -> ST s ()
    markExternallyVisible forest = do
      -- Rows, left-to-right
      forM_ [0 .. size - 1] $ \row -> do
        maxSeenRef <- newSTRef 0
        forM_ [0 .. size - 1] $ \col ->
          markExternallyVisiblePass maxSeenRef forest row col

      -- Rows, right-to-left
      forM_ [0 .. size - 1] $ \row -> do
        maxSeenRef <- newSTRef 0
        forM_ (reverse [0 .. size - 1]) $ \col ->
          markExternallyVisiblePass maxSeenRef forest row col

      -- Columns, top-to-bottom
      forM_ [0 .. size - 1] $ \col -> do
        maxSeenRef <- newSTRef 0
        forM_ [0 .. size - 1] $ \row ->
          markExternallyVisiblePass maxSeenRef forest row col

      -- Columns, bottom-to-top
      forM_ [0 .. size - 1] $ \col -> do
        maxSeenRef <- newSTRef 0
        forM_ (reverse [0 .. size - 1]) $ \row ->
          markExternallyVisiblePass maxSeenRef forest row col

type ScenicScores = (Int, Int, Int, Int)

summarizeScore :: ScenicScores -> Int
summarizeScore (w, n, e, s) = w * n * e * s

maxHeight :: Int
maxHeight = 10

showForest :: (MV.Unbox a, Show a) => Int -> Vector (a, Int) -> String
showForest size forest =
  let (row, rest) = V.splitAt size forest
   in if V.length row > 0
        then "| " ++ show row ++ " |\n" ++ showForest size rest
        else ""

part2 :: HasCallStack => ByteString -> Int
part2 input = maximum $ map (summarizeScore . fst) $ V.toList scenicForest
  where
    (size, initialForest) = readInput input
    scenicForest = V.modify countVisible $ V.map ((0, 0, 0, 0),) initialForest

    index row col = size * row + col

    -- Keep track of the index at which a height was last seen. Each tree can
    -- see back to the index of the closest tree equal to or taller than itself
    countVisible :: forall s. MVector s (ScenicScores, Int) -> ST s ()
    countVisible forest = do
      -- Rows, left-to-right
      forM_ [0 .. size - 1] $ \row -> do
        lastSeenVec <- MV.replicate maxHeight 0
        forM_ [0 .. size - 1] $ \col ->
          MV.unsafeModifyM
            forest
            ( \((_, n, e, s), height) -> do
                closestBlockingIndex <- V.maximum <$> V.freeze (MV.drop height lastSeenVec)
                let directionalScenicScore = col - closestBlockingIndex
                MV.write lastSeenVec height col
                return ((directionalScenicScore, n, e, s), height)
            )
            (index row col)

      -- Rows, right-to-left
      forM_ [0 .. size - 1] $ \row -> do
        lastSeenVec <- MV.replicate maxHeight (size - 1)
        forM_ (reverse [0 .. size - 1]) $ \col ->
          MV.unsafeModifyM
            forest
            ( \((w, n, _, s), height) -> do
                closestBlockingIndex <- V.minimum <$> V.freeze (MV.drop height lastSeenVec)
                let directionalScenicScore = closestBlockingIndex - col
                MV.write lastSeenVec height col
                return ((w, n, directionalScenicScore, s), height)
            )
            (index row col)

      -- Columns, top-to-bottom
      forM_ [0 .. size - 1] $ \col -> do
        lastSeenVec <- MV.replicate maxHeight 0
        forM_ [0 .. size - 1] $ \row ->
          MV.unsafeModifyM
            forest
            ( \((w, _, e, s), height) -> do
                closestBlockingIndex <- V.maximum <$> V.freeze (MV.drop height lastSeenVec)
                let directionalScenicScore = row - closestBlockingIndex
                MV.write lastSeenVec height row
                return ((w, directionalScenicScore, e, s), height)
            )
            (index row col)

      -- Columns, bottom-to-top
      forM_ [0 .. size - 1] $ \col -> do
        lastSeenVec <- MV.replicate 10 (size - 1)
        forM_ (reverse [0 .. size - 1]) $ \row ->
          MV.unsafeModifyM
            forest
            ( \((w, n, e, _), height) -> do
                closestBlockingIndex <- V.minimum <$> V.freeze (MV.drop height lastSeenVec)
                let directionalScenicScore = closestBlockingIndex - row
                MV.write lastSeenVec height row
                return ((w, n, e, directionalScenicScore), height)
            )
            (index row col)
