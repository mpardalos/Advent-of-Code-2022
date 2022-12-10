{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day8 (part1, part2) where

import Control.Monad (forM_, when)
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
part1 input = V.length $ V.filter id visibleMarkers
  where
    (size, forest) = readInput input
    index row col = size * row + col

    visibleMarkers =
      V.zipWith4
        (\e w n s -> e || w || n || s)
        eastVisible
        westVisible
        northVisible
        southVisible

    eastVisible = runST $ do
      eastVisibleMut <- MV.replicate (V.length forest) False
      forM_ [0 .. size - 1] $ \row -> do
        maxSeenRef <- newSTRef (-1)
        forM_ [0 .. size - 1] $ \col ->
          loopBody maxSeenRef eastVisibleMut row col
      V.freeze eastVisibleMut

    westVisible = runST $ do
      westVisibleMut <- MV.replicate (V.length forest) False
      forM_ [0 .. size - 1] $ \row -> do
        maxSeenRef <- newSTRef (-1)
        forM_ (reverse [0 .. size - 1]) $ \col -> do
          loopBody maxSeenRef westVisibleMut row col
      V.freeze westVisibleMut

    northVisible = runST $ do
      northVisibleMut <- MV.replicate (V.length forest) False
      forM_ [0 .. size - 1] $ \col -> do
        maxSeenRef <- newSTRef (-1)
        forM_ [0 .. size - 1] $ \row ->
          loopBody maxSeenRef northVisibleMut row col
      V.freeze northVisibleMut

    southVisible = runST $ do
      southVisibleMut <- MV.replicate (V.length forest) False
      forM_ [0 .. size - 1] $ \col -> do
        maxSeenRef <- newSTRef (-1)
        forM_ (reverse [0 .. size - 1]) $ \row ->
          loopBody maxSeenRef southVisibleMut row col
      V.freeze southVisibleMut

    loopBody :: STRef s Int -> MVector s Bool -> Int -> Int -> ST s ()
    loopBody maxSeenRef outputVec row col = do
      let height = forest V.! index row col
      maxSeen <- readSTRef maxSeenRef
      when (height > maxSeen) $ do
        writeSTRef maxSeenRef height
        MV.write outputVec (index row col) True

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
part2 input = V.maximum scenicScores
  where
    (size, forest) = readInput input
    index row col = size * row + col

    scenicScores = V.zipWith4 (\w e n s -> w * e * n * s) west east north south

    -- Keep track of the index at which a height was last seen. Each tree can
    -- see back to the index of the closest tree equal to or taller than itself
    west = runST $ do
      westMut <- MV.replicate (V.length forest) 0
      forM_ [0 .. size - 1] $ \row -> do
        lastSeenVec <- MV.replicate maxHeight 0
        forM_ [0 .. size - 1] $ \col -> do
          let height = forest V.! index row col
          closestBlockingIndex <- V.maximum <$> V.freeze (MV.drop height lastSeenVec)
          MV.write lastSeenVec height col
          MV.write westMut (index row col) (col - closestBlockingIndex)
      V.freeze westMut

    east = runST $ do
      eastMut <- MV.replicate (V.length forest) 0
      forM_ [0 .. size - 1] $ \row -> do
        lastSeenVec <- MV.replicate maxHeight (size - 1)
        forM_ (reverse [0 .. size - 1]) $ \col -> do
          let height = forest V.! index row col
          closestBlockingIndex <- V.minimum <$> V.freeze (MV.drop height lastSeenVec)
          MV.write lastSeenVec height col
          MV.write eastMut (index row col) (closestBlockingIndex - col)
      V.freeze eastMut

    north = runST $ do
      northMut <- MV.replicate (V.length forest) 0
      forM_ [0 .. size - 1] $ \col -> do
        lastSeenVec <- MV.replicate maxHeight 0
        forM_ [0 .. size - 1] $ \row -> do
          let height = forest V.! index row col
          closestBlockingIndex <- V.maximum <$> V.freeze (MV.drop height lastSeenVec)
          MV.write lastSeenVec height row
          MV.write northMut (index row col) (row - closestBlockingIndex)
      V.freeze northMut

    south = runST $ do
      southMut <- MV.replicate (V.length forest) 0
      forM_ [0 .. size - 1] $ \col -> do
        lastSeenVec <- MV.replicate 10 (size - 1)
        forM_ (reverse [0 .. size - 1]) $ \row -> do
          let height = forest V.! index row col
          closestBlockingIndex <- V.minimum <$> V.freeze (MV.drop height lastSeenVec)
          MV.write lastSeenVec height row
          MV.write southMut (index row col) (closestBlockingIndex - row)
      V.freeze southMut
