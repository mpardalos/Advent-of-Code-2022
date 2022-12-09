{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Day8 (part1, part2) where

import Control.Monad (forM_, zipWithM_)
import Control.Monad.ST
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (ord)
import Data.Foldable (Foldable (..))
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (MVector)
import Data.Vector.Unboxed.Mutable qualified as MV
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
  { west :: {-# UNPACK #-} !Int,
    north :: {-# UNPACK #-} !Int,
    east :: {-# UNPACK #-} !Int,
    south :: {-# UNPACK #-} !Int
  }

instance GM.MVector MVector ScenicScores where
  basicLength :: MVector s ScenicScores -> Int
  basicLength (MV_ScenicScores v) = GM.basicLength v `div` 3

  basicUnsafeSlice :: Int -> Int -> MVector s ScenicScores -> MVector s ScenicScores
  basicUnsafeSlice a b (MV_ScenicScores v) = MV_ScenicScores $ GM.basicUnsafeSlice (a * 4) (b * 4) v

  basicOverlaps :: MVector s ScenicScores -> MVector s ScenicScores -> Bool
  basicOverlaps (MV_ScenicScores v0) (MV_ScenicScores v1) = GM.basicOverlaps v0 v1

  basicUnsafeNew :: Int -> ST s (MVector s ScenicScores)
  basicUnsafeNew n = MV_ScenicScores <$> GM.basicUnsafeNew (4 * n)

  basicUnsafeRead :: MVector s ScenicScores -> Int -> ST s ScenicScores
  basicUnsafeRead (MV_ScenicScores v) n' = do
    [w, n, e, s] <- mapM (GM.basicUnsafeRead v) [4 * n', 4 * n' + 1, 4 * n' + 2, 4 * n' + 3]
    return $ ScenicScores w n e s

  basicUnsafeWrite :: MVector s ScenicScores -> Int -> ScenicScores -> ST s ()
  basicUnsafeWrite (MV_ScenicScores v) n' (ScenicScores w n e s) = zipWithM_ (GM.basicUnsafeWrite v) [4 * n', 4 * n' + 1, 4 * n' + 2, 4 * n' + 3] [w, n, e, s]

instance G.Vector Vector ScenicScores where
  basicUnsafeFreeze :: G.Mutable Vector s ScenicScores -> ST s (Vector ScenicScores)
  basicUnsafeFreeze (MV_ScenicScores v) = V_ScenicScores <$> G.basicUnsafeFreeze v

  basicUnsafeThaw :: Vector ScenicScores -> ST s (G.Mutable Vector s ScenicScores)
  basicUnsafeThaw (V_ScenicScores v) = MV_ScenicScores <$> G.basicUnsafeThaw v

  basicLength :: Vector ScenicScores -> Int
  basicLength (V_ScenicScores v) = G.basicLength v `div` 3

  basicUnsafeSlice :: Int -> Int -> Vector ScenicScores -> Vector ScenicScores
  basicUnsafeSlice a b (V_ScenicScores v) = V_ScenicScores $ G.basicUnsafeSlice (a * 3) (b * 3) v

  basicUnsafeIndexM (V_ScenicScores v) n' = do
    mapM (G.basicUnsafeIndexM v) [4 * n', 4 * n' + 1, 4 * n' + 2, 4 * n' + 3] >>= \case
      [w, n, e, s] -> return $ ScenicScores w n e s
      _ -> undefined

newtype instance MVector s ScenicScores = MV_ScenicScores (MVector s Int)

newtype instance Vector ScenicScores = V_ScenicScores (Vector Int)

instance V.Unbox ScenicScores

instance Show ScenicScores where
  show (ScenicScores w n e s) = printf "%2d*%2d*%2d*%2d" w n e s

summarizeScore :: ScenicScores -> Int
summarizeScore (ScenicScores w n e s) = w * n * e * s

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
          MV.unsafeModifyM
            forest
            ( \(scenicScores, height) -> do
                closestBlockingIndex <- V.maximum <$> V.freeze (MV.drop height lastSeenVec)
                let directionalScenicScore = col - closestBlockingIndex
                MV.write lastSeenVec height col
                return (scenicScores {west = directionalScenicScore}, height)
            )
            (index row col)

      -- Rows, right-to-left
      forM_ [0 .. size - 1] $ \row -> do
        lastSeenVec <- MV.replicate maxHeight (size - 1)
        forM_ (reverse [0 .. size - 1]) $ \col ->
          MV.unsafeModifyM
            forest
            ( \(scenicScores, height) -> do
                closestBlockingIndex <- V.minimum <$> V.freeze (MV.drop height lastSeenVec)
                let directionalScenicScore = closestBlockingIndex - col
                MV.write lastSeenVec height col
                return (scenicScores {east = directionalScenicScore}, height)
            )
            (index row col)

      -- Columns, top-to-bottom
      forM_ [0 .. size - 1] $ \col -> do
        lastSeenVec <- MV.replicate maxHeight 0
        forM_ [0 .. size - 1] $ \row ->
          MV.unsafeModifyM
            forest
            ( \(scenicScores, height) -> do
                closestBlockingIndex <- V.maximum <$> V.freeze (MV.drop height lastSeenVec)
                let directionalScenicScore = row - closestBlockingIndex
                MV.write lastSeenVec height row
                return (scenicScores {north = directionalScenicScore}, height)
            )
            (index row col)

      -- Columns, bottom-to-top
      forM_ [0 .. size - 1] $ \col -> do
        lastSeenVec <- MV.replicate 10 (size - 1)
        forM_ (reverse [0 .. size - 1]) $ \row ->
          MV.unsafeModifyM
            forest
            ( \(scenicScores, height) -> do
                closestBlockingIndex <- V.minimum <$> V.freeze (MV.drop height lastSeenVec)
                let directionalScenicScore = closestBlockingIndex - row
                MV.write lastSeenVec height row
                return (scenicScores {south = directionalScenicScore}, height)
            )
            (index row col)
