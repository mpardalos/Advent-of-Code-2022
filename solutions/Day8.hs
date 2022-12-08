{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
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

readInput :: ByteString -> (Int, Vector (Bool, Int))
readInput input = (length rows, forest)
  where
    forest = V.fromList $ map (False,) $ concat rows
    rows = map (map ord . BS.unpack) $ BS.lines input

part1 :: HasCallStack => ByteString -> Int
part1 input = length $ filter fst $ V.toList visibleForest
  where
    (size, initialForest) = readInput input
    visibleForest = V.modify markVisible initialForest

    index row col = size * row + col

    markVisiblePass :: forall s. STRef s Int -> MVector s (Bool, Int) -> Int -> ST s ()
    markVisiblePass maxSeenRef forest idx =
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

    markVisible :: forall s. MVector s (Bool, Int) -> ST s ()
    markVisible forest = do
      -- Rows, left-to-right
      forM_ [0 .. size - 1] $ \row -> do
        maxSeenRef <- newSTRef 0
        forM_ [0 .. size - 1] $ \col ->
          markVisiblePass maxSeenRef forest (index row col)

      -- Rows, right-to-left
      forM_ [0 .. size - 1] $ \row -> do
        maxSeenRef <- newSTRef 0
        forM_ (reverse [0 .. size - 1]) $ \col ->
          markVisiblePass maxSeenRef forest (index row col)

      -- Columns, top-to-bottom
      forM_ [0 .. size - 1] $ \col -> do
        maxSeenRef <- newSTRef 0
        forM_ [0 .. size - 1] $ \row ->
          markVisiblePass maxSeenRef forest (index row col)

      -- Columns, bottom-to-top
      forM_ [0 .. size - 1] $ \col -> do
        maxSeenRef <- newSTRef 0
        forM_ (reverse [0 .. size - 1]) $ \row ->
          markVisiblePass maxSeenRef forest (index row col)

part2 :: ByteString -> Int
part2 = const 0

-- visibleTreesInLine :: Foldable t => t Int -> Int
-- visibleTreesInLine = fst . foldl' go (0, 0)
--   where
--     go :: (Int, Int) -> Int -> (Int, Int)
--     go (count, maxSeen) tree
--       | tree > maxSeen = (count + 1, tree)
--       | otherwise = (count, maxSeen)
