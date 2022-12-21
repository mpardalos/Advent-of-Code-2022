module Day20 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Debug.Trace
import Text.Printf (printf)

swap :: Int -> Int -> Vector a -> Vector a
swap n1 n2 = V.modify $ \vec -> do
  let n1' = n1 `mod` MV.length vec
  let n2' = n2 `mod` MV.length vec
  v1 <- MV.read vec n1'
  v2 <- MV.read vec n2'
  MV.write vec n1' v2
  MV.write vec n2' v1

slide :: Int -> Int -> Vector a -> Vector a
slide start diff vec =
  snd $ iterate (\(i, v) -> (i + 1, swap i (i + 1) v)) (start, vec) !! slideSteps
  where
    slideSteps
      | start + diff >= V.length vec = - (V.length vec - diff `mod` V.length vec)
      | otherwise = diff `mod` (V.length vec - 1)

-- \| diff < 0 = diff `mod` V.length vec - 1
-- \| otherwise = diff `mod` V.length vec

mix1 :: Int -> Vector (a, Int) -> Vector (a, Int)
mix1 idx v =
  let result = slide idx (snd $ v V.! idx) v
   in result

unmix1 :: Int -> Vector (a, Int) -> Vector (a, Int)
unmix1 idx v = slide idx (-(snd $ v V.! idx)) v

mix :: Vector Int -> Vector Int
mix v0 = V.map snd $ go 0 (V.zip (V.enumFromN 0 (V.length v0)) v0)
  where
    -- go :: Int -> Vector (Int, Int) -> Vector (Int, Int)
    go n v
      | trace (V.foldMap (\(i, x) -> printf (if i == n then "[%d] " else "%d ") x) v) False = undefined
    go n v
      | n == V.length v = v
      | otherwise =
          let Just i = V.findIndex ((== n) . fst) v
           in go (n + 1) (mix1 i v)

    m = V.length v0

    -- TODO: This does not work with wrapping moves
    adjustIndex :: Int -> Int -> Int -> Int
    adjustIndex movedIdx diff idx
      | diff > 0 && movedIdx < idx && idx <= movedIdx + (diff `mod` m) = idx - 1
      | diff < 0 && (movedIdx + diff) `mod` m <= idx && idx < movedIdx `mod` m = idx + 1
      | otherwise = idx

part1 :: ByteString -> Int
part1 input =
  BS.lines input
    & map (fst . fromJust . BS.readInt)
    & sum

part2 :: ByteString -> Int
part2 = const 0
