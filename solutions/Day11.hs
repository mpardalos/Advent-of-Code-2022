{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day11 (part1, part2) where

import Control.Monad (replicateM_)
import Control.Monad.ST (ST, runST)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.List (sort)
import Data.List.Split
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable qualified as MV
import Data.Vector.Mutable (MVector)
import GHC.Stack (HasCallStack)
import Text.Printf (printf)

type MonkeyId = Int

type WorryValue = Integer

data Monkey = Monkey
  { number :: MonkeyId,
    items :: Vector WorryValue,
    operation :: WorryValue -> WorryValue,
    test :: WorryValue -> MonkeyId,
    itemsInspected :: Int
  }

instance Show Monkey where
  show Monkey {..} = printf "Monkey { number = %d, items = %s, ... }" number (show items)

readInput :: ByteString -> Vector Monkey
readInput = V.fromList . map readMonkey . splitOn [""] . BS.lines

readIntPartial :: HasCallStack => ByteString -> Int
readIntPartial = fst . fromJust . BS.readInt

readMonkey :: HasCallStack => [ByteString] -> Monkey
readMonkey
  [ numLine,
    itemsLine,
    operationLine,
    testLine,
    ifTrueLine,
    ifFalseLine
    ] =
    Monkey
      { number = readIntPartial $ BS.drop 7 numLine,
        items = V.fromList $ map (fromIntegral . readIntPartial) $ BS.splitWith (== ' ') $ BS.drop 18 itemsLine,
        operation = operation,
        test = \n -> if n `mod` divisor == 0 then ifTrueTarget else ifFalseTarget,
        itemsInspected = 0
      }
    where
      operationArithmetic = case operationLine `BS.index` 23 of
        '*' -> (*)
        '+' -> (+)
        c -> error ("Unknown operation: " <> show c)

      operation = case operationLine `BS.index` 25 of
        'o' -> \n -> n `operationArithmetic` n
        _ -> \n -> n `operationArithmetic` fromIntegral (readIntPartial (BS.drop 25 operationLine))

      divisor = fromIntegral $ readIntPartial $ BS.drop 21 testLine
      ifTrueTarget = fromIntegral $ readIntPartial $ BS.drop 29 ifTrueLine
      ifFalseTarget = fromIntegral $ readIntPartial $ BS.drop 30 ifFalseLine
readMonkey _ = error "Invalid input"

runMonkeyPart1 :: Monkey -> Vector (MonkeyId, WorryValue)
runMonkeyPart1 Monkey {items, operation, test} =
  V.map
    ( \worryVal ->
        let newWorryVal = operation worryVal `div` 3
         in (test newWorryVal, newWorryVal)
    )
    items

runMonkeyPart2 :: Monkey -> Vector (MonkeyId, WorryValue)
runMonkeyPart2 Monkey {items, operation, test} =
  V.map
    ( \worryVal ->
        let newWorryVal = operation worryVal
         in (test newWorryVal, newWorryVal)
    )
    items

runRound :: (Monkey -> Vector (MonkeyId, WorryValue)) -> MVector s Monkey -> ST s ()
runRound runMonkey monkeys = do
  MV.iforM_ monkeys $ \i monkey -> do
    let thrownItems = runMonkey monkey
    MV.modify
      monkeys
      ( \Monkey {itemsInspected, ..} ->
          Monkey
            { items = V.empty,
              itemsInspected = itemsInspected + V.length thrownItems,
              ..
            }
      )
      i
    V.forM_ thrownItems $ \(toMonkey, worryValue) -> do
      MV.modify
        monkeys
        ( \Monkey {items, itemsInspected, ..} ->
            Monkey {items = V.snoc items worryValue, ..}
        )
        toMonkey

part1 :: ByteString -> Int
part1 input =
  product $ take 2 $ reverse $ sort $ V.toList $ V.map itemsInspected finalMonkeys
  where
    initialMonkeys = readInput input
    finalMonkeys = runST $ do
      monkeysMut <- V.thaw initialMonkeys
      replicateM_ 20 (runRound runMonkeyPart1 monkeysMut)
      V.freeze monkeysMut

part2 :: ByteString -> Int
part2 input = product $ take 2 $ reverse $ sort $ V.toList $ V.map itemsInspected finalMonkeys
  where
    initialMonkeys = readInput input
    finalMonkeys = runST $ do
      monkeysMut <- V.thaw initialMonkeys
      replicateM_ 10000 (runRound runMonkeyPart2 monkeysMut)
      V.freeze monkeysMut
