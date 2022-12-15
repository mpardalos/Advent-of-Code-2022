{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# OPTIONS -fprof-auto #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day11 (part1, part2) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.IntMap (IntMap)
import Data.List (iterate', sort)
import Data.List.Split
import Data.Massiv.Array (Vector)
import Data.Massiv.Array qualified as M
import Data.Maybe (fromJust)

type MonkeyId = Int

type WorryValue = Int

data Monkey = Monkey
  { number :: MonkeyId,
    items :: Vector M.P WorryValue,
    operation :: WorryValue -> WorryValue,
    test :: WorryValue -> MonkeyId,
    testModulo :: Int,
    itemsInspected :: Int
  }

readInput :: ByteString -> Vector M.B Monkey
readInput = M.fromList M.Par . map readMonkey . splitOn [""] . BS.lines

readIntPartial :: ByteString -> Int
readIntPartial = fst . fromJust . BS.readInt

readMonkey :: [ByteString] -> Monkey
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
        items = M.fromList M.Par $ map (fromIntegral . readIntPartial) $ BS.splitWith (== ' ') $ BS.drop 18 itemsLine,
        operation = operation,
        test = \n -> if n `mod` testModulo == 0 then ifTrueTarget else ifFalseTarget,
        testModulo = testModulo,
        itemsInspected = 0
      }
    where
      operation = case (operationLine `BS.index` 23, operationLine `BS.index` 25) of
        ('*', 'o') -> \n -> n * n
        ('+', 'o') -> \n -> n + n
        ('*', _) -> \n -> n * operand
        ('+', _) -> \n -> n + operand
        _ -> error "Invalid input"

      -- should only be used if the second operand is not 'old'
      operand = fromIntegral (readIntPartial (BS.drop 25 operationLine))
      testModulo = fromIntegral $ readIntPartial $ BS.drop 21 testLine
      ifTrueTarget = fromIntegral $ readIntPartial $ BS.drop 29 ifTrueLine
      ifFalseTarget = fromIntegral $ readIntPartial $ BS.drop 30 ifFalseLine
readMonkey _ = error "Invalid input"

runMonkeyPart1 :: Monkey -> Vector M.U (MonkeyId, WorryValue)
runMonkeyPart1 Monkey {items, operation, test} =
  M.compute $
    M.map
      ( \worryVal ->
          let newWorryVal = operation worryVal `div` 3
           in (test newWorryVal, newWorryVal)
      )
      items

-- The critical insight here is that all arithmetic can be done modulo the
-- product of all the moduli used for the monkeys' tests
runMonkeyPart2 :: Int -> Monkey -> Vector M.U (MonkeyId, WorryValue)
runMonkeyPart2 modulo Monkey {items, operation, test} =
  M.compute $
    M.map
      ( \worryVal ->
          let newWorryVal = operation worryVal `mod` modulo
           in (test newWorryVal, newWorryVal)
      )
      items

giveItem :: WorryValue -> Monkey -> Monkey
giveItem x Monkey {items, itemsInspected, ..} = Monkey {items = M.compute $ M.snoc items x, ..}

removeAllItems :: Monkey -> Monkey
removeAllItems Monkey {..} = Monkey {items = M.empty, ..}

addItemsInspected :: Int -> Monkey -> Monkey
addItemsInspected n Monkey {itemsInspected, ..} = Monkey {itemsInspected = itemsInspected + n, ..}

adjust :: M.Source r a => Int -> (a -> a) -> Vector r a -> Vector M.D a
adjust idx f = M.imap (\i v -> if i == idx then f v else v)

-- accumulate :: M.Source r a =>

runRound :: (Monkey -> Vector M.U (MonkeyId, WorryValue)) -> Vector M.B Monkey -> Vector M.B Monkey
runRound runMonkey = M.compute . go 0
  where
    go :: MonkeyId -> Vector M.B Monkey -> Vector M.B Monkey
    go idx monkeys
      | not $ M.isSafeIndex (M.size monkeys) idx = monkeys
      | otherwise =
          let thrownItems = runMonkey (M.evaluate' monkeys idx)
           in monkeys
                & adjust idx (addItemsInspected (M.unSz $ M.size thrownItems) . removeAllItems)
                & accumulate (flip giveItem) thrownItems
                & go (idx + 1)

accumulate ::
  (M.Index ix, M.Index ix', M.Source ixR (ix, b), M.Load srcR ix a, M.Manifest destR a) =>
  (a -> b -> a) ->
  M.Array ixR ix' (ix, b) ->
  M.Array srcR ix a ->
  M.Array destR ix a
accumulate f targets src = M.withLoadMArrayST_ src $ \msrc ->
  M.forM_ targets $ \(i, accVal) -> do
    oldVal <- M.readM msrc i
    M.write_ msrc i (f oldVal accVal)

part1 :: ByteString -> Int
part1 =
  product
    . take 2
    . reverse
    . sort
    . M.toList
    . M.map itemsInspected
    . (!! 20)
    . iterate (runRound runMonkeyPart1)
    . readInput

part2 :: ByteString -> Int
part2 input =
  iterate' (runRound (runMonkeyPart2 modulo)) monkeys !! 10000
    & M.map itemsInspected
    & M.toList
    & sort
    & reverse
    & take 2
    & product
  where
    monkeys = readInput input
    modulo = product $ map testModulo $ M.toList monkeys
