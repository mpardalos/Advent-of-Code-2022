{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# OPTIONS -fprof-auto #-}

module Day11 (part1, part2) where

import Control.Monad (replicateM_)
import Control.Monad.ST (ST, runST)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Functor.Identity (Identity (..))
import Data.List (iterate', sort)
import Data.List.Split
import Data.Maybe (fromJust)
import Data.STRef
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace

type MonkeyId = Int

type WorryValue = Int

data Monkey = Monkey
  { number :: MonkeyId,
    items :: Vector WorryValue,
    operation :: WorryValue -> WorryValue,
    test :: WorryValue -> MonkeyId,
    testModulo :: Int,
    itemsInspected :: Int
  }

readInput :: ByteString -> Vector Monkey
readInput = V.fromList . map readMonkey . splitOn [""] . BS.lines

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
        items = V.fromList $ map (fromIntegral . readIntPartial) $ BS.splitWith (== ' ') $ BS.drop 18 itemsLine,
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

runMonkeyPart1 :: Monkey -> Vector (MonkeyId, WorryValue)
runMonkeyPart1 Monkey {items, operation, test} =
  V.map
    ( \worryVal ->
        let newWorryVal = operation worryVal `div` 3
         in (test newWorryVal, newWorryVal)
    )
    items

-- The critical insight here is that all arithmetic can be done modulo the
-- product of all the moduli used for the monkeys' tests
runMonkeyPart2 :: Int -> Monkey -> Vector (MonkeyId, WorryValue)
runMonkeyPart2 modulo Monkey {items, operation, test} =
  V.map
    ( \worryVal ->
        let newWorryVal = operation worryVal `mod` modulo
         in (test newWorryVal, newWorryVal)
    )
    items

giveItem :: WorryValue -> Monkey -> Monkey
giveItem x Monkey {items, itemsInspected, ..} = Monkey {items = V.snoc items x, ..}

removeAllItems :: Monkey -> Monkey
removeAllItems Monkey {..} = Monkey {items = V.empty, ..}

addItemsInspected :: Int -> Monkey -> Monkey
addItemsInspected n Monkey {itemsInspected, ..} = Monkey {itemsInspected = itemsInspected + n, ..}

adjust :: Int -> (a -> a) -> Vector a -> Vector a
adjust idx f v = V.accumulate (\x _ -> f x) v (V.singleton (idx, ()))

runRound :: (Monkey -> Vector (MonkeyId, WorryValue)) -> Vector Monkey -> Vector Monkey
runRound runMonkey = go 0
  where
    go idx monkeys
      | idx >= V.length monkeys = monkeys
      | otherwise =
          let thrownItems = runMonkey (monkeys V.! idx)
           in go
                (idx + 1)
                ( adjust idx (addItemsInspected (V.length thrownItems) . removeAllItems) $
                    V.accumulate (flip giveItem) monkeys thrownItems
                )

part1 :: ByteString -> Int
part1 =
  product
    . take 2
    . reverse
    . sort
    . V.toList
    . V.map itemsInspected
    . (!! 20)
    . iterate (runRound runMonkeyPart1)
    . readInput

part2 :: ByteString -> Int
part2 input = solution monkeys
  where
    solution =
      product
        . take 2
        . reverse
        . sort
        . V.toList
        . V.map itemsInspected
        . (!! 10000)
        . iterate' (runRound (runMonkeyPart2 modulo))

    monkeys = readInput input
    modulo = product $ map testModulo $ V.toList monkeys
