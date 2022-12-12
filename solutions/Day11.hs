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
import Data.List (sort)
import Data.List.Split
import Data.Maybe (fromJust)
import Data.STRef
import Data.Vector (Vector)
import Data.Vector qualified as V

type MonkeyId = Int

type WorryValue = Int

data GMonkey f = Monkey
  { number :: MonkeyId,
    items :: f (Vector WorryValue),
    operation :: WorryValue -> WorryValue,
    test :: WorryValue -> MonkeyId,
    testModulo :: Int,
    itemsInspected :: f Int
  }

type Monkey = GMonkey Identity

type MutMonkey s = GMonkey (STRef s)

freeze :: MutMonkey s -> ST s Monkey
freeze Monkey {items, itemsInspected, ..} = do
  itemsValue <- readSTRef items
  itemsInspectedValue <- readSTRef itemsInspected
  return Monkey {items = Identity itemsValue, itemsInspected = Identity itemsInspectedValue, ..}

thaw :: Monkey -> ST s (MutMonkey s)
thaw Monkey {items, itemsInspected, ..} = do
  itemsRef <- newSTRef (runIdentity items)
  itemsInspectedRef <- newSTRef (runIdentity itemsInspected)
  return Monkey {items = itemsRef, itemsInspected = itemsInspectedRef, ..}

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
        items = Identity $ V.fromList $ map (fromIntegral . readIntPartial) $ BS.splitWith (== ' ') $ BS.drop 18 itemsLine,
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
    (runIdentity items)

-- The critical insight here is that all arithmetic can be done modulo the
-- product of all the moduli used for the monkeys' tests
runMonkeyPart2 :: Int -> Monkey -> Vector (MonkeyId, WorryValue)
runMonkeyPart2 modulo Monkey {items, operation, test} =
  V.map
    ( \worryVal ->
        let newWorryVal = operation worryVal `mod` modulo
         in (test newWorryVal, newWorryVal)
    )
    (runIdentity items)

runRound :: (Monkey -> Vector (MonkeyId, WorryValue)) -> Vector (MutMonkey s) -> ST s ()
runRound runMonkey monkeys = do
  V.forM_ monkeys $ \monkey -> do
    thrownItems <- runMonkey <$> freeze monkey
    writeSTRef (items monkey) V.empty
    modifySTRef (itemsInspected monkey) (+ V.length thrownItems)
    V.forM_ thrownItems $ \(toMonkeyIdx, worryValue) ->
      modifySTRef (items (monkeys V.! toMonkeyIdx)) (`V.snoc` worryValue)

part1 :: ByteString -> Int
part1 input = product $ take 2 $ reverse $ sort $ V.toList $ V.map (runIdentity . itemsInspected) finalMonkeys
  where
    initialMonkeys = readInput input
    finalMonkeys = runST $ do
      monkeysMut <- V.mapM thaw initialMonkeys
      replicateM_ 20 (runRound runMonkeyPart1 monkeysMut)
      V.mapM freeze monkeysMut

part2 :: ByteString -> Int
part2 input = product $ take 2 $ reverse $ sort $ V.toList $ V.map (runIdentity . itemsInspected) finalMonkeys
  where
    initialMonkeys = readInput input
    modulo = product $ map testModulo $ V.toList initialMonkeys

    finalMonkeys = runST $ do
      monkeysMut <- V.mapM thaw initialMonkeys
      replicateM_ 10000 (runRound (runMonkeyPart2 modulo) monkeysMut)
      V.mapM freeze monkeysMut
