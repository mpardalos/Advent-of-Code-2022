{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day19 (part1, part2) where

import Control.Monad (guard)
import Control.Parallel.Strategies (parMap, rseq)
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Hashable
import GHC.Generics (Generic)
import Safe (maximumDef)
import Util

{-

  +-<-+       +---------------------+
 /     \     /                       \
+- ore -+->-+------------+- obsidian -+ geode
             \          /
              +- clay -+

ore robots need ore
clay robots need ore
obsidian robots need ore and clay
geode robots need ore and obsidian

-}

data Blueprint = Blueprint
  { blueprintId :: Int,
    oreRobotOreCost :: Int,
    clayRobotOreCost :: Int,
    obsidianRobotOreCost :: Int,
    obsidianRobotClayCost :: Int,
    geodeRobotOreCost :: Int,
    geodeRobotObsidianCost :: Int
  }
  deriving (Show)

parseLine :: ByteString -> Blueprint
parseLine = parseOrError $ do
  _ <- P.string "Blueprint "
  blueprintId <- P.decimal
  _ <- P.string ": Each ore robot costs "
  oreRobotOreCost <- P.decimal
  _ <- P.string " ore. Each clay robot costs "
  clayRobotOreCost <- P.decimal
  _ <- P.string " ore. Each obsidian robot costs "
  obsidianRobotOreCost <- P.decimal
  _ <- P.string " ore and "
  obsidianRobotClayCost <- P.decimal
  _ <- P.string " clay. Each geode robot costs "
  geodeRobotOreCost <- P.decimal
  _ <- P.string " ore and "
  geodeRobotObsidianCost <- P.decimal
  _ <- P.string " obsidian."
  return Blueprint {..}

data State = State
  { minute :: Int,
    ore :: Int,
    clay :: Int,
    obsidian :: Int,
    geodes :: Int,
    oreRobots :: Int,
    clayRobots :: Int,
    obsidianRobots :: Int,
    geodeRobots :: Int
  }
  deriving (Show, Eq, Ord, Generic, Hashable)

initialState :: State
initialState =
  State
    { minute = 0,
      ore = 0,
      clay = 0,
      obsidian = 0,
      geodes = 0,
      oreRobots = 1,
      clayRobots = 0,
      obsidianRobots = 0,
      geodeRobots = 0
    }

data RobotType
  = Ore
  | Clay
  | Obsidian
  | Geode
  deriving (Show)

step :: Blueprint -> Int -> State -> [State]
step Blueprint {..} maxGeodes (State {..}) = do
  guard (obsidianRobots <= geodeRobotObsidianCost)
  guard (clayRobots <= obsidianRobotClayCost)
  guard (oreRobots <= maximum @[] [clayRobotOreCost, obsidianRobotOreCost, geodeRobotOreCost])
  guard (geodes >= maxGeodes - 1)

  robotToMake <-
    if
        | ore >= geodeRobotOreCost && obsidian >= geodeRobotObsidianCost -> [Just Geode]
        | ore >= obsidianRobotOreCost && clay >= obsidianRobotClayCost -> [Just Obsidian]
        | otherwise ->
            concat
              [ [Just Clay | ore >= clayRobotOreCost],
                [Just Ore | ore >= oreRobotOreCost],
                [Nothing]
              ]

  let oreRobotsMade = case robotToMake of Just Ore -> 1; _ -> 0
  let clayRobotsMade = case robotToMake of Just Clay -> 1; _ -> 0
  let obsidianRobotsMade = case robotToMake of Just Obsidian -> 1; _ -> 0
  let geodeRobotsMade = case robotToMake of Just Geode -> 1; _ -> 0

  let claySpent = case robotToMake of Just Obsidian -> obsidianRobotClayCost; _ -> 0
  let obsidianSpent = case robotToMake of Just Geode -> geodeRobotObsidianCost; _ -> 0
  let oreSpent = case robotToMake of
        Just Ore -> oreRobotOreCost
        Just Clay -> clayRobotOreCost
        Just Obsidian -> obsidianRobotOreCost
        Just Geode -> geodeRobotOreCost
        Nothing -> 0

  return
    State
      { minute = minute + 1,
        ore = ore + oreRobots - oreSpent,
        clay = clay + clayRobots - claySpent,
        obsidian = obsidian + obsidianRobots - obsidianSpent,
        geodes = geodes + geodeRobots,
        oreRobots = oreRobots + oreRobotsMade,
        clayRobots = clayRobots + clayRobotsMade,
        obsidianRobots = obsidianRobots + obsidianRobotsMade,
        geodeRobots = geodeRobots + geodeRobotsMade
      }

stepAll :: Blueprint -> [State] -> [State]
stepAll blueprint states =
  hashNub $ concatMap (step blueprint maxGeodes) states
  where
    maxGeodes = maximumDef 0 $ map geodes states

part1 :: ByteString -> Int
part1 input =
  BS.lines input
    & map parseLine
    & parMap
      rseq
      ( \blueprint ->
          iterate (stepAll blueprint) [initialState]
            & (!! 24)
            & fmap geodes
            & maximum
            & (* blueprintId blueprint)
      )
    & sum

part2 :: ByteString -> Int
part2 input =
  BS.lines input
    & map parseLine
    & take 3
    & parMap
      rseq
      ( \blueprint ->
          iterate (stepAll blueprint) [initialState]
            & (!! 32)
            & fmap geodes
            & maximum
      )
    & product
