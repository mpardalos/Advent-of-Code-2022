{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day16 (part1, part2) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Graph (Graph, Vertex)
import Data.Graph qualified as Graph
import Data.Maybe (fromJust)
import Debug.Trace

type NodeKey = ByteString

type FlowRate = Int

type InputNode = (FlowRate, NodeKey, [NodeKey])

parseLine :: ByteString -> InputNode
parseLine =
  (\case Right l -> l; Left e -> error ("Cannot parse: " <> e))
    . P.parseOnly
      ( do
          _ <- P.string "Valve "
          nodeKey <- P.take 2

          _ <- P.string " has flow rate="
          flowRate <- P.decimal

          _ <-
            P.string "; tunnels lead to valves "
              <|> P.string "; tunnel leads to valve "
          edges <- (P.take 2) `P.sepBy` (P.string ", ")

          return (flowRate, nodeKey, edges)
      )

constructGraph :: [InputNode] -> (Graph, Vertex -> (FlowRate, NodeKey, [NodeKey]), NodeKey -> Maybe Vertex)
constructGraph = Graph.graphFromEdges

part1 :: ByteString -> Int
part1 input =
  BS.lines input
    & map parseLine
    & constructGraph
    & const 0

part2 :: ByteString -> Int
part2 = const 0
