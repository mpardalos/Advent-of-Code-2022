module Util where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.Set qualified as Set

parseOrError :: Parser a -> ByteString -> a
parseOrError parser input = case parseOnly parser input of
  Left e -> error ("Parse error: " <> show e)
  Right v -> v

hashNub :: Hashable a => [a] -> [a]
hashNub = HashSet.toList . HashSet.fromList

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList
