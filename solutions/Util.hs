module Util where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)

parseOrError :: Parser a -> ByteString -> a
parseOrError parser input = case parseOnly parser input of
  Left e -> error ("Parse error: " <> show e)
  Right v -> v
