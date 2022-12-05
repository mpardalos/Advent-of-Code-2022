module Main where

import           Solutions
import           Criterion.Main
import           Criterion.Types (Config(..))
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = defaultMainWith
  defaultConfig { reportFile = Just "benchmark.html" }
  [ env (BS.readFile ("data/" <> inputName)) $ \input ->
      bench name (whnf solution input)
  | MkSolution name solution inputName <- solutions
  ]
