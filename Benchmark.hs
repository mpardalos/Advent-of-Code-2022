module Main where

import           Solutions
import           Criterion.Main
import           Criterion.Types (Config(..))

main :: IO ()
main = defaultMainWith
  defaultConfig { reportFile = Just "benchmark.html" }
  [ env (readFile ("data/" <> inputName)) $ \input ->
      bench name (whnf solution input)
  | MkSolution name solution inputName <- solutions
  ]
