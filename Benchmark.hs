module Main where

import           Solutions
import           Criterion.Main

main :: IO ()
main = defaultMain
  [ env (readFile ("data/" <> inputName)) $ \input ->
      bench name (whnf solution input)
  | MkSolution name solution inputName <- solutions
  ]
