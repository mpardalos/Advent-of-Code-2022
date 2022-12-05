module Main where

import Criterion.Main
  ( bench,
    defaultConfig,
    defaultMainWith,
    env,
    whnf,
  )
import Criterion.Types (Config (..))
import Data.ByteString.Char8 qualified as BS
import Solutions (Solution (MkSolution), solutions)

main :: IO ()
main =
  defaultMainWith
    defaultConfig {reportFile = Just "benchmark.html"}
    [ env (BS.readFile ("data/" <> inputName)) $ \input ->
        bench name (whnf solution input)
      | MkSolution name solution inputName <- solutions
    ]
