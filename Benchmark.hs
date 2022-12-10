module Main where

import Control.Monad (forM)
import Criterion.Main
  ( bench,
    bgroup,
    defaultConfig,
    defaultMainWith,
    env,
    nf,
    whnf,
  )
import Criterion.Types (Config (..))
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map
import Solutions (DisplaySolution (..), Solution (MkSolution), solutions)

allInputs :: IO (Map String ByteString)
allInputs = fmap Map.fromList $ forM solutions $ \(MkSolution _ _ inputName) -> do
  content <- BS.readFile ("data/" <> inputName)
  return (inputName, content)

main :: IO ()
main =
  defaultMainWith
    defaultConfig {reportFile = Just "benchmark.html"}
    [ bgroup
        "solutions"
        [ env (BS.readFile ("data/" <> inputName)) $ \input ->
            bench name (whnf solution input)
          | MkSolution name solution inputName <- solutions
        ],
      env allInputs $ \inputs ->
        bench "All together" $
          nf
            (map (\(MkSolution _ solution inputName) -> displaySolution $ solution (inputs Map.! inputName)))
            solutions
    ]
