module Main where

import           Solutions
import           Criterion.Main
import qualified Data.Map.Strict as Map
import           Data.Map.Strict(Map, (!))
import           System.Directory
import           Control.Monad (forM)

main :: IO ()
main = do
  inputs <- do
    filenames <- listDirectory "data/"
    list <- forM filenames $ \filename -> do
      contents <- readFile ("data/" <> filename)
      return (filename, contents)
    return (Map.fromList list)

  defaultMain
    $ map (\(MkSolution name solution inputName) -> bench name $ whnf solution (inputs ! inputName))
    $ solutions
