{-# OPTIONS -fdefer-typed-holes #-}
module Main where

import           Solutions
import           Text.Printf   (printf)
import           Control.Monad (forM_)

main :: IO ()
main = do
  forM_ solutions $ \(MkSolution name solution inputFile) -> do
    input <- readFile ("data/" <> inputFile)
    printf "%s: %s\n" name (show $ solution input)
