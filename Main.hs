{-# OPTIONS_GHC -Wno-typed-holes #-}
module Main where

import           Solutions
import           Text.Printf   (printf)
import           Control.Monad (forM_)
import           Data.Foldable

titleLength :: Int
titleLength = maximum [ length name
                      | MkSolution name _ _ <- solutions
                      ]

printTableAnchor :: Bool -> IO ()
printTableAnchor top = do
  putStr (if top then "┌" else "└")
  putStrLn $ take (15 + titleLength) $ repeat '─'

printLine :: String -> String -> IO ()
printLine = printf "│ %*s │ %s \n" titleLength

main :: IO ()
main = do
  printTableAnchor True
  forM_ solutions $ \(MkSolution name solution inputFile) -> do
    input <- readFile ("data/" <> inputFile)
    printLine name (show $ solution input)
  printTableAnchor False
