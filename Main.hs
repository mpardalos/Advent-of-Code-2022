{-# OPTIONS_GHC -Wno-typed-holes #-}
module Main where

import           Solutions ( solutions, Solution(..) )
import           Text.Printf   ( printf )
import           Control.Monad ( forM_ )

titleLength :: Int
titleLength = maximum [ length name
                      | MkSolution name _ _ <- solutions
                      ]

printTableAnchor :: Bool -> IO ()
printTableAnchor top =
  printf "%s─%s─%s───────────\n"
    (if top then "┌" else "└")
    (take titleLength $ repeat '─')
    (if top then "┬" else "┴")

printLine :: String -> String -> IO ()
printLine = printf "│ %*s │ %s \n" titleLength

main :: IO ()
main = do
  printTableAnchor True
  forM_ solutions $ \(MkSolution name solution inputFile) -> do
    input <- readFile ("data/" <> inputFile)
    printLine name (show $ solution input)
  printTableAnchor False
