module Main where

import           Solutions ( solutions, Solution(..) )
import           Text.Printf   ( printf )
import           Control.Monad ( forM_ )
import qualified Data.ByteString.Char8 as BS

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
    input <- BS.readFile ("data/" <> inputFile)
    printLine name (show $ solution input)
  printTableAnchor False
