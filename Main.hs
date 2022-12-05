module Main where

import Control.Monad (forM_)
import Data.ByteString.Char8 qualified as BS
import Solutions (Solution (..), solutions)
import Text.Printf (printf)

titleLength :: Int
titleLength =
  maximum
    [ length name
      | MkSolution name _ _ <- solutions
    ]

printTableAnchor :: Bool -> IO ()
printTableAnchor top =
  printf
    "%s─%s─%s───────────\n"
    (if top then "┌" else "└")
    (replicate titleLength '─')
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
