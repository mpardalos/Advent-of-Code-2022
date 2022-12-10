{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (SomeException, catch, evaluate)
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
    answer <- evaluate (show $ solution input) `catch` \(e :: SomeException) -> pure (show e)
    printLine name answer
  printTableAnchor False
