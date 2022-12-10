{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (forM_)
import Data.ByteString.Char8 qualified as BS
import Solutions (DisplaySolution (displaySolution), Solution (..), solutions)
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
printLine name answer =
  printf
    "│ %*s │ %s \n"
    titleLength
    name
    -- If the answer spans multiple lines, align it all in the right column of the table
    ( concatMap
        ( \case
            '\n' -> printf "\n│ %*s │ " titleLength ""
            c -> [c]
        )
        answer
    )

main :: IO ()
main = do
  printTableAnchor True
  forM_ solutions $ \(MkSolution name solution inputFile) -> do
    input <- BS.readFile ("data/" <> inputFile)
    answer <- evaluate (displaySolution $ solution input) `catch` \(e :: SomeException) -> pure (show e)
    printLine name answer
  printTableAnchor False
