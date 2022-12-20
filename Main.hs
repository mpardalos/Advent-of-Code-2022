{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (forM_)
import Data.ByteString.Char8 qualified as BS
import Data.List (isInfixOf)
import Solutions (DisplaySolution (displaySolution), Solution (..), solutions)
import System.Clock
import System.Environment (getArgs)
import System.Exit (exitFailure)
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
    "%s─%s─%s────────%s───────────\n"
    (if top then "┌" else "└")
    (replicate titleLength '─')
    lineT
    lineT
  where
    lineT = if top then "┬" else "┴"

printLine :: String -> TimeSpec -> String -> IO ()
printLine name time answer =
  printf
    "│ %*s │ %6s │ %s \n"
    titleLength
    name
    formattedTime
    -- If the answer spans multiple lines, align it all in the right column of the table
    ( concatMap
        ( \case
            '\n' -> printf "\n│ %*s │        │ " titleLength ""
            c -> [c]
        )
        answer
    )
  where
    timeNanos :: Integer
    timeNanos = toNanoSecs time

    formattedTime :: String
    formattedTime
      | timeNanos < 1e3 = printf "%d ns" timeNanos
      | timeNanos < 1e6 = printf "%d μs" (timeNanos `div` 1e3)
      | timeNanos < 1e9 = printf "%d ms" (timeNanos `div` 1e6)
      | otherwise = printf "%d  s" (timeNanos `div` 1e9)

main :: IO ()
main = do
  filterFun <-
    getArgs >>= \case
      [] -> return id
      [filterString] -> return $ filter $ \(MkSolution name _ _) ->
        filterString `isInfixOf` name
      _ -> putStrLn "Too many arguments" >> exitFailure

  let filteredSolutions = filterFun solutions

  printTableAnchor True
  forM_ filteredSolutions $ \(MkSolution name solution inputFile) -> do
    input <- BS.readFile ("data/" <> inputFile)

    startTime <- getTime Monotonic
    answer <-
      evaluate (displaySolution $ solution input)
        `catch` \(e :: SomeException) -> pure (show e)
    timeElapsed <- diffTimeSpec startTime <$> getTime Monotonic

    printLine name timeElapsed answer
  printTableAnchor False
