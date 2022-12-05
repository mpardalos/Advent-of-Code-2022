module Solutions(Solution(..),solutions) where

import Day1
import Day2
import Day3
import Day4
import           Control.DeepSeq
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString (ByteString)

data Solution where
  MkSolution :: (NFData a, Show a) => String -> (ByteString -> a) -> FilePath -> Solution

solutions :: [Solution]
solutions =
  [ MkSolution "Day 1 part 1" Day1.part1 "day1"
  , MkSolution "Day 1 part 2" Day1.part2 "day1"
  , MkSolution "Day 2 part 1" Day2.part1 "day2"
  , MkSolution "Day 2 part 2" Day2.part2 "day2"
  , MkSolution "Day 3 part 1" Day3.part1 "day3"
  , MkSolution "Day 3 part 2" Day3.part2 "day3"
  , MkSolution "Day 4 part 1" Day4.part1 "day4"
  , MkSolution "Day 4 part 2" Day4.part2 "day4"
  ]
