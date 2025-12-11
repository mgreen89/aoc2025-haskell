module AoC.Challenge.Day11 (
  day11a,
  day11b,
)
where

import AoC.Solution
import AoC.Util (maybeToEither)
import Data.List (uncons)
import Data.Map (Map)
import qualified Data.Map as M

-- Parse input like:
-- hhh: ccc fff iii
parse :: String -> Either String (Map String [String])
parse =
  maybeToEither "invalid input"
    . fmap M.fromList
    . traverse (fmap (\(x, xs) -> (init x, xs)) . uncons . words)
    . lines

-- Get # of paths from start to end in the given DAG.
paths :: Map String [String] -> String -> String -> Int
paths dag start end =
  M.findWithDefault 0 start m
 where
  m :: Map String Int
  m = fmap (sum . fmap countPaths) dag

  countPaths :: String -> Int
  countPaths s
    | s == end = 1
    | otherwise = M.findWithDefault 0 s m

day11a :: Solution (Map String [String]) Int
day11a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . (\dag -> paths dag "you" "out")
    }

solveB :: Map String [String] -> Int
solveB devices =
  ( paths devices "svr" "fft"
      * paths devices "fft" "dac"
      * paths devices "dac" "out"
  )
    + ( paths devices "svr" "dac"
          * paths devices "dac" "fft"
          * paths devices "fft" "out"
      )

day11b :: Solution (Map String [String]) Int
day11b = Solution{sParse = parse, sShow = show, sSolve = Right . solveB}
