module AoC.Challenge.Day03 (
  day03a,
  day03b,
)
where

import AoC.Solution
import Data.Char (digitToInt)
import Data.List (tails)

day03a :: Solution [[Int]] Int
day03a =
  Solution
    { sParse = Right . fmap (fmap digitToInt) . lines
    , sShow = show
    , sSolve = Right . sum . fmap go
    }
 where
  go xs =
    maximum
      [ x * 10 + y
      | x : ys <- tails xs
      , y <- ys
      ]

pick :: [(Int, [Int])] -> [(Int, [Int])]
pick ps =
  [ (tot * 10 + x, ys)
  | (tot, xs) <- ps
  , -- Prefer high digits!
  n <- [9, 8 .. 1]
  , x : ys <- tails xs
  , x == n
  ]

solveB :: [[Int]] -> Int
solveB =
  sum . fmap go
 where
  go :: [Int] -> Int
  go xs =
    fst . (!! 0) . (!! 12) . iterate pick $ [(0, xs)]

day03b :: Solution [[Int]] Int
day03b =
  Solution
    { sParse = Right . fmap (fmap digitToInt) . lines
    , sShow = show
    , sSolve = Right . solveB
    }
